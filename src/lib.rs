#![crate_type = "dylib"]
#![crate_name = "nadeko"]

#![feature(macro_rules, plugin_registrar, phase, quote, slicing_syntax)]

#[phase(plugin, link)]
extern crate log;

extern crate arena;
extern crate syntax;
extern crate rustc;
extern crate rustc_typeck;
extern crate rustc_back;
extern crate rustc_trans;
extern crate rustc_driver;

use arena::TypedArena;
use syntax::ast;
use syntax::ast::DUMMY_NODE_ID as DID;
use syntax::ast_map;
use syntax::codemap::{Span, DUMMY_SP};
use syntax::ptr::P;
use syntax::parse::token;
use syntax::parse::token::InternedString;
use syntax::ext::base::ExtCtxt;
use syntax::ext::base::{SyntaxExtension};
use syntax::owned_slice::OwnedSlice;
use rustc::plugin::Registry;
use rustc::middle::ty;
use rustc_driver::driver;
use rustc::middle;
use rustc::middle::stability;
use rustc::metadata::creader;

mod trans;

#[plugin_registrar]
pub fn plugin_registar(reg: &mut Registry) {
    reg.register_syntax_extension(token::intern("const_time"),
                                  SyntaxExtension::Modifier(box nadeko));
}

fn nadeko<'a>(cx: &mut ExtCtxt<'a>,
              sp: Span,
              mi: &ast::MetaItem,
              orig_item: P<ast::Item>) -> P<ast::Item> {
    match mi.node {
        ast::MetaWord(ref _ns) => {}
        _ => {
            cx.span_err(sp, "expected `#[const_time]` without any parameter");
            return orig_item;
        }
    }

    let mod_ = match (*orig_item).node {
        ast::ItemMod(ref mod_) => mod_.clone(),
        _ => {
            cx.span_err(sp, "`#[const_time]` on non-mod item");
            return orig_item.clone();
        }
    };

    // to convert functions into assembly code, we have to run *typechecker*
    // because we need to know the type of `(a as u32) * (b as u32)`.
    // but we are in phase 2 and typeck will be run at phase 3.
    //
    // so here's a *crazy* idea: run own rustc to get typecheck result!
    // to achieve this, we first prepare own crate from `orig_item`.
    // then we invoke `rustc_typeck::check_crate` to get inference table.
    // now we have typechecked ast, so we can generate new module.
    //
    // in practice, there are issues regarding invoking rustc inside rustc:
    // rustc uses several `thread_local!` tables, and destroying them will
    // lead ICE.
    // for example, during phase 3, rustc driver purges tables which is used
    // in phase 1 and 2. this is why we *can't* call
    // `rustc_driver::driver::phase_3_run_analysis_passes` directly.
    // we have to be sure other rustc methods do not destroy the table,
    //
    // the more stable solution would be invoking rustc in a separate thread.
    // this is basically TODO FIXME issue right now.

    let sopts = rustc::session::config::basic_options();
    let descriptions = syntax::diagnostics::registry::Registry::new(&[]);
    let mut sess = rustc::session::build_session(sopts, None, descriptions);
    sess.parse_sess.span_diagnostic.cm.files = cx.parse_sess.span_diagnostic.cm.files.clone();

    // this crate does not use core/std.

    let mut new_krate = ast::Crate {
        attrs: Vec::new(),
        config: Vec::new(),
        span: mod_.inner,
        exported_macros: Vec::new(),
        module: mod_,
    };

    // now phase 1: parsing is done. woohoo!
    // phase 2: configure/expand

    // TODO: expand macros!

    // inject lang items

    fn lang_attr(name: &'static str) -> ast::Attribute {
        let lit = ast::Lit {
            node: ast::LitStr(InternedString::new(name), ast::CookedStr),
            span: DUMMY_SP,
        };
        let item = P(ast::MetaItem {
            node: ast::MetaNameValue(InternedString::new("lang"), lit),
            span: DUMMY_SP,
        });

        syntax::attr::mk_attr_inner(syntax::attr::mk_attr_id(), item)
    }

    fn lang_item(lang_name: &'static str,
                 trait_name: &'static str,
                 ty_params: Vec<ast::TyParam>) -> ast::Item {
        let generics = ast::Generics {
            lifetimes: Vec::new(),
            ty_params: OwnedSlice::from_vec(ty_params),
            where_clause: ast::WhereClause {
                id: DID,
                predicates: Vec::new(),
            },
        };

        let node = ast::ItemTrait(generics, None, OwnedSlice::empty(), Vec::new());

        let item = ast::Item {
            ident: token::str_to_ident(trait_name),
            attrs: vec!(lang_attr(lang_name)),
            id: DID,
            node: node,
            vis: ast::Inherited,
            span: DUMMY_SP,
        };

        item
    }

    // `for Sized?`
    let sized_ty_param = ast::TyParam {
        ident: token::str_to_ident("Sized"),
        id: DID,
        bounds: OwnedSlice::empty(),
        unbound: None,
        default: None,
        span: DUMMY_SP,
    };
    let sized_item = lang_item("sized", "Sized", vec!(sized_ty_param));
    new_krate.module.items.push(P(sized_item));

    let sync_item = lang_item("sync", "Sync", Vec::new());
    new_krate.module.items.push(P(sync_item));

    // now the crate is ready to build. run rustc type checker.

    let mut forest = ast_map::Forest::new(new_krate);
    let ast_map = driver::assign_node_ids_and_map(&sess, &mut forest);

    let type_arena = arena::TypedArena::new();
    let analysis = phase_3_run_analysis_passes(sess, ast_map, &type_arena);

    // instead of usual trans, we translate ast into new ast!
    let new_mod = phase_4_trans(cx, &analysis, &*orig_item);
    new_mod
}

/// Run the resolution, typechecking, region checking and other
/// miscellaneous analysis passes on the crate. Return various
/// structures carrying the results of the analysis.
pub fn phase_3_run_analysis_passes<'tcx>(sess: rustc::session::Session,
                                         ast_map: ast_map::Map<'tcx>,
                                         type_arena: &'tcx TypedArena<ty::TyS<'tcx>>) -> ty::ctxt<'tcx> {

    let krate = ast_map.krate();

    creader::read_crates(&sess, krate);
    let lang_items = middle::lang_items::collect_language_items(krate, &sess);

    let middle::resolve::CrateMap {
        def_map,
        freevars,
        capture_mode_map,
        trait_map,
        ..
    } = middle::resolve::resolve_crate(&sess, &lang_items, krate);

    // `rustc_driver::driver::phase_3_run_analysis_passes` *removes* tables
    // which are importantly used after this syntax extension finishes.
    // syntax::ext::mtwt::clear_tables();

    // TODO: we are skipping some non-essential passes, but not sure if it affects typeck..

    let named_region_map = middle::resolve_lifetime::krate(&sess, krate, &def_map);
    let region_map = middle::region::resolve_crate(&sess, krate);
    let stability_index = stability::Index::build(krate);

    let ty_cx = ty::mk_ctxt(sess,
                            type_arena,
                            def_map,
                            named_region_map,
                            ast_map,
                            freevars,
                            capture_mode_map,
                            region_map,
                            lang_items,
                            stability_index);

    rustc_typeck::check_crate(&ty_cx, trait_map);

    // ok, this is all what we need now.
    ty_cx
}

fn phase_4_trans<'a: 'b, 'b>(cx: &mut ExtCtxt<'a>,
                             ty_cx: &ty::ctxt<'b>,
                             orig_item: &ast::Item) -> P<ast::Item> {
    let new_items = trans::trans_mod(cx, ty_cx);

    let new_mod = ast::Mod {
        inner: orig_item.span,
        view_items: Vec::new(),
        items: new_items,
    };

    let new_root_item = ast::Item {
        ident: orig_item.ident,
        attrs: orig_item.attrs.clone(),
        id: DID,
        node: ast::ItemMod(new_mod),
        vis: orig_item.vis,
        span: orig_item.span,
    };
    P(new_root_item)
}
