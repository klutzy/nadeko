#![crate_type = "dylib"]
#![crate_name = "nadeko"]

#![feature(macro_rules, plugin_registrar, phase, globs, asm)]

#[phase(plugin, link)]
extern crate log;

extern crate arena;
extern crate syntax;
extern crate rustc;

use syntax::ast;
use syntax::codemap::Span;
use syntax::ptr::P;
use syntax::parse::token;
use syntax::ext::expand;
use syntax::ext::base::ExtCtxt;
use syntax::ext::base::SyntaxExtension;
use syntax::fold::Folder;
use rustc::plugin::Registry;

mod trans;
pub mod asm;

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

    // we have to expand macros before trans.

    let expanded_item = {
        let mut expander = expand::MacroExpander::new(cx);
        expander.fold_item(orig_item).expect_one("macro expanded into multiple items")
    };

    let new_item = {
        let mut folder = trans::TransFolder::new(cx);

        folder.trans_item(expanded_item)
    };

    new_item
}
