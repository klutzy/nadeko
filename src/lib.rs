#![crate_type = "dylib"]
#![crate_name = "nadeko"]

#![feature(plugin_registrar, asm, rustc_private)]

//#[phase(plugin, link)]
extern crate log;

extern crate arena;
extern crate syntax;
extern crate rustc;

use syntax::ast;
use syntax::codemap::Span;
use syntax::parse::token;
use syntax::ext::expand;
use syntax::ext::base::Annotatable;
use syntax::ext::base::ExtCtxt;
use syntax::ext::base::SyntaxExtension;
use syntax::fold::Folder;
use rustc::plugin::Registry;

mod trans;
pub mod asm;

#[plugin_registrar]
pub fn plugin_registar(reg: &mut Registry) {
    reg.register_syntax_extension(token::intern("const_time"),
                                  SyntaxExtension::MultiModifier(Box::new(nadeko)));
}

fn nadeko<'a>(cx: &mut ExtCtxt<'a>,
              sp: Span,
              mi: &ast::MetaItem,
              orig_item: Annotatable) -> Annotatable {
    match mi.node {
        ast::MetaWord(ref _ns) => {}
        _ => {
            cx.span_err(sp, "expected `#[const_time]` without any parameter");
            return orig_item;
        }
    }

    // we have to expand macros before trans.

    if let Annotatable::Item(orig) = orig_item {
        let expanded_item = {
            let mut expander = expand::MacroExpander::new(cx);
            expander.fold_item(orig).expect_one("macro expanded into multiple items")
        };

        let new_item = {
            let mut folder = trans::TransFolder::new(cx);

            folder.trans_item(expanded_item)
        };

        Annotatable::Item(new_item)
    } else {
        orig_item
    }
}
