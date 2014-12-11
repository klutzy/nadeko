// translate function items

use syntax::ast;
use syntax::ast::{Item, NodeId, Block};
use syntax::ast::DUMMY_NODE_ID as DID;
use syntax::codemap;
use syntax::codemap::Span;
use syntax::ptr::P;
use syntax::parse::token::InternedString;
use syntax::util::small_vector::SmallVector;
use syntax::parse::token;
use syntax::ext::base::ExtCtxt;
use syntax::fold::Folder;
use rustc::middle::ty::{mod, ctxt, sty};

pub fn trans_mod<'a, 'b>(cx: &mut ExtCtxt<'a>, tcx: &ctxt<'b>) -> Vec<P<Item>> {
    let items = tcx.map.krate().module.items[];

    let mut folder = TransFolder {
        cx: cx,
        ctxt: tcx,
    };
    folder.trans_items(items)
}

// this is somewhat messy.. isn't 1~2 lts sufficient?
struct TransFolder<'a, 'b: 'a, 'c: 'a> {
    cx: &'a mut ExtCtxt<'b>,
    ctxt: &'a ctxt<'c>,
}

impl<'a, 'b, 'c> Folder for TransFolder<'a, 'b, 'c> {
    // We're generting ast with DUMMY_NODE_ID from one with proper node id.
    fn new_id(&mut self, _i: ast::NodeId) -> ast::NodeId {
        DID
    }

    fn fold_item(&mut self, item: P<Item>) -> SmallVector<P<Item>> {
        let it = self.trans_item(item);
        match it {
            Some(it) => SmallVector::one(it),
            None => SmallVector::zero(),
        }
    }
}

impl<'a, 'b, 'c> TransFolder<'a, 'b, 'c> {
    fn trans_items(&mut self, items: &[P<ast::Item>]) -> Vec<P<ast::Item>> {
        let mut new_items = Vec::new();
        for item in items.iter() {
            match self.trans_item(item.clone()) {
                Some(i) => new_items.push(i),
                None => {}
            }
        }
        new_items
    }

    fn trans_item(&mut self, item: P<ast::Item>) -> Option<P<ast::Item>> {
        let new_node = match item.node {
            ast::ItemFn(ref decl, fn_style, abi, ref generics, ref body) => {
                if generics.is_parameterized() {
                    self.cx.span_err(item.span, "generics are not supported");
                    return None;
                }

                let new_fn_block = self.trans_block(&**body, item.span);

                let new_decl = self.fold_fn_decl(decl.clone());
                let new_generics = self.fold_generics(generics.clone());

                let item_fn = ast::ItemFn(new_decl, fn_style, abi, new_generics, P(new_fn_block));

                item_fn
            }
            ast::ItemStatic(..) | ast::ItemConst(..) => {
                self.cx.span_err(item.span, "const/static not yet implemeneted");
                return None;
            }
            ast::ItemTrait(..) => {
                // TODO error for any trait if not lang item
                // this is cumbersome since we will eventually inject lang items
                return None;
            }
            _ => {
                self.cx.span_err(item.span, "not supported");
                return None;
            }
        };

        let new_item = ast::Item {
            ident: item.ident,
            attrs: item.attrs.iter().map(|s| self.fold_attribute(s.clone())).collect(),
            id: DID,
            node: new_node,
            vis: item.vis,
            span: item.span,
        };
        Some(P(new_item))
    }

    fn trans_block(&mut self, block: &ast::Block, sp: Span) -> ast::Block {
        let mut blk_cx = BlkCx::new(self, sp);

        {
            for stmt in block.stmts.iter() {
                blk_cx.trans_stmt(&**stmt);
            }

            if let Some(ref expr) = block.expr {
                let final_expr = blk_cx.trans_expr(&**expr);
                blk_cx.final_expr = Some(final_expr);
            }
        }

        let new_block = blk_cx.into_block();

        new_block
    }
}

struct BlkCx<'t, 'a: 't, 'b: 'a, 'c: 'a> {
    tf: &'t mut TransFolder<'a, 'b, 'c>,
    // counter for indeterminate variable; just for easier debugging
    count: uint,
    stmts: Vec<P<ast::Stmt>>,
    final_expr: Option<P<ast::Expr>>,
    span: Span,
}

impl<'t, 'a: 't, 'b: 'a, 'c: 'a> BlkCx<'t, 'a, 'b, 'c> {
    fn new(tf: &'t mut TransFolder<'a, 'b, 'c>, span: Span) -> BlkCx<'t, 'a, 'b, 'c> {
        BlkCx {
            tf: tf,
            count: 0,
            stmts: Vec::new(),
            final_expr: None,
            span: span,
        }
    }

    fn into_block(self) -> ast::Block {
        ast::Block {
            view_items: Vec::new(),
            stmts: self.stmts,
            expr: self.final_expr,
            id: DID,
            rules: ast::DefaultBlock,
            span: self.span,
        }
    }

    fn expr_ty(&self, expr: &ast::Expr) -> P<ast::Ty> {
        self.get_ty(expr.id, expr.span)
    }

    fn get_ty(&self, id: NodeId, span: Span) -> P<ast::Ty> {
        let s = &ty::node_id_to_type(self.tf.ctxt, id).sty;
        let ty_name = match *s {
            sty::ty_bool => "bool",
            sty::ty_char => "char",
            sty::ty_uint(u) => match u {
                ast::TyU => "uint",
                ast::TyU8 => "u8",
                ast::TyU16 => "u16",
                ast::TyU32 => "u32",
                ast::TyU64 => "u64",
            },
            sty::ty_int(i) => match i {
                ast::TyI => "int",
                ast::TyI8 => "i8",
                ast::TyI16 => "i16",
                ast::TyI32 => "i32",
                ast::TyI64 => "i64",
            },
            _ => {
                let err = format!("unsupported type: {}", *s);
                self.tf.cx.span_err(span, &*err);
                // FIXME this seems horrible..
                return new_ty(ast::TyInfer, span);
            }
        };
        ty_from_str(ty_name, span)
    }

    // if `expr` has type `T`, this method creates new statmenet
    // `let mut <gensym-name>: T;` and return path of `name`.
    fn decl_new_name(&mut self, name: &str, expr: &ast::Expr) -> ast::Path {
        let sp = expr.span;
        let ty = self.expr_ty(expr);

        let name = format!("{}{}", name, self.count);
        self.count += 1;
        let new_ident = token::gensym_ident(&*name);
        let path = ast::Path {
            span: sp,
            global: false,
            segments: vec!(
                ast::PathSegment {
                    identifier: new_ident,
                    parameters: ast::PathParameters::none(),
                }
            ),
        };

        let sid = ast::SpannedIdent {
            node: new_ident,
            span: sp,
        };

        let pat_node = ast::PatIdent(ast::BindByValue(ast::MutMutable), sid, None);
        let pat = P(ast::Pat {
            node: pat_node,
            span: sp,
            id: DID,
        });

        let new_local = ast::Local {
            ty: ty,
            pat: pat,
            init: None,
            id: DID,
            span: sp,
            source: ast::LocalLet,
        };
        let new_decl = spanned(ast::DeclLocal(P(new_local)), sp);
        let let_stmt = spanned(ast::StmtDecl(new_decl, DID), sp);

        self.stmts.push(let_stmt);

        path
    }

    // `unsafe { asm!(asm : outputs : inputs : clobbers) }`
    // all outputs must be "=", not "+". (i.e. is_rw = False)
    fn asm(&self,
           asm: &str,
           outputs: &[(&str, P<ast::Expr>)],
           inputs: &[(&str, P<ast::Expr>)],
           clobbers: &[&str],
           sp: Span) -> P<ast::Expr> {
        let outputs: Vec<_> = outputs.iter().map(|&(p, ref q)| {
            (InternedString::new(p), q.clone(), /* is_rw: */ false)
        }).collect();

        let inputs: Vec<_> = inputs.iter().map(|&(p, ref q)| {
            (InternedString::new(p), q.clone())
        }).collect();

        let clobbers: Vec<_> = clobbers.iter().map(|&p| InternedString::new(p)).collect();

        let expn_id = self.tf.cx.codemap().record_expansion(codemap::ExpnInfo {
            call_site: sp,
            callee: codemap::NameAndSpan {
                name: "asm".to_string(),
                format: codemap::MacroBang,
                span: None,
            },
        });
        let asm = token::intern_and_get_ident(asm[]);
        let asm = ast::InlineAsm {
            asm: asm,
            asm_str_style: ast::CookedStr,
            outputs: outputs,
            inputs: inputs,
            clobbers: clobbers,
            volatile: false,
            alignstack: false,
            dialect: ast::AsmAtt,
            expn_id: expn_id,
        };

        let asm_expr = new_expr(ast::ExprInlineAsm(asm), sp);

        let unsafe_block = ast::Block {
            view_items: Vec::new(),
            stmts: Vec::new(),
            rules: ast::UnsafeBlock(ast::CompilerGenerated),
            expr: Some(asm_expr),
            id: ast::DUMMY_NODE_ID,
            span: sp,
        };

        new_expr(ast::ExprBlock(P(unsafe_block)), sp)
    }
}

impl<'t, 'a: 't, 'b: 'a, 'c: 'a> BlkCx<'t, 'a, 'b, 'c> {
    fn trans_stmt(&mut self, stmt: &ast::Stmt) {
        match stmt.node {
            ast::StmtDecl(ref decl, _) => {
                match decl.node {
                    // let pat = expr;
                    ast::DeclLocal(ref local) => {
                        self.trans_decl_local(&**local, decl.span);
                    }
                    ast::DeclItem(..) => {
                        self.tf.cx.span_err(decl.span, "DeclItem not supported");
                        return;
                    }
                }
            }
            ast::StmtSemi(ref expr, _) | ast::StmtExpr(ref expr, _) => {
                if let ast::ExprAssign(ref a, ref b) = expr.node {
                    self.trans_assign_expr(&**a, &**b);
                } else {
                    let new_expr = self.trans_expr(&**expr);
                    let new_node = match stmt.node {
                        ast::StmtSemi(..) => ast::StmtSemi(new_expr, DID),
                        ast::StmtExpr(..) => ast::StmtExpr(new_expr, DID),
                        _ => unreachable!(),
                    };
                    let stmt = spanned(new_node, stmt.span);
                    self.stmts.push(stmt);
                }
            }
            ast::StmtMac(..) => {
                self.tf.cx.span_err(stmt.span, "StmtMac not supported");
                return;
            }
        }
    }

    pub fn trans_decl_local(&mut self, local: &ast::Local, decl_sp: Span) {
        let (pat_sp, new_pat) = match local.pat.node {
            ast::PatIdent(bmode, sid, ref p) => {
                let span = sid.span;
                let new_bmode = match bmode {
                    ast::BindByValue(m) => ast::BindByValue(m),
                    _ => {
                        self.tf.cx.span_err(local.pat.span, "unsupported bind mode");
                        return;
                    }
                };
                let new_pat_node = ast::PatIdent(new_bmode, sid, p.clone());

                let pat = ast::Pat {
                    node: new_pat_node,
                    span: local.pat.span,
                    id: DID,
                };
                (span, P(pat))
            }
            _ => {
                self.tf.cx.span_err(local.pat.span, "unimplemented/unsupported pat");
                return;
            }
        };

        let pat_ty = self.get_ty(local.pat.id, pat_sp);

        let init_expr = if let Some(ref expr) = local.init {
            let init_block_expr = self.trans_expr(&**expr);
            Some(init_block_expr)
        } else {
            None
        };

        let new_local = ast::Local {
            ty: self.tf.fold_ty(pat_ty),
            pat: new_pat.clone(),
            init: init_expr,
            id: DID,
            span: local.span,
            source: local.source,
        };
        let new_decl = spanned(ast::DeclLocal(P(new_local)), decl_sp);
        let let_stmt = spanned(ast::StmtDecl(new_decl, DID), decl_sp);
        self.stmts.push(let_stmt);
    }

    fn trans_expr(&mut self, expr: &ast::Expr) -> P<ast::Expr> {
        if let Some(e) = self.trans_simple_expr(expr) {
            return e;
        }

        let new_node = match expr.node {
            ast::ExprAssign(ref a, ref b) => {
                if !expr_is_lhs(&**a) {
                    self.tf.cx.span_err(a.span, "inappropriate lhs");
                }

                let new_a = self.tf.fold_expr(a.clone());
                let new_b = self.trans_expr(&**b);

                ast::ExprAssign(new_a, new_b)
            }
            _ => {
                // `{ let mut blk; trans_assign_expr(blk <- b); blk }`
                let blk = {
                    let mut new_cx = BlkCx::new(self.tf, expr.span);

                    let blk = new_cx.trans_expr_to_new_var("blk", &*expr).1;
                    new_cx.final_expr = Some(blk);

                    new_cx.into_block()
                };
                ast::ExprBlock(P(blk))
            }
        };
        new_expr(new_node, expr.span)
    }

    fn trans_simple_expr(&mut self, expr: &ast::Expr) -> Option<P<ast::Expr>> {
        let new_node = match expr.node {
            ast::ExprPath(..) | ast::ExprLit(..) => {
                return Some(self.tf.fold_expr(P(expr.clone())));
            }
            ast::ExprParen(ref e) => {
                return Some(self.trans_expr(&**e));
            }
            ast::ExprCall(ref path, ref args) => {
                if !expr_is_lhs(&**path) {
                    self.tf.cx.span_err(path.span, "found non-trivial path");
                }
                let new_path = self.tf.fold_expr(path.clone());

                let new_args = args.iter().map(|arg| {
                    // evalute args left-to-right.
                    let new_arg_expr = self.trans_expr_to_new_var("arg", &**arg).1;
                    new_arg_expr
                }).collect();
                ast::ExprCall(new_path, new_args)
            }
            ast::ExprBlock(ref blk) => {
                let new_blk = self.tf.trans_block(&**blk, expr.span);
                ast::ExprBlock(P(new_blk))
            }
            ast::ExprRet(ref e) => {
                let new_ret_expr = match *e {
                    Some(ref e) => Some(self.trans_expr(&**e)),
                    None => None,
                };
                ast::ExprRet(new_ret_expr)
            }
            ast::ExprUnary(ast::UnDeref, ref e) => {
                ast::ExprUnary(ast::UnDeref, self.trans_expr(&**e))
            }

            _ => return None,
        };

        Some(new_expr(new_node, expr.span))
    }

    // given expr, return new_name and new_expr s.t. `let new_name = expr` and
    // `new_expr` is `path_expr(new_name)`.
    // if `expr` is ExprPath, it just returns the path without introducing
    // new variable name.
    fn trans_expr_to_new_var(&mut self,
                                new_name: &str,
                                expr: &ast::Expr) -> (ast::Path, P<ast::Expr>) {
        match expr.node {
            ast::ExprPath(ref path) => {
                let path = path.clone();
                let new_expr = self.tf.fold_expr(P(expr.clone()));
                (path, new_expr)
            }
            _ => {
                let new_name = self.decl_new_name(new_name, &*expr);
                let new_expr = path_expr(new_name.clone(), expr.span);
                self.trans_assign_expr(&*new_expr, &*expr);
                (new_name, new_expr)
            }
        }
    }

    fn trans_assign_expr(&mut self, lhs: &ast::Expr, expr: &ast::Expr) {
        if !expr_is_lhs(lhs) {
            self.tf.cx.span_err(lhs.span, "inappropriate lhs");
            return;
        }

        let lhs = self.tf.fold_expr(P(lhs.clone()));
        let expr_sty = &ty::node_id_to_type(self.tf.ctxt, expr.id).sty;

        if let Some(e) = self.trans_simple_expr(expr) {
            let new_expr = new_expr(ast::ExprAssign(lhs, e), expr.span);
            let stmt = spanned(ast::StmtExpr(new_expr, DID), expr.span);
            self.stmts.push(stmt);
            return;
        }

        match expr.node {
            ast::ExprParen(ref e) => {
                self.trans_assign_expr(&*lhs, &**e);
            }

            ast::ExprUnary(uop, ref a) => {
                let a_name = self.trans_expr_to_new_var("tmp", &**a).0;

                match uop {
                    ast::UnNot | ast::UnNeg => {
                        let op = match *expr_sty {
                            ty::ty_uint(..) | ty::ty_int(..) => {
                                match uop {
                                    ast::UnNot => "not",
                                    ast::UnNeg => "neg",
                                    _ => unreachable!(),
                                }
                            }
                            ty::ty_bool => {
                                if uop == ast::UnNot {
                                    "not"
                                } else {
                                    unreachable!();
                                }
                            }
                            _ => {
                                let err = format!("unsupported type: {}", *expr_sty);
                                self.tf.cx.span_err(expr.span, &*err);
                                return;
                            }
                        };
                        let asm = format!("{} $0", op);
                        let outputs = [("=r", lhs.clone())];
                        let inputs = [
                            ("0", path_expr(a_name.clone(), expr.span)),
                        ];

                        let asm_expr = self.asm(&*asm, &outputs, &inputs, &["cc"], expr.span);
                        let asm_stmt = spanned(ast::StmtExpr(asm_expr, DID), expr.span);
                        self.stmts.push(asm_stmt);
                    }
                    // UnDeref is done in `trans_simple_expr`
                    _ => {
                        self.tf.cx.span_err(expr.span, "unsupported UnOp");
                    }
                }
            }
            ast::ExprBinary(bop, ref a, ref b) => {
                let a_name = self.trans_expr_to_new_var("lhs", &**a).0;
                let b_name = self.trans_expr_to_new_var("rhs", &**b).0;

                match bop {
                    ast::BiBitXor | ast::BiBitAnd | ast::BiBitOr | ast::BiAdd | ast::BiSub => {
                        match *expr_sty {
                            ty::ty_uint(..) | ty::ty_int(..) => {}
                            _ => {
                                let err = format!("unsupported type: {}", *expr_sty);
                                self.tf.cx.span_err(expr.span, &*err);
                            }
                        }

                        let op = match bop {
                            ast::BiBitXor => "xor",
                            ast::BiBitAnd => "and",
                            ast::BiBitOr => "or",
                            ast::BiAdd => "add",
                            ast::BiSub => "sub",
                            _ => unreachable!(),
                        };

                        let asm = format!("{} $1, $0", op);
                        let outputs = [("=r", lhs.clone())];
                        let inputs = [
                            ("r", path_expr(b_name.clone(), expr.span)),
                            ("0", path_expr(a_name.clone(), expr.span)),
                        ];

                        let asm_expr = self.asm(&*asm, &outputs, &inputs, &["cc"], expr.span);
                        debug!("asm expr: {}", asm_expr);
                        let asm_stmt = spanned(ast::StmtExpr(asm_expr, DID), expr.span);
                        self.stmts.push(asm_stmt);
                    }
                    ast::BiShl | ast::BiShr => {
                        match *expr_sty {
                            ty::ty_uint(..) | ty::ty_int(..) => {}
                            _ => {
                                let err = format!("unsupported type: {}", *expr_sty);
                                self.tf.cx.span_err(expr.span, &*err);
                            }
                        }

                        let op = match bop {
                            ast::BiShl => "shl",
                            ast::BiShr => "shr",
                            _ => unreachable!(),
                        };
                        let asm = format!("{} $1, $0", op);

                        let outputs = [("=r", lhs.clone())];

                        // FIXME: overlong shift is undefined in Rust (rust-lang/rust#10183)
                        // Since Rust doesn't define it, we have to define *something*.
                        // here we do is truncate it as u8 and just pass it to cpu..
                        // `lhs = a << ((b as u8) as uint)`
                        // yes, this is stupid. - if we want overlong shift == no-op then
                        // we have to mask it properly. (e.g. `a << (b & 0x7)`)
                        // or we may want to panic?
                        // also, we can be better if `b` is integer literal.
                        let e = path_expr(b_name.clone(), expr.span);
                        let tyu = ty_from_str("u8", expr.span);
                        let e = new_expr(ast::ExprCast(e, tyu), expr.span);
                        let inputs = [
                            ("{cl}", e),
                            ("0", path_expr(a_name.clone(), expr.span)),
                        ];

                        let asm_expr = self.asm(&*asm, &outputs, &inputs, &["cc"], expr.span);
                        let asm_stmt = spanned(ast::StmtExpr(asm_expr, DID), expr.span);
                        self.stmts.push(asm_stmt);
                    }
                    ast::BiMul => {
                        match *expr_sty {
                            ty::ty_uint(ast::TyU8) | ty::ty_int(ast::TyI8) => {
                                // there is no two-op form of `mulb`/`imulb`
                                let asm = "mulb $1";
                                let outputs = &[("={al}", lhs.clone())];
                                let inputs = &[
                                    ("rm", path_expr(b_name.clone(), expr.span)),
                                    ("{al}", path_expr(a_name.clone(), expr.span)),
                                ];
                                let clobbers = &["cc", "ax"];

                                let asm_expr = self.asm(asm, outputs, inputs, clobbers, expr.span);
                                let asm_stmt = spanned(ast::StmtExpr(asm_expr, DID), expr.span);
                                self.stmts.push(asm_stmt);
                            }
                            ty::ty_uint(..) | ty::ty_int(..) => {
                                let asm = "imul $1, $0";
                                let outputs = [("=r", lhs.clone())];
                                let inputs = [
                                    ("rm", path_expr(b_name.clone(), expr.span)),
                                    ("0", path_expr(a_name.clone(), expr.span)),
                                ];

                                let asm_expr = self.asm(asm, &outputs, &inputs, &["cc"], expr.span);
                                debug!("asm expr: {}", asm_expr);
                                let asm_stmt = spanned(ast::StmtExpr(asm_expr, DID), expr.span);
                                self.stmts.push(asm_stmt);
                            }
                            _ => {
                                let err = format!("unsupported type: {}", *expr_sty);
                                self.tf.cx.span_err(expr.span, &*err);
                            }
                        }
                    }
                    ast::BiLt | ast::BiLe | ast::BiGt | ast::BiGe | ast::BiEq => {
                        let flag = match bop {
                            ast::BiLt | ast::BiGt => "b",
                            ast::BiLe | ast::BiGe => "be",
                            ast::BiEq => "e",
                            _ => unreachable!()
                        };
                        // a: $1, b: $2
                        let (input1, input2) = match bop {
                            ast::BiGt | ast::BiGe => ("$1", "$2"),
                            _ => ("$2", "$1"),
                        };
                        let asm = format!("cmp {}, {}; set{} $0", input1, input2, flag);
                        let outputs = [("=r", lhs.clone())];
                        let inputs = [
                            ("r", path_expr(a_name.clone(), a.span)),
                            ("r", path_expr(b_name.clone(), b.span)),
                        ];

                        let asm_expr = self.asm(&*asm, &outputs, &inputs, &["cc"], expr.span);
                        let asm_stmt = spanned(ast::StmtExpr(asm_expr, DID), expr.span);
                        self.stmts.push(asm_stmt);

                    }
                    _ => {
                        // TODO
                        let err = format!("unimplemented BinOp: {}", bop);
                        self.tf.cx.span_err(expr.span, &*err);
                    }
                }
            }
            _ => {
                // TODO
                // NOTE for future work:
                // - ExprIndex or ExprSlice should be prohibited if [expr] contains
                // non-constant expression
                self.tf.cx.span_err(expr.span, "unimplemented Expr");
            }
        }
    }
}

fn new_expr(node: ast::Expr_, sp: Span) -> P<ast::Expr> {
    P(ast::Expr {
        node: node,
        span: sp,
        id: DID,
    })
}

fn path_expr(p: ast::Path, sp: Span) -> P<ast::Expr> {
    new_expr(ast::ExprPath(p), sp)
}

fn spanned<T: 'static>(node: T, sp: Span) -> P<codemap::Spanned<T>> {
    P(codemap::Spanned {
        node: node,
        span: sp,
    })
}

fn new_ty(node: ast::Ty_, sp: Span) -> P<ast::Ty> {
    P(ast::Ty {
        node: node,
        span: sp,
        id: DID,
    })
}

fn ty_from_str(s: &str, span: Span) -> P<ast::Ty> {
    let p = ast::Path {
        span: span,
        global: false,
        segments: vec!(
            ast::PathSegment {
                identifier: token::str_to_ident(s),
                parameters: ast::PathParameters::none(),
            }
        ),
    };
    let ty_ = ast::TyPath(p, DID);
    new_ty(ty_, span)
}

fn expr_is_lhs(e: &ast::Expr) -> bool {
    fn expr_is_clean(e: &ast::Expr, lit_ok: bool) -> bool {
        match e.node {
            ast::ExprLit(..) => lit_ok,
            ast::ExprPath(..) => true,
            ast::ExprIndex(ref a, ref b) => {
                expr_is_clean(&**a, false) && expr_is_clean(&**b, true)
            }
            // TODO references?
            _ => false,
        }
    }
    expr_is_clean(e, false)
}
