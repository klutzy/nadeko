// translate function items

use syntax::ast;
use syntax::ast::Item;
use syntax::codemap::{mod, DUMMY_SP};
use syntax::ast::DUMMY_NODE_ID as DID;
use syntax::ptr::P;
use syntax::parse::token;
use syntax::ext::base::ExtCtxt;
use syntax::visit::{mod, Visitor};

pub struct TransFolder<'a, 'b: 'a> {
    cx: &'a mut ExtCtxt<'b>,
}

impl<'a, 'b: 'a> TransFolder<'a, 'b> {
    pub fn new(cx: &'a mut ExtCtxt<'b>) -> TransFolder<'a, 'b> {
        TransFolder {
            cx: cx,
        }
    }

    pub fn trans_item(&mut self, item: P<Item>) -> P<Item> {
        let ret_default = match item.node {
            ast::ItemFn(_, _, _, ref generics, _) => {
                if generics.is_parameterized() {
                    self.cx.span_err(item.span, "generics are not supported");
                    true
                } else {
                    false
                }
            }
            ast::ItemMod(..) => false,
            _ => true
        };
        if ret_default {
            return item;
        }

        item.map(|item| {
            let new_item_node = match item.node {
                ast::ItemFn(decl, fn_style, abi, generics, body) => {
                    // inject `use nadeko::asm::*;`
                    let body = body.map(|mut body| {
                        let prelude_path = ast::Path {
                            span: DUMMY_SP,
                            global: false,
                            segments: vec!(
                                ast::PathSegment {
                                    identifier: token::str_to_ident("nadeko"),
                                    parameters: ast::PathParameters::none(),
                                },
                                ast::PathSegment {
                                    identifier: token::str_to_ident("asm"),
                                    parameters: ast::PathParameters::none(),
                                }
                            ),
                        };
                        let path = ast::ViewPathGlob(prelude_path, DID);
                        let path = P(codemap::dummy_spanned(path));

                        body.view_items.push(ast::ViewItem {
                            node: ast::ViewItemUse(path),
                            attrs: Vec::new(),
                            vis: ast::Inherited,
                            span: DUMMY_SP,
                        });
                        body
                    });

                    let new_fn_block = self.trans_block(body);
                    ast::ItemFn(decl, fn_style, abi, generics, new_fn_block)
                }
                ast::ItemMod(mod_) => {
                    let new_mod = ast::Mod {
                        items: mod_.items.into_iter().map(|i| self.trans_item(i)).collect(),
                        .. mod_
                    };
                    ast::ItemMod(new_mod)
                }
                _ => unreachable!(),
            };
            ast::Item {
                node: new_item_node,
                .. item
            }
        })
    }

    fn trans_block(&mut self, block: P<ast::Block>) -> P<ast::Block> {
        block.map(|block| {
            let new_stmts = block.stmts.into_iter().map(|stmt| {
                self.trans_stmt(stmt)
            }).collect();

            let new_expr = match block.expr {
                Some(expr) => Some(self.trans_expr(expr)),
                None => None,
            };

            ast::Block {
                stmts: new_stmts,
                expr: new_expr,
                .. block
            }
        })
    }

    fn trans_stmt(&mut self, stmt: P<ast::Stmt>) -> P<ast::Stmt> {
        stmt.map(|stmt| {
            let ret_default = match stmt.node {
                ast::StmtDecl(ref decl, _) => {
                    match decl.node {
                        ast::DeclLocal(..) => false,
                        _ => true,
                    }
                }
                ast::StmtSemi(..) | ast::StmtExpr(..) => false,
                _ => true,
            };
            if ret_default {
                return stmt;
            }

            let new_node = match stmt.node {
                ast::StmtDecl(decl, _) => {
                    let new_decl = decl.map(|decl| {
                        match decl.node {
                            // let pat = expr;
                            ast::DeclLocal(local) => {
                                let new_local = self.trans_decl_local(local);
                                ast::Decl {
                                    node: ast::DeclLocal(new_local),
                                    .. decl
                                }
                            }
                            _ => unreachable!(),
                        }
                    });
                    ast::StmtDecl(new_decl, DID)
                }
                ast::StmtSemi(expr, _) => {
                    let new_expr = self.trans_expr(expr);
                    ast::StmtSemi(new_expr, DID)
                }
                ast::StmtExpr(expr, _) => {
                    let new_expr = self.trans_expr(expr);
                    ast::StmtSemi(new_expr, DID)
                }
                _ => unreachable!(),
            };

            ast::Stmt {
                node: new_node,
                .. stmt
            }
        })
    }

    fn trans_decl_local(&mut self, local: P<ast::Local>) -> P<ast::Local> {
        local.map(|local| {
            let init_expr = if let Some(expr) = local.init {
                Some(self.trans_expr(expr))
            } else {
                None
            };

            ast::Local {
                init: init_expr,
                .. local
            }
        })
    }

    fn trans_expr(&mut self, expr: P<ast::Expr>) -> P<ast::Expr> {
        expr.map(|expr| {
            let new_node = match expr.node {
                ast::ExprPath(p) => ast::ExprPath(p.clone()),
                ast::ExprLit(l) => ast::ExprLit(l.clone()),
                ast::ExprParen(e) => ast::ExprParen(self.trans_expr(e)),
                ast::ExprCall(path, args) => {
                    match path.node {
                        ast::ExprPath(..) => {}
                        _ => self.cx.span_err(path.span, "not a path"),
                    }

                    let new_path = self.trans_expr(path);
                    let new_args = self.trans_expr_vec(args);

                    ast::ExprCall(new_path, new_args)
                }
                ast::ExprBlock(blk) => {
                    let new_blk = self.trans_block(blk);
                    ast::ExprBlock(new_blk)
                }
                ast::ExprRet(opt_e) => {
                    let new_ret_expr = match opt_e {
                        Some(e) => Some(self.trans_expr(e)),
                        None => None,
                    };
                    ast::ExprRet(new_ret_expr)
                }

                // convert `a op b` into `a.const_op(b)`
                ast::ExprBinary(bop, a, b) => {
                    let new_a = self.trans_expr(a);
                    let new_b = self.trans_expr(b);
                    let method_name = bop_method_name(bop);
                    let ident = ast::SpannedIdent {
                        node: token::str_to_ident(method_name),
                        span: expr.span,
                    };
                    ast::ExprMethodCall(ident, Vec::new(), vec!(new_a, new_b))
                }

                ast::ExprUnary(uop, a) => {
                    let new_a = self.trans_expr(a);
                    match uop {
                        ast::UnUniq => {
                            self.cx.span_err(expr.span, "UnUniq not supported");
                            ast::ExprUnary(ast::UnUniq, new_a)
                        }
                        ast::UnDeref => {
                            ast::ExprUnary(ast::UnDeref, new_a)
                        }
                        ast::UnNot | ast::UnNeg => {
                            let method_name = match uop {
                                ast::UnNot => "const_not",
                                ast::UnNeg => "const_neg",
                                _ => unreachable!(),
                            };
                            let ident = ast::SpannedIdent {
                                node: token::str_to_ident(method_name),
                                span: expr.span,
                            };
                            ast::ExprMethodCall(ident, Vec::new(), vec!(new_a))
                        }
                    }
                }

                ast::ExprAssign(lhs, rhs) => {
                    if !expr_is_assignable(&*lhs) {
                        self.cx.span_err(lhs.span, "non-assignable lhs");
                    }
                    let new_lhs = self.trans_expr(lhs);
                    let new_rhs = self.trans_expr(rhs);
                    ast::ExprAssign(new_lhs, new_rhs)
                }

                ast::ExprAssignOp(bop, lhs, rhs) => {
                    // `a += b` => `a = a + b`
                    if !expr_is_assignable(&*lhs) {
                        self.cx.span_err(lhs.span, "non-assignable lhs");
                    }

                    let new_lhs = self.trans_expr(lhs);
                    let new_rhs = self.trans_expr(rhs);
                    let method_name = bop_method_name(bop);
                    let ident = ast::SpannedIdent {
                        node: token::str_to_ident(method_name),
                        span: expr.span,
                    };

                    let new_lhs_2 = new_lhs.clone();
                    let assign = P(ast::Expr {
                        node: ast::ExprMethodCall(ident, Vec::new(), vec!(new_lhs_2, new_rhs)),
                        span: expr.span,
                        id: DID,
                    });

                    ast::ExprAssign(new_lhs, assign)
                }

                ast::ExprVec(exprs) => {
                    let new_exprs = self.trans_expr_vec(exprs);
                    ast::ExprVec(new_exprs)
                }

                ast::ExprIndex(expr, idx) => {
                    if !expr_is_lit(&*idx) {
                        self.cx.span_err(idx.span, "non-constant index");
                    }
                    let new_expr = self.trans_expr(expr);
                    let new_idx = self.trans_expr(idx);
                    ast::ExprIndex(new_expr, new_idx)
                }

                ast::ExprCast(expr, ty) => {
                    match ty.node {
                        ast::TyPath(..) => {}
                        _ => self.cx.span_err(ty.span, "non-path cast"),
                    }
                    let new_expr = self.trans_expr(expr);
                    ast::ExprCast(new_expr, ty)
                }

                ast::ExprIf(cond, if_blk, else_expr) => {
                    if block_has_side_effect(&*if_blk) {
                        self.cx.span_err(if_blk.span, "block has side effects");
                    }

                    let new_cond = self.trans_expr(cond);
                    let new_if_blk = self.trans_block(if_blk);
                    let new_if_span = new_if_blk.span;
                    let new_if_expr = P(ast::Expr {
                        node: ast::ExprBlock(new_if_blk),
                        span: new_if_span,
                        id: DID,
                    });

                    // we expect else is always found, since we do not permit
                    // any side effect right now.
                    let new_else_expr = match else_expr {
                        Some(e) => {
                            if expr_has_side_effect(&*e) {
                                self.cx.span_err(e.span, "block has side effects");
                            }
                            self.trans_expr(e)
                        }
                        None => {
                            self.cx.span_err(expr.span, "`else` needed");
                            // dummy: return `()`
                            P(ast::Expr {
                                node: ast::ExprTup(Vec::new()),
                                span: DUMMY_SP,
                                id: DID,
                            })
                        }
                    };

                    let ident = ast::SpannedIdent {
                        node: token::str_to_ident("const_if"),
                        span: expr.span,
                    };
                    let args = vec!(new_cond, new_if_expr, new_else_expr);
                    ast::ExprMethodCall(ident, Vec::new(), args)
                }

                ast::ExprMac(..) => {
                    self.cx.span_err(expr.span, "macros cannot not be here");
                    ast::ExprTup(Vec::new())
                }

                _ => {
                    let err = format!("unimplemented: {}", expr.node);
                    self.cx.span_err(expr.span, &*err);
                    // dummy
                    ast::ExprTup(Vec::new())
                }
            };

            ast::Expr {
                node: new_node,
                .. expr
            }
        })
    }

    fn trans_expr_vec(&mut self, expr_list: Vec<P<ast::Expr>>) -> Vec<P<ast::Expr>> {
        expr_list.into_iter().map(|arg| self.trans_expr(arg)).collect()
    }
}

// check if `e` is appropriate for `e = ...;`.
fn expr_is_assignable(e: &ast::Expr) -> bool {
    fn expr_is_clean(e: &ast::Expr, lit_ok: bool) -> bool {
        match e.node {
            ast::ExprLit(..) => lit_ok,
            ast::ExprPath(..) => true,
            ast::ExprIndex(ref a, ref b) => {
                expr_is_clean(&**a, false) && expr_is_lit(&**b)
            }
            _ => false,
        }
    }
    expr_is_clean(e, false)
}

// check if `e` is constant lit.
fn expr_is_lit(e: &ast::Expr) -> bool {
    match e.node {
        ast::ExprLit(..) => true,
        ast::ExprParen(ref expr) => expr_is_lit(&**expr),
        ast::ExprBinary(_, ref a, ref b) => expr_is_lit(&**a) && expr_is_lit(&**b),
        ast::ExprUnary(_, ref a) => expr_is_lit(&**a),
        ast::ExprCast(ref expr, _) => expr_is_lit(&**expr),
        // there are actually other possibilities (e.g. ExprBlock with no stmt)
        _ => false,
    }
}

struct SideEffectChecker(bool);

impl SideEffectChecker {
    fn new() -> SideEffectChecker {
        SideEffectChecker(false)
    }

    fn into_inner(self) -> bool {
        self.0
    }
}

impl<'a> Visitor<'a> for SideEffectChecker {
    fn visit_expr(&mut self, e: &'a ast::Expr) {
        match e.node {
            ast::ExprAssign(..) | ast::ExprAssignOp(..) => {
                self.0 = true;
            }
            ast::ExprCall(..) => {
                // we currently don't know if a function is pure or not.
                self.0 = true;
            }
            _ => {}
        }
        visit::walk_expr(self, e);
    }
}

fn block_has_side_effect(blk: &ast::Block) -> bool {
    let mut checker = SideEffectChecker::new();
    checker.visit_block(blk);
    checker.into_inner()
}

fn expr_has_side_effect(e: &ast::Expr) -> bool {
    let mut checker = SideEffectChecker::new();
    checker.visit_expr(e);
    checker.into_inner()
}

fn bop_method_name(bop: ast::BinOp) -> &'static str {
    match bop {
        ast::BiAdd => "const_add",
        ast::BiSub => "const_sub",
        ast::BiMul => "const_mul",
        ast::BiDiv => "const_div",
        ast::BiRem => "const_rem",
        ast::BiAnd => "const_and",
        ast::BiOr => "const_or",
        ast::BiBitXor => "const_bit_xor",
        ast::BiBitAnd => "const_bit_and",
        ast::BiBitOr => "const_bit_or",
        ast::BiShl => "const_shl",
        ast::BiShr => "const_shr",
        ast::BiEq => "const_eq",
        ast::BiLt => "const_lt",
        ast::BiLe => "const_le",
        ast::BiNe => "const_ne",
        ast::BiGe => "const_ge",
        ast::BiGt => "const_gt",
    }
}
