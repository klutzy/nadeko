// translate function items

use syntax::ast;
use syntax::ast::DUMMY_NODE_ID as DID;
use syntax::ast::Item;
use syntax::codemap::DUMMY_SP;
use syntax::ext::base::ExtCtxt;
use syntax::ext::build::AstBuilder;
use syntax::ptr::P;
use syntax::visit::{self, Visitor};

pub struct TransFolder<'a, 'b: 'a> {
    cx: &'a ExtCtxt<'b>,
}

impl<'a, 'b: 'a> TransFolder<'a, 'b> {
    pub fn new(cx: &'a ExtCtxt<'b>) -> TransFolder<'a, 'b> {
        TransFolder {
            cx: cx,
        }
    }

    pub fn trans_item(&mut self, item: P<Item>) -> P<Item> {
        let ret_default = match item.node {
            ast::ItemFn(_, _, _, _, ref generics, _) => {
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
                ast::ItemFn(decl, fn_style, constness, abi, generics, body) => {
                    let new_fn_block = self.trans_block(body);
                    ast::ItemFn(decl, fn_style, constness, abi, generics, new_fn_block)
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
                ast::ExprPath(p, x) => ast::ExprPath(p.clone(), x),
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
                    let (trait_name, method_name) = bop_method_name(bop);
                    self.cx.expr_call_global(
                        expr.span,
                        vec![
                            self.cx.ident_of("nadeko"),
                            self.cx.ident_of(trait_name),
                            self.cx.ident_of(method_name),
                        ],
                        vec![
                            self.trans_expr(a),
                            self.trans_expr(b),
                        ]).node.clone()
                }

                ast::ExprUnary(uop, a) => {
                    let new_a = self.trans_expr(a);
                    match uop {
                        ast::UnDeref => {
                            ast::ExprUnary(ast::UnDeref, new_a)
                        }
                        ast::UnNot | ast::UnNeg => {
                            let (trait_name, method_name) = match uop {
                                ast::UnNot => ("ConstNot", "const_not"),
                                ast::UnNeg => ("ConstNeg", "const_neg"),
                                _ => unreachable!(),
                            };
                            self.cx.expr_call_global(
                                expr.span,
                                vec![
                                    self.cx.ident_of("nadeko"),
                                    self.cx.ident_of(trait_name),
                                    self.cx.ident_of(method_name),
                                ],
                                vec![new_a]
                            ).node.clone()
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
                    let (trait_name, method_name) = bop_method_name(bop);

                    // `a += b` => `a = a + b`
                    if !expr_is_assignable(&*lhs) {
                        self.cx.span_err(lhs.span, "non-assignable lhs");
                    }

                    let new_lhs = self.trans_expr(lhs);
                    let new_rhs = self.trans_expr(rhs);
                    let new_lhs_2 = new_lhs.clone();

                    let assign = self.cx.expr_call_global(
                        expr.span,
                        vec![
                            self.cx.ident_of("nadeko"),
                            self.cx.ident_of(trait_name),
                            self.cx.ident_of(method_name),
                        ],
                        vec![
                            new_lhs_2,
                            new_rhs,
                        ]);

                    ast::ExprAssign(new_lhs, assign)

                    /*
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
                     */
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

                    self.cx.expr_call_global(
                        expr.span,
                        vec![
                            self.cx.ident_of("nadeko"),
                            self.cx.ident_of("ConstIf"),
                            self.cx.ident_of("const_if"),
                        ],
                        vec![new_cond, new_if_expr, new_else_expr]
                    ).node.clone()
                }

                // allow `for i in const..const` only.
                ast::ExprForLoop(for_pat, for_expr, for_blk, id) => {
                    if let ast::ExprRange(ref start, ref end) = for_expr.node {
                        if start.iter().next().map(|x| match x.node { ast::ExprLit(_) => false, _ => true }).unwrap_or(false) {
                            self.cx.span_err(start.iter().next().map( |x| x.span ).unwrap_or( DUMMY_SP ), "non-constant range");
                        }
                        if let Some(ref end) = *end {
                            if !expr_is_lit(&**end) {
                                self.cx.span_err(end.span, "non-constant range");
                            }
                        }
                    } else {
                        self.cx.span_err(for_expr.span, "expected `start..end` range");
                    };

                    let new_blk = self.trans_block(for_blk);

                    ast::ExprForLoop(for_pat, for_expr, new_blk, id)
                }

                ast::ExprMac(..) => {
                    self.cx.span_err(expr.span, "macros cannot not be here");
                    ast::ExprTup(Vec::new())
                }

                ast::ExprMatch(discriminant, arms, match_source) => {
                    let discriminant = self.trans_expr(discriminant);
                    let arms = arms.into_iter()
                        .map(|arm| {
                            ast::Arm {
                                attrs: arm.attrs,
                                pats: arm.pats,
                                guard: arm.guard.map(|guard| self.trans_expr(guard)),
                                body: self.trans_expr(arm.body),
                            }
                        })
                        .collect();

                    ast::ExprMatch(discriminant, arms, match_source)
                }

                ast::ExprRange(min, max) => {
                    ast::ExprRange(
                        min.map(|min| self.trans_expr(min)),
                        max.map(|max| self.trans_expr(max))
                    )
                }

                ast::ExprLoop(block, ident) => {
                    ast::ExprLoop(
                        self.trans_block(block),
                        ident
                    )
                }

                ast::ExprAddrOf(mutability, expr) => {
                    ast::ExprAddrOf(
                        mutability,
                        self.trans_expr(expr),
                    )
                }

                ast::ExprBreak(ident) => {
                    ast::ExprBreak(ident)
                }

                _ => {
                    let err = format!("unimplemented: {:?}", expr.node);
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

fn bop_method_name(bop: ast::BinOp) -> (&'static str, &'static str) {
    match bop.node {
        ast::BiAdd => ("ConstAdd", "const_add"),
        ast::BiSub => ("ConstSub", "const_sub"),
        ast::BiMul => ("ConstMul", "const_mul"),
        ast::BiDiv => ("ConstDiv", "const_div"),
        ast::BiRem => ("ConstRem", "const_rem"),
        ast::BiAnd => ("ConstAnd", "const_and"),
        ast::BiOr => ("ConstOr", "const_or"),
        ast::BiBitXor => ("ConstBitXor", "const_bit_xor"),
        ast::BiBitAnd => ("ConstBitAnd", "const_bit_and"),
        ast::BiBitOr => ("ConstBitOr", "const_bit_or"),
        ast::BiShl => ("ConstShl", "const_shl"),
        ast::BiShr => ("ConstShr", "const_shr"),
        ast::BiEq => ("ConstEq", "const_eq"),
        ast::BiLt => ("ConstLt", "const_lt"),
        ast::BiLe => ("ConstLe", "const_le"),
        ast::BiNe => ("ConstNe", "const_ne"),
        ast::BiGe => ("ConstGe", "const_ge"),
        ast::BiGt => ("ConstGt", "const_gt"),
    }
}
