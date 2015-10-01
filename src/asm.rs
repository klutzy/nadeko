macro_rules! const_bop_def {
    ($($tr:ident $m:ident),*) => (
        $(
            pub trait $tr {
                fn $m(self, other: Self) -> Self;
            }
        )*
    )
}
const_bop_def! {
    ConstAdd const_add,
    ConstSub const_sub,
    ConstMul const_mul,
    ConstDiv const_div,
    ConstRem const_rem,
    ConstBiAnd const_and,
    ConstOr const_or,
    ConstBitXor const_bit_xor,
    ConstBitAnd const_bit_and,
    ConstBitOr const_bit_or
}

macro_rules! const_bop_cmp_def {
    ($($tr:ident $m:ident),*) => (
        $(
            pub trait $tr {
                fn $m(self, other: Self) -> bool;
            }
        )*
    )
}
const_bop_cmp_def! {
    ConstEq const_eq,
    ConstLt const_lt,
    ConstLe const_le,
    ConstNe const_ne,
    ConstGe const_ge,
    ConstGt const_gt
}

pub trait ConstShl {
    fn const_shl(self, b: usize) -> Self;
}
pub trait ConstShr {
    fn const_shr(self, b: usize) -> Self;
}

macro_rules! const_uop_def {
    ($($tr:ident $m:ident),*) => (
        $(
            pub trait $tr {
                fn $m(self) -> Self;
            }
        )*
    )
}
const_uop_def! {
    ConstNeg const_neg,
    ConstNot const_not
}

pub trait ConstIf<T> {
    fn const_if(self, if_expr: T, else_expr: T) -> T;
}

// amd64 implementation

macro_rules! const_bop {
    (
        $tr:ident,
        $m:ident,
        $asm:expr,
        $($t:ty),*
    ) => (
        $(
            impl $tr for $t {
                #[inline(always)]
                #[allow(unused_mut)]
                fn $m(self, b: $t) -> $t {
                    let mut ret: $t;
                    unsafe {
                        asm!($asm : "=r"(ret) : "r"(b), "0"(self) : "cc");
                    }
                    ret
                }
            }
        )*
    )
}

const_bop! {
    ConstAdd, const_add,
    "add $1, $0",
    u8, u16, u32, u64, usize, i8, i16, i32, i64, isize
}

const_bop! {
    ConstSub, const_sub,
    "sub $1, $0",
    u8, u16, u32, u64, usize, i8, i16, i32, i64, isize
}

const_bop! {
    ConstBitXor, const_bit_xor,
    "xor $1, $0",
    u8, u16, u32, u64, usize, i8, i16, i32, i64, isize
}

const_bop! {
    ConstBitAnd, const_bit_and,
    "and $1, $0",
    u8, u16, u32, u64, usize, i8, i16, i32, i64, isize
}

const_bop! {
    ConstBitOr, const_bit_or,
    "or $1, $0",
    u8, u16, u32, u64, usize, i8, i16, i32, i64, isize
}

const_bop! {
    ConstMul, const_mul,
    "imul $1, $0",
    u16, u32, u64, usize, i16, i32, i64, isize
}

// u8/i8 is exceptional
macro_rules! const_mul8 {
    ($($t:ty)*) => (
        $(
            impl ConstMul for $t {
                #[inline(always)]
                #[allow(unused_mut)]
                fn const_mul(self, b: $t) -> $t {
                    let mut ret: $t;
                    unsafe {
                        asm!("mulb $1" : "={al}"(ret) : "r"(self), "{al}"(b) : "cc", "ax");
                    }
                    ret
                }
            }
        )*
    )
}
const_mul8! {u8 i8}

// shl/shr
macro_rules! const_shift {
    (
        $tr:ident,
        $m:ident,
        $asm:expr,
        $($t:ty),*
    ) => (
        $(
            impl $tr for $t {
                #[inline(always)]
                #[allow(unused_mut)]
                fn $m(self, b: usize) -> $t {
                    let mut ret: $t;
                    unsafe {
                        // FIXME: overlong shift is undefined in Rust (rust-lang/rust#10183)
                        asm!($asm : "=r"(ret) : "{cl}"(b as u8), "0"(self) : "cc");
                    }
                    ret
                }
            }
        )*
    )
}

const_shift! {
    ConstShl, const_shl,
    "shl $1, $0",
    u8, u16, u32, u64, usize, i8, i16, i32, i64, isize
}
const_shift! {
    ConstShr, const_shr,
    "shr $1, $0",
    u8, u16, u32, u64, usize
}
const_shift! {
    ConstShr, const_shr,
    "sar $1, $0",
    i8, i16, i32, i64, isize
}

macro_rules! const_cmp {
    (
        $tr:ident,
        $m:ident,
        $asm:expr,
        $($t:ty),*
    ) => (
        $(
            impl $tr for $t {
                #[inline(always)]
                #[allow(unused_mut)]
                fn $m(self, b: $t) -> bool {
                    let mut ret: bool;
                    unsafe {
                        asm!($asm : "=r"(ret) : "r"(b), "r"(self) : "cc");
                    }
                    ret
                }
            }
        )*
    )
}

const_cmp! {
    ConstEq, const_eq,
    "cmp $1, $2; sete $0",
    u8, u16, u32, u64, usize, i8, i16, i32, i64, isize
}

const_cmp! {
    ConstLt, const_lt,
    "cmp $1, $2; setb $0",
    u8, u16, u32, u64, usize, i8, i16, i32, i64, isize
}
const_cmp! {
    ConstGt, const_gt,
    "cmp $2, $1; setb $0",
    u8, u16, u32, u64, usize, i8, i16, i32, i64, isize
}
const_cmp! {
    ConstLe, const_le,
    "cmp $1, $2; setbe $0",
    u8, u16, u32, u64, usize, i8, i16, i32, i64, isize
}
const_cmp! {
    ConstGe, const_ge,
    "cmp $2, $1; setbe $0",
    u8, u16, u32, u64, usize, i8, i16, i32, i64, isize
}

macro_rules! const_uop {
    (
        $tr:ident,
        $m:ident,
        $asm:expr,
        $($t:ty),*
    ) => (
        $(
            impl $tr for $t {
                #[inline(always)]
                #[allow(unused_mut)]
                fn $m(self) -> $t {
                    let mut ret: $t;
                    unsafe {
                        asm!($asm : "=r"(ret) : "0"(self) : "cc");
                    }
                    ret
                }
            }
        )*
    )
}

const_uop! {
    ConstNeg, const_neg,
    "neg $0",
    u8, u16, u32, u64, usize, i8, i16, i32, i64, isize
}
const_uop! {
    ConstNot, const_not,
    "not $0",
    u8, u16, u32, u64, usize, i8, i16, i32, i64, isize
}

macro_rules! const_if {
    ($($t:ty),*) => (
        $(
            impl ConstIf<$t> for bool {
                fn const_if(self, a: $t, b: $t) -> $t {
                    // switch = if cond { 0 } else { 0b111...111 }
                    let switch = (self as $t).const_sub(1);

                    let axb = a.const_bit_xor(b);
                    // mask = if cond { 0 } else { a ^ b }
                    let mask = axb.const_bit_and(switch);
                    // ret = if cond { a } else { b }
                    let ret = mask.const_bit_xor(a);

                    ret
                }
            }
        )*
    )
}

const_if! {u8, u16, u32, u64, usize, i8, i16, i32, i64, isize}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_const_if() {
        for a in 0u8 .. 255 {
            for b in 0u8 .. 255 {
                let must_be_a = true.const_if(a, b);
                let must_be_b = false.const_if(a, b);

                assert_eq!(must_be_a, a);
                assert_eq!(must_be_b, b);
            }
        }
    }
}
