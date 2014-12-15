nadeko is an experimental syntax extension which converts functions into
amd64 assembly code.

For example,

```rust
#[const_time]
pub fn add3(a: u8, b: u8, c: u8) -> u8 {
    return a + b + c;
}
```

becomes


```rust
pub fn add3(a: u8, b: u8, c: u8) -> u8 {
    return a.const_add(b).const_add(c);
}
```

where `const_add` is implemented as:

```rust
pub trait ConstAdd {
    fn const_add(self, b: Self) -> Self;
}

impl ConstAdd for u8 {
    #[inline(always)]
    fn const_add(self, b: u8) -> u8 {
        let mut ret: u8;
        unsafe {
            asm!("add $1, $0": "=r"(ret) : "r"(b), "0"(self) : "cc");
        }
        ret
    }
}
```

# Why?

Cryptographic primitives require constant time behavior regardless of input.
However, this is not easy with usual compilers like llvm.

For example, the following function does not have any branch, therefore
it seems that it runs in constant time.

```rust
/// returns `if cond { a } else { b }`.
fn choice(cond: bool, a: u8, b: u8) -> u8 {
    // switch = if cond { 0 } else { 0b111...111 }
    let switch = (cond as u8) - 1;

    let axb = a ^ b;

    // mask = if cond { 0 } else { a ^ b }
    let mask = axb & switch;

    // ret = if cond { a } else { b }
    let ret = mask ^ a;

    ret
}
```

However, with `--opt-level=1` or higher, llvm produces the following code:

```asm
    testb    %dil, %dil
    jne    .LBB0_1
    xorl    %esi, %edx
    jmp    .LBB0_3
.LBB0_1:
    xorl    %edx, %edx
.LBB0_3:
    xorb    %sil, %dl
    movb    %dl, %al
    retq
```

Here llvm is too smart, so `axb & switch` is "optimized" and becomes
`if cond { 0 } else { axb }`.

On the other hand, with `#[const_time]` llvm produces the following code:

```asm
    movb    $1, %al
    subb    %al, %dil
    movb    %sil, %al
    xorb    %dl, %al
    andb    %dil, %al
    xorb    %sil, %al
    retq
```

# Current status

Currently basic primitive arithmetic and restricted form of
`if ... {...} else {...} ` are implemented.
`a + b` becomes `a.const_add(b)`, and `if cond { a } else { b }` becomes
`cond.const_if(a, b)`.

Also, `slice[expr]` does not compile if `expr` is not a constant literal.

`for` is not implemented yet.
