(crazy idea. wip. experimental status. do not use this.)

nadeko is a syntax extension which converts functions into amd64 assembly code.
For example,

```rust
#[const_time]
mod example {
    pub fn add3(a: u8, b: u8, c: u8) -> u8 {
        return a + b + c;
    }
}
```

becomes


```rust
mod example {
    pub fn add3(a: u8, b: u8, c: u8) -> u8 {
        return {
                   let mut blk0: u8;
                   let mut lhs1: u8;
                   unsafe {
                       asm!("add $1, $0": "=r"(lhs1) : "r"(b), "0"(a) : "cc")
                   }
                   unsafe {
                       asm!("add $1, $0": "=r"(blk0) : "r"(c), "0"(lhs1) :
                           "cc")
                   }
                   blk0
               };
    }
}
```
