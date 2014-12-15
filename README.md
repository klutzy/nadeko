This is old archival branch of nadeko.

In this branch, *rustc typecker* is invoked to get types of each expr.
Does it sound crazy? Yes it really is. It is also really fun though. :)

However, it does not work well, because rustc engine is under the assumption
that there is only one driver per thread.
This method violates it because new driver is run inside normal driver.

So far it seems to work unless macros are involved. During macro expansion
some ast nodes get wrong span and I don't know the exact reason.
Although it does seem to work for certain situations, since it's fundamentally
fragile way, I'm going to do much simpler approach:
use traits to generate type-dependent asm code.

See master branch for the progress.

# Old desc

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
