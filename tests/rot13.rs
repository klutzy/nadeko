#![feature(phase, slicing_syntax, asm)]
#![feature(lang_items)]

#[phase(plugin)]
extern crate nadeko;

// mecha mecha secure crypto
#[const_time]
mod rot13 {
    pub fn choose(a: u8, b:u8, choose_a: u8) -> u8 {
        let axb = a ^ b;
        // a ^ b if choose_a, 0 if not
        let mut mask = axb;
        mask = mask * choose_a;
        // a if choose_a, b if not
        let ret = mask ^ b;
        return ret;
    }

    pub fn encrypt(a: u8) -> u8 {
        // for A ~ M, add 13.
        let a_high = a + 13u8;
        // for N ~ Z, subtract 13.
        let a_low = a - 13u8;

        // check if `a` is N ~ Z.
        let a_is_low = ((a & 0b_1101_1111) - b'N') >> 7;

        let a_encrypted = choose(a_high, a_low, a_is_low);

        return a_encrypted;
    }
}

#[test]
fn test_choose() {
    for a in range(0u8, 255) {
        for b in range(0u8, 255) {
            let must_be_a = rot13::choose(a, b, 1);
            let must_be_b = rot13::choose(a, b, 0);

            assert_eq!(must_be_a, a);
            assert_eq!(must_be_b, b);
        }
    }
}

#[test]
fn test_rot13() {
    for a in range(b'A', b'Z' + 1) {
        let expected = if a <= b'M' { a + 13 } else { a - 13 };
        let output = rot13::encrypt(a);
        println!("input: {}", a);
        assert_eq!(expected, output);
    }
}
