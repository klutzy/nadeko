#![feature(plugin)]
#![plugin(nadeko)]

extern crate nadeko;

// mecha mecha secure crypto
// http://www.anagram.com/jcrap/Volume_3/caesar.pdf
#[const_time]
mod rot13 {
    const KEY: u8 = b'N' - b'A';
    pub fn encrypt(a: u8) -> u8 {
        // check if `a` is N ~ Z.
        let a_is_low = ((a & 0b_1101_1111) - b'N') >> 7;

        let encrypted = if a_is_low == 1 {
            a + KEY
        } else {
            a - KEY
        };

        return encrypted;
    }
}

#[test]
fn test_rot13() {
    for a in b'A' .. b'Z' + 1 {
        let expected = if a <= b'M' { a + 13 } else { a - 13 };
        let output = rot13::encrypt(a);
        println!("input: {}", a);
        assert_eq!(expected, output);
    }
}
