#![feature(plugin)]
#![plugin(nadeko_plugin)]

extern crate nadeko;

#[const_time]
mod ops {
    pub fn add8(a: u8, b: u8) -> u8 {
        let ret = a + b;
        ret
    }

    pub fn add16(a: u16, b: u16) -> u16 {
        return a + b;
    }

    pub fn xor8(a: u8, b: u8) -> u8 {
        a ^ b
    }

    pub fn xor16(a: u16, b: u16) -> u16 {
        return a ^ b;
    }

    pub fn mul8(a: u8, b: u8) -> u8 {
        return a * b;
    }

    pub fn mul16(a: u16, b: u16) -> u16 {
        return a * b;
    }

    pub fn neg8(a: u8) -> u8 {
        return -a;
    }

    pub fn not8(a: u8) -> u8 {
        return !a;
    }

    pub fn gt8(a: u8, b: u8) -> bool {
        return a > b;
    }
    pub fn lt8(a: u8, b: u8) -> bool {
        return a < b;
    }
    pub fn ge8(a: u8, b: u8) -> bool {
        return a >= b;
    }
    pub fn le8(a: u8, b: u8) -> bool {
        return a <= b;
    }
    pub fn eq8(a: u8, b: u8) -> bool {
        return a == b;
    }

    pub fn max8(a: u8, b: u8) -> u8 {
        // constant-time if
        if a > b {
            a
        } else {
            b
        }
    }

    pub fn shr8(a: u8, b: usize) -> u8 {
        a >> b
    }

    pub fn sar8(a: i8, b: usize) -> i8 {
        a >> b
    }
}

#[test]
fn test_ops() {
    for a in 0u8 .. 128 {
        assert_eq!(ops::neg8(a), a.wrapping_neg());
        assert_eq!(ops::not8(a), !a);

        for b in 1usize .. 7 {
            assert_eq!(ops::shr8(a, b), a >> b);
            assert_eq!(ops::sar8(a as i8, b), (a as i8) >> b);
        }

        for b in 0u8 .. 255 {
            assert_eq!(ops::add8(a, b), a.wrapping_add(b));
            assert_eq!(ops::xor8(a, b), a ^ b);
            assert_eq!(ops::mul8(a, b), a.wrapping_mul(b));

            assert_eq!(ops::gt8(a, b), a > b);
            assert_eq!(ops::lt8(a, b), a < b);
            assert_eq!(ops::ge8(a, b), a >= b);
            assert_eq!(ops::le8(a, b), a <= b);
            assert_eq!(ops::eq8(a, b), a == b);

            assert_eq!(ops::max8(a, b), if a > b { a } else { b });

            let a16 = a as u16;
            let a16 = (a16 << 8) | a16;
            let b16 = b as u16;
            let b16 = (b16 << 8) | b16;
            assert_eq!(ops::add16(a16, b16), a16.wrapping_add(b16));
            assert_eq!(ops::xor16(a16, b16), a16 ^ b16);
            assert_eq!(ops::mul16(a16, b16), a16.wrapping_mul(b16));
        }
    }
}
