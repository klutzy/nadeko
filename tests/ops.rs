#![feature(phase)]

#[phase(plugin, link)]
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

    #[allow(unsigned_negation)]
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

    pub fn shr8(a: u8, b: uint) -> u8 {
        a >> b
    }

    pub fn sar8(a: i8, b: uint) -> i8 {
        a >> b
    }
}

#[allow(unsigned_negation)]
#[test]
fn test_ops() {
    for a in range(0u8, 255) {
        assert_eq!(ops::neg8(a), -a);
        assert_eq!(ops::not8(a), !a);

        for b in 1u .. 7 {
            assert_eq!(ops::shr8(a, b), a >> b);
            assert_eq!(ops::sar8(a as i8, b), (a as i8) >> b);
        }

        for b in 0u8 .. 255 {
            assert_eq!(ops::add8(a, b), a + b);
            assert_eq!(ops::xor8(a, b), a ^ b);
            assert_eq!(ops::mul8(a, b), a * b);

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
            assert_eq!(ops::add16(a16, b16), a16 + b16);
            assert_eq!(ops::xor16(a16, b16), a16 ^ b16);
            assert_eq!(ops::mul16(a16, b16), a16 * b16);
        }
    }
}
