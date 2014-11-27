#![feature(phase, slicing_syntax, asm)]
#![feature(lang_items)]

#[phase(plugin)]
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

    pub fn use_ref(a: &u8) -> u8 {
        let b = *a;
        return b;
    }

    pub fn name_clash_check(a: u8) -> u8 {
        let a = a + 1;
        let a = a + 2;
        return a;
    }
}

#[allow(unsigned_negation)]
#[test]
fn test_ops() {
    for a in range(0u8, 255) {
        assert_eq!(ops::neg8(a), -a);
        assert_eq!(ops::not8(a), !a);

        assert_eq!(ops::use_ref(&a), a);

        for b in range(0u8, 255) {
            assert_eq!(ops::add8(a, b), a + b);
            assert_eq!(ops::xor8(a, b), a ^ b);
            assert_eq!(ops::mul8(a, b), a * b);

            assert_eq!(ops::gt8(a, b), a > b);
            assert_eq!(ops::lt8(a, b), a < b);
            assert_eq!(ops::ge8(a, b), a >= b);
            assert_eq!(ops::le8(a, b), a <= b);
            assert_eq!(ops::eq8(a, b), a == b);

            let a16 = a as u16;
            let a16 = (a16 << 8) | a16;
            let b16 = b as u16;
            let b16 = (b16 << 8) | b16;
            assert_eq!(ops::add16(a16, b16), a16 + b16);
            assert_eq!(ops::xor16(a16, b16), a16 ^ b16);
            assert_eq!(ops::mul16(a16, b16), a16 * b16);
        }

        assert_eq!(ops::name_clash_check(a), a + 3);
    }
}
