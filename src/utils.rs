use bitflags::bitflags;

bitflags! {
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct Mutable: u8 {
        const Reassignable = 1;
        const Modifiable = 1 << 2;
        const NONE = 0b0000;
        const ALL = Self::Reassignable.bits() | Self::Modifiable.bits();
    }
}
