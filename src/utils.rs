use bitflags::bitflags;

bitflags! {
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct Mutable: u8 {
        const Reassignable = 0b0001;
        const Modifiable = 0b0010;
        const NONE = 0b0000;
        const ALL = Self::Reassignable.bits() | Self::Modifiable.bits();
    }
}
