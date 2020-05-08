//! Wrapper around MMIO accesses.
//!
//! Passthrough implementation that performs volatile reads and writes.

pub unsafe fn read_volatile<T>(src: *const T) -> T {
    ::core::ptr::read_volatile(src)
}

pub unsafe fn write_volatile<T>(dst: *mut T, src: T) {
    ::core::ptr::write_volatile(dst, src)
}
