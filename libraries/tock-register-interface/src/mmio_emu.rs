//! MMIO backend for emulating register reads and writes.
//!

// This module runs on the host and can use libstd.
use std::cmp::{Eq, Ord, Ordering, PartialEq, PartialOrd};
use std::collections::BTreeMap;
use std::fmt;
use std::mem::{MaybeUninit, size_of};
use std::slice;
use std::sync::{Arc, Mutex};

use crate::lazy::Lazy;

#[derive(Debug)]
pub enum Error {
    RegionOverlaps(usize, usize),
}

pub type Result<T> = std::result::Result<T, Error>;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Error::*;

        match self {
            RegionOverlaps(b, s) => write!(f, "region overlaps with existing region of base {:x} size {:x}", b, s),
        }
    }
}

#[derive(Copy, Clone)]
pub struct MmioRegion {
    base: usize,
    size: usize,
}

impl PartialEq for MmioRegion {
    fn eq(&self, other: &Self) -> bool {
        self.base == other.base
    }
}

impl Eq for MmioRegion {}

impl PartialOrd for MmioRegion {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.base.partial_cmp(&other.base)
    }
}

impl Ord for MmioRegion {
    fn cmp(&self, other: &Self) -> Ordering {
        self.base.cmp(&other.base)
    }
}

struct MmioEmu {
    devices: BTreeMap<MmioRegion, Arc<Mutex<dyn MmioDevice>>>,
}

impl MmioEmu {
    fn new() -> Self {
        MmioEmu {
            devices: BTreeMap::new(),
        }
    }

    fn add_device(&mut self, region: MmioRegion, device: Arc<Mutex<dyn MmioDevice>>) -> Result<()> {
        // TODO(check for overlap)
        let foo = self.devices.insert(region, device);
        match foo {
            Some(_) => Err(Error::RegionOverlaps(region.base, region.size)),
            None => Ok(())
        }
    }

    fn first_before(&self, addr: usize) -> Option<(MmioRegion, &Mutex<dyn MmioDevice>)> {
        let (range, dev) = self
            .devices
            .range(
                ..=MmioRegion {
                    base: addr,
                    size: 1,
                },
            )
            .rev()
            .next()?;
        Some((*range, dev))
    }

    fn get_device(&self, addr: usize) -> Option<(usize, &Mutex<dyn MmioDevice>)> {
        if let Some((range, dev)) = self.first_before(addr) {
            let offset = addr - range.base;
            if offset < range.size {
                return Some((offset, dev));
            }
        }
        None
    }

    /// Reads data from the device that owns the range containing `addr` and puts it into `data`.
    ///
    /// Returns true on success, otherwise `data` is untouched.
    fn read(&self, addr: usize, data: &mut [u8]) -> bool {
        if let Some((offset, dev)) = self.get_device(addr) {
            dev.lock().unwrap().mmio_read(offset, data);
            true
        } else {
            false
        }
    }

    /// Writes `data` to the device that owns the range containing `addr`.
    ///
    /// Returns true on success, otherwise `data` is untouched.
    fn write(&self, addr: usize, data: &[u8]) -> bool {
        if let Some((offset, dev)) = self.get_device(addr) {
            dev.lock().unwrap().mmio_write(offset, data);
            true
        } else {
            false
        }
    }

}

static MMIO_STATE: Lazy<Mutex<MmioEmu>> = Lazy::new();

fn get_mmio_state() -> &'static Mutex<MmioEmu> {
    MMIO_STATE.get(|| Mutex::new(MmioEmu::new()))
}

pub trait MmioDevice: Send {
    /// Reads at `offset` from this device
    fn mmio_read(&mut self, offset: usize, data: &mut [u8]);
    /// Writes at `offset` into this device
    fn mmio_write(&mut self, offset: usize, data: &[u8]);
}

pub fn register_mmio_device<R: 'static>(device: Arc<Mutex<dyn MmioDevice>>, registers: &'static R) -> Result<()> {
    let region = MmioRegion {
        base: registers as *const R as usize,
        size: size_of::<R>(),
    };

    let mut mmio_state = get_mmio_state().lock().unwrap();
    mmio_state.add_device(region, device)?;

    Ok(())
}

pub unsafe fn read_volatile<T>(src: *const T) -> T {
    let mut result = MaybeUninit::<T>::uninit();
    let bytes_slice = slice::from_raw_parts_mut(result.as_mut_ptr() as *mut u8, size_of::<T>());
    let mmio_state = get_mmio_state().lock().unwrap();
    mmio_state.read(src as usize, bytes_slice);
    result.assume_init()
}

pub unsafe fn write_volatile<T>(dst: *mut T, src: T) {
    let bytes_slice = slice::from_raw_parts(&src as *const T as  *const u8, size_of::<T>());
    let mmio_state = get_mmio_state().lock().unwrap();
    mmio_state.write(dst as usize, bytes_slice);
}

#[cfg(test)]
mod tests {
    use std::convert::TryInto;
    use crate::registers::*;
    use super::*;

    #[repr(C)]
    struct CounterRegisters {
        counter: ReadOnly<u64>,
        increment: WriteOnly<u32>,
    }

    struct TestDevice {
        value: u64,
    }
    impl MmioDevice for TestDevice {
        /// Reads at `offset` from this device
        fn mmio_read(&mut self, offset: usize, data: &mut [u8]) {
            match offset {
                0x0 => data.copy_from_slice(&self.value.to_ne_bytes()),
                _ => panic!("TestDevice: illegal read offset {:x}", offset),
            }
        }
        /// Writes at `offset` into this device
        fn mmio_write(&mut self, offset: usize, data: &[u8]) {
            match offset {
                0x8 => {
                    let increment = u32::from_ne_bytes(data.try_into().unwrap());
                    self.value += u64::from(increment);
                },
                _ => panic!("TestDevice: illegal write offset {:x}", offset),
            }
        }
    }

    #[test]
    fn counter_device() {
        static FAKE_REGS: MaybeUninit<CounterRegisters> = MaybeUninit::uninit();
        let device = Arc::new(Mutex::new(TestDevice{value: 0}));
        register_mmio_device(device, &FAKE_REGS).unwrap();

        let regs = unsafe { &*FAKE_REGS.as_ptr() };

        assert_eq!(regs.counter.get(), 0);
        regs.increment.set(5);
        assert_eq!(regs.counter.get(), 5);
        regs.increment.set(1);
        assert_eq!(regs.counter.get(), 6);
    }
}
