//! Lazy global singleton.
//!
//! A safe, lazily initialized global singleton with interior mutability.

use std::cell::Cell;
use std::mem::MaybeUninit;
use std::sync::Once;

pub(crate) struct Lazy<T: Sync> {
    inner: Cell<MaybeUninit<T>>,
    once: Once,
}

impl<T: Sync> Lazy<T> {
    pub const fn new() -> Lazy<T> {
        Lazy{
            inner: Cell::new(MaybeUninit::uninit()),
            once: Once::new(),
        }
    }

    pub fn get<F>(&'static self, f: F) -> &'static T
    where
        F: FnOnce() -> T {
        self.once.call_once(||
            self.inner.set(MaybeUninit::new(f()))
        );

        // Safe because inner's MaybeUninit must be initialized by call_once.
        unsafe {
            (*self.inner.as_ptr()).as_mut_ptr().as_ref().unwrap()
        }
    }
}

// Safe because inner's Cell and MaybeUninit are only ever modified in a thread-safe way via Once.
unsafe impl<T: Sync> Sync for Lazy<T> {}

