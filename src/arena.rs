use std::cell::RefCell;
use std::mem;

pub struct GArena {
    items: RefCell<Vec<GArenaItem>>
}
impl GArena {
    pub fn new() -> GArena {
        GArena { items: RefCell::new(Vec::new()) }
    }

    pub unsafe fn alloc<T>(&self, x: T) -> &mut T {
        let (ptr, gai) = GArenaItem::new(x);

        // Record the GArenaItem so that it can be disposed of later
        self.items.borrow_mut().push(gai);

        // Get out the pointer
        &mut *ptr
    }
}

struct GArenaItem {
    data: *mut u8,
    drop_handler: fn (*mut u8),
}
impl GArenaItem {
    fn new<T>(data: T) -> (*mut T, GArenaItem) {
        fn drop_handler<T>(ptr: *mut u8) {
            println!("Dropping something of size {}", mem::size_of::<T>());
            unsafe { let _: Box<T> = mem::transmute(ptr); }
            // b is dropped as it leaves scope here
        }

        let dp: *mut T = unsafe {
            mem::transmute(Box::new(data))
        };

        (dp, GArenaItem {
            data: dp as *mut u8,
            // Pass in a specialized drop_handler for the given type
            // so that drops are peformed correctly
            drop_handler: drop_handler::<T>,
        })
    }
}

impl Drop for GArenaItem {
    fn drop(&mut self) {
        (self.drop_handler)(self.data);
    }
}
