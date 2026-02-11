# Dynamic Classes: Haskell-Backed Objective-C Objects

This document explains how `ObjC.Runtime.DynClass` and `ObjC.Runtime.TH` work
together to let you define Objective-C classes whose method implementations are
Haskell closures.

## The Problem

Objective-C expects method implementations (IMPs) to be C function pointers.
Haskell closures are not C function pointers — they capture environment and live
on the GHC heap. We need a way to:

1. Register C-callable function pointers with the ObjC runtime.
2. Route calls from those function pointers back to per-instance Haskell
   closures.
3. Do this without requiring the user to write any C or FFI boilerplate.

## The Solution: Per-Instance Vtables

The approach is inspired by C++ vtables but applied per-instance rather than
per-class.

### Overview

Each instance of a Haskell-backed class carries a **vtable** — a heap-allocated
C array of `void *` slots, one per instance method. Each slot holds a `FunPtr`
pointing to a Haskell closure (wrapped via `foreign import ccall "wrapper"`).

The class has a single set of **stub IMPs** registered at class-creation time.
When the ObjC runtime calls a stub, it:

1. Reads the vtable pointer from `self`'s `_hs_vtable` ivar.
2. Indexes into the vtable by a compile-time-known slot number.
3. Casts the slot to a `FunPtr` and calls through it.

This means different instances of the same class can have different Haskell
implementations for the same selector — each instance's vtable points to its
own set of wrapped closures.

```
                 ObjC Runtime
                      │
                      │ calls stub IMP with (self, _cmd, args...)
                      ▼
              ┌───────────────┐
              │   Stub IMP    │  (one per selector, shared by all instances)
              │               │
              │  1. vt = self._hs_vtable
              │  2. fp = vt[slot]
              │  3. call fp(self, _cmd, args...)
              └───────┬───────┘
                      │
          ┌───────────┴───────────┐
          ▼                       ▼
   ┌─────────────┐        ┌─────────────┐
   │ Instance A  │        │ Instance B  │
   │  vtable:    │        │  vtable:    │
   │  [0] = fpA0 │        │  [0] = fpB0 │
   │  [1] = fpA1 │        │  [1] = fpB1 │
   └─────────────┘        └─────────────┘
         │                       │
         ▼                       ▼
   Haskell closure A       Haskell closure B
   (captures IORef, etc.)  (captures different state)
```

## Module Breakdown

### `DynClass.hs` — Low-Level Vtable Infrastructure

This module provides the building blocks that everything else is built on.

#### The `_hs_vtable` Ivar

Every Haskell-backed class has one extra ivar added during class construction:

```haskell
addVtableIvar :: Class -> IO ()
```

This adds an ivar named `_hs_vtable` with type encoding `^v` (pointer to void)
to a class that is being constructed (i.e., between `objc_allocateClassPair` and
`objc_registerClassPair`). The ivar is pointer-sized and pointer-aligned.

The ivar holds the address of the instance's vtable array.

#### Reading and Writing the Vtable

```haskell
readVtable  :: Ptr ObjCObject -> IO (Ptr (Ptr ()))
writeVtable :: RawId -> Ptr (Ptr ()) -> IO ()
```

Both functions look up the `_hs_vtable` ivar descriptor from the object's class,
get the byte offset via `ivar_getOffset`, and then `peek`/`poke` at that offset
from the object pointer.

`readVtable` takes a raw `Ptr ObjCObject` rather than `RawId` because it is
called from within IMP stubs where the `self` argument arrives as a bare pointer.
This avoids constructing and immediately destructing a `RawId` wrapper in the
hot dispatch path.

`writeVtable` takes `RawId` because it is called during object construction
where you already have a `RawId`.

#### How the Offset is Found

```haskell
vtableOffset :: Ptr ObjCObject -> IO CPtrdiff
vtableOffset self = do
  cls  <- c_object_getClass (RawId self)
  ivar <- withCString "_hs_vtable" $ \nameC ->
    class_getInstanceVariable cls nameC
  ivar_getOffset ivar
```

This queries the runtime for the ivar metadata every time. The offset is
stable for the lifetime of the class, so in principle it could be cached... but
the runtime lookup is a fast hash-table probe and the simplicity is worth it.

#### Method Registration

```haskell
addObjCMethod :: Class -> String -> String -> FunPtr a -> IO ()
```

A convenience wrapper around `class_addMethod` that takes Haskell `String`s
for the selector name and type encoding. The `FunPtr` is cast to `IMP` via:

```haskell
castIMP :: FunPtr a -> IMP
castIMP fp = IMP (castPtrToFunPtr (castFunPtrToPtr fp))
```

The Objective-C runtime stores all method implementations as `IMP` regardless of
actual signature; the type encoding string tells the runtime (and bridge tools)
what the actual calling convention is.

### `TH.hs` — Template Haskell Class Definition

`TH.hs` provides `defineClass`, which generates all the boilerplate to wire
up `DynClass`. A typical invocation:

```haskell
$(defineClass "CounterTarget" "NSObject" $ do
    instanceMethod "increment:" [t| RawId -> IO () |]
    instanceMethod "decrement:" [t| RawId -> IO () |]
  )
```

This splice produces five groups of declarations:

#### 1. The Impl Record

```haskell
data CounterTargetImpl = CounterTargetImpl
  { _increment :: RawId -> IO ()
  , _decrement :: RawId -> IO ()
  }
```

Each instance method becomes a field. The user provides an `IO
CounterTargetImpl` builder to the constructor, which is where Haskell closures
(capturing `IORef`s, `MVar`s, etc.) get wired in.

#### 2. Foreign Wrapper Imports

For each unique method signature, TH generates:

```haskell
foreign import ccall "wrapper"
  hs_wrap_CounterTarget_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))
```

This is GHC's mechanism for turning a Haskell closure into a C-callable
`FunPtr`. The signature includes the implicit `self` and `_cmd` parameters that
every ObjC IMP receives.

The `_at_v` suffix is a fingerprint of the argument/return types (here: one
`@`-typed arg, `v`oid return). If two methods share a signature, they share
the wrapper import.

#### 3. Foreign Dynamic Imports

The inverse: calling a `FunPtr` as a Haskell function:

```haskell
foreign import ccall "dynamic"
  hs_call_CounterTarget_at_v
    :: FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
```

These are used inside the stub IMPs to call back through the vtable slots.

#### 4. The Class CAF

A top-level `Class` value created once via `unsafePerformIO`:

```haskell
{-# NOINLINE counterTargetClass #-}
counterTargetClass :: Class
counterTargetClass = unsafePerformIO $ do
  super <- getRequiredClass "NSObject"
  cls   <- withCString "CounterTarget" $ \n ->
    objc_allocateClassPair super n 0
  addVtableIvar cls                          -- (a)

  stub_increment <- hs_wrap_CounterTarget_at_v $ \self _cmd x0 -> do
    vt <- readVtable self                    -- (b)
    p  <- peekElemOff vt 0                   -- (c)
    hs_call_CounterTarget_at_v               -- (d)
      (castPtrToFunPtr p) self _cmd x0
  addObjCMethod cls "increment:" "v@:@" stub_increment

  stub_decrement <- hs_wrap_CounterTarget_at_v $ \self _cmd x0 -> do
    vt <- readVtable self
    p  <- peekElemOff vt 1                   -- slot 1
    hs_call_CounterTarget_at_v
      (castPtrToFunPtr p) self _cmd x0
  addObjCMethod cls "decrement:" "v@:@" stub_decrement

  addDeallocHandler cls 2                    -- (e)

  objc_registerClassPair cls
  pure cls
```

Step by step:

- **(a)** Adds the `_hs_vtable` ivar to the class under construction.
- **(b)** When the stub is called, it reads the vtable from `self`.
- **(c)** It indexes into the vtable at the method's slot (compile-time constant).
- **(d)** It calls through the `FunPtr` found at that slot, forwarding all
  arguments.
- **(e)** Registers a `dealloc` override that frees the vtable (see below).

The stub is wrapped with `hs_wrap_...` (the `"wrapper"` foreign import) so it
becomes a stable `FunPtr` suitable for `addObjCMethod`.

The `NOINLINE` pragma + `unsafePerformIO` pattern ensures the class is created
exactly once, the first time `counterTargetClass` is evaluated.

#### 5. The Constructor

```haskell
newCounterTarget :: IO CounterTargetImpl -> IO RawId
newCounterTarget mkImpl = do
  inst <- class_createInstance counterTargetClass 0
  impl <- mkImpl                                -- run the user's builder

  vtbl <- mallocArray 2                         -- 2 instance methods
  fp0  <- hs_wrap_CounterTarget_at_v $ \_self _cmd x0 ->
    _increment impl (RawId x0)                  -- call the record field
  pokeElemOff vtbl 0 (castFunPtrToPtr fp0)

  fp1  <- hs_wrap_CounterTarget_at_v $ \_self _cmd x0 ->
    _decrement impl (RawId x0)
  pokeElemOff vtbl 1 (castFunPtrToPtr fp1)

  writeVtable inst vtbl                         -- store vtable in ivar
  pure inst
```

The constructor:

1. Creates a new ObjC instance of the dynamic class.
2. Runs the user's `IO CounterTargetImpl` to get the method implementations
   (this is where the user captures `IORef`s and other state).
3. Allocates a vtable array with one slot per instance method.
4. For each method, wraps the record field (a Haskell closure) into a `FunPtr`
   and stores it in the vtable.
5. Writes the vtable pointer into the instance's `_hs_vtable` ivar.

Note that the wrappers in step 4 do type conversion between C types and Haskell
types (e.g., `Ptr ObjCObject` to `RawId`, `CLong` to `Int`), so the user's
impl record uses idiomatic Haskell types.

## Full Dispatch Flow

Here is what happens when Objective-C sends `increment:` to a `CounterTarget`
instance:

```
1.  ObjC runtime: [target increment:sender]
2.  Runtime looks up IMP for "increment:" on CounterTarget
3.  Finds stub_increment (registered during class creation)
4.  Calls stub_increment(self, _cmd, sender)
5.  stub reads self._hs_vtable → vtbl pointer
6.  stub reads vtbl[0] → FunPtr to wrapped Haskell closure
7.  stub calls through FunPtr, forwarding (self, _cmd, sender)
8.  Wrapper converts Ptr ObjCObject args to RawId
9.  Calls _increment field of the CounterTargetImpl
10. Haskell closure runs (e.g., modifyIORef counter (+1))
```

## Usage Example

From `examples/counter/Main.hs`:

```haskell
$(defineClass "CounterTarget" "NSObject" $ do
    instanceMethod "increment:" [t| RawId -> IO () |]
    instanceMethod "decrement:" [t| RawId -> IO () |]
  )

main :: IO ()
main = do
  counter <- newIORef (0 :: Int)

  target <- newCounterTarget $ do
    pure CounterTargetImpl
      { _increment = \_ -> do
          modifyIORef' counter (+ 1)
          updateLabel counter label
      , _decrement = \_ -> do
          modifyIORef' counter (subtract 1)
          updateLabel counter label
      }

  -- target is now a RawId that ObjC can send messages to.
  -- Pass it as a button's target with "increment:" as the action selector.
```

Each `CounterTarget` instance created by `newCounterTarget` gets its own vtable
pointing to its own closures. Two instances can close over different `IORef`s
and behave independently — the vtable is per-instance, not per-class.

## Dealloc: Cleaning Up the Vtable

When an ObjC object's reference count hits zero, the runtime calls its
`-dealloc` method. `addDeallocHandler` registers a `dealloc` override on the
dynamic class that handles cleanup:

```haskell
addDeallocHandler :: Class -> Int -> IO ()
```

The generated `dealloc` IMP does three things:

1. **Frees each `FunPtr` in the vtable** via `freeHaskellFunPtr`. Each slot was
   created by `foreign import ccall "wrapper"`, which allocates a C-callable
   adjustor thunk. These must be explicitly freed or they leak.

2. **Frees the vtable array** itself via `free` (it was allocated with
   `mallocArray`).

3. **Calls `[super dealloc]`** via `objc_msgSendSuper` (through `sendSuperMsg`)
   to let NSObject (or whatever superclass) do its own teardown.

The handler guards against null vtable pointers and null slots defensively,
though neither should occur in normal use.

This is registered during class construction (in the CAF), so the `dealloc`
IMP is shared by all instances — just like the dispatch stubs. The per-instance
data (the vtable array and its FunPtr slots) is what gets freed.

### Lifecycle Summary

```
newCounterTarget:
  1. class_createInstance  →  ObjC allocates instance, _hs_vtable = NULL
  2. mallocArray nSlots    →  allocate vtable array
  3. hs_wrap_... per slot  →  wrap closures into FunPtrs, poke into vtable
  4. writeVtable           →  store vtable pointer in _hs_vtable ivar

... instance is used, messages dispatched through vtable ...

-[instance dealloc]:       (called when refcount hits 0)
  1. readVtable            →  get vtable pointer
  2. freeHaskellFunPtr     →  free each FunPtr slot
  3. free vtable           →  free the array
  4. [super dealloc]       →  NSObject cleanup
```

## Design Tradeoffs

**Why per-instance vtables instead of per-class?** ObjC method dispatch is
per-class (all instances share the same IMP). But Haskell closures capture
per-instance state. The vtable indirection bridges this gap: the class has one
IMP per selector (the stub), but each instance's stub dispatches to a different
closure via its own vtable.

**Why not use associated objects?** Associated objects (objc_setAssociatedObject)
would work but add a hash-table lookup per dispatch. An ivar holding a direct
pointer to the vtable array is a single pointer dereference — faster and
simpler.

**Why `unsafePerformIO` for the class CAF?** ObjC classes are registered once
and live forever. A `NOINLINE` CAF with `unsafePerformIO` is the standard
Haskell pattern for this (similar to how top-level `IORef`s or `MVar`s are
created). The `NOINLINE` prevents the optimizer from duplicating the class
registration.

**Why does `readVtable` take `Ptr ObjCObject` instead of `RawId`?** Performance.
The IMP stub receives `self` as a raw pointer. Wrapping it in `RawId` just to
immediately unwrap it would be wasted work on every method call.
