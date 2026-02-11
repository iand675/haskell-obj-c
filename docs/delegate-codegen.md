# Haskell-Backed ObjC Classes Without Template Haskell

Design for a unified "overrides" pattern that eliminates Template Haskell from
the common cases: protocol delegates, target/action, multi-protocol objects,
and subclass overrides.

## Unifying Principle

Every use of `defineClass` today follows the same shape:

1. Create a dynamic subclass of some ObjC class.
2. Register IMP stubs for specific methods.
3. Store per-instance Haskell state so the stubs can call back into it.
4. Provide a constructor that wires up an instance.

Whether you're implementing `NSTableViewDelegate`, hooking up a button
target, or overriding `drawRect:` on `NSView`, the mechanics are identical.
The only axes of variation are:

- **Base class**: `NSObject` for delegates/targets, `NSView`/etc. for
  subclass overrides.
- **Which methods**: Protocol methods for delegates, `-(void)action:(id)sender`
  for targets, declared methods for subclass overrides.
- **Init pattern**: `class_createInstance` (no init) for delegates/targets,
  `initWithFrame:` etc. for view subclasses.

The design factors this into:

- **Shared runtime infrastructure** (`ObjC.Runtime.StableIvar`): StablePtr-in-ivar
  storage, dealloc handler, method registration.
- **Runtime helpers** that need no codegen at all:
  - `ObjC.Runtime.ActionTarget`: target/action pattern
  - `ObjC.Runtime.DelegateProxy`: multi-protocol forwarding proxy
- **Codegen'd modules** (future): one per protocol, one per commonly-subclassed
  class — generated as ordinary Haskell source, no TH.

## Architecture: StablePtr Instead of Vtable

The current TH approach (`DynClass` + `TH.hs`) allocates a per-instance C
array of `FunPtr`s and uses `foreign import ccall "dynamic"` to call back
through them.  This was necessary because TH wraps each record field into a
separate `FunPtr` at instance creation time.

The new approach stores a `StablePtr` to a Haskell value (a record, a Map,
etc.) in the same ivar, and has the shared IMP stubs deref it directly:

```
Current (vtable per instance):          New (StablePtr per instance):

  ObjC dispatch                           ObjC dispatch
       |                                       |
       v                                       v
  Stub IMP (shared)                       Stub IMP (shared)
       |                                       |
  read _hs_vtable ivar                   read _hs_vtable ivar
       |                                       |
  index vtable[slot]                      deRefStablePtr
       |                                       |
  foreign call "dynamic"                  access Haskell value
  through FunPtr (C -> Haskell)           (pure Haskell, no FFI)
       |                                       |
       v                                       v
  Haskell closure                         Haskell closure
```

### Advantages

- **No per-instance `FunPtr` allocation**: No `mallocArray`, no N x `wrapper`
  calls per instance, no `freeHaskellFunPtr` cleanup per slot.
- **No `foreign import ccall "dynamic"`**: The stub accesses the Haskell
  record directly — one fewer C-to-Haskell boundary crossing per dispatch.
- **Simpler dealloc**: Free one `StablePtr` vs N `FunPtr`s + array.
- **Optional methods are trivial**: Pattern-match on `Maybe` in Haskell;
  no null-pointer protocol in C vtable slots.
- **Uniform**: Works for delegates, targets, and subclass overrides with
  the same infrastructure.

### What We Still Need

Per dynamic class (not per instance):

- One `foreign import ccall "wrapper"` per unique IMP C-type signature.
  These are created once in the class CAF and shared by all instances.
- For codegen'd modules, these `wrapper` imports are emitted as ordinary
  top-level FFI declarations — no TH.
- For `ActionTarget`, only one wrapper type is needed since all actions have
  the same signature (`v@:@`).

## Runtime Module 1: `ObjC.Runtime.StableIvar`

Shared infrastructure used by all three patterns.

### API

```haskell
module ObjC.Runtime.StableIvar
  ( -- * Ivar management
    addHsDataIvar
  , writeHsData
  , readHsData

    -- * Dealloc handler
  , addStablePtrDeallocHandler

    -- * Re-exports from DynClass
  , addObjCMethod
  , castIMP
  ) where
```

### `addHsDataIvar`

Same as `addVtableIvar` — adds a pointer-sized ivar named `_hs_vtable` to
a class under construction.  We reuse the same ivar name so a single class
can use either the vtable approach (via `DynClass`) or the StablePtr approach
(via `StableIvar`), but not both simultaneously.

### `writeHsData` / `readHsData`

```haskell
-- | Store a StablePtr in an instance's ivar.
writeHsData :: RawId -> StablePtr a -> IO ()

-- | Read the StablePtr from an instance and deref it.
-- Takes raw Ptr ObjCObject (as received by an IMP stub).
readHsData :: Ptr ObjCObject -> IO (StablePtr a)
```

The ivar stores the `StablePtr` as a raw pointer.  `readHsData` returns the
`StablePtr` itself (not the dereffed value) so the caller can choose whether
to deref or peek.  In practice every call site immediately `deRefStablePtr`s.

### `addStablePtrDeallocHandler`

```haskell
-- | Register a dealloc method that frees the StablePtr and calls
-- [super dealloc].  No per-slot cleanup — just one freeStablePtr.
addStablePtrDeallocHandler :: Class -> IO ()
```

Replaces `addDeallocHandler` (which freed N `FunPtr`s + the vtable array).

## Runtime Module 2: `ObjC.Runtime.ActionTarget`

Covers target/action without TH or codegen.  Every Cocoa target/action method
has the same C signature: `-(void)action:(id)sender`, encoding `v@:@`.  This
means one `foreign import ccall "wrapper"` declaration suffices for all action
methods.

### API

```haskell
module ObjC.Runtime.ActionTarget
  ( newActionTarget
  ) where

-- | Create an ObjC object responding to the given action selectors.
--
-- Each selector is wired to a Haskell callback.  The returned 'RawId'
-- can be used as a button target, text field target, etc.
--
-- @
-- target <- newActionTarget
--   [ ("increment:", \\sender -> modifyIORef' counter (+1))
--   , ("decrement:", \\sender -> modifyIORef' counter (subtract 1))
--   ]
-- Ctrl.setTarget button target
-- Ctrl.setAction button (mkSelector "increment:")
-- @
newActionTarget :: [(String, RawId -> IO ())] -> IO RawId
```

### Implementation Sketch

1. Sort the selector names to produce a canonical key.
2. Look up in a global `MVar (Map [String] Class)` cache.
3. On cache miss, create a new dynamic class:
   - Name: `HsActionTarget_<hash>` (hash of sorted selector names).
   - Superclass: `NSObject`.
   - Add the `_hs_vtable` ivar.
   - For each selector, register an IMP stub that:
     a. `readHsData self` to get the `Map String (RawId -> IO ())`.
     b. Look up the handler by selector name (captured in the closure at
        class-creation time).
     c. Call the handler.
   - Register StablePtr dealloc handler.
   - Register the class pair.
4. Cache the class.
5. Create an instance via `class_createInstance`.
6. Build a `Map String (RawId -> IO ())` from the input list.
7. `newStablePtr` the map, `writeHsData` into the instance.
8. Return the `RawId`.

Since the action signature is always `v@:@`, there is exactly one
`foreign import ccall "wrapper"` in the module:

```haskell
foreign import ccall "wrapper"
  wrapActionIMP
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))
```

Each stub IMP is a different Haskell closure wrapping the same C type. The
closure captures which selector name to look up in the map.

### Class Caching

Action targets are typically created a handful of times (one per view
controller setup).  A global `MVar (Map [String] Class)` cache avoids
re-creating classes for the same selector set.  Two `ActionTarget` objects
with selectors `["increment:", "decrement:"]` share a single ObjC class.

Classes registered with the ObjC runtime live forever.  This is correct:
ObjC classes cannot be unregistered.

## Runtime Module 3: `ObjC.Runtime.DelegateProxy`

Composes multiple delegate objects into one using ObjC message forwarding.

### Motivation

Some ObjC APIs require the **same object** to serve as both delegate and
data source.  With per-protocol delegate objects, you'd otherwise need
`defineClass` to combine them.

### API

```haskell
module ObjC.Runtime.DelegateProxy
  ( newDelegateProxy
  ) where

-- | Create a proxy that forwards messages to the appropriate delegate.
--
-- When the proxy receives a message it doesn't directly implement, the
-- ObjC runtime calls @forwardingTargetForSelector:@ on the proxy.
-- The proxy iterates through the delegates and returns the first one
-- that responds to the selector.
--
-- @
-- dataSource <- newNSTableViewDataSource $ ...
-- delegate   <- newNSTableViewDelegate $ ...
-- combined   <- newDelegateProxy [dataSource, delegate]
--
-- sendMsg tableView (mkSelector "setDelegate:") retVoid
--   [argPtr (castPtr (unRawId combined))]
-- sendMsg tableView (mkSelector "setDataSource:") retVoid
--   [argPtr (castPtr (unRawId combined))]
-- @
newDelegateProxy :: [RawId] -> IO RawId
```

### Implementation Sketch

A single ObjC class (`HsDelegateProxy`) with three methods:

1. **`forwardingTargetForSelector:`** — The key method.  When the proxy
   receives any message it doesn't implement, the ObjC runtime calls this
   instead of raising `doesNotRecognizeSelector:`.  The stub:
   - `readHsData self` to get the `[RawId]` delegate list.
   - For each delegate, call `[delegate respondsToSelector:sel]`.
   - Return the first delegate that responds (as a `Ptr ObjCObject`).
   - If none responds, return nil (lets the runtime fall through to
     `doesNotRecognizeSelector:`).

2. **`respondsToSelector:`** — Override so that `[proxy respondsToSelector:sel]`
   returns YES for selectors handled by any delegate, not just selectors on
   the proxy class itself.  The stub:
   - Check each delegate's `respondsToSelector:`.
   - If any returns YES, return YES.
   - Otherwise, call `[super respondsToSelector:sel]`.

3. **`dealloc`** — Frees the StablePtr and calls `[super dealloc]`.

The proxy class is created once (NOINLINE CAF).  Only three `foreign import
ccall "wrapper"` declarations are needed:

```haskell
-- forwardingTargetForSelector: has signature @:: -> id
foreign import ccall "wrapper"
  wrapForwardingIMP :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel
                        -> IO (Ptr ObjCObject))
                    -> IO (FunPtr ...)

-- respondsToSelector: has signature @:: -> BOOL (unsigned long on ARM64)
foreign import ccall "wrapper"
  wrapRespondsIMP :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel
                      -> IO CULong)
                  -> IO (FunPtr ...)
```

(The dealloc wrapper is shared from StableIvar.)

### Performance

`forwardingTargetForSelector:` is called once per message send to the
proxy.  It iterates the delegate list (typically 2-3 entries) and does one
`respondsToSelector:` call per delegate.  At delegate-call frequency
(UI events, table view data requests) this is invisible.

The forwarding is a single extra message dispatch — the runtime re-sends
the message to the returned target.  No `NSInvocation` is involved.

## Codegen'd Modules (Future)

The codegen pipeline generates these as ordinary Haskell source, using
`StableIvar` infrastructure.  No TH in the generated code.

### Protocol Delegates

For each protocol, the codegen produces:

```haskell
-- ObjC/AppKit/Delegate/NSTableViewDelegate.hs

-- Overrides record — Maybe for optional, bare for required
data NSTableViewDelegateOverrides = NSTableViewDelegateOverrides
  { _tableView_viewForTableColumn_row :: Maybe (RawId -> RawId -> Int -> IO RawId)
  , _tableView_heightOfRow            :: Maybe (RawId -> Int -> IO Double)
  , ...
  }

-- All-Nothing default
defaultNSTableViewDelegateOverrides :: NSTableViewDelegateOverrides

-- foreign import ccall "wrapper" (one per unique IMP signature)
-- Class CAF with stubs + respondsToSelector: + dealloc
-- Constructor
newNSTableViewDelegate :: IO NSTableViewDelegateOverrides -> IO RawId
```

### Subclass Overrides

For commonly-subclassed classes, the codegen produces the same pattern but
with a different base class and appropriate init variants:

```haskell
-- ObjC/AppKit/Subclass/NSView.hs

data NSViewOverrides = NSViewOverrides
  { _drawRect            :: Maybe (NSRect -> IO ())
  , _mouseDown           :: Maybe (RawId -> IO ())
  , _mouseUp             :: Maybe (RawId -> IO ())
  , _keyDown             :: Maybe (RawId -> IO ())
  , _acceptsFirstResponder :: Maybe (IO Bool)
  , ...
  }

defaultNSViewOverrides :: NSViewOverrides

-- Constructor calls initWithFrame: on the subclass instance
newNSViewSubclass :: NSRect -> IO NSViewOverrides -> IO RawId
```

The init pattern for subclasses:

1. `class_createInstance` the dynamic subclass.
2. `writeHsData` the StablePtr into the ivar.
3. Send the appropriate `init` message to the instance.
4. Return the initialized instance.

Step 2 happens before step 3 so that if `init` triggers any overridden methods
(e.g., some inits call `layoutSubviews` internally), the StablePtr is already
in place and the stubs can read it.

### Which Methods to Include in Subclass Overrides

The codegen knows which methods each class **declares** (vs inherits).
Methods declared directly on `NSView` are the designed-to-be-overridden
entry points.  This is a reasonable heuristic for which methods appear in
the overrides record.  A curated annotation file could refine this for
popular classes.

## Coverage Summary

| Pattern | Module | TH? | Codegen? |
|---|---|---|---|
| Target/action | `ObjC.Runtime.ActionTarget` | No | No |
| Multi-protocol proxy | `ObjC.Runtime.DelegateProxy` | No | No |
| Protocol delegates | `ObjC.<Fw>.Delegate.<Proto>` | No | Yes |
| Subclass overrides | `ObjC.<Fw>.Subclass.<Class>` | No | Yes |
| Arbitrary (escape hatch) | `ObjC.Runtime.TH` | Yes | No |

The first two are pure runtime modules landing now.  The codegen modules
follow once the codegen pipeline is extended (optional method parsing,
inverse type mapping, delegate/subclass generators).  `defineClass` remains
for fully custom cases that don't fit any pattern.

## Type Mapping for Overrides Records

The codegen needs to map ObjC types to Haskell types for the **callback
direction** (ObjC calls into Haskell).  This is the inverse of the existing
`TypeMap.hs` which maps for the calling direction (Haskell calls ObjC).

### Parameter Types (ObjC -> Haskell)

| ObjC type | Record field type | Stub conversion |
|---|---|---|
| `id` / `NSFoo *` | `RawId` | `RawId ptr` |
| `NSInteger` / `NSUInteger` | `Int` | `fromIntegral` |
| `BOOL` | `Bool` | `/= 0` |
| `CGFloat` / `double` | `Double` | `realToFrac` |
| `float` | `Float` | `realToFrac` |
| `SEL` | `Selector` | `Selector ptr` |
| Struct (NSRect etc.) | struct type | pass-through |

### Return Types (Haskell -> ObjC)

| ObjC return | Record return | Stub conversion |
|---|---|---|
| `void` | `()` | — |
| `id` / `NSFoo *` | `RawId` | `castPtr . unRawId` |
| `NSInteger` | `Int` | `fromIntegral` |
| `BOOL` | `Bool` | `if r then 1 else 0` |
| `CGFloat` / `double` | `Double` | `realToFrac` |

All object types use `RawId` in the record.  Creating managed `Id` wrappers
inside callbacks would require retain/release management; `RawId` is safe
because ObjC guarantees the objects are alive for the callback's duration.

### Safe Defaults for Optional Methods

When an optional method's `Maybe` field is `Nothing`, the stub returns a
type-appropriate zero value:

| ObjC return type | Default |
|---|---|
| `void` | `pure ()` |
| `id` / pointer | `pure nullPtr` |
| Integer types | `pure 0` |
| `BOOL` | `pure 0` (NO) |
| Floating point | `pure 0.0` |

## `respondsToSelector:` Override

Required for all codegen'd delegate classes (and useful for DelegateProxy).
Cocoa callers check `respondsToSelector:` before invoking optional delegate
methods.

The override stub:
1. Reads the overrides record via `readHsData` + `deRefStablePtr`.
2. Checks whether the queried selector maps to a `Just` field.
3. If yes, returns YES.
4. If no, falls through to `[super respondsToSelector:sel]` via
   `sendSuperMsg`.

The selector-to-field mapping is a static chain of comparisons generated at
class-creation time (in the CAF).  Selector comparison is pointer equality
(selectors are interned by the ObjC runtime).

## Open Questions

- **Clang AST optional methods**: `ClangAST.hs` currently puts all protocol
  methods into `protoDeclRequired` and leaves `protoDeclOptional` empty.
  Need to investigate how `-ast-dump=json` represents `@optional` sections
  and fix the parser.

- **Subclass override heuristics**: Which methods belong in the overrides
  record for a given class?  Declared-on-this-class is a good start, but a
  curated list per popular class would be ideal.

- **Class naming collisions**: Dynamic classes use `Hs<ProtocolName>` or
  `HsActionTarget_<hash>`.  Could collide with user classes in theory.
  A longer prefix or UUID suffix would mitigate this.

- **Superclass init return value**: Some `init` methods return a different
  pointer than `self` (rare but possible).  The subclass constructor should
  handle this by checking whether the returned pointer differs and updating
  the StablePtr location if so.
