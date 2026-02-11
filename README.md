# haskell-obj-c

Comprehensive Haskell bindings to Apple's Objective-C runtime (`libobjc`). This
library provides type-safe access to the full Objective-C runtime API, dynamic
message sending via `libffi`, Haskell-backed Objective-C classes, and a code
generator that produces typed bindings from framework headers.

## Project Structure

```
haskell-obj-c/
├── src/            Runtime library — FFI bindings to libobjc + higher-level API
├── cbits/          C stubs for objc_msgSend, retain/release, and ObjC blocks
├── codegen/        Standalone tool that parses Clang AST JSON and emits Haskell modules
├── generated/      Output of the codegen — per-framework packages (Foundation, AppKit, …)
├── examples/       Sample macOS apps using the bindings
└── test/           Test suite exercising the runtime API
```

## Components

### Runtime Library (`src/ObjC/Runtime/`)

The core library, exposed as `ObjC.Runtime`. It wraps `libobjc` and provides:

| Module              | Purpose                                                       |
|---------------------|---------------------------------------------------------------|
| `Types`             | Phantom-typed `Id a`, raw `RawId`, `Class`, `Selector`, etc.  |
| `MsgSend`           | Dynamic message sending via `libffi`                          |
| `Class`             | Class lookup and introspection                                |
| `Object`            | `alloc`, `init`, ivar access                                  |
| `Selector`          | `mkSelector`, selector registration                           |
| `Cast`              | Safe downcasting via `isKindOfClass:`                         |
| `ClassBuilder`      | `objc_allocateClassPair` / `objc_registerClassPair`           |
| `DynClass`          | Vtable infrastructure for Haskell-backed ObjC classes         |
| `TH`                | Template Haskell API (`defineClass`) for declaring classes     |
| `Block`             | Create ObjC blocks from Haskell callbacks                     |
| `NSString`          | `pureNSString` convenience                                    |
| `Framework`         | `loadFramework` / `loadFrameworkAt` via `dlopen`              |
| `AutoreleasePool`   | `withAutoreleasePool`                                         |
| `Association`       | Associated objects                                            |
| `Ivar`              | Ivar introspection                                            |
| `Property`          | Property introspection                                        |
| `Protocol`          | Protocol introspection                                        |
| `Method`            | Method introspection                                          |

**Key design decisions:**

- **Phantom types for safety.** `Id a` is a managed `ForeignPtr` with a phantom
  type parameter. Generated bindings produce `IsNSObject`, `IsNSView`, etc.
  typeclasses that form a subclass hierarchy, so a method expecting
  `IsNSView a => Id a` will accept an `Id NSButton` but not an `Id NSString`.

- **`libffi` for message sending.** `objc_msgSend` is a trampoline whose calling
  convention must match the target method. Rather than generating a `foreign
  import` per signature, the library uses `libffi` to build the call
  dynamically with the correct types.

- **Automatic memory management.** `Id a` wraps a `ForeignPtr` with a release
  finalizer. `ownedObject` (for `+1` references from init/copy/new) and
  `retainedObject` (for `+0` references that need a retain) handle the two
  common ownership patterns.

- **Vtable-based dynamic classes.** Haskell-backed ObjC classes use a per-instance
  vtable (a C array of `FunPtr`s stored in an ivar). The Template Haskell
  `defineClass` macro generates stub IMPs that dispatch through this vtable,
  so each instance can have its own closure-backed implementations. See
  [docs/dynclass.md](docs/dynclass.md) for details.

### C Stubs (`cbits/objc_stubs.c`)

A small C file that:

- Exports the addresses of `objc_msgSend` (and `_stret`, `_fpret`, `_super`
  variants) as function pointers for Haskell to call via `libffi`.
- Provides `hs_objc_retain` / `hs_objc_release` for `ForeignPtr` finalizers.
- Implements `hs_create_block` / `hs_release_block` using the ObjC block ABI.
- Forces the linker to keep Foundation loaded despite `-dead_strip_dylibs`.

Architecture-specific: on ARM64, `_stret` and `_fpret` both resolve to plain
`objc_msgSend`. The stret/fpret variants only exist on x86_64.

### Code Generator (`codegen/`)

A standalone Haskell tool that produces typed bindings from Objective-C
framework headers:

1. Builds a header that `#import`s the framework.
2. Runs `clang -Xclang -ast-dump=json` to get the full AST.
3. Parses the JSON into an IR (`ClangAST.hs` -> `IR.hs`).
4. Builds a class hierarchy and resolves cross-framework dependencies
   (`Hierarchy.hs`).
5. Emits one Haskell package per framework (`Generate/*.hs`), each containing:
   - `Internal.Classes` — phantom types and `IsXxx` typeclass instances
   - `Internal.Structs` — `Storable` instances for C structs
   - `Internal.Enums` — enum types with pattern synonyms
   - One module per class with typed method wrappers

### Generated Bindings (`generated/`)

Per-framework packages produced by the codegen:

- `objc-foundation` — Foundation (NSObject, NSString, NSArray, …)
- `objc-appkit` — AppKit (NSApplication, NSWindow, NSView, …)
- `objc-coredata`, `objc-corefoundation`, `objc-quartzcore`, etc.

Each generated method wrapper calls `sendMsg` / `sendClassMsg` with the
correct `libffi` argument and return types, and uses `IsXxx` constraints for
polymorphic dispatch.

### Examples (`examples/`)

Sample macOS applications demonstrating usage:

| Example                | What it shows                                              |
|------------------------|------------------------------------------------------------|
| `hello-world`          | Window with a text field, basic AppKit object creation      |
| `counter`              | Haskell-backed ObjC class via `defineClass` + `IORef`       |
| `temperature-converter`| Two-way binding with text fields and actions                |
| `color-mixer`          | Color picker                                                |
| `controls-gallery`     | Various AppKit controls                                     |

## How Things Fit Together

```
┌─────────────────────────────────────────────────────────┐
│                  User Application                       │
│  Uses generated bindings + ObjC.Runtime for manual work │
└───────────────────────────┬─────────────────────────────┘
                            │
        ┌───────────────────┼───────────────────┐
        ▼                   ▼                   ▼
  Generated Bindings   ObjC.Runtime.TH     ObjC.Runtime
  (objc-foundation,    (defineClass →       (Types, MsgSend,
   objc-appkit, …)      DynClass vtable)    Class, Object, …)
        │                   │                   │
        └───────────────────┼───────────────────┘
                            ▼
                     cbits/objc_stubs.c
                  (msgSend ptrs, retain/
                   release, block ABI)
                            │
                            ▼
                    libobjc + libffi
```

The **generated bindings** depend on the **runtime library** for message sending
and type infrastructure. The **TH module** depends on the runtime library and
`DynClass` to produce Haskell-backed ObjC classes. Everything bottoms out in
the C stubs, `libobjc`, and `libffi`.

The **codegen** is a build-time tool that reads Clang's AST and writes `.hs`
files. It does not link against the runtime at all — it just generates source
code that imports it.

## Building

```bash
stack build               # build the runtime library + tests
stack test                # run tests
stack build --stack-yaml stack.yaml haskell-obj-c-examples   # build examples
```

## Further Reading

- [docs/dynclass.md](docs/dynclass.md) — How Haskell-backed ObjC classes work
  (vtable design, TH code generation, dispatch flow)
