# haskell-obj-c

Type-safe Haskell bindings to Apple's Objective-C runtime. Write native macOS
apps in Haskell with generated bindings for AppKit, Foundation, and 150+ other
Apple frameworks.

Note: This project is a work in progress. It is not yet ready for production use.

![Todo List Example](docs/images/todo.png)

## Features

- **Generated bindings** for Apple frameworks — typed method wrappers, enums
  with pattern synonyms, and `Storable` struct instances
- **Phantom-typed object references** — `Id a` carries a type parameter that
  encodes the ObjC class hierarchy, so the compiler rejects type mismatches
- **Automatic memory management** — `Id a` is a `ForeignPtr` with
  retain/release semantics; no manual reference counting
- **Haskell-backed ObjC classes** — use Template Haskell to define ObjC classes
  whose methods are Haskell closures (for targets, delegates, etc.)
- **ObjC blocks from Haskell** — pass Haskell functions as ObjC block arguments

## Quick Start

```bash
stack build
```

### Hello World

```haskell
main :: IO ()
main = do
  loadFramework "AppKit"

  app <- App.sharedApplication
  _ <- App.setActivationPolicy app NSApplicationActivationPolicyRegular

  let styleMask = NSWindowStyleMaskTitled <> NSWindowStyleMaskClosable
               <> NSWindowStyleMaskResizable
      rect = NSRect (NSPoint 200 200) (NSSize 480 260)

  window <- alloc @NSWindow >>=
    \w -> Win.initWithContentRect_styleMask_backing_defer
            w rect styleMask NSBackingStoreBuffered False

  Win.setTitle window ("Hello Haskell" :: Id NSString)

  label <- TF.labelWithString ("Hello, World from Haskell!" :: Id NSString)
  View.setFrame label (NSRect (NSPoint 40 80) (NSSize 400 100))

  cv <- Win.contentView window
  View.addSubview cv (toNSView label)

  Win.makeKeyAndOrderFront window (RawId nullPtr)
  App.run app
```

### Haskell-backed ObjC classes

Use `defineClass` to create ObjC classes with Haskell closure implementations.
This is how you wire up button targets, delegates, and data sources:

```haskell
$(defineClass "CounterTarget" "NSObject" $ do
  instanceMethod "increment:" [t| RawId -> IO () |]
  instanceMethod "decrement:" [t| RawId -> IO () |]
 )

main :: IO ()
main = withAutoreleasePool $ do
  -- ...
  target <- newCounterTarget $ do
    ref <- newIORef (0 :: Int)
    pure CounterTargetImpl
      { _increment = \_sender -> modifyIORef' ref (+ 1) >> readIORef ref >>= updateLabel label
      , _decrement = \_sender -> modifyIORef' ref (subtract 1) >> readIORef ref >>= updateLabel label
      }

  plusBtn <- Btn.buttonWithTitle_target_action
    ("+" :: Id NSString) target (mkSelector "increment:")
  -- ...
```

## Examples

Complete example apps live in `examples/`:

| Example                | Description                                        |
|------------------------|----------------------------------------------------|
| `hello-world`          | Window with a label — basic AppKit usage            |
| `counter`              | Haskell-backed ObjC class with `defineClass`        |
| `temperature-converter`| Two-way binding with text fields and actions        |
| `color-mixer`          | Color picker                                        |
| `controls-gallery`     | Various AppKit controls                             |

Build and run an example:

```bash
stack build --stack-yaml stack.yaml haskell-obj-c-examples
stack exec hello-world
```

## Available Frameworks

The codegen produces typed bindings for 150+ Apple frameworks, including:

- **Foundation** — NSObject, NSString, NSArray, NSDictionary, ...
- **AppKit** — NSApplication, NSWindow, NSView, NSButton, NSTextField, ...
- **CoreData**, **CoreFoundation**, **QuartzCore**, **WebKit**, **Metal**,
  **SceneKit**, **SpriteKit**, **MapKit**, **CoreML**, **Vision**, and many more

See `generated/` for the full list.

## Building

```bash
stack build               # build the runtime library + tests
stack test                # run tests
stack build --stack-yaml stack.yaml haskell-obj-c-examples   # build examples
```

### Regenerating framework bindings

```bash
scripts/regen-frameworks.sh
```

## Documentation

- [Architecture](docs/architecture.md) — project structure, component overview,
  and design decisions
- [Dynamic Classes](docs/dynclass.md) — how Haskell-backed ObjC classes work
  (vtable design, TH code generation, dispatch flow)

## License

BSD-3-Clause
