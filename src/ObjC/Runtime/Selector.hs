{-# LANGUAGE ForeignFunctionInterface #-}

-- | Bindings to Objective-C selector functions from @\<objc\/runtime.h\>@.
module ObjC.Runtime.Selector
  ( -- * Selector operations
    sel_registerName
  , sel_getName
  , sel_isEqual

    -- * Convenience
  , mkSelector
  , selName
  , asSel
  ) where

import Foreign.C.String (CString, withCString, peekCString)
import Foreign.C.Types (CBool(..))
import Foreign.Ptr (Ptr)
import System.IO.Unsafe (unsafePerformIO)
import ObjC.Runtime.Types

-- ---------------------------------------------------------------------------
-- Raw FFI
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "sel_registerName"
  c_sel_registerName :: CString -> IO (Ptr ObjCSel)

foreign import ccall unsafe "sel_getName"
  c_sel_getName :: Ptr ObjCSel -> IO CString

foreign import ccall unsafe "sel_isEqual"
  c_sel_isEqual :: Ptr ObjCSel -> Ptr ObjCSel -> IO ObjCBool

-- ---------------------------------------------------------------------------
-- Haskell wrappers
-- ---------------------------------------------------------------------------

-- | Register (or look up) a selector by name. Maps a C string to a @Selector@.
sel_registerName :: CString -> IO Sel
sel_registerName cs = Selector <$> c_sel_registerName cs

-- | Get the name of a selector as a C string.
sel_getName :: Selector args ret -> IO CString
sel_getName (Selector p) = c_sel_getName p

-- | Test whether two selectors are equal.
sel_isEqual :: Selector a1 r1 -> Selector a2 r2 -> IO Bool
sel_isEqual (Selector a) (Selector b) = fromObjCBool <$> c_sel_isEqual a b

-- ---------------------------------------------------------------------------
-- Convenience
-- ---------------------------------------------------------------------------

-- | Create a selector from a Haskell 'String'.
--
-- The type parameters are phantom and inferred from the call site,
-- so a type annotation controls the selector's type signature:
--
-- @
-- sel :: Selector '[Id NSString] (IO Bool)
-- sel = mkSelector "validate:"
-- @
--
-- This is pure because @sel_registerName@ is idempotent and thread-safe:
-- it always returns the same @Selector@ pointer for a given name.
mkSelector :: String -> Selector args ret
mkSelector name = Selector (unsafePerformIO (withCString name c_sel_registerName))
{-# NOINLINE mkSelector #-}

-- | Get the name of a selector as a Haskell 'String'.
selName :: Selector args ret -> IO String
selName (Selector p) = c_sel_getName p >>= peekCString

-- | Erase the phantom type parameters from a selector, producing an
-- untyped 'Sel'.
--
-- Useful for passing a typed action selector to functions like
-- @setAction@ or @buttonWithTitle_target_action@ that expect 'Sel'.
--
-- @
-- toggleSel :: Selector '[Id NSView] ()
-- toggleSel = mkSelector \"toggle:\"
--
-- Ctrl.setAction button (asSel toggleSel)
-- @
asSel :: Selector args ret -> Sel
asSel (Selector p) = Selector p
