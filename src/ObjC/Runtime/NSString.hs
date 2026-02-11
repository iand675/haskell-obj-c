{-# LANGUAGE ScopedTypeVariables #-}

-- | Pure NSString construction.
--
-- This module provides 'pureNSString', which creates an @NSString@ from a
-- Haskell 'String' purely (outside of 'IO').  This is safe because:
--
-- * @NSString@ is immutable — the same input always produces a semantically
--   identical object.
-- * The result is retained with a release finalizer on the 'ForeignPtr',
--   ensuring deterministic cleanup by the Haskell GC.
module ObjC.Runtime.NSString
  ( pureNSString
  ) where

import Foreign.Ptr (castPtr)
import Foreign.LibFFI (retPtr, retVoid, argPtr)
import GHC.Foreign (withCString)
import GHC.IO.Encoding (utf8)
import System.IO.Unsafe (unsafePerformIO)

import ObjC.Runtime.Types (Id, retainedObject)
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.MsgSend (sendClassMsg)

-- | Create an @NSString@ from a Haskell 'String', purely.
--
-- The Haskell 'String' is encoded as UTF-8 and passed to
-- @+[NSString stringWithUTF8String:]@.
--
-- Safe because NSString is immutable and construction is deterministic.
-- The result is retained with a release finalizer on the 'ForeignPtr'.
--
-- >>> :set -XOverloadedStrings
-- >>> let s = pureNSString "hello"
pureNSString :: String -> Id a
pureNSString s = unsafePerformIO $
  withCString utf8 s $ \cstr -> do
    cls <- getRequiredClass "NSString"
    sendClassMsg cls (mkSelector "stringWithUTF8String:") (retPtr retVoid) [argPtr cstr]
      >>= retainedObject . castPtr
{-# NOINLINE pureNSString #-}
