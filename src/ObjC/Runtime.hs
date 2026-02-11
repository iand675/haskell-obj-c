-- | Comprehensive Haskell bindings to the Objective-C runtime (@libobjc@).
--
-- This module re-exports everything you need to interact with the
-- Objective-C runtime: type definitions, class/object introspection,
-- dynamic message sending (via @libffi@), framework loading, autorelease
-- pool management, and runtime class construction.
--
-- = Quick start
--
-- @
-- import ObjC.Runtime
-- import Foreign.LibFFI
-- import Foreign.C.String
--
-- main :: IO ()
-- main = withAutoreleasePool $ do
--   -- Get the NSString class
--   nsCls <- getRequiredClass \"NSString\"
--
--   -- Register selectors
--   allocSel  <- mkSelector \"alloc\"
--   initSel   <- mkSelector \"initWithUTF8String:\"
--   lenSel    <- mkSelector \"length\"
--
--   -- Send messages
--   raw   <- sendClassMsg nsCls allocSel (retPtr retVoid) []
--   let obj = Id (castPtr raw)  -- uninitialized NSString
--   -- ... etc
-- @
module ObjC.Runtime
  (     -- * Types
    module ObjC.Runtime.Types

    -- * Safe casting
  , module ObjC.Runtime.Cast

    -- * Selectors
  , module ObjC.Runtime.Selector

    -- * Classes
  , module ObjC.Runtime.Class

    -- * Objects
  , module ObjC.Runtime.Object

    -- * Methods
  , module ObjC.Runtime.Method

    -- * Instance variables
  , module ObjC.Runtime.Ivar

    -- * Properties
  , module ObjC.Runtime.Property

    -- * Protocols
  , module ObjC.Runtime.Protocol

    -- * Associated objects
  , module ObjC.Runtime.Association

    -- * Dynamic class creation
  , module ObjC.Runtime.ClassBuilder

    -- * Dynamic class helpers (vtable-based)
  , module ObjC.Runtime.DynClass

    -- * Objective-C blocks
  , module ObjC.Runtime.Block

    -- * Message sending (via libffi)
  , module ObjC.Runtime.MsgSend

    -- * Autorelease pools
  , module ObjC.Runtime.AutoreleasePool

    -- * Framework loading
  , module ObjC.Runtime.Framework

    -- * NSString construction
  , module ObjC.Runtime.NSString
  ) where

import ObjC.Runtime.Types
import ObjC.Runtime.NSString
import ObjC.Runtime.Cast
import ObjC.Runtime.Selector
import ObjC.Runtime.Class
import ObjC.Runtime.Object
import ObjC.Runtime.Method
import ObjC.Runtime.Ivar
import ObjC.Runtime.Property
import ObjC.Runtime.Protocol
import ObjC.Runtime.Association
import ObjC.Runtime.ClassBuilder
import ObjC.Runtime.DynClass
import ObjC.Runtime.Block
import ObjC.Runtime.MsgSend
import ObjC.Runtime.AutoreleasePool
import ObjC.Runtime.Framework
