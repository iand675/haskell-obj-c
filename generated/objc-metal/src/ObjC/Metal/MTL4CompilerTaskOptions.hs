{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The configuration options that control the behavior of a compilation task for a Metal 4 compiler instance.
--
-- You can configure task-specific settings that affect a compilation task by creating an instance of this class, setting its properties, and passing it to one of the applicable methods of an ``MTL4Compiler`` instance.
--
-- Generated bindings for @MTL4CompilerTaskOptions@.
module ObjC.Metal.MTL4CompilerTaskOptions
  ( MTL4CompilerTaskOptions
  , IsMTL4CompilerTaskOptions(..)


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

