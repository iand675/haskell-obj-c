{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTLArchitecture
--
-- Contains information about the device's architecture
--
-- Generated bindings for @MTLArchitecture@.
module ObjC.Metal.MTLArchitecture
  ( MTLArchitecture
  , IsMTLArchitecture(..)
  , name
  , nameSelector


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

-- | name
--
-- The device's architecture name.
--
-- ObjC selector: @- name@
name :: IsMTLArchitecture mtlArchitecture => mtlArchitecture -> IO (Id NSString)
name mtlArchitecture  =
  sendMsg mtlArchitecture (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

