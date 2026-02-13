{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
name mtlArchitecture =
  sendMessage mtlArchitecture nameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

