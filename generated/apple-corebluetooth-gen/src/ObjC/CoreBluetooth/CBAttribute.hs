{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CBAttribute@.
module ObjC.CoreBluetooth.CBAttribute
  ( CBAttribute
  , IsCBAttribute(..)
  , init_
  , uuid
  , initSelector
  , uuidSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreBluetooth.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCBAttribute cbAttribute => cbAttribute -> IO (Id CBAttribute)
init_ cbAttribute =
  sendOwnedMessage cbAttribute initSelector

-- | UUID
--
-- The Bluetooth UUID of the attribute.
--
-- ObjC selector: @- UUID@
uuid :: IsCBAttribute cbAttribute => cbAttribute -> IO (Id CBUUID)
uuid cbAttribute =
  sendMessage cbAttribute uuidSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CBAttribute)
initSelector = mkSelector "init"

-- | @Selector@ for @UUID@
uuidSelector :: Selector '[] (Id CBUUID)
uuidSelector = mkSelector "UUID"

