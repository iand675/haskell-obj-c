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

import ObjC.CoreBluetooth.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCBAttribute cbAttribute => cbAttribute -> IO (Id CBAttribute)
init_ cbAttribute  =
  sendMsg cbAttribute (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | UUID
--
-- The Bluetooth UUID of the attribute.
--
-- ObjC selector: @- UUID@
uuid :: IsCBAttribute cbAttribute => cbAttribute -> IO (Id CBUUID)
uuid cbAttribute  =
  sendMsg cbAttribute (mkSelector "UUID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @UUID@
uuidSelector :: Selector
uuidSelector = mkSelector "UUID"

