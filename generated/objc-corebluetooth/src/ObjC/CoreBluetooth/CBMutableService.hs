{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CBMutableService
--
-- Used to create a local service or included service, which can be added to the local database via CBPeripheralManager.		Once a service is published, it is cached and can no longer be changed. This class adds write access to all properties in the
--
-- CBService
--
-- class.
--
-- Generated bindings for @CBMutableService@.
module ObjC.CoreBluetooth.CBMutableService
  ( CBMutableService
  , IsCBMutableService(..)
  , initWithType_primary
  , includedServices
  , setIncludedServices
  , characteristics
  , setCharacteristics
  , initWithType_primarySelector
  , includedServicesSelector
  , setIncludedServicesSelector
  , characteristicsSelector
  , setCharacteristicsSelector


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

-- | initWithType:primary:
--
-- @UUID@ — The Bluetooth UUID of the service.
--
-- @isPrimary@ — The type of the service (primary or secondary).
--
-- Returns a service, initialized with a service type and UUID.
--
-- ObjC selector: @- initWithType:primary:@
initWithType_primary :: (IsCBMutableService cbMutableService, IsCBUUID uuid) => cbMutableService -> uuid -> Bool -> IO (Id CBMutableService)
initWithType_primary cbMutableService  uuid isPrimary =
withObjCPtr uuid $ \raw_uuid ->
    sendMsg cbMutableService (mkSelector "initWithType:primary:") (retPtr retVoid) [argPtr (castPtr raw_uuid :: Ptr ()), argCULong (if isPrimary then 1 else 0)] >>= ownedObject . castPtr

-- | @- includedServices@
includedServices :: IsCBMutableService cbMutableService => cbMutableService -> IO (Id NSArray)
includedServices cbMutableService  =
  sendMsg cbMutableService (mkSelector "includedServices") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIncludedServices:@
setIncludedServices :: (IsCBMutableService cbMutableService, IsNSArray value) => cbMutableService -> value -> IO ()
setIncludedServices cbMutableService  value =
withObjCPtr value $ \raw_value ->
    sendMsg cbMutableService (mkSelector "setIncludedServices:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- characteristics@
characteristics :: IsCBMutableService cbMutableService => cbMutableService -> IO (Id NSArray)
characteristics cbMutableService  =
  sendMsg cbMutableService (mkSelector "characteristics") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCharacteristics:@
setCharacteristics :: (IsCBMutableService cbMutableService, IsNSArray value) => cbMutableService -> value -> IO ()
setCharacteristics cbMutableService  value =
withObjCPtr value $ \raw_value ->
    sendMsg cbMutableService (mkSelector "setCharacteristics:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithType:primary:@
initWithType_primarySelector :: Selector
initWithType_primarySelector = mkSelector "initWithType:primary:"

-- | @Selector@ for @includedServices@
includedServicesSelector :: Selector
includedServicesSelector = mkSelector "includedServices"

-- | @Selector@ for @setIncludedServices:@
setIncludedServicesSelector :: Selector
setIncludedServicesSelector = mkSelector "setIncludedServices:"

-- | @Selector@ for @characteristics@
characteristicsSelector :: Selector
characteristicsSelector = mkSelector "characteristics"

-- | @Selector@ for @setCharacteristics:@
setCharacteristicsSelector :: Selector
setCharacteristicsSelector = mkSelector "setCharacteristics:"

