{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CBCharacteristic
--
-- Represents a service's characteristic.
--
-- Generated bindings for @CBCharacteristic@.
module ObjC.CoreBluetooth.CBCharacteristic
  ( CBCharacteristic
  , IsCBCharacteristic(..)
  , service
  , properties
  , value
  , descriptors
  , isBroadcasted
  , isNotifying
  , serviceSelector
  , propertiesSelector
  , valueSelector
  , descriptorsSelector
  , isBroadcastedSelector
  , isNotifyingSelector

  -- * Enum types
  , CBCharacteristicProperties(CBCharacteristicProperties)
  , pattern CBCharacteristicPropertyBroadcast
  , pattern CBCharacteristicPropertyRead
  , pattern CBCharacteristicPropertyWriteWithoutResponse
  , pattern CBCharacteristicPropertyWrite
  , pattern CBCharacteristicPropertyNotify
  , pattern CBCharacteristicPropertyIndicate
  , pattern CBCharacteristicPropertyAuthenticatedSignedWrites
  , pattern CBCharacteristicPropertyExtendedProperties
  , pattern CBCharacteristicPropertyNotifyEncryptionRequired
  , pattern CBCharacteristicPropertyIndicateEncryptionRequired

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
import ObjC.CoreBluetooth.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | service
--
-- A back-pointer to the service this characteristic belongs to.
--
-- ObjC selector: @- service@
service :: IsCBCharacteristic cbCharacteristic => cbCharacteristic -> IO (Id CBService)
service cbCharacteristic  =
  sendMsg cbCharacteristic (mkSelector "service") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | properties
--
-- The properties of the characteristic.
--
-- ObjC selector: @- properties@
properties :: IsCBCharacteristic cbCharacteristic => cbCharacteristic -> IO CBCharacteristicProperties
properties cbCharacteristic  =
  fmap (coerce :: CULong -> CBCharacteristicProperties) $ sendMsg cbCharacteristic (mkSelector "properties") retCULong []

-- | value
--
-- The value of the characteristic.
--
-- ObjC selector: @- value@
value :: IsCBCharacteristic cbCharacteristic => cbCharacteristic -> IO (Id NSData)
value cbCharacteristic  =
  sendMsg cbCharacteristic (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | descriptors
--
-- A list of the CBDescriptors that have so far been discovered in this characteristic.
--
-- ObjC selector: @- descriptors@
descriptors :: IsCBCharacteristic cbCharacteristic => cbCharacteristic -> IO (Id NSArray)
descriptors cbCharacteristic  =
  sendMsg cbCharacteristic (mkSelector "descriptors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | isBroadcasted
--
-- Whether the characteristic is currently broadcasted or not.
--
-- ObjC selector: @- isBroadcasted@
isBroadcasted :: IsCBCharacteristic cbCharacteristic => cbCharacteristic -> IO Bool
isBroadcasted cbCharacteristic  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cbCharacteristic (mkSelector "isBroadcasted") retCULong []

-- | isNotifying
--
-- Whether the characteristic is currently notifying or not.
--
-- ObjC selector: @- isNotifying@
isNotifying :: IsCBCharacteristic cbCharacteristic => cbCharacteristic -> IO Bool
isNotifying cbCharacteristic  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cbCharacteristic (mkSelector "isNotifying") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @service@
serviceSelector :: Selector
serviceSelector = mkSelector "service"

-- | @Selector@ for @properties@
propertiesSelector :: Selector
propertiesSelector = mkSelector "properties"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @descriptors@
descriptorsSelector :: Selector
descriptorsSelector = mkSelector "descriptors"

-- | @Selector@ for @isBroadcasted@
isBroadcastedSelector :: Selector
isBroadcastedSelector = mkSelector "isBroadcasted"

-- | @Selector@ for @isNotifying@
isNotifyingSelector :: Selector
isNotifyingSelector = mkSelector "isNotifying"

