{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Apple Device Information    This cluster provides Apple-specific information about the device.
--
-- Generated bindings for @MTRClusterAppleDeviceInformation@.
module ObjC.Matter.MTRClusterAppleDeviceInformation
  ( MTRClusterAppleDeviceInformation
  , IsMTRClusterAppleDeviceInformation(..)
  , readAttributeSupportsTapToUnlockWithParams
  , readAttributeSupportsWEDWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeSupportsTapToUnlockWithParamsSelector
  , readAttributeSupportsWEDWithParamsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeSupportsTapToUnlockWithParams:@
readAttributeSupportsTapToUnlockWithParams :: (IsMTRClusterAppleDeviceInformation mtrClusterAppleDeviceInformation, IsMTRReadParams params) => mtrClusterAppleDeviceInformation -> params -> IO (Id NSDictionary)
readAttributeSupportsTapToUnlockWithParams mtrClusterAppleDeviceInformation params =
  sendMessage mtrClusterAppleDeviceInformation readAttributeSupportsTapToUnlockWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSupportsWEDWithParams:@
readAttributeSupportsWEDWithParams :: (IsMTRClusterAppleDeviceInformation mtrClusterAppleDeviceInformation, IsMTRReadParams params) => mtrClusterAppleDeviceInformation -> params -> IO (Id NSDictionary)
readAttributeSupportsWEDWithParams mtrClusterAppleDeviceInformation params =
  sendMessage mtrClusterAppleDeviceInformation readAttributeSupportsWEDWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterAppleDeviceInformation mtrClusterAppleDeviceInformation, IsMTRReadParams params) => mtrClusterAppleDeviceInformation -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterAppleDeviceInformation params =
  sendMessage mtrClusterAppleDeviceInformation readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterAppleDeviceInformation mtrClusterAppleDeviceInformation, IsMTRReadParams params) => mtrClusterAppleDeviceInformation -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterAppleDeviceInformation params =
  sendMessage mtrClusterAppleDeviceInformation readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterAppleDeviceInformation mtrClusterAppleDeviceInformation, IsMTRReadParams params) => mtrClusterAppleDeviceInformation -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterAppleDeviceInformation params =
  sendMessage mtrClusterAppleDeviceInformation readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterAppleDeviceInformation mtrClusterAppleDeviceInformation, IsMTRReadParams params) => mtrClusterAppleDeviceInformation -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterAppleDeviceInformation params =
  sendMessage mtrClusterAppleDeviceInformation readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterAppleDeviceInformation mtrClusterAppleDeviceInformation, IsMTRReadParams params) => mtrClusterAppleDeviceInformation -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterAppleDeviceInformation params =
  sendMessage mtrClusterAppleDeviceInformation readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterAppleDeviceInformation mtrClusterAppleDeviceInformation => mtrClusterAppleDeviceInformation -> IO (Id MTRClusterAppleDeviceInformation)
init_ mtrClusterAppleDeviceInformation =
  sendOwnedMessage mtrClusterAppleDeviceInformation initSelector

-- | @+ new@
new :: IO (Id MTRClusterAppleDeviceInformation)
new  =
  do
    cls' <- getRequiredClass "MTRClusterAppleDeviceInformation"
    sendOwnedClassMessage cls' newSelector

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterAppleDeviceInformation mtrClusterAppleDeviceInformation, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterAppleDeviceInformation -> device -> endpointID -> queue -> IO (Id MTRClusterAppleDeviceInformation)
initWithDevice_endpointID_queue mtrClusterAppleDeviceInformation device endpointID queue =
  sendOwnedMessage mtrClusterAppleDeviceInformation initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeSupportsTapToUnlockWithParams:@
readAttributeSupportsTapToUnlockWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSupportsTapToUnlockWithParamsSelector = mkSelector "readAttributeSupportsTapToUnlockWithParams:"

-- | @Selector@ for @readAttributeSupportsWEDWithParams:@
readAttributeSupportsWEDWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSupportsWEDWithParamsSelector = mkSelector "readAttributeSupportsWEDWithParams:"

-- | @Selector@ for @readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeGeneratedCommandListWithParamsSelector = mkSelector "readAttributeGeneratedCommandListWithParams:"

-- | @Selector@ for @readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAcceptedCommandListWithParamsSelector = mkSelector "readAttributeAcceptedCommandListWithParams:"

-- | @Selector@ for @readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAttributeListWithParamsSelector = mkSelector "readAttributeAttributeListWithParams:"

-- | @Selector@ for @readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeFeatureMapWithParamsSelector = mkSelector "readAttributeFeatureMapWithParams:"

-- | @Selector@ for @readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeClusterRevisionWithParamsSelector = mkSelector "readAttributeClusterRevisionWithParams:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTRClusterAppleDeviceInformation)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterAppleDeviceInformation)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterAppleDeviceInformation)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

