{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Ecosystem Information    Provides extended device information for all the logical devices represented by a Bridged Node.
--
-- Generated bindings for @MTRClusterEcosystemInformation@.
module ObjC.Matter.MTRClusterEcosystemInformation
  ( MTRClusterEcosystemInformation
  , IsMTRClusterEcosystemInformation(..)
  , readAttributeDeviceDirectoryWithParams
  , readAttributeLocationDirectoryWithParams
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
  , readAttributeDeviceDirectoryWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeLocationDirectoryWithParamsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeDeviceDirectoryWithParams:@
readAttributeDeviceDirectoryWithParams :: (IsMTRClusterEcosystemInformation mtrClusterEcosystemInformation, IsMTRReadParams params) => mtrClusterEcosystemInformation -> params -> IO (Id NSDictionary)
readAttributeDeviceDirectoryWithParams mtrClusterEcosystemInformation params =
  sendMessage mtrClusterEcosystemInformation readAttributeDeviceDirectoryWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeLocationDirectoryWithParams:@
readAttributeLocationDirectoryWithParams :: (IsMTRClusterEcosystemInformation mtrClusterEcosystemInformation, IsMTRReadParams params) => mtrClusterEcosystemInformation -> params -> IO (Id NSDictionary)
readAttributeLocationDirectoryWithParams mtrClusterEcosystemInformation params =
  sendMessage mtrClusterEcosystemInformation readAttributeLocationDirectoryWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterEcosystemInformation mtrClusterEcosystemInformation, IsMTRReadParams params) => mtrClusterEcosystemInformation -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterEcosystemInformation params =
  sendMessage mtrClusterEcosystemInformation readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterEcosystemInformation mtrClusterEcosystemInformation, IsMTRReadParams params) => mtrClusterEcosystemInformation -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterEcosystemInformation params =
  sendMessage mtrClusterEcosystemInformation readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterEcosystemInformation mtrClusterEcosystemInformation, IsMTRReadParams params) => mtrClusterEcosystemInformation -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterEcosystemInformation params =
  sendMessage mtrClusterEcosystemInformation readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterEcosystemInformation mtrClusterEcosystemInformation, IsMTRReadParams params) => mtrClusterEcosystemInformation -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterEcosystemInformation params =
  sendMessage mtrClusterEcosystemInformation readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterEcosystemInformation mtrClusterEcosystemInformation, IsMTRReadParams params) => mtrClusterEcosystemInformation -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterEcosystemInformation params =
  sendMessage mtrClusterEcosystemInformation readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterEcosystemInformation mtrClusterEcosystemInformation => mtrClusterEcosystemInformation -> IO (Id MTRClusterEcosystemInformation)
init_ mtrClusterEcosystemInformation =
  sendOwnedMessage mtrClusterEcosystemInformation initSelector

-- | @+ new@
new :: IO (Id MTRClusterEcosystemInformation)
new  =
  do
    cls' <- getRequiredClass "MTRClusterEcosystemInformation"
    sendOwnedClassMessage cls' newSelector

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterEcosystemInformation mtrClusterEcosystemInformation, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterEcosystemInformation -> device -> endpointID -> queue -> IO (Id MTRClusterEcosystemInformation)
initWithDevice_endpointID_queue mtrClusterEcosystemInformation device endpointID queue =
  sendOwnedMessage mtrClusterEcosystemInformation initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeDeviceDirectoryWithParams:@
readAttributeDeviceDirectoryWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDeviceDirectoryWithParamsSelector = mkSelector "readAttributeDeviceDirectoryWithParams:"

-- | @Selector@ for @readAttributeLocationDirectoryWithParams:@
readAttributeLocationDirectoryWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeLocationDirectoryWithParamsSelector = mkSelector "readAttributeLocationDirectoryWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterEcosystemInformation)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterEcosystemInformation)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterEcosystemInformation)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

