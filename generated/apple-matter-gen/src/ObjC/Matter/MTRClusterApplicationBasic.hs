{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Application Basic    This cluster provides information about an application running on a TV or media player device which is represented as an endpoint.
--
-- Generated bindings for @MTRClusterApplicationBasic@.
module ObjC.Matter.MTRClusterApplicationBasic
  ( MTRClusterApplicationBasic
  , IsMTRClusterApplicationBasic(..)
  , readAttributeVendorNameWithParams
  , readAttributeVendorIDWithParams
  , readAttributeApplicationNameWithParams
  , readAttributeProductIDWithParams
  , readAttributeApplicationWithParams
  , readAttributeStatusWithParams
  , readAttributeApplicationVersionWithParams
  , readAttributeAllowedVendorListWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , initWithDevice_endpointID_queue
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAllowedVendorListWithParamsSelector
  , readAttributeApplicationNameWithParamsSelector
  , readAttributeApplicationVersionWithParamsSelector
  , readAttributeApplicationWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeProductIDWithParamsSelector
  , readAttributeStatusWithParamsSelector
  , readAttributeVendorIDWithParamsSelector
  , readAttributeVendorNameWithParamsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeVendorNameWithParams:@
readAttributeVendorNameWithParams :: (IsMTRClusterApplicationBasic mtrClusterApplicationBasic, IsMTRReadParams params) => mtrClusterApplicationBasic -> params -> IO (Id NSDictionary)
readAttributeVendorNameWithParams mtrClusterApplicationBasic params =
  sendMessage mtrClusterApplicationBasic readAttributeVendorNameWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeVendorIDWithParams:@
readAttributeVendorIDWithParams :: (IsMTRClusterApplicationBasic mtrClusterApplicationBasic, IsMTRReadParams params) => mtrClusterApplicationBasic -> params -> IO (Id NSDictionary)
readAttributeVendorIDWithParams mtrClusterApplicationBasic params =
  sendMessage mtrClusterApplicationBasic readAttributeVendorIDWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeApplicationNameWithParams:@
readAttributeApplicationNameWithParams :: (IsMTRClusterApplicationBasic mtrClusterApplicationBasic, IsMTRReadParams params) => mtrClusterApplicationBasic -> params -> IO (Id NSDictionary)
readAttributeApplicationNameWithParams mtrClusterApplicationBasic params =
  sendMessage mtrClusterApplicationBasic readAttributeApplicationNameWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeProductIDWithParams:@
readAttributeProductIDWithParams :: (IsMTRClusterApplicationBasic mtrClusterApplicationBasic, IsMTRReadParams params) => mtrClusterApplicationBasic -> params -> IO (Id NSDictionary)
readAttributeProductIDWithParams mtrClusterApplicationBasic params =
  sendMessage mtrClusterApplicationBasic readAttributeProductIDWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeApplicationWithParams:@
readAttributeApplicationWithParams :: (IsMTRClusterApplicationBasic mtrClusterApplicationBasic, IsMTRReadParams params) => mtrClusterApplicationBasic -> params -> IO (Id NSDictionary)
readAttributeApplicationWithParams mtrClusterApplicationBasic params =
  sendMessage mtrClusterApplicationBasic readAttributeApplicationWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeStatusWithParams:@
readAttributeStatusWithParams :: (IsMTRClusterApplicationBasic mtrClusterApplicationBasic, IsMTRReadParams params) => mtrClusterApplicationBasic -> params -> IO (Id NSDictionary)
readAttributeStatusWithParams mtrClusterApplicationBasic params =
  sendMessage mtrClusterApplicationBasic readAttributeStatusWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeApplicationVersionWithParams:@
readAttributeApplicationVersionWithParams :: (IsMTRClusterApplicationBasic mtrClusterApplicationBasic, IsMTRReadParams params) => mtrClusterApplicationBasic -> params -> IO (Id NSDictionary)
readAttributeApplicationVersionWithParams mtrClusterApplicationBasic params =
  sendMessage mtrClusterApplicationBasic readAttributeApplicationVersionWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAllowedVendorListWithParams:@
readAttributeAllowedVendorListWithParams :: (IsMTRClusterApplicationBasic mtrClusterApplicationBasic, IsMTRReadParams params) => mtrClusterApplicationBasic -> params -> IO (Id NSDictionary)
readAttributeAllowedVendorListWithParams mtrClusterApplicationBasic params =
  sendMessage mtrClusterApplicationBasic readAttributeAllowedVendorListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterApplicationBasic mtrClusterApplicationBasic, IsMTRReadParams params) => mtrClusterApplicationBasic -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterApplicationBasic params =
  sendMessage mtrClusterApplicationBasic readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterApplicationBasic mtrClusterApplicationBasic, IsMTRReadParams params) => mtrClusterApplicationBasic -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterApplicationBasic params =
  sendMessage mtrClusterApplicationBasic readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterApplicationBasic mtrClusterApplicationBasic, IsMTRReadParams params) => mtrClusterApplicationBasic -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterApplicationBasic params =
  sendMessage mtrClusterApplicationBasic readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterApplicationBasic mtrClusterApplicationBasic, IsMTRReadParams params) => mtrClusterApplicationBasic -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterApplicationBasic params =
  sendMessage mtrClusterApplicationBasic readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterApplicationBasic mtrClusterApplicationBasic, IsMTRReadParams params) => mtrClusterApplicationBasic -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterApplicationBasic params =
  sendMessage mtrClusterApplicationBasic readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterApplicationBasic mtrClusterApplicationBasic => mtrClusterApplicationBasic -> IO (Id MTRClusterApplicationBasic)
init_ mtrClusterApplicationBasic =
  sendOwnedMessage mtrClusterApplicationBasic initSelector

-- | @+ new@
new :: IO (Id MTRClusterApplicationBasic)
new  =
  do
    cls' <- getRequiredClass "MTRClusterApplicationBasic"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterApplicationBasic mtrClusterApplicationBasic, IsMTRDevice device, IsNSObject queue) => mtrClusterApplicationBasic -> device -> CUShort -> queue -> IO (Id MTRClusterApplicationBasic)
initWithDevice_endpoint_queue mtrClusterApplicationBasic device endpoint queue =
  sendOwnedMessage mtrClusterApplicationBasic initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterApplicationBasic mtrClusterApplicationBasic, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterApplicationBasic -> device -> endpointID -> queue -> IO (Id MTRClusterApplicationBasic)
initWithDevice_endpointID_queue mtrClusterApplicationBasic device endpointID queue =
  sendOwnedMessage mtrClusterApplicationBasic initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeVendorNameWithParams:@
readAttributeVendorNameWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeVendorNameWithParamsSelector = mkSelector "readAttributeVendorNameWithParams:"

-- | @Selector@ for @readAttributeVendorIDWithParams:@
readAttributeVendorIDWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeVendorIDWithParamsSelector = mkSelector "readAttributeVendorIDWithParams:"

-- | @Selector@ for @readAttributeApplicationNameWithParams:@
readAttributeApplicationNameWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeApplicationNameWithParamsSelector = mkSelector "readAttributeApplicationNameWithParams:"

-- | @Selector@ for @readAttributeProductIDWithParams:@
readAttributeProductIDWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeProductIDWithParamsSelector = mkSelector "readAttributeProductIDWithParams:"

-- | @Selector@ for @readAttributeApplicationWithParams:@
readAttributeApplicationWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeApplicationWithParamsSelector = mkSelector "readAttributeApplicationWithParams:"

-- | @Selector@ for @readAttributeStatusWithParams:@
readAttributeStatusWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeStatusWithParamsSelector = mkSelector "readAttributeStatusWithParams:"

-- | @Selector@ for @readAttributeApplicationVersionWithParams:@
readAttributeApplicationVersionWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeApplicationVersionWithParamsSelector = mkSelector "readAttributeApplicationVersionWithParams:"

-- | @Selector@ for @readAttributeAllowedVendorListWithParams:@
readAttributeAllowedVendorListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAllowedVendorListWithParamsSelector = mkSelector "readAttributeAllowedVendorListWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterApplicationBasic)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterApplicationBasic)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterApplicationBasic)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterApplicationBasic)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

