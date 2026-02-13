{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThreadBorderRouterManagementClusterDatasetResponseParams@.
module ObjC.Matter.MTRThreadBorderRouterManagementClusterDatasetResponseParams
  ( MTRThreadBorderRouterManagementClusterDatasetResponseParams
  , IsMTRThreadBorderRouterManagementClusterDatasetResponseParams(..)
  , initWithResponseValue_error
  , dataset
  , setDataset
  , datasetSelector
  , initWithResponseValue_errorSelector
  , setDatasetSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRThreadBorderRouterManagementClusterDatasetResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRThreadBorderRouterManagementClusterDatasetResponseParams mtrThreadBorderRouterManagementClusterDatasetResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrThreadBorderRouterManagementClusterDatasetResponseParams -> responseValue -> error_ -> IO (Id MTRThreadBorderRouterManagementClusterDatasetResponseParams)
initWithResponseValue_error mtrThreadBorderRouterManagementClusterDatasetResponseParams responseValue error_ =
  sendOwnedMessage mtrThreadBorderRouterManagementClusterDatasetResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- dataset@
dataset :: IsMTRThreadBorderRouterManagementClusterDatasetResponseParams mtrThreadBorderRouterManagementClusterDatasetResponseParams => mtrThreadBorderRouterManagementClusterDatasetResponseParams -> IO (Id NSData)
dataset mtrThreadBorderRouterManagementClusterDatasetResponseParams =
  sendMessage mtrThreadBorderRouterManagementClusterDatasetResponseParams datasetSelector

-- | @- setDataset:@
setDataset :: (IsMTRThreadBorderRouterManagementClusterDatasetResponseParams mtrThreadBorderRouterManagementClusterDatasetResponseParams, IsNSData value) => mtrThreadBorderRouterManagementClusterDatasetResponseParams -> value -> IO ()
setDataset mtrThreadBorderRouterManagementClusterDatasetResponseParams value =
  sendMessage mtrThreadBorderRouterManagementClusterDatasetResponseParams setDatasetSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRThreadBorderRouterManagementClusterDatasetResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @dataset@
datasetSelector :: Selector '[] (Id NSData)
datasetSelector = mkSelector "dataset"

-- | @Selector@ for @setDataset:@
setDatasetSelector :: Selector '[Id NSData] ()
setDatasetSelector = mkSelector "setDataset:"

