{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThreadNetworkDirectoryClusterOperationalDatasetResponseParams@.
module ObjC.Matter.MTRThreadNetworkDirectoryClusterOperationalDatasetResponseParams
  ( MTRThreadNetworkDirectoryClusterOperationalDatasetResponseParams
  , IsMTRThreadNetworkDirectoryClusterOperationalDatasetResponseParams(..)
  , initWithResponseValue_error
  , operationalDataset
  , setOperationalDataset
  , initWithResponseValue_errorSelector
  , operationalDatasetSelector
  , setOperationalDatasetSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRThreadNetworkDirectoryClusterOperationalDatasetResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRThreadNetworkDirectoryClusterOperationalDatasetResponseParams mtrThreadNetworkDirectoryClusterOperationalDatasetResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrThreadNetworkDirectoryClusterOperationalDatasetResponseParams -> responseValue -> error_ -> IO (Id MTRThreadNetworkDirectoryClusterOperationalDatasetResponseParams)
initWithResponseValue_error mtrThreadNetworkDirectoryClusterOperationalDatasetResponseParams responseValue error_ =
  sendOwnedMessage mtrThreadNetworkDirectoryClusterOperationalDatasetResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- operationalDataset@
operationalDataset :: IsMTRThreadNetworkDirectoryClusterOperationalDatasetResponseParams mtrThreadNetworkDirectoryClusterOperationalDatasetResponseParams => mtrThreadNetworkDirectoryClusterOperationalDatasetResponseParams -> IO (Id NSData)
operationalDataset mtrThreadNetworkDirectoryClusterOperationalDatasetResponseParams =
  sendMessage mtrThreadNetworkDirectoryClusterOperationalDatasetResponseParams operationalDatasetSelector

-- | @- setOperationalDataset:@
setOperationalDataset :: (IsMTRThreadNetworkDirectoryClusterOperationalDatasetResponseParams mtrThreadNetworkDirectoryClusterOperationalDatasetResponseParams, IsNSData value) => mtrThreadNetworkDirectoryClusterOperationalDatasetResponseParams -> value -> IO ()
setOperationalDataset mtrThreadNetworkDirectoryClusterOperationalDatasetResponseParams value =
  sendMessage mtrThreadNetworkDirectoryClusterOperationalDatasetResponseParams setOperationalDatasetSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRThreadNetworkDirectoryClusterOperationalDatasetResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @operationalDataset@
operationalDatasetSelector :: Selector '[] (Id NSData)
operationalDatasetSelector = mkSelector "operationalDataset"

-- | @Selector@ for @setOperationalDataset:@
setOperationalDatasetSelector :: Selector '[Id NSData] ()
setOperationalDatasetSelector = mkSelector "setOperationalDataset:"

