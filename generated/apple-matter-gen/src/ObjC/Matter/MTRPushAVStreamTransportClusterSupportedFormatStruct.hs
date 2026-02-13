{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPushAVStreamTransportClusterSupportedFormatStruct@.
module ObjC.Matter.MTRPushAVStreamTransportClusterSupportedFormatStruct
  ( MTRPushAVStreamTransportClusterSupportedFormatStruct
  , IsMTRPushAVStreamTransportClusterSupportedFormatStruct(..)
  , containerFormat
  , setContainerFormat
  , ingestMethod
  , setIngestMethod
  , containerFormatSelector
  , ingestMethodSelector
  , setContainerFormatSelector
  , setIngestMethodSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- containerFormat@
containerFormat :: IsMTRPushAVStreamTransportClusterSupportedFormatStruct mtrPushAVStreamTransportClusterSupportedFormatStruct => mtrPushAVStreamTransportClusterSupportedFormatStruct -> IO (Id NSNumber)
containerFormat mtrPushAVStreamTransportClusterSupportedFormatStruct =
  sendMessage mtrPushAVStreamTransportClusterSupportedFormatStruct containerFormatSelector

-- | @- setContainerFormat:@
setContainerFormat :: (IsMTRPushAVStreamTransportClusterSupportedFormatStruct mtrPushAVStreamTransportClusterSupportedFormatStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterSupportedFormatStruct -> value -> IO ()
setContainerFormat mtrPushAVStreamTransportClusterSupportedFormatStruct value =
  sendMessage mtrPushAVStreamTransportClusterSupportedFormatStruct setContainerFormatSelector (toNSNumber value)

-- | @- ingestMethod@
ingestMethod :: IsMTRPushAVStreamTransportClusterSupportedFormatStruct mtrPushAVStreamTransportClusterSupportedFormatStruct => mtrPushAVStreamTransportClusterSupportedFormatStruct -> IO (Id NSNumber)
ingestMethod mtrPushAVStreamTransportClusterSupportedFormatStruct =
  sendMessage mtrPushAVStreamTransportClusterSupportedFormatStruct ingestMethodSelector

-- | @- setIngestMethod:@
setIngestMethod :: (IsMTRPushAVStreamTransportClusterSupportedFormatStruct mtrPushAVStreamTransportClusterSupportedFormatStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterSupportedFormatStruct -> value -> IO ()
setIngestMethod mtrPushAVStreamTransportClusterSupportedFormatStruct value =
  sendMessage mtrPushAVStreamTransportClusterSupportedFormatStruct setIngestMethodSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @containerFormat@
containerFormatSelector :: Selector '[] (Id NSNumber)
containerFormatSelector = mkSelector "containerFormat"

-- | @Selector@ for @setContainerFormat:@
setContainerFormatSelector :: Selector '[Id NSNumber] ()
setContainerFormatSelector = mkSelector "setContainerFormat:"

-- | @Selector@ for @ingestMethod@
ingestMethodSelector :: Selector '[] (Id NSNumber)
ingestMethodSelector = mkSelector "ingestMethod"

-- | @Selector@ for @setIngestMethod:@
setIngestMethodSelector :: Selector '[Id NSNumber] ()
setIngestMethodSelector = mkSelector "setIngestMethod:"

