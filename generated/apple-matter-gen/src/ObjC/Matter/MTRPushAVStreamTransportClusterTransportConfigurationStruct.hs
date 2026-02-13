{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPushAVStreamTransportClusterTransportConfigurationStruct@.
module ObjC.Matter.MTRPushAVStreamTransportClusterTransportConfigurationStruct
  ( MTRPushAVStreamTransportClusterTransportConfigurationStruct
  , IsMTRPushAVStreamTransportClusterTransportConfigurationStruct(..)
  , connectionID
  , setConnectionID
  , transportStatus
  , setTransportStatus
  , transportOptions
  , setTransportOptions
  , fabricIndex
  , setFabricIndex
  , connectionIDSelector
  , fabricIndexSelector
  , setConnectionIDSelector
  , setFabricIndexSelector
  , setTransportOptionsSelector
  , setTransportStatusSelector
  , transportOptionsSelector
  , transportStatusSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- connectionID@
connectionID :: IsMTRPushAVStreamTransportClusterTransportConfigurationStruct mtrPushAVStreamTransportClusterTransportConfigurationStruct => mtrPushAVStreamTransportClusterTransportConfigurationStruct -> IO (Id NSNumber)
connectionID mtrPushAVStreamTransportClusterTransportConfigurationStruct =
  sendMessage mtrPushAVStreamTransportClusterTransportConfigurationStruct connectionIDSelector

-- | @- setConnectionID:@
setConnectionID :: (IsMTRPushAVStreamTransportClusterTransportConfigurationStruct mtrPushAVStreamTransportClusterTransportConfigurationStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterTransportConfigurationStruct -> value -> IO ()
setConnectionID mtrPushAVStreamTransportClusterTransportConfigurationStruct value =
  sendMessage mtrPushAVStreamTransportClusterTransportConfigurationStruct setConnectionIDSelector (toNSNumber value)

-- | @- transportStatus@
transportStatus :: IsMTRPushAVStreamTransportClusterTransportConfigurationStruct mtrPushAVStreamTransportClusterTransportConfigurationStruct => mtrPushAVStreamTransportClusterTransportConfigurationStruct -> IO (Id NSNumber)
transportStatus mtrPushAVStreamTransportClusterTransportConfigurationStruct =
  sendMessage mtrPushAVStreamTransportClusterTransportConfigurationStruct transportStatusSelector

-- | @- setTransportStatus:@
setTransportStatus :: (IsMTRPushAVStreamTransportClusterTransportConfigurationStruct mtrPushAVStreamTransportClusterTransportConfigurationStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterTransportConfigurationStruct -> value -> IO ()
setTransportStatus mtrPushAVStreamTransportClusterTransportConfigurationStruct value =
  sendMessage mtrPushAVStreamTransportClusterTransportConfigurationStruct setTransportStatusSelector (toNSNumber value)

-- | @- transportOptions@
transportOptions :: IsMTRPushAVStreamTransportClusterTransportConfigurationStruct mtrPushAVStreamTransportClusterTransportConfigurationStruct => mtrPushAVStreamTransportClusterTransportConfigurationStruct -> IO (Id MTRPushAVStreamTransportClusterTransportOptionsStruct)
transportOptions mtrPushAVStreamTransportClusterTransportConfigurationStruct =
  sendMessage mtrPushAVStreamTransportClusterTransportConfigurationStruct transportOptionsSelector

-- | @- setTransportOptions:@
setTransportOptions :: (IsMTRPushAVStreamTransportClusterTransportConfigurationStruct mtrPushAVStreamTransportClusterTransportConfigurationStruct, IsMTRPushAVStreamTransportClusterTransportOptionsStruct value) => mtrPushAVStreamTransportClusterTransportConfigurationStruct -> value -> IO ()
setTransportOptions mtrPushAVStreamTransportClusterTransportConfigurationStruct value =
  sendMessage mtrPushAVStreamTransportClusterTransportConfigurationStruct setTransportOptionsSelector (toMTRPushAVStreamTransportClusterTransportOptionsStruct value)

-- | @- fabricIndex@
fabricIndex :: IsMTRPushAVStreamTransportClusterTransportConfigurationStruct mtrPushAVStreamTransportClusterTransportConfigurationStruct => mtrPushAVStreamTransportClusterTransportConfigurationStruct -> IO (Id NSNumber)
fabricIndex mtrPushAVStreamTransportClusterTransportConfigurationStruct =
  sendMessage mtrPushAVStreamTransportClusterTransportConfigurationStruct fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRPushAVStreamTransportClusterTransportConfigurationStruct mtrPushAVStreamTransportClusterTransportConfigurationStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterTransportConfigurationStruct -> value -> IO ()
setFabricIndex mtrPushAVStreamTransportClusterTransportConfigurationStruct value =
  sendMessage mtrPushAVStreamTransportClusterTransportConfigurationStruct setFabricIndexSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @connectionID@
connectionIDSelector :: Selector '[] (Id NSNumber)
connectionIDSelector = mkSelector "connectionID"

-- | @Selector@ for @setConnectionID:@
setConnectionIDSelector :: Selector '[Id NSNumber] ()
setConnectionIDSelector = mkSelector "setConnectionID:"

-- | @Selector@ for @transportStatus@
transportStatusSelector :: Selector '[] (Id NSNumber)
transportStatusSelector = mkSelector "transportStatus"

-- | @Selector@ for @setTransportStatus:@
setTransportStatusSelector :: Selector '[Id NSNumber] ()
setTransportStatusSelector = mkSelector "setTransportStatus:"

-- | @Selector@ for @transportOptions@
transportOptionsSelector :: Selector '[] (Id MTRPushAVStreamTransportClusterTransportOptionsStruct)
transportOptionsSelector = mkSelector "transportOptions"

-- | @Selector@ for @setTransportOptions:@
setTransportOptionsSelector :: Selector '[Id MTRPushAVStreamTransportClusterTransportOptionsStruct] ()
setTransportOptionsSelector = mkSelector "setTransportOptions:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector '[] (Id NSNumber)
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector '[Id NSNumber] ()
setFabricIndexSelector = mkSelector "setFabricIndex:"

