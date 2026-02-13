{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPushAVStreamTransportClusterContainerOptionsStruct@.
module ObjC.Matter.MTRPushAVStreamTransportClusterContainerOptionsStruct
  ( MTRPushAVStreamTransportClusterContainerOptionsStruct
  , IsMTRPushAVStreamTransportClusterContainerOptionsStruct(..)
  , containerType
  , setContainerType
  , cmafContainerOptions
  , setCmafContainerOptions
  , cmafContainerOptionsSelector
  , containerTypeSelector
  , setCmafContainerOptionsSelector
  , setContainerTypeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- containerType@
containerType :: IsMTRPushAVStreamTransportClusterContainerOptionsStruct mtrPushAVStreamTransportClusterContainerOptionsStruct => mtrPushAVStreamTransportClusterContainerOptionsStruct -> IO (Id NSNumber)
containerType mtrPushAVStreamTransportClusterContainerOptionsStruct =
  sendMessage mtrPushAVStreamTransportClusterContainerOptionsStruct containerTypeSelector

-- | @- setContainerType:@
setContainerType :: (IsMTRPushAVStreamTransportClusterContainerOptionsStruct mtrPushAVStreamTransportClusterContainerOptionsStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterContainerOptionsStruct -> value -> IO ()
setContainerType mtrPushAVStreamTransportClusterContainerOptionsStruct value =
  sendMessage mtrPushAVStreamTransportClusterContainerOptionsStruct setContainerTypeSelector (toNSNumber value)

-- | @- cmafContainerOptions@
cmafContainerOptions :: IsMTRPushAVStreamTransportClusterContainerOptionsStruct mtrPushAVStreamTransportClusterContainerOptionsStruct => mtrPushAVStreamTransportClusterContainerOptionsStruct -> IO (Id MTRPushAVStreamTransportClusterCMAFContainerOptionsStruct)
cmafContainerOptions mtrPushAVStreamTransportClusterContainerOptionsStruct =
  sendMessage mtrPushAVStreamTransportClusterContainerOptionsStruct cmafContainerOptionsSelector

-- | @- setCmafContainerOptions:@
setCmafContainerOptions :: (IsMTRPushAVStreamTransportClusterContainerOptionsStruct mtrPushAVStreamTransportClusterContainerOptionsStruct, IsMTRPushAVStreamTransportClusterCMAFContainerOptionsStruct value) => mtrPushAVStreamTransportClusterContainerOptionsStruct -> value -> IO ()
setCmafContainerOptions mtrPushAVStreamTransportClusterContainerOptionsStruct value =
  sendMessage mtrPushAVStreamTransportClusterContainerOptionsStruct setCmafContainerOptionsSelector (toMTRPushAVStreamTransportClusterCMAFContainerOptionsStruct value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @containerType@
containerTypeSelector :: Selector '[] (Id NSNumber)
containerTypeSelector = mkSelector "containerType"

-- | @Selector@ for @setContainerType:@
setContainerTypeSelector :: Selector '[Id NSNumber] ()
setContainerTypeSelector = mkSelector "setContainerType:"

-- | @Selector@ for @cmafContainerOptions@
cmafContainerOptionsSelector :: Selector '[] (Id MTRPushAVStreamTransportClusterCMAFContainerOptionsStruct)
cmafContainerOptionsSelector = mkSelector "cmafContainerOptions"

-- | @Selector@ for @setCmafContainerOptions:@
setCmafContainerOptionsSelector :: Selector '[Id MTRPushAVStreamTransportClusterCMAFContainerOptionsStruct] ()
setCmafContainerOptionsSelector = mkSelector "setCmafContainerOptions:"

