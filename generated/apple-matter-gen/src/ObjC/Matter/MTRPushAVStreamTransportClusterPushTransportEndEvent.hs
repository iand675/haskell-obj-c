{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPushAVStreamTransportClusterPushTransportEndEvent@.
module ObjC.Matter.MTRPushAVStreamTransportClusterPushTransportEndEvent
  ( MTRPushAVStreamTransportClusterPushTransportEndEvent
  , IsMTRPushAVStreamTransportClusterPushTransportEndEvent(..)
  , connectionID
  , setConnectionID
  , connectionIDSelector
  , setConnectionIDSelector


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
connectionID :: IsMTRPushAVStreamTransportClusterPushTransportEndEvent mtrPushAVStreamTransportClusterPushTransportEndEvent => mtrPushAVStreamTransportClusterPushTransportEndEvent -> IO (Id NSNumber)
connectionID mtrPushAVStreamTransportClusterPushTransportEndEvent =
  sendMessage mtrPushAVStreamTransportClusterPushTransportEndEvent connectionIDSelector

-- | @- setConnectionID:@
setConnectionID :: (IsMTRPushAVStreamTransportClusterPushTransportEndEvent mtrPushAVStreamTransportClusterPushTransportEndEvent, IsNSNumber value) => mtrPushAVStreamTransportClusterPushTransportEndEvent -> value -> IO ()
setConnectionID mtrPushAVStreamTransportClusterPushTransportEndEvent value =
  sendMessage mtrPushAVStreamTransportClusterPushTransportEndEvent setConnectionIDSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @connectionID@
connectionIDSelector :: Selector '[] (Id NSNumber)
connectionIDSelector = mkSelector "connectionID"

-- | @Selector@ for @setConnectionID:@
setConnectionIDSelector :: Selector '[Id NSNumber] ()
setConnectionIDSelector = mkSelector "setConnectionID:"

