{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommissionerControlClusterCommissioningRequestResultEvent@.
module ObjC.Matter.MTRCommissionerControlClusterCommissioningRequestResultEvent
  ( MTRCommissionerControlClusterCommissioningRequestResultEvent
  , IsMTRCommissionerControlClusterCommissioningRequestResultEvent(..)
  , requestID
  , setRequestID
  , clientNodeID
  , setClientNodeID
  , statusCode
  , setStatusCode
  , fabricIndex
  , setFabricIndex
  , clientNodeIDSelector
  , fabricIndexSelector
  , requestIDSelector
  , setClientNodeIDSelector
  , setFabricIndexSelector
  , setRequestIDSelector
  , setStatusCodeSelector
  , statusCodeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- requestID@
requestID :: IsMTRCommissionerControlClusterCommissioningRequestResultEvent mtrCommissionerControlClusterCommissioningRequestResultEvent => mtrCommissionerControlClusterCommissioningRequestResultEvent -> IO (Id NSNumber)
requestID mtrCommissionerControlClusterCommissioningRequestResultEvent =
  sendMessage mtrCommissionerControlClusterCommissioningRequestResultEvent requestIDSelector

-- | @- setRequestID:@
setRequestID :: (IsMTRCommissionerControlClusterCommissioningRequestResultEvent mtrCommissionerControlClusterCommissioningRequestResultEvent, IsNSNumber value) => mtrCommissionerControlClusterCommissioningRequestResultEvent -> value -> IO ()
setRequestID mtrCommissionerControlClusterCommissioningRequestResultEvent value =
  sendMessage mtrCommissionerControlClusterCommissioningRequestResultEvent setRequestIDSelector (toNSNumber value)

-- | @- clientNodeID@
clientNodeID :: IsMTRCommissionerControlClusterCommissioningRequestResultEvent mtrCommissionerControlClusterCommissioningRequestResultEvent => mtrCommissionerControlClusterCommissioningRequestResultEvent -> IO (Id NSNumber)
clientNodeID mtrCommissionerControlClusterCommissioningRequestResultEvent =
  sendMessage mtrCommissionerControlClusterCommissioningRequestResultEvent clientNodeIDSelector

-- | @- setClientNodeID:@
setClientNodeID :: (IsMTRCommissionerControlClusterCommissioningRequestResultEvent mtrCommissionerControlClusterCommissioningRequestResultEvent, IsNSNumber value) => mtrCommissionerControlClusterCommissioningRequestResultEvent -> value -> IO ()
setClientNodeID mtrCommissionerControlClusterCommissioningRequestResultEvent value =
  sendMessage mtrCommissionerControlClusterCommissioningRequestResultEvent setClientNodeIDSelector (toNSNumber value)

-- | @- statusCode@
statusCode :: IsMTRCommissionerControlClusterCommissioningRequestResultEvent mtrCommissionerControlClusterCommissioningRequestResultEvent => mtrCommissionerControlClusterCommissioningRequestResultEvent -> IO (Id NSNumber)
statusCode mtrCommissionerControlClusterCommissioningRequestResultEvent =
  sendMessage mtrCommissionerControlClusterCommissioningRequestResultEvent statusCodeSelector

-- | @- setStatusCode:@
setStatusCode :: (IsMTRCommissionerControlClusterCommissioningRequestResultEvent mtrCommissionerControlClusterCommissioningRequestResultEvent, IsNSNumber value) => mtrCommissionerControlClusterCommissioningRequestResultEvent -> value -> IO ()
setStatusCode mtrCommissionerControlClusterCommissioningRequestResultEvent value =
  sendMessage mtrCommissionerControlClusterCommissioningRequestResultEvent setStatusCodeSelector (toNSNumber value)

-- | @- fabricIndex@
fabricIndex :: IsMTRCommissionerControlClusterCommissioningRequestResultEvent mtrCommissionerControlClusterCommissioningRequestResultEvent => mtrCommissionerControlClusterCommissioningRequestResultEvent -> IO (Id NSNumber)
fabricIndex mtrCommissionerControlClusterCommissioningRequestResultEvent =
  sendMessage mtrCommissionerControlClusterCommissioningRequestResultEvent fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRCommissionerControlClusterCommissioningRequestResultEvent mtrCommissionerControlClusterCommissioningRequestResultEvent, IsNSNumber value) => mtrCommissionerControlClusterCommissioningRequestResultEvent -> value -> IO ()
setFabricIndex mtrCommissionerControlClusterCommissioningRequestResultEvent value =
  sendMessage mtrCommissionerControlClusterCommissioningRequestResultEvent setFabricIndexSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requestID@
requestIDSelector :: Selector '[] (Id NSNumber)
requestIDSelector = mkSelector "requestID"

-- | @Selector@ for @setRequestID:@
setRequestIDSelector :: Selector '[Id NSNumber] ()
setRequestIDSelector = mkSelector "setRequestID:"

-- | @Selector@ for @clientNodeID@
clientNodeIDSelector :: Selector '[] (Id NSNumber)
clientNodeIDSelector = mkSelector "clientNodeID"

-- | @Selector@ for @setClientNodeID:@
setClientNodeIDSelector :: Selector '[Id NSNumber] ()
setClientNodeIDSelector = mkSelector "setClientNodeID:"

-- | @Selector@ for @statusCode@
statusCodeSelector :: Selector '[] (Id NSNumber)
statusCodeSelector = mkSelector "statusCode"

-- | @Selector@ for @setStatusCode:@
setStatusCodeSelector :: Selector '[Id NSNumber] ()
setStatusCodeSelector = mkSelector "setStatusCode:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector '[] (Id NSNumber)
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector '[Id NSNumber] ()
setFabricIndexSelector = mkSelector "setFabricIndex:"

