{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGroupcastClusterLeaveGroupResponseParams@.
module ObjC.Matter.MTRGroupcastClusterLeaveGroupResponseParams
  ( MTRGroupcastClusterLeaveGroupResponseParams
  , IsMTRGroupcastClusterLeaveGroupResponseParams(..)
  , initWithResponseValue_error
  , groupID
  , setGroupID
  , endpoints
  , setEndpoints
  , listTooLarge
  , setListTooLarge
  , endpointsSelector
  , groupIDSelector
  , initWithResponseValue_errorSelector
  , listTooLargeSelector
  , setEndpointsSelector
  , setGroupIDSelector
  , setListTooLargeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRGroupcastClusterLeaveGroupResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRGroupcastClusterLeaveGroupResponseParams mtrGroupcastClusterLeaveGroupResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrGroupcastClusterLeaveGroupResponseParams -> responseValue -> error_ -> IO (Id MTRGroupcastClusterLeaveGroupResponseParams)
initWithResponseValue_error mtrGroupcastClusterLeaveGroupResponseParams responseValue error_ =
  sendOwnedMessage mtrGroupcastClusterLeaveGroupResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- groupID@
groupID :: IsMTRGroupcastClusterLeaveGroupResponseParams mtrGroupcastClusterLeaveGroupResponseParams => mtrGroupcastClusterLeaveGroupResponseParams -> IO (Id NSNumber)
groupID mtrGroupcastClusterLeaveGroupResponseParams =
  sendMessage mtrGroupcastClusterLeaveGroupResponseParams groupIDSelector

-- | @- setGroupID:@
setGroupID :: (IsMTRGroupcastClusterLeaveGroupResponseParams mtrGroupcastClusterLeaveGroupResponseParams, IsNSNumber value) => mtrGroupcastClusterLeaveGroupResponseParams -> value -> IO ()
setGroupID mtrGroupcastClusterLeaveGroupResponseParams value =
  sendMessage mtrGroupcastClusterLeaveGroupResponseParams setGroupIDSelector (toNSNumber value)

-- | @- endpoints@
endpoints :: IsMTRGroupcastClusterLeaveGroupResponseParams mtrGroupcastClusterLeaveGroupResponseParams => mtrGroupcastClusterLeaveGroupResponseParams -> IO (Id NSArray)
endpoints mtrGroupcastClusterLeaveGroupResponseParams =
  sendMessage mtrGroupcastClusterLeaveGroupResponseParams endpointsSelector

-- | @- setEndpoints:@
setEndpoints :: (IsMTRGroupcastClusterLeaveGroupResponseParams mtrGroupcastClusterLeaveGroupResponseParams, IsNSArray value) => mtrGroupcastClusterLeaveGroupResponseParams -> value -> IO ()
setEndpoints mtrGroupcastClusterLeaveGroupResponseParams value =
  sendMessage mtrGroupcastClusterLeaveGroupResponseParams setEndpointsSelector (toNSArray value)

-- | @- listTooLarge@
listTooLarge :: IsMTRGroupcastClusterLeaveGroupResponseParams mtrGroupcastClusterLeaveGroupResponseParams => mtrGroupcastClusterLeaveGroupResponseParams -> IO (Id NSNumber)
listTooLarge mtrGroupcastClusterLeaveGroupResponseParams =
  sendMessage mtrGroupcastClusterLeaveGroupResponseParams listTooLargeSelector

-- | @- setListTooLarge:@
setListTooLarge :: (IsMTRGroupcastClusterLeaveGroupResponseParams mtrGroupcastClusterLeaveGroupResponseParams, IsNSNumber value) => mtrGroupcastClusterLeaveGroupResponseParams -> value -> IO ()
setListTooLarge mtrGroupcastClusterLeaveGroupResponseParams value =
  sendMessage mtrGroupcastClusterLeaveGroupResponseParams setListTooLargeSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRGroupcastClusterLeaveGroupResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @groupID@
groupIDSelector :: Selector '[] (Id NSNumber)
groupIDSelector = mkSelector "groupID"

-- | @Selector@ for @setGroupID:@
setGroupIDSelector :: Selector '[Id NSNumber] ()
setGroupIDSelector = mkSelector "setGroupID:"

-- | @Selector@ for @endpoints@
endpointsSelector :: Selector '[] (Id NSArray)
endpointsSelector = mkSelector "endpoints"

-- | @Selector@ for @setEndpoints:@
setEndpointsSelector :: Selector '[Id NSArray] ()
setEndpointsSelector = mkSelector "setEndpoints:"

-- | @Selector@ for @listTooLarge@
listTooLargeSelector :: Selector '[] (Id NSNumber)
listTooLargeSelector = mkSelector "listTooLarge"

-- | @Selector@ for @setListTooLarge:@
setListTooLargeSelector :: Selector '[Id NSNumber] ()
setListTooLargeSelector = mkSelector "setListTooLarge:"

