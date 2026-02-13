{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRRVCCleanModeClusterChangeToModeResponseParams@.
module ObjC.Matter.MTRRVCCleanModeClusterChangeToModeResponseParams
  ( MTRRVCCleanModeClusterChangeToModeResponseParams
  , IsMTRRVCCleanModeClusterChangeToModeResponseParams(..)
  , initWithResponseValue_error
  , status
  , setStatus
  , statusText
  , setStatusText
  , initWithResponseValue_errorSelector
  , setStatusSelector
  , setStatusTextSelector
  , statusSelector
  , statusTextSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRRVCCleanModeClusterChangeToModeResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRRVCCleanModeClusterChangeToModeResponseParams mtrrvcCleanModeClusterChangeToModeResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrrvcCleanModeClusterChangeToModeResponseParams -> responseValue -> error_ -> IO (Id MTRRVCCleanModeClusterChangeToModeResponseParams)
initWithResponseValue_error mtrrvcCleanModeClusterChangeToModeResponseParams responseValue error_ =
  sendOwnedMessage mtrrvcCleanModeClusterChangeToModeResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- status@
status :: IsMTRRVCCleanModeClusterChangeToModeResponseParams mtrrvcCleanModeClusterChangeToModeResponseParams => mtrrvcCleanModeClusterChangeToModeResponseParams -> IO (Id NSNumber)
status mtrrvcCleanModeClusterChangeToModeResponseParams =
  sendMessage mtrrvcCleanModeClusterChangeToModeResponseParams statusSelector

-- | @- setStatus:@
setStatus :: (IsMTRRVCCleanModeClusterChangeToModeResponseParams mtrrvcCleanModeClusterChangeToModeResponseParams, IsNSNumber value) => mtrrvcCleanModeClusterChangeToModeResponseParams -> value -> IO ()
setStatus mtrrvcCleanModeClusterChangeToModeResponseParams value =
  sendMessage mtrrvcCleanModeClusterChangeToModeResponseParams setStatusSelector (toNSNumber value)

-- | @- statusText@
statusText :: IsMTRRVCCleanModeClusterChangeToModeResponseParams mtrrvcCleanModeClusterChangeToModeResponseParams => mtrrvcCleanModeClusterChangeToModeResponseParams -> IO (Id NSString)
statusText mtrrvcCleanModeClusterChangeToModeResponseParams =
  sendMessage mtrrvcCleanModeClusterChangeToModeResponseParams statusTextSelector

-- | @- setStatusText:@
setStatusText :: (IsMTRRVCCleanModeClusterChangeToModeResponseParams mtrrvcCleanModeClusterChangeToModeResponseParams, IsNSString value) => mtrrvcCleanModeClusterChangeToModeResponseParams -> value -> IO ()
setStatusText mtrrvcCleanModeClusterChangeToModeResponseParams value =
  sendMessage mtrrvcCleanModeClusterChangeToModeResponseParams setStatusTextSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRRVCCleanModeClusterChangeToModeResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @status@
statusSelector :: Selector '[] (Id NSNumber)
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector '[Id NSNumber] ()
setStatusSelector = mkSelector "setStatus:"

-- | @Selector@ for @statusText@
statusTextSelector :: Selector '[] (Id NSString)
statusTextSelector = mkSelector "statusText"

-- | @Selector@ for @setStatusText:@
setStatusTextSelector :: Selector '[Id NSString] ()
setStatusTextSelector = mkSelector "setStatusText:"

