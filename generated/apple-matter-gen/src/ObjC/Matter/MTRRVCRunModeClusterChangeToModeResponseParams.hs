{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRRVCRunModeClusterChangeToModeResponseParams@.
module ObjC.Matter.MTRRVCRunModeClusterChangeToModeResponseParams
  ( MTRRVCRunModeClusterChangeToModeResponseParams
  , IsMTRRVCRunModeClusterChangeToModeResponseParams(..)
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

-- | Initialize an MTRRVCRunModeClusterChangeToModeResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRRVCRunModeClusterChangeToModeResponseParams mtrrvcRunModeClusterChangeToModeResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrrvcRunModeClusterChangeToModeResponseParams -> responseValue -> error_ -> IO (Id MTRRVCRunModeClusterChangeToModeResponseParams)
initWithResponseValue_error mtrrvcRunModeClusterChangeToModeResponseParams responseValue error_ =
  sendOwnedMessage mtrrvcRunModeClusterChangeToModeResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- status@
status :: IsMTRRVCRunModeClusterChangeToModeResponseParams mtrrvcRunModeClusterChangeToModeResponseParams => mtrrvcRunModeClusterChangeToModeResponseParams -> IO (Id NSNumber)
status mtrrvcRunModeClusterChangeToModeResponseParams =
  sendMessage mtrrvcRunModeClusterChangeToModeResponseParams statusSelector

-- | @- setStatus:@
setStatus :: (IsMTRRVCRunModeClusterChangeToModeResponseParams mtrrvcRunModeClusterChangeToModeResponseParams, IsNSNumber value) => mtrrvcRunModeClusterChangeToModeResponseParams -> value -> IO ()
setStatus mtrrvcRunModeClusterChangeToModeResponseParams value =
  sendMessage mtrrvcRunModeClusterChangeToModeResponseParams setStatusSelector (toNSNumber value)

-- | @- statusText@
statusText :: IsMTRRVCRunModeClusterChangeToModeResponseParams mtrrvcRunModeClusterChangeToModeResponseParams => mtrrvcRunModeClusterChangeToModeResponseParams -> IO (Id NSString)
statusText mtrrvcRunModeClusterChangeToModeResponseParams =
  sendMessage mtrrvcRunModeClusterChangeToModeResponseParams statusTextSelector

-- | @- setStatusText:@
setStatusText :: (IsMTRRVCRunModeClusterChangeToModeResponseParams mtrrvcRunModeClusterChangeToModeResponseParams, IsNSString value) => mtrrvcRunModeClusterChangeToModeResponseParams -> value -> IO ()
setStatusText mtrrvcRunModeClusterChangeToModeResponseParams value =
  sendMessage mtrrvcRunModeClusterChangeToModeResponseParams setStatusTextSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRRVCRunModeClusterChangeToModeResponseParams)
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

