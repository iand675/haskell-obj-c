{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROvenModeClusterChangeToModeResponseParams@.
module ObjC.Matter.MTROvenModeClusterChangeToModeResponseParams
  ( MTROvenModeClusterChangeToModeResponseParams
  , IsMTROvenModeClusterChangeToModeResponseParams(..)
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

-- | Initialize an MTROvenModeClusterChangeToModeResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTROvenModeClusterChangeToModeResponseParams mtrOvenModeClusterChangeToModeResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrOvenModeClusterChangeToModeResponseParams -> responseValue -> error_ -> IO (Id MTROvenModeClusterChangeToModeResponseParams)
initWithResponseValue_error mtrOvenModeClusterChangeToModeResponseParams responseValue error_ =
  sendOwnedMessage mtrOvenModeClusterChangeToModeResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- status@
status :: IsMTROvenModeClusterChangeToModeResponseParams mtrOvenModeClusterChangeToModeResponseParams => mtrOvenModeClusterChangeToModeResponseParams -> IO (Id NSNumber)
status mtrOvenModeClusterChangeToModeResponseParams =
  sendMessage mtrOvenModeClusterChangeToModeResponseParams statusSelector

-- | @- setStatus:@
setStatus :: (IsMTROvenModeClusterChangeToModeResponseParams mtrOvenModeClusterChangeToModeResponseParams, IsNSNumber value) => mtrOvenModeClusterChangeToModeResponseParams -> value -> IO ()
setStatus mtrOvenModeClusterChangeToModeResponseParams value =
  sendMessage mtrOvenModeClusterChangeToModeResponseParams setStatusSelector (toNSNumber value)

-- | @- statusText@
statusText :: IsMTROvenModeClusterChangeToModeResponseParams mtrOvenModeClusterChangeToModeResponseParams => mtrOvenModeClusterChangeToModeResponseParams -> IO (Id NSString)
statusText mtrOvenModeClusterChangeToModeResponseParams =
  sendMessage mtrOvenModeClusterChangeToModeResponseParams statusTextSelector

-- | @- setStatusText:@
setStatusText :: (IsMTROvenModeClusterChangeToModeResponseParams mtrOvenModeClusterChangeToModeResponseParams, IsNSString value) => mtrOvenModeClusterChangeToModeResponseParams -> value -> IO ()
setStatusText mtrOvenModeClusterChangeToModeResponseParams value =
  sendMessage mtrOvenModeClusterChangeToModeResponseParams setStatusTextSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTROvenModeClusterChangeToModeResponseParams)
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

