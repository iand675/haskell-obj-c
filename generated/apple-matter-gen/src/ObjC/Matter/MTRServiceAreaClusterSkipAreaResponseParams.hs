{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRServiceAreaClusterSkipAreaResponseParams@.
module ObjC.Matter.MTRServiceAreaClusterSkipAreaResponseParams
  ( MTRServiceAreaClusterSkipAreaResponseParams
  , IsMTRServiceAreaClusterSkipAreaResponseParams(..)
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

-- | Initialize an MTRServiceAreaClusterSkipAreaResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRServiceAreaClusterSkipAreaResponseParams mtrServiceAreaClusterSkipAreaResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrServiceAreaClusterSkipAreaResponseParams -> responseValue -> error_ -> IO (Id MTRServiceAreaClusterSkipAreaResponseParams)
initWithResponseValue_error mtrServiceAreaClusterSkipAreaResponseParams responseValue error_ =
  sendOwnedMessage mtrServiceAreaClusterSkipAreaResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- status@
status :: IsMTRServiceAreaClusterSkipAreaResponseParams mtrServiceAreaClusterSkipAreaResponseParams => mtrServiceAreaClusterSkipAreaResponseParams -> IO (Id NSNumber)
status mtrServiceAreaClusterSkipAreaResponseParams =
  sendMessage mtrServiceAreaClusterSkipAreaResponseParams statusSelector

-- | @- setStatus:@
setStatus :: (IsMTRServiceAreaClusterSkipAreaResponseParams mtrServiceAreaClusterSkipAreaResponseParams, IsNSNumber value) => mtrServiceAreaClusterSkipAreaResponseParams -> value -> IO ()
setStatus mtrServiceAreaClusterSkipAreaResponseParams value =
  sendMessage mtrServiceAreaClusterSkipAreaResponseParams setStatusSelector (toNSNumber value)

-- | @- statusText@
statusText :: IsMTRServiceAreaClusterSkipAreaResponseParams mtrServiceAreaClusterSkipAreaResponseParams => mtrServiceAreaClusterSkipAreaResponseParams -> IO (Id NSString)
statusText mtrServiceAreaClusterSkipAreaResponseParams =
  sendMessage mtrServiceAreaClusterSkipAreaResponseParams statusTextSelector

-- | @- setStatusText:@
setStatusText :: (IsMTRServiceAreaClusterSkipAreaResponseParams mtrServiceAreaClusterSkipAreaResponseParams, IsNSString value) => mtrServiceAreaClusterSkipAreaResponseParams -> value -> IO ()
setStatusText mtrServiceAreaClusterSkipAreaResponseParams value =
  sendMessage mtrServiceAreaClusterSkipAreaResponseParams setStatusTextSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRServiceAreaClusterSkipAreaResponseParams)
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

