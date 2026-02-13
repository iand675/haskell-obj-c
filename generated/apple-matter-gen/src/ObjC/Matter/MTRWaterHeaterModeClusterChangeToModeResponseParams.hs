{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWaterHeaterModeClusterChangeToModeResponseParams@.
module ObjC.Matter.MTRWaterHeaterModeClusterChangeToModeResponseParams
  ( MTRWaterHeaterModeClusterChangeToModeResponseParams
  , IsMTRWaterHeaterModeClusterChangeToModeResponseParams(..)
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

-- | Initialize an MTRWaterHeaterModeClusterChangeToModeResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRWaterHeaterModeClusterChangeToModeResponseParams mtrWaterHeaterModeClusterChangeToModeResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrWaterHeaterModeClusterChangeToModeResponseParams -> responseValue -> error_ -> IO (Id MTRWaterHeaterModeClusterChangeToModeResponseParams)
initWithResponseValue_error mtrWaterHeaterModeClusterChangeToModeResponseParams responseValue error_ =
  sendOwnedMessage mtrWaterHeaterModeClusterChangeToModeResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- status@
status :: IsMTRWaterHeaterModeClusterChangeToModeResponseParams mtrWaterHeaterModeClusterChangeToModeResponseParams => mtrWaterHeaterModeClusterChangeToModeResponseParams -> IO (Id NSNumber)
status mtrWaterHeaterModeClusterChangeToModeResponseParams =
  sendMessage mtrWaterHeaterModeClusterChangeToModeResponseParams statusSelector

-- | @- setStatus:@
setStatus :: (IsMTRWaterHeaterModeClusterChangeToModeResponseParams mtrWaterHeaterModeClusterChangeToModeResponseParams, IsNSNumber value) => mtrWaterHeaterModeClusterChangeToModeResponseParams -> value -> IO ()
setStatus mtrWaterHeaterModeClusterChangeToModeResponseParams value =
  sendMessage mtrWaterHeaterModeClusterChangeToModeResponseParams setStatusSelector (toNSNumber value)

-- | @- statusText@
statusText :: IsMTRWaterHeaterModeClusterChangeToModeResponseParams mtrWaterHeaterModeClusterChangeToModeResponseParams => mtrWaterHeaterModeClusterChangeToModeResponseParams -> IO (Id NSString)
statusText mtrWaterHeaterModeClusterChangeToModeResponseParams =
  sendMessage mtrWaterHeaterModeClusterChangeToModeResponseParams statusTextSelector

-- | @- setStatusText:@
setStatusText :: (IsMTRWaterHeaterModeClusterChangeToModeResponseParams mtrWaterHeaterModeClusterChangeToModeResponseParams, IsNSString value) => mtrWaterHeaterModeClusterChangeToModeResponseParams -> value -> IO ()
setStatusText mtrWaterHeaterModeClusterChangeToModeResponseParams value =
  sendMessage mtrWaterHeaterModeClusterChangeToModeResponseParams setStatusTextSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRWaterHeaterModeClusterChangeToModeResponseParams)
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

