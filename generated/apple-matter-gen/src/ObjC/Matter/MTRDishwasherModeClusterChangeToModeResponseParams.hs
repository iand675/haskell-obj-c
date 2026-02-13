{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDishwasherModeClusterChangeToModeResponseParams@.
module ObjC.Matter.MTRDishwasherModeClusterChangeToModeResponseParams
  ( MTRDishwasherModeClusterChangeToModeResponseParams
  , IsMTRDishwasherModeClusterChangeToModeResponseParams(..)
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

-- | Initialize an MTRDishwasherModeClusterChangeToModeResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRDishwasherModeClusterChangeToModeResponseParams mtrDishwasherModeClusterChangeToModeResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrDishwasherModeClusterChangeToModeResponseParams -> responseValue -> error_ -> IO (Id MTRDishwasherModeClusterChangeToModeResponseParams)
initWithResponseValue_error mtrDishwasherModeClusterChangeToModeResponseParams responseValue error_ =
  sendOwnedMessage mtrDishwasherModeClusterChangeToModeResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- status@
status :: IsMTRDishwasherModeClusterChangeToModeResponseParams mtrDishwasherModeClusterChangeToModeResponseParams => mtrDishwasherModeClusterChangeToModeResponseParams -> IO (Id NSNumber)
status mtrDishwasherModeClusterChangeToModeResponseParams =
  sendMessage mtrDishwasherModeClusterChangeToModeResponseParams statusSelector

-- | @- setStatus:@
setStatus :: (IsMTRDishwasherModeClusterChangeToModeResponseParams mtrDishwasherModeClusterChangeToModeResponseParams, IsNSNumber value) => mtrDishwasherModeClusterChangeToModeResponseParams -> value -> IO ()
setStatus mtrDishwasherModeClusterChangeToModeResponseParams value =
  sendMessage mtrDishwasherModeClusterChangeToModeResponseParams setStatusSelector (toNSNumber value)

-- | @- statusText@
statusText :: IsMTRDishwasherModeClusterChangeToModeResponseParams mtrDishwasherModeClusterChangeToModeResponseParams => mtrDishwasherModeClusterChangeToModeResponseParams -> IO (Id NSString)
statusText mtrDishwasherModeClusterChangeToModeResponseParams =
  sendMessage mtrDishwasherModeClusterChangeToModeResponseParams statusTextSelector

-- | @- setStatusText:@
setStatusText :: (IsMTRDishwasherModeClusterChangeToModeResponseParams mtrDishwasherModeClusterChangeToModeResponseParams, IsNSString value) => mtrDishwasherModeClusterChangeToModeResponseParams -> value -> IO ()
setStatusText mtrDishwasherModeClusterChangeToModeResponseParams value =
  sendMessage mtrDishwasherModeClusterChangeToModeResponseParams setStatusTextSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRDishwasherModeClusterChangeToModeResponseParams)
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

