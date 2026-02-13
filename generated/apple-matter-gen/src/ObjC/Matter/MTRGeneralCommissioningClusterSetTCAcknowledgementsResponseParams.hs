{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGeneralCommissioningClusterSetTCAcknowledgementsResponseParams@.
module ObjC.Matter.MTRGeneralCommissioningClusterSetTCAcknowledgementsResponseParams
  ( MTRGeneralCommissioningClusterSetTCAcknowledgementsResponseParams
  , IsMTRGeneralCommissioningClusterSetTCAcknowledgementsResponseParams(..)
  , initWithResponseValue_error
  , errorCode
  , setErrorCode
  , errorCodeSelector
  , initWithResponseValue_errorSelector
  , setErrorCodeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRGeneralCommissioningClusterSetTCAcknowledgementsResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRGeneralCommissioningClusterSetTCAcknowledgementsResponseParams mtrGeneralCommissioningClusterSetTCAcknowledgementsResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrGeneralCommissioningClusterSetTCAcknowledgementsResponseParams -> responseValue -> error_ -> IO (Id MTRGeneralCommissioningClusterSetTCAcknowledgementsResponseParams)
initWithResponseValue_error mtrGeneralCommissioningClusterSetTCAcknowledgementsResponseParams responseValue error_ =
  sendOwnedMessage mtrGeneralCommissioningClusterSetTCAcknowledgementsResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- errorCode@
errorCode :: IsMTRGeneralCommissioningClusterSetTCAcknowledgementsResponseParams mtrGeneralCommissioningClusterSetTCAcknowledgementsResponseParams => mtrGeneralCommissioningClusterSetTCAcknowledgementsResponseParams -> IO (Id NSNumber)
errorCode mtrGeneralCommissioningClusterSetTCAcknowledgementsResponseParams =
  sendMessage mtrGeneralCommissioningClusterSetTCAcknowledgementsResponseParams errorCodeSelector

-- | @- setErrorCode:@
setErrorCode :: (IsMTRGeneralCommissioningClusterSetTCAcknowledgementsResponseParams mtrGeneralCommissioningClusterSetTCAcknowledgementsResponseParams, IsNSNumber value) => mtrGeneralCommissioningClusterSetTCAcknowledgementsResponseParams -> value -> IO ()
setErrorCode mtrGeneralCommissioningClusterSetTCAcknowledgementsResponseParams value =
  sendMessage mtrGeneralCommissioningClusterSetTCAcknowledgementsResponseParams setErrorCodeSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRGeneralCommissioningClusterSetTCAcknowledgementsResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @errorCode@
errorCodeSelector :: Selector '[] (Id NSNumber)
errorCodeSelector = mkSelector "errorCode"

-- | @Selector@ for @setErrorCode:@
setErrorCodeSelector :: Selector '[Id NSNumber] ()
setErrorCodeSelector = mkSelector "setErrorCode:"

