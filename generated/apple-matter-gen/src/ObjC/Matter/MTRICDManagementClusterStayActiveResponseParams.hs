{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRICDManagementClusterStayActiveResponseParams@.
module ObjC.Matter.MTRICDManagementClusterStayActiveResponseParams
  ( MTRICDManagementClusterStayActiveResponseParams
  , IsMTRICDManagementClusterStayActiveResponseParams(..)
  , initWithResponseValue_error
  , promisedActiveDuration
  , setPromisedActiveDuration
  , initWithResponseValue_errorSelector
  , promisedActiveDurationSelector
  , setPromisedActiveDurationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRICDManagementClusterStayActiveResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRICDManagementClusterStayActiveResponseParams mtricdManagementClusterStayActiveResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtricdManagementClusterStayActiveResponseParams -> responseValue -> error_ -> IO (Id MTRICDManagementClusterStayActiveResponseParams)
initWithResponseValue_error mtricdManagementClusterStayActiveResponseParams responseValue error_ =
  sendOwnedMessage mtricdManagementClusterStayActiveResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- promisedActiveDuration@
promisedActiveDuration :: IsMTRICDManagementClusterStayActiveResponseParams mtricdManagementClusterStayActiveResponseParams => mtricdManagementClusterStayActiveResponseParams -> IO (Id NSNumber)
promisedActiveDuration mtricdManagementClusterStayActiveResponseParams =
  sendMessage mtricdManagementClusterStayActiveResponseParams promisedActiveDurationSelector

-- | @- setPromisedActiveDuration:@
setPromisedActiveDuration :: (IsMTRICDManagementClusterStayActiveResponseParams mtricdManagementClusterStayActiveResponseParams, IsNSNumber value) => mtricdManagementClusterStayActiveResponseParams -> value -> IO ()
setPromisedActiveDuration mtricdManagementClusterStayActiveResponseParams value =
  sendMessage mtricdManagementClusterStayActiveResponseParams setPromisedActiveDurationSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRICDManagementClusterStayActiveResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @promisedActiveDuration@
promisedActiveDurationSelector :: Selector '[] (Id NSNumber)
promisedActiveDurationSelector = mkSelector "promisedActiveDuration"

-- | @Selector@ for @setPromisedActiveDuration:@
setPromisedActiveDurationSelector :: Selector '[Id NSNumber] ()
setPromisedActiveDurationSelector = mkSelector "setPromisedActiveDuration:"

