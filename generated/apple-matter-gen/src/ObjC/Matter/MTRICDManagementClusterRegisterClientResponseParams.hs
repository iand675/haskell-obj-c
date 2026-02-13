{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRICDManagementClusterRegisterClientResponseParams@.
module ObjC.Matter.MTRICDManagementClusterRegisterClientResponseParams
  ( MTRICDManagementClusterRegisterClientResponseParams
  , IsMTRICDManagementClusterRegisterClientResponseParams(..)
  , initWithResponseValue_error
  , icdCounter
  , setIcdCounter
  , icdCounterSelector
  , initWithResponseValue_errorSelector
  , setIcdCounterSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRICDManagementClusterRegisterClientResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRICDManagementClusterRegisterClientResponseParams mtricdManagementClusterRegisterClientResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtricdManagementClusterRegisterClientResponseParams -> responseValue -> error_ -> IO (Id MTRICDManagementClusterRegisterClientResponseParams)
initWithResponseValue_error mtricdManagementClusterRegisterClientResponseParams responseValue error_ =
  sendOwnedMessage mtricdManagementClusterRegisterClientResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- icdCounter@
icdCounter :: IsMTRICDManagementClusterRegisterClientResponseParams mtricdManagementClusterRegisterClientResponseParams => mtricdManagementClusterRegisterClientResponseParams -> IO (Id NSNumber)
icdCounter mtricdManagementClusterRegisterClientResponseParams =
  sendMessage mtricdManagementClusterRegisterClientResponseParams icdCounterSelector

-- | @- setIcdCounter:@
setIcdCounter :: (IsMTRICDManagementClusterRegisterClientResponseParams mtricdManagementClusterRegisterClientResponseParams, IsNSNumber value) => mtricdManagementClusterRegisterClientResponseParams -> value -> IO ()
setIcdCounter mtricdManagementClusterRegisterClientResponseParams value =
  sendMessage mtricdManagementClusterRegisterClientResponseParams setIcdCounterSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRICDManagementClusterRegisterClientResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @icdCounter@
icdCounterSelector :: Selector '[] (Id NSNumber)
icdCounterSelector = mkSelector "icdCounter"

-- | @Selector@ for @setIcdCounter:@
setIcdCounterSelector :: Selector '[Id NSNumber] ()
setIcdCounterSelector = mkSelector "setIcdCounter:"

