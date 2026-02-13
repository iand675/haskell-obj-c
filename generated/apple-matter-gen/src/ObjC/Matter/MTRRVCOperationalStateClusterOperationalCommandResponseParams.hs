{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRRVCOperationalStateClusterOperationalCommandResponseParams@.
module ObjC.Matter.MTRRVCOperationalStateClusterOperationalCommandResponseParams
  ( MTRRVCOperationalStateClusterOperationalCommandResponseParams
  , IsMTRRVCOperationalStateClusterOperationalCommandResponseParams(..)
  , initWithResponseValue_error
  , commandResponseState
  , setCommandResponseState
  , commandResponseStateSelector
  , initWithResponseValue_errorSelector
  , setCommandResponseStateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRRVCOperationalStateClusterOperationalCommandResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRRVCOperationalStateClusterOperationalCommandResponseParams mtrrvcOperationalStateClusterOperationalCommandResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrrvcOperationalStateClusterOperationalCommandResponseParams -> responseValue -> error_ -> IO (Id MTRRVCOperationalStateClusterOperationalCommandResponseParams)
initWithResponseValue_error mtrrvcOperationalStateClusterOperationalCommandResponseParams responseValue error_ =
  sendOwnedMessage mtrrvcOperationalStateClusterOperationalCommandResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- commandResponseState@
commandResponseState :: IsMTRRVCOperationalStateClusterOperationalCommandResponseParams mtrrvcOperationalStateClusterOperationalCommandResponseParams => mtrrvcOperationalStateClusterOperationalCommandResponseParams -> IO (Id MTRRVCOperationalStateClusterErrorStateStruct)
commandResponseState mtrrvcOperationalStateClusterOperationalCommandResponseParams =
  sendMessage mtrrvcOperationalStateClusterOperationalCommandResponseParams commandResponseStateSelector

-- | @- setCommandResponseState:@
setCommandResponseState :: (IsMTRRVCOperationalStateClusterOperationalCommandResponseParams mtrrvcOperationalStateClusterOperationalCommandResponseParams, IsMTRRVCOperationalStateClusterErrorStateStruct value) => mtrrvcOperationalStateClusterOperationalCommandResponseParams -> value -> IO ()
setCommandResponseState mtrrvcOperationalStateClusterOperationalCommandResponseParams value =
  sendMessage mtrrvcOperationalStateClusterOperationalCommandResponseParams setCommandResponseStateSelector (toMTRRVCOperationalStateClusterErrorStateStruct value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRRVCOperationalStateClusterOperationalCommandResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @commandResponseState@
commandResponseStateSelector :: Selector '[] (Id MTRRVCOperationalStateClusterErrorStateStruct)
commandResponseStateSelector = mkSelector "commandResponseState"

-- | @Selector@ for @setCommandResponseState:@
setCommandResponseStateSelector :: Selector '[Id MTRRVCOperationalStateClusterErrorStateStruct] ()
setCommandResponseStateSelector = mkSelector "setCommandResponseState:"

