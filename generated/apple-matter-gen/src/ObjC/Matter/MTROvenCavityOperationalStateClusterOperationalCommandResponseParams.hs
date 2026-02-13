{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROvenCavityOperationalStateClusterOperationalCommandResponseParams@.
module ObjC.Matter.MTROvenCavityOperationalStateClusterOperationalCommandResponseParams
  ( MTROvenCavityOperationalStateClusterOperationalCommandResponseParams
  , IsMTROvenCavityOperationalStateClusterOperationalCommandResponseParams(..)
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

-- | Initialize an MTROvenCavityOperationalStateClusterOperationalCommandResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTROvenCavityOperationalStateClusterOperationalCommandResponseParams mtrOvenCavityOperationalStateClusterOperationalCommandResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrOvenCavityOperationalStateClusterOperationalCommandResponseParams -> responseValue -> error_ -> IO (Id MTROvenCavityOperationalStateClusterOperationalCommandResponseParams)
initWithResponseValue_error mtrOvenCavityOperationalStateClusterOperationalCommandResponseParams responseValue error_ =
  sendOwnedMessage mtrOvenCavityOperationalStateClusterOperationalCommandResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- commandResponseState@
commandResponseState :: IsMTROvenCavityOperationalStateClusterOperationalCommandResponseParams mtrOvenCavityOperationalStateClusterOperationalCommandResponseParams => mtrOvenCavityOperationalStateClusterOperationalCommandResponseParams -> IO (Id MTROvenCavityOperationalStateClusterErrorStateStruct)
commandResponseState mtrOvenCavityOperationalStateClusterOperationalCommandResponseParams =
  sendMessage mtrOvenCavityOperationalStateClusterOperationalCommandResponseParams commandResponseStateSelector

-- | @- setCommandResponseState:@
setCommandResponseState :: (IsMTROvenCavityOperationalStateClusterOperationalCommandResponseParams mtrOvenCavityOperationalStateClusterOperationalCommandResponseParams, IsMTROvenCavityOperationalStateClusterErrorStateStruct value) => mtrOvenCavityOperationalStateClusterOperationalCommandResponseParams -> value -> IO ()
setCommandResponseState mtrOvenCavityOperationalStateClusterOperationalCommandResponseParams value =
  sendMessage mtrOvenCavityOperationalStateClusterOperationalCommandResponseParams setCommandResponseStateSelector (toMTROvenCavityOperationalStateClusterErrorStateStruct value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTROvenCavityOperationalStateClusterOperationalCommandResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @commandResponseState@
commandResponseStateSelector :: Selector '[] (Id MTROvenCavityOperationalStateClusterErrorStateStruct)
commandResponseStateSelector = mkSelector "commandResponseState"

-- | @Selector@ for @setCommandResponseState:@
setCommandResponseStateSelector :: Selector '[Id MTROvenCavityOperationalStateClusterErrorStateStruct] ()
setCommandResponseStateSelector = mkSelector "setCommandResponseState:"

