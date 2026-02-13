{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROperationalStateClusterOperationalCommandResponseParams@.
module ObjC.Matter.MTROperationalStateClusterOperationalCommandResponseParams
  ( MTROperationalStateClusterOperationalCommandResponseParams
  , IsMTROperationalStateClusterOperationalCommandResponseParams(..)
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

-- | Initialize an MTROperationalStateClusterOperationalCommandResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTROperationalStateClusterOperationalCommandResponseParams mtrOperationalStateClusterOperationalCommandResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrOperationalStateClusterOperationalCommandResponseParams -> responseValue -> error_ -> IO (Id MTROperationalStateClusterOperationalCommandResponseParams)
initWithResponseValue_error mtrOperationalStateClusterOperationalCommandResponseParams responseValue error_ =
  sendOwnedMessage mtrOperationalStateClusterOperationalCommandResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- commandResponseState@
commandResponseState :: IsMTROperationalStateClusterOperationalCommandResponseParams mtrOperationalStateClusterOperationalCommandResponseParams => mtrOperationalStateClusterOperationalCommandResponseParams -> IO (Id MTROperationalStateClusterErrorStateStruct)
commandResponseState mtrOperationalStateClusterOperationalCommandResponseParams =
  sendMessage mtrOperationalStateClusterOperationalCommandResponseParams commandResponseStateSelector

-- | @- setCommandResponseState:@
setCommandResponseState :: (IsMTROperationalStateClusterOperationalCommandResponseParams mtrOperationalStateClusterOperationalCommandResponseParams, IsMTROperationalStateClusterErrorStateStruct value) => mtrOperationalStateClusterOperationalCommandResponseParams -> value -> IO ()
setCommandResponseState mtrOperationalStateClusterOperationalCommandResponseParams value =
  sendMessage mtrOperationalStateClusterOperationalCommandResponseParams setCommandResponseStateSelector (toMTROperationalStateClusterErrorStateStruct value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTROperationalStateClusterOperationalCommandResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @commandResponseState@
commandResponseStateSelector :: Selector '[] (Id MTROperationalStateClusterErrorStateStruct)
commandResponseStateSelector = mkSelector "commandResponseState"

-- | @Selector@ for @setCommandResponseState:@
setCommandResponseStateSelector :: Selector '[Id MTROperationalStateClusterErrorStateStruct] ()
setCommandResponseStateSelector = mkSelector "setCommandResponseState:"

