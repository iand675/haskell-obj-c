{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRLaundryWasherModeClusterChangeToModeResponseParams@.
module ObjC.Matter.MTRLaundryWasherModeClusterChangeToModeResponseParams
  ( MTRLaundryWasherModeClusterChangeToModeResponseParams
  , IsMTRLaundryWasherModeClusterChangeToModeResponseParams(..)
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

-- | Initialize an MTRLaundryWasherModeClusterChangeToModeResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRLaundryWasherModeClusterChangeToModeResponseParams mtrLaundryWasherModeClusterChangeToModeResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrLaundryWasherModeClusterChangeToModeResponseParams -> responseValue -> error_ -> IO (Id MTRLaundryWasherModeClusterChangeToModeResponseParams)
initWithResponseValue_error mtrLaundryWasherModeClusterChangeToModeResponseParams responseValue error_ =
  sendOwnedMessage mtrLaundryWasherModeClusterChangeToModeResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- status@
status :: IsMTRLaundryWasherModeClusterChangeToModeResponseParams mtrLaundryWasherModeClusterChangeToModeResponseParams => mtrLaundryWasherModeClusterChangeToModeResponseParams -> IO (Id NSNumber)
status mtrLaundryWasherModeClusterChangeToModeResponseParams =
  sendMessage mtrLaundryWasherModeClusterChangeToModeResponseParams statusSelector

-- | @- setStatus:@
setStatus :: (IsMTRLaundryWasherModeClusterChangeToModeResponseParams mtrLaundryWasherModeClusterChangeToModeResponseParams, IsNSNumber value) => mtrLaundryWasherModeClusterChangeToModeResponseParams -> value -> IO ()
setStatus mtrLaundryWasherModeClusterChangeToModeResponseParams value =
  sendMessage mtrLaundryWasherModeClusterChangeToModeResponseParams setStatusSelector (toNSNumber value)

-- | @- statusText@
statusText :: IsMTRLaundryWasherModeClusterChangeToModeResponseParams mtrLaundryWasherModeClusterChangeToModeResponseParams => mtrLaundryWasherModeClusterChangeToModeResponseParams -> IO (Id NSString)
statusText mtrLaundryWasherModeClusterChangeToModeResponseParams =
  sendMessage mtrLaundryWasherModeClusterChangeToModeResponseParams statusTextSelector

-- | @- setStatusText:@
setStatusText :: (IsMTRLaundryWasherModeClusterChangeToModeResponseParams mtrLaundryWasherModeClusterChangeToModeResponseParams, IsNSString value) => mtrLaundryWasherModeClusterChangeToModeResponseParams -> value -> IO ()
setStatusText mtrLaundryWasherModeClusterChangeToModeResponseParams value =
  sendMessage mtrLaundryWasherModeClusterChangeToModeResponseParams setStatusTextSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRLaundryWasherModeClusterChangeToModeResponseParams)
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

