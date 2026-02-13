{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRSampleMEIClusterAddArgumentsResponseParams@.
module ObjC.Matter.MTRSampleMEIClusterAddArgumentsResponseParams
  ( MTRSampleMEIClusterAddArgumentsResponseParams
  , IsMTRSampleMEIClusterAddArgumentsResponseParams(..)
  , initWithResponseValue_error
  , returnValue
  , setReturnValue
  , initWithResponseValue_errorSelector
  , returnValueSelector
  , setReturnValueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRSampleMEIClusterAddArgumentsResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRSampleMEIClusterAddArgumentsResponseParams mtrSampleMEIClusterAddArgumentsResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrSampleMEIClusterAddArgumentsResponseParams -> responseValue -> error_ -> IO (Id MTRSampleMEIClusterAddArgumentsResponseParams)
initWithResponseValue_error mtrSampleMEIClusterAddArgumentsResponseParams responseValue error_ =
  sendOwnedMessage mtrSampleMEIClusterAddArgumentsResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- returnValue@
returnValue :: IsMTRSampleMEIClusterAddArgumentsResponseParams mtrSampleMEIClusterAddArgumentsResponseParams => mtrSampleMEIClusterAddArgumentsResponseParams -> IO (Id NSNumber)
returnValue mtrSampleMEIClusterAddArgumentsResponseParams =
  sendMessage mtrSampleMEIClusterAddArgumentsResponseParams returnValueSelector

-- | @- setReturnValue:@
setReturnValue :: (IsMTRSampleMEIClusterAddArgumentsResponseParams mtrSampleMEIClusterAddArgumentsResponseParams, IsNSNumber value) => mtrSampleMEIClusterAddArgumentsResponseParams -> value -> IO ()
setReturnValue mtrSampleMEIClusterAddArgumentsResponseParams value =
  sendMessage mtrSampleMEIClusterAddArgumentsResponseParams setReturnValueSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRSampleMEIClusterAddArgumentsResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @returnValue@
returnValueSelector :: Selector '[] (Id NSNumber)
returnValueSelector = mkSelector "returnValue"

-- | @Selector@ for @setReturnValue:@
setReturnValueSelector :: Selector '[Id NSNumber] ()
setReturnValueSelector = mkSelector "setReturnValue:"

