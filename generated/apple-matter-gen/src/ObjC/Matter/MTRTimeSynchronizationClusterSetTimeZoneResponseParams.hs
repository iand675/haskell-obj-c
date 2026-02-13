{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTimeSynchronizationClusterSetTimeZoneResponseParams@.
module ObjC.Matter.MTRTimeSynchronizationClusterSetTimeZoneResponseParams
  ( MTRTimeSynchronizationClusterSetTimeZoneResponseParams
  , IsMTRTimeSynchronizationClusterSetTimeZoneResponseParams(..)
  , initWithResponseValue_error
  , dstOffsetRequired
  , setDstOffsetRequired
  , dstOffsetRequiredSelector
  , initWithResponseValue_errorSelector
  , setDstOffsetRequiredSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRTimeSynchronizationClusterSetTimeZoneResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRTimeSynchronizationClusterSetTimeZoneResponseParams mtrTimeSynchronizationClusterSetTimeZoneResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrTimeSynchronizationClusterSetTimeZoneResponseParams -> responseValue -> error_ -> IO (Id MTRTimeSynchronizationClusterSetTimeZoneResponseParams)
initWithResponseValue_error mtrTimeSynchronizationClusterSetTimeZoneResponseParams responseValue error_ =
  sendOwnedMessage mtrTimeSynchronizationClusterSetTimeZoneResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- dstOffsetRequired@
dstOffsetRequired :: IsMTRTimeSynchronizationClusterSetTimeZoneResponseParams mtrTimeSynchronizationClusterSetTimeZoneResponseParams => mtrTimeSynchronizationClusterSetTimeZoneResponseParams -> IO (Id NSNumber)
dstOffsetRequired mtrTimeSynchronizationClusterSetTimeZoneResponseParams =
  sendMessage mtrTimeSynchronizationClusterSetTimeZoneResponseParams dstOffsetRequiredSelector

-- | @- setDstOffsetRequired:@
setDstOffsetRequired :: (IsMTRTimeSynchronizationClusterSetTimeZoneResponseParams mtrTimeSynchronizationClusterSetTimeZoneResponseParams, IsNSNumber value) => mtrTimeSynchronizationClusterSetTimeZoneResponseParams -> value -> IO ()
setDstOffsetRequired mtrTimeSynchronizationClusterSetTimeZoneResponseParams value =
  sendMessage mtrTimeSynchronizationClusterSetTimeZoneResponseParams setDstOffsetRequiredSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRTimeSynchronizationClusterSetTimeZoneResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @dstOffsetRequired@
dstOffsetRequiredSelector :: Selector '[] (Id NSNumber)
dstOffsetRequiredSelector = mkSelector "dstOffsetRequired"

-- | @Selector@ for @setDstOffsetRequired:@
setDstOffsetRequiredSelector :: Selector '[Id NSNumber] ()
setDstOffsetRequiredSelector = mkSelector "setDstOffsetRequired:"

