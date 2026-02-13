{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterTestNullableOptionalResponseParams@.
module ObjC.Matter.MTRUnitTestingClusterTestNullableOptionalResponseParams
  ( MTRUnitTestingClusterTestNullableOptionalResponseParams
  , IsMTRUnitTestingClusterTestNullableOptionalResponseParams(..)
  , initWithResponseValue_error
  , wasPresent
  , setWasPresent
  , wasNull
  , setWasNull
  , value
  , setValue
  , originalValue
  , setOriginalValue
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , initWithResponseValue_errorSelector
  , originalValueSelector
  , setOriginalValueSelector
  , setTimedInvokeTimeoutMsSelector
  , setValueSelector
  , setWasNullSelector
  , setWasPresentSelector
  , timedInvokeTimeoutMsSelector
  , valueSelector
  , wasNullSelector
  , wasPresentSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRUnitTestingClusterTestNullableOptionalResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRUnitTestingClusterTestNullableOptionalResponseParams mtrUnitTestingClusterTestNullableOptionalResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrUnitTestingClusterTestNullableOptionalResponseParams -> responseValue -> error_ -> IO (Id MTRUnitTestingClusterTestNullableOptionalResponseParams)
initWithResponseValue_error mtrUnitTestingClusterTestNullableOptionalResponseParams responseValue error_ =
  sendOwnedMessage mtrUnitTestingClusterTestNullableOptionalResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- wasPresent@
wasPresent :: IsMTRUnitTestingClusterTestNullableOptionalResponseParams mtrUnitTestingClusterTestNullableOptionalResponseParams => mtrUnitTestingClusterTestNullableOptionalResponseParams -> IO (Id NSNumber)
wasPresent mtrUnitTestingClusterTestNullableOptionalResponseParams =
  sendMessage mtrUnitTestingClusterTestNullableOptionalResponseParams wasPresentSelector

-- | @- setWasPresent:@
setWasPresent :: (IsMTRUnitTestingClusterTestNullableOptionalResponseParams mtrUnitTestingClusterTestNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestNullableOptionalResponseParams -> value -> IO ()
setWasPresent mtrUnitTestingClusterTestNullableOptionalResponseParams value =
  sendMessage mtrUnitTestingClusterTestNullableOptionalResponseParams setWasPresentSelector (toNSNumber value)

-- | @- wasNull@
wasNull :: IsMTRUnitTestingClusterTestNullableOptionalResponseParams mtrUnitTestingClusterTestNullableOptionalResponseParams => mtrUnitTestingClusterTestNullableOptionalResponseParams -> IO (Id NSNumber)
wasNull mtrUnitTestingClusterTestNullableOptionalResponseParams =
  sendMessage mtrUnitTestingClusterTestNullableOptionalResponseParams wasNullSelector

-- | @- setWasNull:@
setWasNull :: (IsMTRUnitTestingClusterTestNullableOptionalResponseParams mtrUnitTestingClusterTestNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestNullableOptionalResponseParams -> value -> IO ()
setWasNull mtrUnitTestingClusterTestNullableOptionalResponseParams value =
  sendMessage mtrUnitTestingClusterTestNullableOptionalResponseParams setWasNullSelector (toNSNumber value)

-- | @- value@
value :: IsMTRUnitTestingClusterTestNullableOptionalResponseParams mtrUnitTestingClusterTestNullableOptionalResponseParams => mtrUnitTestingClusterTestNullableOptionalResponseParams -> IO (Id NSNumber)
value mtrUnitTestingClusterTestNullableOptionalResponseParams =
  sendMessage mtrUnitTestingClusterTestNullableOptionalResponseParams valueSelector

-- | @- setValue:@
setValue :: (IsMTRUnitTestingClusterTestNullableOptionalResponseParams mtrUnitTestingClusterTestNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestNullableOptionalResponseParams -> value -> IO ()
setValue mtrUnitTestingClusterTestNullableOptionalResponseParams value =
  sendMessage mtrUnitTestingClusterTestNullableOptionalResponseParams setValueSelector (toNSNumber value)

-- | @- originalValue@
originalValue :: IsMTRUnitTestingClusterTestNullableOptionalResponseParams mtrUnitTestingClusterTestNullableOptionalResponseParams => mtrUnitTestingClusterTestNullableOptionalResponseParams -> IO (Id NSNumber)
originalValue mtrUnitTestingClusterTestNullableOptionalResponseParams =
  sendMessage mtrUnitTestingClusterTestNullableOptionalResponseParams originalValueSelector

-- | @- setOriginalValue:@
setOriginalValue :: (IsMTRUnitTestingClusterTestNullableOptionalResponseParams mtrUnitTestingClusterTestNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestNullableOptionalResponseParams -> value -> IO ()
setOriginalValue mtrUnitTestingClusterTestNullableOptionalResponseParams value =
  sendMessage mtrUnitTestingClusterTestNullableOptionalResponseParams setOriginalValueSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRUnitTestingClusterTestNullableOptionalResponseParams mtrUnitTestingClusterTestNullableOptionalResponseParams => mtrUnitTestingClusterTestNullableOptionalResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrUnitTestingClusterTestNullableOptionalResponseParams =
  sendMessage mtrUnitTestingClusterTestNullableOptionalResponseParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRUnitTestingClusterTestNullableOptionalResponseParams mtrUnitTestingClusterTestNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestNullableOptionalResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrUnitTestingClusterTestNullableOptionalResponseParams value =
  sendMessage mtrUnitTestingClusterTestNullableOptionalResponseParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRUnitTestingClusterTestNullableOptionalResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @wasPresent@
wasPresentSelector :: Selector '[] (Id NSNumber)
wasPresentSelector = mkSelector "wasPresent"

-- | @Selector@ for @setWasPresent:@
setWasPresentSelector :: Selector '[Id NSNumber] ()
setWasPresentSelector = mkSelector "setWasPresent:"

-- | @Selector@ for @wasNull@
wasNullSelector :: Selector '[] (Id NSNumber)
wasNullSelector = mkSelector "wasNull"

-- | @Selector@ for @setWasNull:@
setWasNullSelector :: Selector '[Id NSNumber] ()
setWasNullSelector = mkSelector "setWasNull:"

-- | @Selector@ for @value@
valueSelector :: Selector '[] (Id NSNumber)
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector '[Id NSNumber] ()
setValueSelector = mkSelector "setValue:"

-- | @Selector@ for @originalValue@
originalValueSelector :: Selector '[] (Id NSNumber)
originalValueSelector = mkSelector "originalValue"

-- | @Selector@ for @setOriginalValue:@
setOriginalValueSelector :: Selector '[Id NSNumber] ()
setOriginalValueSelector = mkSelector "setOriginalValue:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector '[] (Id NSNumber)
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector '[Id NSNumber] ()
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

