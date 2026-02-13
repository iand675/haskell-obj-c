{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTestClusterClusterTestNullableOptionalResponseParams@.
module ObjC.Matter.MTRTestClusterClusterTestNullableOptionalResponseParams
  ( MTRTestClusterClusterTestNullableOptionalResponseParams
  , IsMTRTestClusterClusterTestNullableOptionalResponseParams(..)
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

-- | @- wasPresent@
wasPresent :: IsMTRTestClusterClusterTestNullableOptionalResponseParams mtrTestClusterClusterTestNullableOptionalResponseParams => mtrTestClusterClusterTestNullableOptionalResponseParams -> IO (Id NSNumber)
wasPresent mtrTestClusterClusterTestNullableOptionalResponseParams =
  sendMessage mtrTestClusterClusterTestNullableOptionalResponseParams wasPresentSelector

-- | @- setWasPresent:@
setWasPresent :: (IsMTRTestClusterClusterTestNullableOptionalResponseParams mtrTestClusterClusterTestNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestNullableOptionalResponseParams -> value -> IO ()
setWasPresent mtrTestClusterClusterTestNullableOptionalResponseParams value =
  sendMessage mtrTestClusterClusterTestNullableOptionalResponseParams setWasPresentSelector (toNSNumber value)

-- | @- wasNull@
wasNull :: IsMTRTestClusterClusterTestNullableOptionalResponseParams mtrTestClusterClusterTestNullableOptionalResponseParams => mtrTestClusterClusterTestNullableOptionalResponseParams -> IO (Id NSNumber)
wasNull mtrTestClusterClusterTestNullableOptionalResponseParams =
  sendMessage mtrTestClusterClusterTestNullableOptionalResponseParams wasNullSelector

-- | @- setWasNull:@
setWasNull :: (IsMTRTestClusterClusterTestNullableOptionalResponseParams mtrTestClusterClusterTestNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestNullableOptionalResponseParams -> value -> IO ()
setWasNull mtrTestClusterClusterTestNullableOptionalResponseParams value =
  sendMessage mtrTestClusterClusterTestNullableOptionalResponseParams setWasNullSelector (toNSNumber value)

-- | @- value@
value :: IsMTRTestClusterClusterTestNullableOptionalResponseParams mtrTestClusterClusterTestNullableOptionalResponseParams => mtrTestClusterClusterTestNullableOptionalResponseParams -> IO (Id NSNumber)
value mtrTestClusterClusterTestNullableOptionalResponseParams =
  sendMessage mtrTestClusterClusterTestNullableOptionalResponseParams valueSelector

-- | @- setValue:@
setValue :: (IsMTRTestClusterClusterTestNullableOptionalResponseParams mtrTestClusterClusterTestNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestNullableOptionalResponseParams -> value -> IO ()
setValue mtrTestClusterClusterTestNullableOptionalResponseParams value =
  sendMessage mtrTestClusterClusterTestNullableOptionalResponseParams setValueSelector (toNSNumber value)

-- | @- originalValue@
originalValue :: IsMTRTestClusterClusterTestNullableOptionalResponseParams mtrTestClusterClusterTestNullableOptionalResponseParams => mtrTestClusterClusterTestNullableOptionalResponseParams -> IO (Id NSNumber)
originalValue mtrTestClusterClusterTestNullableOptionalResponseParams =
  sendMessage mtrTestClusterClusterTestNullableOptionalResponseParams originalValueSelector

-- | @- setOriginalValue:@
setOriginalValue :: (IsMTRTestClusterClusterTestNullableOptionalResponseParams mtrTestClusterClusterTestNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestNullableOptionalResponseParams -> value -> IO ()
setOriginalValue mtrTestClusterClusterTestNullableOptionalResponseParams value =
  sendMessage mtrTestClusterClusterTestNullableOptionalResponseParams setOriginalValueSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRTestClusterClusterTestNullableOptionalResponseParams mtrTestClusterClusterTestNullableOptionalResponseParams => mtrTestClusterClusterTestNullableOptionalResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrTestClusterClusterTestNullableOptionalResponseParams =
  sendMessage mtrTestClusterClusterTestNullableOptionalResponseParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRTestClusterClusterTestNullableOptionalResponseParams mtrTestClusterClusterTestNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestNullableOptionalResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrTestClusterClusterTestNullableOptionalResponseParams value =
  sendMessage mtrTestClusterClusterTestNullableOptionalResponseParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

