{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTestClusterClusterTestEmitTestFabricScopedEventResponseParams@.
module ObjC.Matter.MTRTestClusterClusterTestEmitTestFabricScopedEventResponseParams
  ( MTRTestClusterClusterTestEmitTestFabricScopedEventResponseParams
  , IsMTRTestClusterClusterTestEmitTestFabricScopedEventResponseParams(..)
  , value
  , setValue
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , setTimedInvokeTimeoutMsSelector
  , setValueSelector
  , timedInvokeTimeoutMsSelector
  , valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- value@
value :: IsMTRTestClusterClusterTestEmitTestFabricScopedEventResponseParams mtrTestClusterClusterTestEmitTestFabricScopedEventResponseParams => mtrTestClusterClusterTestEmitTestFabricScopedEventResponseParams -> IO (Id NSNumber)
value mtrTestClusterClusterTestEmitTestFabricScopedEventResponseParams =
  sendMessage mtrTestClusterClusterTestEmitTestFabricScopedEventResponseParams valueSelector

-- | @- setValue:@
setValue :: (IsMTRTestClusterClusterTestEmitTestFabricScopedEventResponseParams mtrTestClusterClusterTestEmitTestFabricScopedEventResponseParams, IsNSNumber value) => mtrTestClusterClusterTestEmitTestFabricScopedEventResponseParams -> value -> IO ()
setValue mtrTestClusterClusterTestEmitTestFabricScopedEventResponseParams value =
  sendMessage mtrTestClusterClusterTestEmitTestFabricScopedEventResponseParams setValueSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRTestClusterClusterTestEmitTestFabricScopedEventResponseParams mtrTestClusterClusterTestEmitTestFabricScopedEventResponseParams => mtrTestClusterClusterTestEmitTestFabricScopedEventResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrTestClusterClusterTestEmitTestFabricScopedEventResponseParams =
  sendMessage mtrTestClusterClusterTestEmitTestFabricScopedEventResponseParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRTestClusterClusterTestEmitTestFabricScopedEventResponseParams mtrTestClusterClusterTestEmitTestFabricScopedEventResponseParams, IsNSNumber value) => mtrTestClusterClusterTestEmitTestFabricScopedEventResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrTestClusterClusterTestEmitTestFabricScopedEventResponseParams value =
  sendMessage mtrTestClusterClusterTestEmitTestFabricScopedEventResponseParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @value@
valueSelector :: Selector '[] (Id NSNumber)
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector '[Id NSNumber] ()
setValueSelector = mkSelector "setValue:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector '[] (Id NSNumber)
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector '[Id NSNumber] ()
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

