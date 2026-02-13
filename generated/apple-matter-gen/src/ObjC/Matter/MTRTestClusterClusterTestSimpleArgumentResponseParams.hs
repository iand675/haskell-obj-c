{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTestClusterClusterTestSimpleArgumentResponseParams@.
module ObjC.Matter.MTRTestClusterClusterTestSimpleArgumentResponseParams
  ( MTRTestClusterClusterTestSimpleArgumentResponseParams
  , IsMTRTestClusterClusterTestSimpleArgumentResponseParams(..)
  , returnValue
  , setReturnValue
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , returnValueSelector
  , setReturnValueSelector
  , setTimedInvokeTimeoutMsSelector
  , timedInvokeTimeoutMsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- returnValue@
returnValue :: IsMTRTestClusterClusterTestSimpleArgumentResponseParams mtrTestClusterClusterTestSimpleArgumentResponseParams => mtrTestClusterClusterTestSimpleArgumentResponseParams -> IO (Id NSNumber)
returnValue mtrTestClusterClusterTestSimpleArgumentResponseParams =
  sendMessage mtrTestClusterClusterTestSimpleArgumentResponseParams returnValueSelector

-- | @- setReturnValue:@
setReturnValue :: (IsMTRTestClusterClusterTestSimpleArgumentResponseParams mtrTestClusterClusterTestSimpleArgumentResponseParams, IsNSNumber value) => mtrTestClusterClusterTestSimpleArgumentResponseParams -> value -> IO ()
setReturnValue mtrTestClusterClusterTestSimpleArgumentResponseParams value =
  sendMessage mtrTestClusterClusterTestSimpleArgumentResponseParams setReturnValueSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRTestClusterClusterTestSimpleArgumentResponseParams mtrTestClusterClusterTestSimpleArgumentResponseParams => mtrTestClusterClusterTestSimpleArgumentResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrTestClusterClusterTestSimpleArgumentResponseParams =
  sendMessage mtrTestClusterClusterTestSimpleArgumentResponseParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRTestClusterClusterTestSimpleArgumentResponseParams mtrTestClusterClusterTestSimpleArgumentResponseParams, IsNSNumber value) => mtrTestClusterClusterTestSimpleArgumentResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrTestClusterClusterTestSimpleArgumentResponseParams value =
  sendMessage mtrTestClusterClusterTestSimpleArgumentResponseParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @returnValue@
returnValueSelector :: Selector '[] (Id NSNumber)
returnValueSelector = mkSelector "returnValue"

-- | @Selector@ for @setReturnValue:@
setReturnValueSelector :: Selector '[Id NSNumber] ()
setReturnValueSelector = mkSelector "setReturnValue:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector '[] (Id NSNumber)
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector '[Id NSNumber] ()
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

