{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTestClusterClusterTestSpecificResponseParams@.
module ObjC.Matter.MTRTestClusterClusterTestSpecificResponseParams
  ( MTRTestClusterClusterTestSpecificResponseParams
  , IsMTRTestClusterClusterTestSpecificResponseParams(..)
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
returnValue :: IsMTRTestClusterClusterTestSpecificResponseParams mtrTestClusterClusterTestSpecificResponseParams => mtrTestClusterClusterTestSpecificResponseParams -> IO (Id NSNumber)
returnValue mtrTestClusterClusterTestSpecificResponseParams =
  sendMessage mtrTestClusterClusterTestSpecificResponseParams returnValueSelector

-- | @- setReturnValue:@
setReturnValue :: (IsMTRTestClusterClusterTestSpecificResponseParams mtrTestClusterClusterTestSpecificResponseParams, IsNSNumber value) => mtrTestClusterClusterTestSpecificResponseParams -> value -> IO ()
setReturnValue mtrTestClusterClusterTestSpecificResponseParams value =
  sendMessage mtrTestClusterClusterTestSpecificResponseParams setReturnValueSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRTestClusterClusterTestSpecificResponseParams mtrTestClusterClusterTestSpecificResponseParams => mtrTestClusterClusterTestSpecificResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrTestClusterClusterTestSpecificResponseParams =
  sendMessage mtrTestClusterClusterTestSpecificResponseParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRTestClusterClusterTestSpecificResponseParams mtrTestClusterClusterTestSpecificResponseParams, IsNSNumber value) => mtrTestClusterClusterTestSpecificResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrTestClusterClusterTestSpecificResponseParams value =
  sendMessage mtrTestClusterClusterTestSpecificResponseParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

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

