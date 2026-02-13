{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTestClusterClusterTestListInt8UReverseResponseParams@.
module ObjC.Matter.MTRTestClusterClusterTestListInt8UReverseResponseParams
  ( MTRTestClusterClusterTestListInt8UReverseResponseParams
  , IsMTRTestClusterClusterTestListInt8UReverseResponseParams(..)
  , arg1
  , setArg1
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , arg1Selector
  , setArg1Selector
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

-- | @- arg1@
arg1 :: IsMTRTestClusterClusterTestListInt8UReverseResponseParams mtrTestClusterClusterTestListInt8UReverseResponseParams => mtrTestClusterClusterTestListInt8UReverseResponseParams -> IO (Id NSArray)
arg1 mtrTestClusterClusterTestListInt8UReverseResponseParams =
  sendMessage mtrTestClusterClusterTestListInt8UReverseResponseParams arg1Selector

-- | @- setArg1:@
setArg1 :: (IsMTRTestClusterClusterTestListInt8UReverseResponseParams mtrTestClusterClusterTestListInt8UReverseResponseParams, IsNSArray value) => mtrTestClusterClusterTestListInt8UReverseResponseParams -> value -> IO ()
setArg1 mtrTestClusterClusterTestListInt8UReverseResponseParams value =
  sendMessage mtrTestClusterClusterTestListInt8UReverseResponseParams setArg1Selector (toNSArray value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRTestClusterClusterTestListInt8UReverseResponseParams mtrTestClusterClusterTestListInt8UReverseResponseParams => mtrTestClusterClusterTestListInt8UReverseResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrTestClusterClusterTestListInt8UReverseResponseParams =
  sendMessage mtrTestClusterClusterTestListInt8UReverseResponseParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRTestClusterClusterTestListInt8UReverseResponseParams mtrTestClusterClusterTestListInt8UReverseResponseParams, IsNSNumber value) => mtrTestClusterClusterTestListInt8UReverseResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrTestClusterClusterTestListInt8UReverseResponseParams value =
  sendMessage mtrTestClusterClusterTestListInt8UReverseResponseParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @arg1@
arg1Selector :: Selector '[] (Id NSArray)
arg1Selector = mkSelector "arg1"

-- | @Selector@ for @setArg1:@
setArg1Selector :: Selector '[Id NSArray] ()
setArg1Selector = mkSelector "setArg1:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector '[] (Id NSNumber)
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector '[Id NSNumber] ()
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

