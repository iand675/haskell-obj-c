{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTestClusterClusterTestStructArrayArgumentResponseParams@.
module ObjC.Matter.MTRTestClusterClusterTestStructArrayArgumentResponseParams
  ( MTRTestClusterClusterTestStructArrayArgumentResponseParams
  , IsMTRTestClusterClusterTestStructArrayArgumentResponseParams(..)
  , arg1
  , setArg1
  , arg2
  , setArg2
  , arg3
  , setArg3
  , arg4
  , setArg4
  , arg5
  , setArg5
  , arg6
  , setArg6
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , arg1Selector
  , arg2Selector
  , arg3Selector
  , arg4Selector
  , arg5Selector
  , arg6Selector
  , setArg1Selector
  , setArg2Selector
  , setArg3Selector
  , setArg4Selector
  , setArg5Selector
  , setArg6Selector
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
arg1 :: IsMTRTestClusterClusterTestStructArrayArgumentResponseParams mtrTestClusterClusterTestStructArrayArgumentResponseParams => mtrTestClusterClusterTestStructArrayArgumentResponseParams -> IO (Id NSArray)
arg1 mtrTestClusterClusterTestStructArrayArgumentResponseParams =
  sendMessage mtrTestClusterClusterTestStructArrayArgumentResponseParams arg1Selector

-- | @- setArg1:@
setArg1 :: (IsMTRTestClusterClusterTestStructArrayArgumentResponseParams mtrTestClusterClusterTestStructArrayArgumentResponseParams, IsNSArray value) => mtrTestClusterClusterTestStructArrayArgumentResponseParams -> value -> IO ()
setArg1 mtrTestClusterClusterTestStructArrayArgumentResponseParams value =
  sendMessage mtrTestClusterClusterTestStructArrayArgumentResponseParams setArg1Selector (toNSArray value)

-- | @- arg2@
arg2 :: IsMTRTestClusterClusterTestStructArrayArgumentResponseParams mtrTestClusterClusterTestStructArrayArgumentResponseParams => mtrTestClusterClusterTestStructArrayArgumentResponseParams -> IO (Id NSArray)
arg2 mtrTestClusterClusterTestStructArrayArgumentResponseParams =
  sendMessage mtrTestClusterClusterTestStructArrayArgumentResponseParams arg2Selector

-- | @- setArg2:@
setArg2 :: (IsMTRTestClusterClusterTestStructArrayArgumentResponseParams mtrTestClusterClusterTestStructArrayArgumentResponseParams, IsNSArray value) => mtrTestClusterClusterTestStructArrayArgumentResponseParams -> value -> IO ()
setArg2 mtrTestClusterClusterTestStructArrayArgumentResponseParams value =
  sendMessage mtrTestClusterClusterTestStructArrayArgumentResponseParams setArg2Selector (toNSArray value)

-- | @- arg3@
arg3 :: IsMTRTestClusterClusterTestStructArrayArgumentResponseParams mtrTestClusterClusterTestStructArrayArgumentResponseParams => mtrTestClusterClusterTestStructArrayArgumentResponseParams -> IO (Id NSArray)
arg3 mtrTestClusterClusterTestStructArrayArgumentResponseParams =
  sendMessage mtrTestClusterClusterTestStructArrayArgumentResponseParams arg3Selector

-- | @- setArg3:@
setArg3 :: (IsMTRTestClusterClusterTestStructArrayArgumentResponseParams mtrTestClusterClusterTestStructArrayArgumentResponseParams, IsNSArray value) => mtrTestClusterClusterTestStructArrayArgumentResponseParams -> value -> IO ()
setArg3 mtrTestClusterClusterTestStructArrayArgumentResponseParams value =
  sendMessage mtrTestClusterClusterTestStructArrayArgumentResponseParams setArg3Selector (toNSArray value)

-- | @- arg4@
arg4 :: IsMTRTestClusterClusterTestStructArrayArgumentResponseParams mtrTestClusterClusterTestStructArrayArgumentResponseParams => mtrTestClusterClusterTestStructArrayArgumentResponseParams -> IO (Id NSArray)
arg4 mtrTestClusterClusterTestStructArrayArgumentResponseParams =
  sendMessage mtrTestClusterClusterTestStructArrayArgumentResponseParams arg4Selector

-- | @- setArg4:@
setArg4 :: (IsMTRTestClusterClusterTestStructArrayArgumentResponseParams mtrTestClusterClusterTestStructArrayArgumentResponseParams, IsNSArray value) => mtrTestClusterClusterTestStructArrayArgumentResponseParams -> value -> IO ()
setArg4 mtrTestClusterClusterTestStructArrayArgumentResponseParams value =
  sendMessage mtrTestClusterClusterTestStructArrayArgumentResponseParams setArg4Selector (toNSArray value)

-- | @- arg5@
arg5 :: IsMTRTestClusterClusterTestStructArrayArgumentResponseParams mtrTestClusterClusterTestStructArrayArgumentResponseParams => mtrTestClusterClusterTestStructArrayArgumentResponseParams -> IO (Id NSNumber)
arg5 mtrTestClusterClusterTestStructArrayArgumentResponseParams =
  sendMessage mtrTestClusterClusterTestStructArrayArgumentResponseParams arg5Selector

-- | @- setArg5:@
setArg5 :: (IsMTRTestClusterClusterTestStructArrayArgumentResponseParams mtrTestClusterClusterTestStructArrayArgumentResponseParams, IsNSNumber value) => mtrTestClusterClusterTestStructArrayArgumentResponseParams -> value -> IO ()
setArg5 mtrTestClusterClusterTestStructArrayArgumentResponseParams value =
  sendMessage mtrTestClusterClusterTestStructArrayArgumentResponseParams setArg5Selector (toNSNumber value)

-- | @- arg6@
arg6 :: IsMTRTestClusterClusterTestStructArrayArgumentResponseParams mtrTestClusterClusterTestStructArrayArgumentResponseParams => mtrTestClusterClusterTestStructArrayArgumentResponseParams -> IO (Id NSNumber)
arg6 mtrTestClusterClusterTestStructArrayArgumentResponseParams =
  sendMessage mtrTestClusterClusterTestStructArrayArgumentResponseParams arg6Selector

-- | @- setArg6:@
setArg6 :: (IsMTRTestClusterClusterTestStructArrayArgumentResponseParams mtrTestClusterClusterTestStructArrayArgumentResponseParams, IsNSNumber value) => mtrTestClusterClusterTestStructArrayArgumentResponseParams -> value -> IO ()
setArg6 mtrTestClusterClusterTestStructArrayArgumentResponseParams value =
  sendMessage mtrTestClusterClusterTestStructArrayArgumentResponseParams setArg6Selector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRTestClusterClusterTestStructArrayArgumentResponseParams mtrTestClusterClusterTestStructArrayArgumentResponseParams => mtrTestClusterClusterTestStructArrayArgumentResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrTestClusterClusterTestStructArrayArgumentResponseParams =
  sendMessage mtrTestClusterClusterTestStructArrayArgumentResponseParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRTestClusterClusterTestStructArrayArgumentResponseParams mtrTestClusterClusterTestStructArrayArgumentResponseParams, IsNSNumber value) => mtrTestClusterClusterTestStructArrayArgumentResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrTestClusterClusterTestStructArrayArgumentResponseParams value =
  sendMessage mtrTestClusterClusterTestStructArrayArgumentResponseParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @arg1@
arg1Selector :: Selector '[] (Id NSArray)
arg1Selector = mkSelector "arg1"

-- | @Selector@ for @setArg1:@
setArg1Selector :: Selector '[Id NSArray] ()
setArg1Selector = mkSelector "setArg1:"

-- | @Selector@ for @arg2@
arg2Selector :: Selector '[] (Id NSArray)
arg2Selector = mkSelector "arg2"

-- | @Selector@ for @setArg2:@
setArg2Selector :: Selector '[Id NSArray] ()
setArg2Selector = mkSelector "setArg2:"

-- | @Selector@ for @arg3@
arg3Selector :: Selector '[] (Id NSArray)
arg3Selector = mkSelector "arg3"

-- | @Selector@ for @setArg3:@
setArg3Selector :: Selector '[Id NSArray] ()
setArg3Selector = mkSelector "setArg3:"

-- | @Selector@ for @arg4@
arg4Selector :: Selector '[] (Id NSArray)
arg4Selector = mkSelector "arg4"

-- | @Selector@ for @setArg4:@
setArg4Selector :: Selector '[Id NSArray] ()
setArg4Selector = mkSelector "setArg4:"

-- | @Selector@ for @arg5@
arg5Selector :: Selector '[] (Id NSNumber)
arg5Selector = mkSelector "arg5"

-- | @Selector@ for @setArg5:@
setArg5Selector :: Selector '[Id NSNumber] ()
setArg5Selector = mkSelector "setArg5:"

-- | @Selector@ for @arg6@
arg6Selector :: Selector '[] (Id NSNumber)
arg6Selector = mkSelector "arg6"

-- | @Selector@ for @setArg6:@
setArg6Selector :: Selector '[Id NSNumber] ()
setArg6Selector = mkSelector "setArg6:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector '[] (Id NSNumber)
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector '[Id NSNumber] ()
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

