{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentControlClusterUpdatePINParams@.
module ObjC.Matter.MTRContentControlClusterUpdatePINParams
  ( MTRContentControlClusterUpdatePINParams
  , IsMTRContentControlClusterUpdatePINParams(..)
  , oldPIN
  , setOldPIN
  , newPIN
  , setNewPIN
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , newPINSelector
  , oldPINSelector
  , serverSideProcessingTimeoutSelector
  , setNewPINSelector
  , setOldPINSelector
  , setServerSideProcessingTimeoutSelector
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

-- | @- oldPIN@
oldPIN :: IsMTRContentControlClusterUpdatePINParams mtrContentControlClusterUpdatePINParams => mtrContentControlClusterUpdatePINParams -> IO (Id NSString)
oldPIN mtrContentControlClusterUpdatePINParams =
  sendMessage mtrContentControlClusterUpdatePINParams oldPINSelector

-- | @- setOldPIN:@
setOldPIN :: (IsMTRContentControlClusterUpdatePINParams mtrContentControlClusterUpdatePINParams, IsNSString value) => mtrContentControlClusterUpdatePINParams -> value -> IO ()
setOldPIN mtrContentControlClusterUpdatePINParams value =
  sendMessage mtrContentControlClusterUpdatePINParams setOldPINSelector (toNSString value)

-- | @- newPIN@
newPIN :: IsMTRContentControlClusterUpdatePINParams mtrContentControlClusterUpdatePINParams => mtrContentControlClusterUpdatePINParams -> IO (Id NSString)
newPIN mtrContentControlClusterUpdatePINParams =
  sendOwnedMessage mtrContentControlClusterUpdatePINParams newPINSelector

-- | @- setNewPIN:@
setNewPIN :: (IsMTRContentControlClusterUpdatePINParams mtrContentControlClusterUpdatePINParams, IsNSString value) => mtrContentControlClusterUpdatePINParams -> value -> IO ()
setNewPIN mtrContentControlClusterUpdatePINParams value =
  sendMessage mtrContentControlClusterUpdatePINParams setNewPINSelector (toNSString value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRContentControlClusterUpdatePINParams mtrContentControlClusterUpdatePINParams => mtrContentControlClusterUpdatePINParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrContentControlClusterUpdatePINParams =
  sendMessage mtrContentControlClusterUpdatePINParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRContentControlClusterUpdatePINParams mtrContentControlClusterUpdatePINParams, IsNSNumber value) => mtrContentControlClusterUpdatePINParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrContentControlClusterUpdatePINParams value =
  sendMessage mtrContentControlClusterUpdatePINParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRContentControlClusterUpdatePINParams mtrContentControlClusterUpdatePINParams => mtrContentControlClusterUpdatePINParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrContentControlClusterUpdatePINParams =
  sendMessage mtrContentControlClusterUpdatePINParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRContentControlClusterUpdatePINParams mtrContentControlClusterUpdatePINParams, IsNSNumber value) => mtrContentControlClusterUpdatePINParams -> value -> IO ()
setServerSideProcessingTimeout mtrContentControlClusterUpdatePINParams value =
  sendMessage mtrContentControlClusterUpdatePINParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @oldPIN@
oldPINSelector :: Selector '[] (Id NSString)
oldPINSelector = mkSelector "oldPIN"

-- | @Selector@ for @setOldPIN:@
setOldPINSelector :: Selector '[Id NSString] ()
setOldPINSelector = mkSelector "setOldPIN:"

-- | @Selector@ for @newPIN@
newPINSelector :: Selector '[] (Id NSString)
newPINSelector = mkSelector "newPIN"

-- | @Selector@ for @setNewPIN:@
setNewPINSelector :: Selector '[Id NSString] ()
setNewPINSelector = mkSelector "setNewPIN:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector '[] (Id NSNumber)
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector '[Id NSNumber] ()
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

-- | @Selector@ for @serverSideProcessingTimeout@
serverSideProcessingTimeoutSelector :: Selector '[] (Id NSNumber)
serverSideProcessingTimeoutSelector = mkSelector "serverSideProcessingTimeout"

-- | @Selector@ for @setServerSideProcessingTimeout:@
setServerSideProcessingTimeoutSelector :: Selector '[Id NSNumber] ()
setServerSideProcessingTimeoutSelector = mkSelector "setServerSideProcessingTimeout:"

