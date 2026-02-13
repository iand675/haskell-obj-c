{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGroupcastClusterUpdateGroupKeyParams@.
module ObjC.Matter.MTRGroupcastClusterUpdateGroupKeyParams
  ( MTRGroupcastClusterUpdateGroupKeyParams
  , IsMTRGroupcastClusterUpdateGroupKeyParams(..)
  , groupID
  , setGroupID
  , keyID
  , setKeyID
  , key
  , setKey
  , gracePeriod
  , setGracePeriod
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , gracePeriodSelector
  , groupIDSelector
  , keyIDSelector
  , keySelector
  , serverSideProcessingTimeoutSelector
  , setGracePeriodSelector
  , setGroupIDSelector
  , setKeyIDSelector
  , setKeySelector
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

-- | @- groupID@
groupID :: IsMTRGroupcastClusterUpdateGroupKeyParams mtrGroupcastClusterUpdateGroupKeyParams => mtrGroupcastClusterUpdateGroupKeyParams -> IO (Id NSNumber)
groupID mtrGroupcastClusterUpdateGroupKeyParams =
  sendMessage mtrGroupcastClusterUpdateGroupKeyParams groupIDSelector

-- | @- setGroupID:@
setGroupID :: (IsMTRGroupcastClusterUpdateGroupKeyParams mtrGroupcastClusterUpdateGroupKeyParams, IsNSNumber value) => mtrGroupcastClusterUpdateGroupKeyParams -> value -> IO ()
setGroupID mtrGroupcastClusterUpdateGroupKeyParams value =
  sendMessage mtrGroupcastClusterUpdateGroupKeyParams setGroupIDSelector (toNSNumber value)

-- | @- keyID@
keyID :: IsMTRGroupcastClusterUpdateGroupKeyParams mtrGroupcastClusterUpdateGroupKeyParams => mtrGroupcastClusterUpdateGroupKeyParams -> IO (Id NSNumber)
keyID mtrGroupcastClusterUpdateGroupKeyParams =
  sendMessage mtrGroupcastClusterUpdateGroupKeyParams keyIDSelector

-- | @- setKeyID:@
setKeyID :: (IsMTRGroupcastClusterUpdateGroupKeyParams mtrGroupcastClusterUpdateGroupKeyParams, IsNSNumber value) => mtrGroupcastClusterUpdateGroupKeyParams -> value -> IO ()
setKeyID mtrGroupcastClusterUpdateGroupKeyParams value =
  sendMessage mtrGroupcastClusterUpdateGroupKeyParams setKeyIDSelector (toNSNumber value)

-- | @- key@
key :: IsMTRGroupcastClusterUpdateGroupKeyParams mtrGroupcastClusterUpdateGroupKeyParams => mtrGroupcastClusterUpdateGroupKeyParams -> IO (Id NSData)
key mtrGroupcastClusterUpdateGroupKeyParams =
  sendMessage mtrGroupcastClusterUpdateGroupKeyParams keySelector

-- | @- setKey:@
setKey :: (IsMTRGroupcastClusterUpdateGroupKeyParams mtrGroupcastClusterUpdateGroupKeyParams, IsNSData value) => mtrGroupcastClusterUpdateGroupKeyParams -> value -> IO ()
setKey mtrGroupcastClusterUpdateGroupKeyParams value =
  sendMessage mtrGroupcastClusterUpdateGroupKeyParams setKeySelector (toNSData value)

-- | @- gracePeriod@
gracePeriod :: IsMTRGroupcastClusterUpdateGroupKeyParams mtrGroupcastClusterUpdateGroupKeyParams => mtrGroupcastClusterUpdateGroupKeyParams -> IO (Id NSNumber)
gracePeriod mtrGroupcastClusterUpdateGroupKeyParams =
  sendMessage mtrGroupcastClusterUpdateGroupKeyParams gracePeriodSelector

-- | @- setGracePeriod:@
setGracePeriod :: (IsMTRGroupcastClusterUpdateGroupKeyParams mtrGroupcastClusterUpdateGroupKeyParams, IsNSNumber value) => mtrGroupcastClusterUpdateGroupKeyParams -> value -> IO ()
setGracePeriod mtrGroupcastClusterUpdateGroupKeyParams value =
  sendMessage mtrGroupcastClusterUpdateGroupKeyParams setGracePeriodSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRGroupcastClusterUpdateGroupKeyParams mtrGroupcastClusterUpdateGroupKeyParams => mtrGroupcastClusterUpdateGroupKeyParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrGroupcastClusterUpdateGroupKeyParams =
  sendMessage mtrGroupcastClusterUpdateGroupKeyParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRGroupcastClusterUpdateGroupKeyParams mtrGroupcastClusterUpdateGroupKeyParams, IsNSNumber value) => mtrGroupcastClusterUpdateGroupKeyParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrGroupcastClusterUpdateGroupKeyParams value =
  sendMessage mtrGroupcastClusterUpdateGroupKeyParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRGroupcastClusterUpdateGroupKeyParams mtrGroupcastClusterUpdateGroupKeyParams => mtrGroupcastClusterUpdateGroupKeyParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrGroupcastClusterUpdateGroupKeyParams =
  sendMessage mtrGroupcastClusterUpdateGroupKeyParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRGroupcastClusterUpdateGroupKeyParams mtrGroupcastClusterUpdateGroupKeyParams, IsNSNumber value) => mtrGroupcastClusterUpdateGroupKeyParams -> value -> IO ()
setServerSideProcessingTimeout mtrGroupcastClusterUpdateGroupKeyParams value =
  sendMessage mtrGroupcastClusterUpdateGroupKeyParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @groupID@
groupIDSelector :: Selector '[] (Id NSNumber)
groupIDSelector = mkSelector "groupID"

-- | @Selector@ for @setGroupID:@
setGroupIDSelector :: Selector '[Id NSNumber] ()
setGroupIDSelector = mkSelector "setGroupID:"

-- | @Selector@ for @keyID@
keyIDSelector :: Selector '[] (Id NSNumber)
keyIDSelector = mkSelector "keyID"

-- | @Selector@ for @setKeyID:@
setKeyIDSelector :: Selector '[Id NSNumber] ()
setKeyIDSelector = mkSelector "setKeyID:"

-- | @Selector@ for @key@
keySelector :: Selector '[] (Id NSData)
keySelector = mkSelector "key"

-- | @Selector@ for @setKey:@
setKeySelector :: Selector '[Id NSData] ()
setKeySelector = mkSelector "setKey:"

-- | @Selector@ for @gracePeriod@
gracePeriodSelector :: Selector '[] (Id NSNumber)
gracePeriodSelector = mkSelector "gracePeriod"

-- | @Selector@ for @setGracePeriod:@
setGracePeriodSelector :: Selector '[Id NSNumber] ()
setGracePeriodSelector = mkSelector "setGracePeriod:"

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

