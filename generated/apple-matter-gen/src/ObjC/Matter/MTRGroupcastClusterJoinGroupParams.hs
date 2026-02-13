{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGroupcastClusterJoinGroupParams@.
module ObjC.Matter.MTRGroupcastClusterJoinGroupParams
  ( MTRGroupcastClusterJoinGroupParams
  , IsMTRGroupcastClusterJoinGroupParams(..)
  , groupID
  , setGroupID
  , endpoints
  , setEndpoints
  , keyID
  , setKeyID
  , key
  , setKey
  , gracePeriod
  , setGracePeriod
  , useAuxiliaryACL
  , setUseAuxiliaryACL
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , endpointsSelector
  , gracePeriodSelector
  , groupIDSelector
  , keyIDSelector
  , keySelector
  , serverSideProcessingTimeoutSelector
  , setEndpointsSelector
  , setGracePeriodSelector
  , setGroupIDSelector
  , setKeyIDSelector
  , setKeySelector
  , setServerSideProcessingTimeoutSelector
  , setTimedInvokeTimeoutMsSelector
  , setUseAuxiliaryACLSelector
  , timedInvokeTimeoutMsSelector
  , useAuxiliaryACLSelector


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
groupID :: IsMTRGroupcastClusterJoinGroupParams mtrGroupcastClusterJoinGroupParams => mtrGroupcastClusterJoinGroupParams -> IO (Id NSNumber)
groupID mtrGroupcastClusterJoinGroupParams =
  sendMessage mtrGroupcastClusterJoinGroupParams groupIDSelector

-- | @- setGroupID:@
setGroupID :: (IsMTRGroupcastClusterJoinGroupParams mtrGroupcastClusterJoinGroupParams, IsNSNumber value) => mtrGroupcastClusterJoinGroupParams -> value -> IO ()
setGroupID mtrGroupcastClusterJoinGroupParams value =
  sendMessage mtrGroupcastClusterJoinGroupParams setGroupIDSelector (toNSNumber value)

-- | @- endpoints@
endpoints :: IsMTRGroupcastClusterJoinGroupParams mtrGroupcastClusterJoinGroupParams => mtrGroupcastClusterJoinGroupParams -> IO (Id NSArray)
endpoints mtrGroupcastClusterJoinGroupParams =
  sendMessage mtrGroupcastClusterJoinGroupParams endpointsSelector

-- | @- setEndpoints:@
setEndpoints :: (IsMTRGroupcastClusterJoinGroupParams mtrGroupcastClusterJoinGroupParams, IsNSArray value) => mtrGroupcastClusterJoinGroupParams -> value -> IO ()
setEndpoints mtrGroupcastClusterJoinGroupParams value =
  sendMessage mtrGroupcastClusterJoinGroupParams setEndpointsSelector (toNSArray value)

-- | @- keyID@
keyID :: IsMTRGroupcastClusterJoinGroupParams mtrGroupcastClusterJoinGroupParams => mtrGroupcastClusterJoinGroupParams -> IO (Id NSNumber)
keyID mtrGroupcastClusterJoinGroupParams =
  sendMessage mtrGroupcastClusterJoinGroupParams keyIDSelector

-- | @- setKeyID:@
setKeyID :: (IsMTRGroupcastClusterJoinGroupParams mtrGroupcastClusterJoinGroupParams, IsNSNumber value) => mtrGroupcastClusterJoinGroupParams -> value -> IO ()
setKeyID mtrGroupcastClusterJoinGroupParams value =
  sendMessage mtrGroupcastClusterJoinGroupParams setKeyIDSelector (toNSNumber value)

-- | @- key@
key :: IsMTRGroupcastClusterJoinGroupParams mtrGroupcastClusterJoinGroupParams => mtrGroupcastClusterJoinGroupParams -> IO (Id NSData)
key mtrGroupcastClusterJoinGroupParams =
  sendMessage mtrGroupcastClusterJoinGroupParams keySelector

-- | @- setKey:@
setKey :: (IsMTRGroupcastClusterJoinGroupParams mtrGroupcastClusterJoinGroupParams, IsNSData value) => mtrGroupcastClusterJoinGroupParams -> value -> IO ()
setKey mtrGroupcastClusterJoinGroupParams value =
  sendMessage mtrGroupcastClusterJoinGroupParams setKeySelector (toNSData value)

-- | @- gracePeriod@
gracePeriod :: IsMTRGroupcastClusterJoinGroupParams mtrGroupcastClusterJoinGroupParams => mtrGroupcastClusterJoinGroupParams -> IO (Id NSNumber)
gracePeriod mtrGroupcastClusterJoinGroupParams =
  sendMessage mtrGroupcastClusterJoinGroupParams gracePeriodSelector

-- | @- setGracePeriod:@
setGracePeriod :: (IsMTRGroupcastClusterJoinGroupParams mtrGroupcastClusterJoinGroupParams, IsNSNumber value) => mtrGroupcastClusterJoinGroupParams -> value -> IO ()
setGracePeriod mtrGroupcastClusterJoinGroupParams value =
  sendMessage mtrGroupcastClusterJoinGroupParams setGracePeriodSelector (toNSNumber value)

-- | @- useAuxiliaryACL@
useAuxiliaryACL :: IsMTRGroupcastClusterJoinGroupParams mtrGroupcastClusterJoinGroupParams => mtrGroupcastClusterJoinGroupParams -> IO (Id NSNumber)
useAuxiliaryACL mtrGroupcastClusterJoinGroupParams =
  sendMessage mtrGroupcastClusterJoinGroupParams useAuxiliaryACLSelector

-- | @- setUseAuxiliaryACL:@
setUseAuxiliaryACL :: (IsMTRGroupcastClusterJoinGroupParams mtrGroupcastClusterJoinGroupParams, IsNSNumber value) => mtrGroupcastClusterJoinGroupParams -> value -> IO ()
setUseAuxiliaryACL mtrGroupcastClusterJoinGroupParams value =
  sendMessage mtrGroupcastClusterJoinGroupParams setUseAuxiliaryACLSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRGroupcastClusterJoinGroupParams mtrGroupcastClusterJoinGroupParams => mtrGroupcastClusterJoinGroupParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrGroupcastClusterJoinGroupParams =
  sendMessage mtrGroupcastClusterJoinGroupParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRGroupcastClusterJoinGroupParams mtrGroupcastClusterJoinGroupParams, IsNSNumber value) => mtrGroupcastClusterJoinGroupParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrGroupcastClusterJoinGroupParams value =
  sendMessage mtrGroupcastClusterJoinGroupParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRGroupcastClusterJoinGroupParams mtrGroupcastClusterJoinGroupParams => mtrGroupcastClusterJoinGroupParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrGroupcastClusterJoinGroupParams =
  sendMessage mtrGroupcastClusterJoinGroupParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRGroupcastClusterJoinGroupParams mtrGroupcastClusterJoinGroupParams, IsNSNumber value) => mtrGroupcastClusterJoinGroupParams -> value -> IO ()
setServerSideProcessingTimeout mtrGroupcastClusterJoinGroupParams value =
  sendMessage mtrGroupcastClusterJoinGroupParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @groupID@
groupIDSelector :: Selector '[] (Id NSNumber)
groupIDSelector = mkSelector "groupID"

-- | @Selector@ for @setGroupID:@
setGroupIDSelector :: Selector '[Id NSNumber] ()
setGroupIDSelector = mkSelector "setGroupID:"

-- | @Selector@ for @endpoints@
endpointsSelector :: Selector '[] (Id NSArray)
endpointsSelector = mkSelector "endpoints"

-- | @Selector@ for @setEndpoints:@
setEndpointsSelector :: Selector '[Id NSArray] ()
setEndpointsSelector = mkSelector "setEndpoints:"

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

-- | @Selector@ for @useAuxiliaryACL@
useAuxiliaryACLSelector :: Selector '[] (Id NSNumber)
useAuxiliaryACLSelector = mkSelector "useAuxiliaryACL"

-- | @Selector@ for @setUseAuxiliaryACL:@
setUseAuxiliaryACLSelector :: Selector '[Id NSNumber] ()
setUseAuxiliaryACLSelector = mkSelector "setUseAuxiliaryACL:"

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

