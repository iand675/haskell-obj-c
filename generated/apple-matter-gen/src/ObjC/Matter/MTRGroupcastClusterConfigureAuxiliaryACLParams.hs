{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGroupcastClusterConfigureAuxiliaryACLParams@.
module ObjC.Matter.MTRGroupcastClusterConfigureAuxiliaryACLParams
  ( MTRGroupcastClusterConfigureAuxiliaryACLParams
  , IsMTRGroupcastClusterConfigureAuxiliaryACLParams(..)
  , groupID
  , setGroupID
  , useAuxiliaryACL
  , setUseAuxiliaryACL
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , groupIDSelector
  , serverSideProcessingTimeoutSelector
  , setGroupIDSelector
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
groupID :: IsMTRGroupcastClusterConfigureAuxiliaryACLParams mtrGroupcastClusterConfigureAuxiliaryACLParams => mtrGroupcastClusterConfigureAuxiliaryACLParams -> IO (Id NSNumber)
groupID mtrGroupcastClusterConfigureAuxiliaryACLParams =
  sendMessage mtrGroupcastClusterConfigureAuxiliaryACLParams groupIDSelector

-- | @- setGroupID:@
setGroupID :: (IsMTRGroupcastClusterConfigureAuxiliaryACLParams mtrGroupcastClusterConfigureAuxiliaryACLParams, IsNSNumber value) => mtrGroupcastClusterConfigureAuxiliaryACLParams -> value -> IO ()
setGroupID mtrGroupcastClusterConfigureAuxiliaryACLParams value =
  sendMessage mtrGroupcastClusterConfigureAuxiliaryACLParams setGroupIDSelector (toNSNumber value)

-- | @- useAuxiliaryACL@
useAuxiliaryACL :: IsMTRGroupcastClusterConfigureAuxiliaryACLParams mtrGroupcastClusterConfigureAuxiliaryACLParams => mtrGroupcastClusterConfigureAuxiliaryACLParams -> IO (Id NSNumber)
useAuxiliaryACL mtrGroupcastClusterConfigureAuxiliaryACLParams =
  sendMessage mtrGroupcastClusterConfigureAuxiliaryACLParams useAuxiliaryACLSelector

-- | @- setUseAuxiliaryACL:@
setUseAuxiliaryACL :: (IsMTRGroupcastClusterConfigureAuxiliaryACLParams mtrGroupcastClusterConfigureAuxiliaryACLParams, IsNSNumber value) => mtrGroupcastClusterConfigureAuxiliaryACLParams -> value -> IO ()
setUseAuxiliaryACL mtrGroupcastClusterConfigureAuxiliaryACLParams value =
  sendMessage mtrGroupcastClusterConfigureAuxiliaryACLParams setUseAuxiliaryACLSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRGroupcastClusterConfigureAuxiliaryACLParams mtrGroupcastClusterConfigureAuxiliaryACLParams => mtrGroupcastClusterConfigureAuxiliaryACLParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrGroupcastClusterConfigureAuxiliaryACLParams =
  sendMessage mtrGroupcastClusterConfigureAuxiliaryACLParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRGroupcastClusterConfigureAuxiliaryACLParams mtrGroupcastClusterConfigureAuxiliaryACLParams, IsNSNumber value) => mtrGroupcastClusterConfigureAuxiliaryACLParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrGroupcastClusterConfigureAuxiliaryACLParams value =
  sendMessage mtrGroupcastClusterConfigureAuxiliaryACLParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRGroupcastClusterConfigureAuxiliaryACLParams mtrGroupcastClusterConfigureAuxiliaryACLParams => mtrGroupcastClusterConfigureAuxiliaryACLParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrGroupcastClusterConfigureAuxiliaryACLParams =
  sendMessage mtrGroupcastClusterConfigureAuxiliaryACLParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRGroupcastClusterConfigureAuxiliaryACLParams mtrGroupcastClusterConfigureAuxiliaryACLParams, IsNSNumber value) => mtrGroupcastClusterConfigureAuxiliaryACLParams -> value -> IO ()
setServerSideProcessingTimeout mtrGroupcastClusterConfigureAuxiliaryACLParams value =
  sendMessage mtrGroupcastClusterConfigureAuxiliaryACLParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @groupID@
groupIDSelector :: Selector '[] (Id NSNumber)
groupIDSelector = mkSelector "groupID"

-- | @Selector@ for @setGroupID:@
setGroupIDSelector :: Selector '[Id NSNumber] ()
setGroupIDSelector = mkSelector "setGroupID:"

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

