{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRMessagesClusterCancelMessagesRequestParams@.
module ObjC.Matter.MTRMessagesClusterCancelMessagesRequestParams
  ( MTRMessagesClusterCancelMessagesRequestParams
  , IsMTRMessagesClusterCancelMessagesRequestParams(..)
  , messageIDs
  , setMessageIDs
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , messageIDsSelector
  , serverSideProcessingTimeoutSelector
  , setMessageIDsSelector
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

-- | @- messageIDs@
messageIDs :: IsMTRMessagesClusterCancelMessagesRequestParams mtrMessagesClusterCancelMessagesRequestParams => mtrMessagesClusterCancelMessagesRequestParams -> IO (Id NSArray)
messageIDs mtrMessagesClusterCancelMessagesRequestParams =
  sendMessage mtrMessagesClusterCancelMessagesRequestParams messageIDsSelector

-- | @- setMessageIDs:@
setMessageIDs :: (IsMTRMessagesClusterCancelMessagesRequestParams mtrMessagesClusterCancelMessagesRequestParams, IsNSArray value) => mtrMessagesClusterCancelMessagesRequestParams -> value -> IO ()
setMessageIDs mtrMessagesClusterCancelMessagesRequestParams value =
  sendMessage mtrMessagesClusterCancelMessagesRequestParams setMessageIDsSelector (toNSArray value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRMessagesClusterCancelMessagesRequestParams mtrMessagesClusterCancelMessagesRequestParams => mtrMessagesClusterCancelMessagesRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrMessagesClusterCancelMessagesRequestParams =
  sendMessage mtrMessagesClusterCancelMessagesRequestParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRMessagesClusterCancelMessagesRequestParams mtrMessagesClusterCancelMessagesRequestParams, IsNSNumber value) => mtrMessagesClusterCancelMessagesRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrMessagesClusterCancelMessagesRequestParams value =
  sendMessage mtrMessagesClusterCancelMessagesRequestParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRMessagesClusterCancelMessagesRequestParams mtrMessagesClusterCancelMessagesRequestParams => mtrMessagesClusterCancelMessagesRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrMessagesClusterCancelMessagesRequestParams =
  sendMessage mtrMessagesClusterCancelMessagesRequestParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRMessagesClusterCancelMessagesRequestParams mtrMessagesClusterCancelMessagesRequestParams, IsNSNumber value) => mtrMessagesClusterCancelMessagesRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrMessagesClusterCancelMessagesRequestParams value =
  sendMessage mtrMessagesClusterCancelMessagesRequestParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @messageIDs@
messageIDsSelector :: Selector '[] (Id NSArray)
messageIDsSelector = mkSelector "messageIDs"

-- | @Selector@ for @setMessageIDs:@
setMessageIDsSelector :: Selector '[Id NSArray] ()
setMessageIDsSelector = mkSelector "setMessageIDs:"

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

