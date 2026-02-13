{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRMessagesClusterPresentMessagesRequestParams@.
module ObjC.Matter.MTRMessagesClusterPresentMessagesRequestParams
  ( MTRMessagesClusterPresentMessagesRequestParams
  , IsMTRMessagesClusterPresentMessagesRequestParams(..)
  , messageID
  , setMessageID
  , priority
  , setPriority
  , messageControl
  , setMessageControl
  , startTime
  , setStartTime
  , duration
  , setDuration
  , messageText
  , setMessageText
  , responses
  , setResponses
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , durationSelector
  , messageControlSelector
  , messageIDSelector
  , messageTextSelector
  , prioritySelector
  , responsesSelector
  , serverSideProcessingTimeoutSelector
  , setDurationSelector
  , setMessageControlSelector
  , setMessageIDSelector
  , setMessageTextSelector
  , setPrioritySelector
  , setResponsesSelector
  , setServerSideProcessingTimeoutSelector
  , setStartTimeSelector
  , setTimedInvokeTimeoutMsSelector
  , startTimeSelector
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

-- | @- messageID@
messageID :: IsMTRMessagesClusterPresentMessagesRequestParams mtrMessagesClusterPresentMessagesRequestParams => mtrMessagesClusterPresentMessagesRequestParams -> IO (Id NSData)
messageID mtrMessagesClusterPresentMessagesRequestParams =
  sendMessage mtrMessagesClusterPresentMessagesRequestParams messageIDSelector

-- | @- setMessageID:@
setMessageID :: (IsMTRMessagesClusterPresentMessagesRequestParams mtrMessagesClusterPresentMessagesRequestParams, IsNSData value) => mtrMessagesClusterPresentMessagesRequestParams -> value -> IO ()
setMessageID mtrMessagesClusterPresentMessagesRequestParams value =
  sendMessage mtrMessagesClusterPresentMessagesRequestParams setMessageIDSelector (toNSData value)

-- | @- priority@
priority :: IsMTRMessagesClusterPresentMessagesRequestParams mtrMessagesClusterPresentMessagesRequestParams => mtrMessagesClusterPresentMessagesRequestParams -> IO (Id NSNumber)
priority mtrMessagesClusterPresentMessagesRequestParams =
  sendMessage mtrMessagesClusterPresentMessagesRequestParams prioritySelector

-- | @- setPriority:@
setPriority :: (IsMTRMessagesClusterPresentMessagesRequestParams mtrMessagesClusterPresentMessagesRequestParams, IsNSNumber value) => mtrMessagesClusterPresentMessagesRequestParams -> value -> IO ()
setPriority mtrMessagesClusterPresentMessagesRequestParams value =
  sendMessage mtrMessagesClusterPresentMessagesRequestParams setPrioritySelector (toNSNumber value)

-- | @- messageControl@
messageControl :: IsMTRMessagesClusterPresentMessagesRequestParams mtrMessagesClusterPresentMessagesRequestParams => mtrMessagesClusterPresentMessagesRequestParams -> IO (Id NSNumber)
messageControl mtrMessagesClusterPresentMessagesRequestParams =
  sendMessage mtrMessagesClusterPresentMessagesRequestParams messageControlSelector

-- | @- setMessageControl:@
setMessageControl :: (IsMTRMessagesClusterPresentMessagesRequestParams mtrMessagesClusterPresentMessagesRequestParams, IsNSNumber value) => mtrMessagesClusterPresentMessagesRequestParams -> value -> IO ()
setMessageControl mtrMessagesClusterPresentMessagesRequestParams value =
  sendMessage mtrMessagesClusterPresentMessagesRequestParams setMessageControlSelector (toNSNumber value)

-- | @- startTime@
startTime :: IsMTRMessagesClusterPresentMessagesRequestParams mtrMessagesClusterPresentMessagesRequestParams => mtrMessagesClusterPresentMessagesRequestParams -> IO (Id NSNumber)
startTime mtrMessagesClusterPresentMessagesRequestParams =
  sendMessage mtrMessagesClusterPresentMessagesRequestParams startTimeSelector

-- | @- setStartTime:@
setStartTime :: (IsMTRMessagesClusterPresentMessagesRequestParams mtrMessagesClusterPresentMessagesRequestParams, IsNSNumber value) => mtrMessagesClusterPresentMessagesRequestParams -> value -> IO ()
setStartTime mtrMessagesClusterPresentMessagesRequestParams value =
  sendMessage mtrMessagesClusterPresentMessagesRequestParams setStartTimeSelector (toNSNumber value)

-- | @- duration@
duration :: IsMTRMessagesClusterPresentMessagesRequestParams mtrMessagesClusterPresentMessagesRequestParams => mtrMessagesClusterPresentMessagesRequestParams -> IO (Id NSNumber)
duration mtrMessagesClusterPresentMessagesRequestParams =
  sendMessage mtrMessagesClusterPresentMessagesRequestParams durationSelector

-- | @- setDuration:@
setDuration :: (IsMTRMessagesClusterPresentMessagesRequestParams mtrMessagesClusterPresentMessagesRequestParams, IsNSNumber value) => mtrMessagesClusterPresentMessagesRequestParams -> value -> IO ()
setDuration mtrMessagesClusterPresentMessagesRequestParams value =
  sendMessage mtrMessagesClusterPresentMessagesRequestParams setDurationSelector (toNSNumber value)

-- | @- messageText@
messageText :: IsMTRMessagesClusterPresentMessagesRequestParams mtrMessagesClusterPresentMessagesRequestParams => mtrMessagesClusterPresentMessagesRequestParams -> IO (Id NSString)
messageText mtrMessagesClusterPresentMessagesRequestParams =
  sendMessage mtrMessagesClusterPresentMessagesRequestParams messageTextSelector

-- | @- setMessageText:@
setMessageText :: (IsMTRMessagesClusterPresentMessagesRequestParams mtrMessagesClusterPresentMessagesRequestParams, IsNSString value) => mtrMessagesClusterPresentMessagesRequestParams -> value -> IO ()
setMessageText mtrMessagesClusterPresentMessagesRequestParams value =
  sendMessage mtrMessagesClusterPresentMessagesRequestParams setMessageTextSelector (toNSString value)

-- | @- responses@
responses :: IsMTRMessagesClusterPresentMessagesRequestParams mtrMessagesClusterPresentMessagesRequestParams => mtrMessagesClusterPresentMessagesRequestParams -> IO (Id NSArray)
responses mtrMessagesClusterPresentMessagesRequestParams =
  sendMessage mtrMessagesClusterPresentMessagesRequestParams responsesSelector

-- | @- setResponses:@
setResponses :: (IsMTRMessagesClusterPresentMessagesRequestParams mtrMessagesClusterPresentMessagesRequestParams, IsNSArray value) => mtrMessagesClusterPresentMessagesRequestParams -> value -> IO ()
setResponses mtrMessagesClusterPresentMessagesRequestParams value =
  sendMessage mtrMessagesClusterPresentMessagesRequestParams setResponsesSelector (toNSArray value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRMessagesClusterPresentMessagesRequestParams mtrMessagesClusterPresentMessagesRequestParams => mtrMessagesClusterPresentMessagesRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrMessagesClusterPresentMessagesRequestParams =
  sendMessage mtrMessagesClusterPresentMessagesRequestParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRMessagesClusterPresentMessagesRequestParams mtrMessagesClusterPresentMessagesRequestParams, IsNSNumber value) => mtrMessagesClusterPresentMessagesRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrMessagesClusterPresentMessagesRequestParams value =
  sendMessage mtrMessagesClusterPresentMessagesRequestParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRMessagesClusterPresentMessagesRequestParams mtrMessagesClusterPresentMessagesRequestParams => mtrMessagesClusterPresentMessagesRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrMessagesClusterPresentMessagesRequestParams =
  sendMessage mtrMessagesClusterPresentMessagesRequestParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRMessagesClusterPresentMessagesRequestParams mtrMessagesClusterPresentMessagesRequestParams, IsNSNumber value) => mtrMessagesClusterPresentMessagesRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrMessagesClusterPresentMessagesRequestParams value =
  sendMessage mtrMessagesClusterPresentMessagesRequestParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @messageID@
messageIDSelector :: Selector '[] (Id NSData)
messageIDSelector = mkSelector "messageID"

-- | @Selector@ for @setMessageID:@
setMessageIDSelector :: Selector '[Id NSData] ()
setMessageIDSelector = mkSelector "setMessageID:"

-- | @Selector@ for @priority@
prioritySelector :: Selector '[] (Id NSNumber)
prioritySelector = mkSelector "priority"

-- | @Selector@ for @setPriority:@
setPrioritySelector :: Selector '[Id NSNumber] ()
setPrioritySelector = mkSelector "setPriority:"

-- | @Selector@ for @messageControl@
messageControlSelector :: Selector '[] (Id NSNumber)
messageControlSelector = mkSelector "messageControl"

-- | @Selector@ for @setMessageControl:@
setMessageControlSelector :: Selector '[Id NSNumber] ()
setMessageControlSelector = mkSelector "setMessageControl:"

-- | @Selector@ for @startTime@
startTimeSelector :: Selector '[] (Id NSNumber)
startTimeSelector = mkSelector "startTime"

-- | @Selector@ for @setStartTime:@
setStartTimeSelector :: Selector '[Id NSNumber] ()
setStartTimeSelector = mkSelector "setStartTime:"

-- | @Selector@ for @duration@
durationSelector :: Selector '[] (Id NSNumber)
durationSelector = mkSelector "duration"

-- | @Selector@ for @setDuration:@
setDurationSelector :: Selector '[Id NSNumber] ()
setDurationSelector = mkSelector "setDuration:"

-- | @Selector@ for @messageText@
messageTextSelector :: Selector '[] (Id NSString)
messageTextSelector = mkSelector "messageText"

-- | @Selector@ for @setMessageText:@
setMessageTextSelector :: Selector '[Id NSString] ()
setMessageTextSelector = mkSelector "setMessageText:"

-- | @Selector@ for @responses@
responsesSelector :: Selector '[] (Id NSArray)
responsesSelector = mkSelector "responses"

-- | @Selector@ for @setResponses:@
setResponsesSelector :: Selector '[Id NSArray] ()
setResponsesSelector = mkSelector "setResponses:"

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

