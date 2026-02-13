{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRMessagesClusterMessageStruct@.
module ObjC.Matter.MTRMessagesClusterMessageStruct
  ( MTRMessagesClusterMessageStruct
  , IsMTRMessagesClusterMessageStruct(..)
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
  , durationSelector
  , messageControlSelector
  , messageIDSelector
  , messageTextSelector
  , prioritySelector
  , responsesSelector
  , setDurationSelector
  , setMessageControlSelector
  , setMessageIDSelector
  , setMessageTextSelector
  , setPrioritySelector
  , setResponsesSelector
  , setStartTimeSelector
  , startTimeSelector


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
messageID :: IsMTRMessagesClusterMessageStruct mtrMessagesClusterMessageStruct => mtrMessagesClusterMessageStruct -> IO (Id NSData)
messageID mtrMessagesClusterMessageStruct =
  sendMessage mtrMessagesClusterMessageStruct messageIDSelector

-- | @- setMessageID:@
setMessageID :: (IsMTRMessagesClusterMessageStruct mtrMessagesClusterMessageStruct, IsNSData value) => mtrMessagesClusterMessageStruct -> value -> IO ()
setMessageID mtrMessagesClusterMessageStruct value =
  sendMessage mtrMessagesClusterMessageStruct setMessageIDSelector (toNSData value)

-- | @- priority@
priority :: IsMTRMessagesClusterMessageStruct mtrMessagesClusterMessageStruct => mtrMessagesClusterMessageStruct -> IO (Id NSNumber)
priority mtrMessagesClusterMessageStruct =
  sendMessage mtrMessagesClusterMessageStruct prioritySelector

-- | @- setPriority:@
setPriority :: (IsMTRMessagesClusterMessageStruct mtrMessagesClusterMessageStruct, IsNSNumber value) => mtrMessagesClusterMessageStruct -> value -> IO ()
setPriority mtrMessagesClusterMessageStruct value =
  sendMessage mtrMessagesClusterMessageStruct setPrioritySelector (toNSNumber value)

-- | @- messageControl@
messageControl :: IsMTRMessagesClusterMessageStruct mtrMessagesClusterMessageStruct => mtrMessagesClusterMessageStruct -> IO (Id NSNumber)
messageControl mtrMessagesClusterMessageStruct =
  sendMessage mtrMessagesClusterMessageStruct messageControlSelector

-- | @- setMessageControl:@
setMessageControl :: (IsMTRMessagesClusterMessageStruct mtrMessagesClusterMessageStruct, IsNSNumber value) => mtrMessagesClusterMessageStruct -> value -> IO ()
setMessageControl mtrMessagesClusterMessageStruct value =
  sendMessage mtrMessagesClusterMessageStruct setMessageControlSelector (toNSNumber value)

-- | @- startTime@
startTime :: IsMTRMessagesClusterMessageStruct mtrMessagesClusterMessageStruct => mtrMessagesClusterMessageStruct -> IO (Id NSNumber)
startTime mtrMessagesClusterMessageStruct =
  sendMessage mtrMessagesClusterMessageStruct startTimeSelector

-- | @- setStartTime:@
setStartTime :: (IsMTRMessagesClusterMessageStruct mtrMessagesClusterMessageStruct, IsNSNumber value) => mtrMessagesClusterMessageStruct -> value -> IO ()
setStartTime mtrMessagesClusterMessageStruct value =
  sendMessage mtrMessagesClusterMessageStruct setStartTimeSelector (toNSNumber value)

-- | @- duration@
duration :: IsMTRMessagesClusterMessageStruct mtrMessagesClusterMessageStruct => mtrMessagesClusterMessageStruct -> IO (Id NSNumber)
duration mtrMessagesClusterMessageStruct =
  sendMessage mtrMessagesClusterMessageStruct durationSelector

-- | @- setDuration:@
setDuration :: (IsMTRMessagesClusterMessageStruct mtrMessagesClusterMessageStruct, IsNSNumber value) => mtrMessagesClusterMessageStruct -> value -> IO ()
setDuration mtrMessagesClusterMessageStruct value =
  sendMessage mtrMessagesClusterMessageStruct setDurationSelector (toNSNumber value)

-- | @- messageText@
messageText :: IsMTRMessagesClusterMessageStruct mtrMessagesClusterMessageStruct => mtrMessagesClusterMessageStruct -> IO (Id NSString)
messageText mtrMessagesClusterMessageStruct =
  sendMessage mtrMessagesClusterMessageStruct messageTextSelector

-- | @- setMessageText:@
setMessageText :: (IsMTRMessagesClusterMessageStruct mtrMessagesClusterMessageStruct, IsNSString value) => mtrMessagesClusterMessageStruct -> value -> IO ()
setMessageText mtrMessagesClusterMessageStruct value =
  sendMessage mtrMessagesClusterMessageStruct setMessageTextSelector (toNSString value)

-- | @- responses@
responses :: IsMTRMessagesClusterMessageStruct mtrMessagesClusterMessageStruct => mtrMessagesClusterMessageStruct -> IO (Id NSArray)
responses mtrMessagesClusterMessageStruct =
  sendMessage mtrMessagesClusterMessageStruct responsesSelector

-- | @- setResponses:@
setResponses :: (IsMTRMessagesClusterMessageStruct mtrMessagesClusterMessageStruct, IsNSArray value) => mtrMessagesClusterMessageStruct -> value -> IO ()
setResponses mtrMessagesClusterMessageStruct value =
  sendMessage mtrMessagesClusterMessageStruct setResponsesSelector (toNSArray value)

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

