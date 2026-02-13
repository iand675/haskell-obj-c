{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRMessagesClusterMessageCompleteEvent@.
module ObjC.Matter.MTRMessagesClusterMessageCompleteEvent
  ( MTRMessagesClusterMessageCompleteEvent
  , IsMTRMessagesClusterMessageCompleteEvent(..)
  , messageID
  , setMessageID
  , responseID
  , setResponseID
  , reply
  , setReply
  , futureMessagesPreference
  , setFutureMessagesPreference
  , futureMessagesPreferenceSelector
  , messageIDSelector
  , replySelector
  , responseIDSelector
  , setFutureMessagesPreferenceSelector
  , setMessageIDSelector
  , setReplySelector
  , setResponseIDSelector


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
messageID :: IsMTRMessagesClusterMessageCompleteEvent mtrMessagesClusterMessageCompleteEvent => mtrMessagesClusterMessageCompleteEvent -> IO (Id NSData)
messageID mtrMessagesClusterMessageCompleteEvent =
  sendMessage mtrMessagesClusterMessageCompleteEvent messageIDSelector

-- | @- setMessageID:@
setMessageID :: (IsMTRMessagesClusterMessageCompleteEvent mtrMessagesClusterMessageCompleteEvent, IsNSData value) => mtrMessagesClusterMessageCompleteEvent -> value -> IO ()
setMessageID mtrMessagesClusterMessageCompleteEvent value =
  sendMessage mtrMessagesClusterMessageCompleteEvent setMessageIDSelector (toNSData value)

-- | @- responseID@
responseID :: IsMTRMessagesClusterMessageCompleteEvent mtrMessagesClusterMessageCompleteEvent => mtrMessagesClusterMessageCompleteEvent -> IO (Id NSNumber)
responseID mtrMessagesClusterMessageCompleteEvent =
  sendMessage mtrMessagesClusterMessageCompleteEvent responseIDSelector

-- | @- setResponseID:@
setResponseID :: (IsMTRMessagesClusterMessageCompleteEvent mtrMessagesClusterMessageCompleteEvent, IsNSNumber value) => mtrMessagesClusterMessageCompleteEvent -> value -> IO ()
setResponseID mtrMessagesClusterMessageCompleteEvent value =
  sendMessage mtrMessagesClusterMessageCompleteEvent setResponseIDSelector (toNSNumber value)

-- | @- reply@
reply :: IsMTRMessagesClusterMessageCompleteEvent mtrMessagesClusterMessageCompleteEvent => mtrMessagesClusterMessageCompleteEvent -> IO (Id NSString)
reply mtrMessagesClusterMessageCompleteEvent =
  sendMessage mtrMessagesClusterMessageCompleteEvent replySelector

-- | @- setReply:@
setReply :: (IsMTRMessagesClusterMessageCompleteEvent mtrMessagesClusterMessageCompleteEvent, IsNSString value) => mtrMessagesClusterMessageCompleteEvent -> value -> IO ()
setReply mtrMessagesClusterMessageCompleteEvent value =
  sendMessage mtrMessagesClusterMessageCompleteEvent setReplySelector (toNSString value)

-- | @- futureMessagesPreference@
futureMessagesPreference :: IsMTRMessagesClusterMessageCompleteEvent mtrMessagesClusterMessageCompleteEvent => mtrMessagesClusterMessageCompleteEvent -> IO (Id NSNumber)
futureMessagesPreference mtrMessagesClusterMessageCompleteEvent =
  sendMessage mtrMessagesClusterMessageCompleteEvent futureMessagesPreferenceSelector

-- | @- setFutureMessagesPreference:@
setFutureMessagesPreference :: (IsMTRMessagesClusterMessageCompleteEvent mtrMessagesClusterMessageCompleteEvent, IsNSNumber value) => mtrMessagesClusterMessageCompleteEvent -> value -> IO ()
setFutureMessagesPreference mtrMessagesClusterMessageCompleteEvent value =
  sendMessage mtrMessagesClusterMessageCompleteEvent setFutureMessagesPreferenceSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @messageID@
messageIDSelector :: Selector '[] (Id NSData)
messageIDSelector = mkSelector "messageID"

-- | @Selector@ for @setMessageID:@
setMessageIDSelector :: Selector '[Id NSData] ()
setMessageIDSelector = mkSelector "setMessageID:"

-- | @Selector@ for @responseID@
responseIDSelector :: Selector '[] (Id NSNumber)
responseIDSelector = mkSelector "responseID"

-- | @Selector@ for @setResponseID:@
setResponseIDSelector :: Selector '[Id NSNumber] ()
setResponseIDSelector = mkSelector "setResponseID:"

-- | @Selector@ for @reply@
replySelector :: Selector '[] (Id NSString)
replySelector = mkSelector "reply"

-- | @Selector@ for @setReply:@
setReplySelector :: Selector '[Id NSString] ()
setReplySelector = mkSelector "setReply:"

-- | @Selector@ for @futureMessagesPreference@
futureMessagesPreferenceSelector :: Selector '[] (Id NSNumber)
futureMessagesPreferenceSelector = mkSelector "futureMessagesPreference"

-- | @Selector@ for @setFutureMessagesPreference:@
setFutureMessagesPreferenceSelector :: Selector '[Id NSNumber] ()
setFutureMessagesPreferenceSelector = mkSelector "setFutureMessagesPreference:"

