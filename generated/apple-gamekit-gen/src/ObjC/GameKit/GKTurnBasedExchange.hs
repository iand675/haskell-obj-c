{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GKTurnBasedExchange@.
module ObjC.GameKit.GKTurnBasedExchange
  ( GKTurnBasedExchange
  , IsGKTurnBasedExchange(..)
  , cancelWithLocalizableMessageKey_arguments_completionHandler
  , replyWithLocalizableMessageKey_arguments_data_completionHandler
  , exchangeID
  , sender
  , recipients
  , status
  , message
  , data_
  , sendDate
  , timeoutDate
  , completionDate
  , replies
  , cancelWithLocalizableMessageKey_arguments_completionHandlerSelector
  , completionDateSelector
  , dataSelector
  , exchangeIDSelector
  , messageSelector
  , recipientsSelector
  , repliesSelector
  , replyWithLocalizableMessageKey_arguments_data_completionHandlerSelector
  , sendDateSelector
  , senderSelector
  , statusSelector
  , timeoutDateSelector

  -- * Enum types
  , GKTurnBasedExchangeStatus(GKTurnBasedExchangeStatus)
  , pattern GKTurnBasedExchangeStatusUnknown
  , pattern GKTurnBasedExchangeStatusActive
  , pattern GKTurnBasedExchangeStatusComplete
  , pattern GKTurnBasedExchangeStatusResolved
  , pattern GKTurnBasedExchangeStatusCanceled

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameKit.Internal.Classes
import ObjC.GameKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- cancelWithLocalizableMessageKey:arguments:completionHandler:@
cancelWithLocalizableMessageKey_arguments_completionHandler :: (IsGKTurnBasedExchange gkTurnBasedExchange, IsNSString key, IsNSArray arguments) => gkTurnBasedExchange -> key -> arguments -> Ptr () -> IO ()
cancelWithLocalizableMessageKey_arguments_completionHandler gkTurnBasedExchange key arguments completionHandler =
  sendMessage gkTurnBasedExchange cancelWithLocalizableMessageKey_arguments_completionHandlerSelector (toNSString key) (toNSArray arguments) completionHandler

-- | @- replyWithLocalizableMessageKey:arguments:data:completionHandler:@
replyWithLocalizableMessageKey_arguments_data_completionHandler :: (IsGKTurnBasedExchange gkTurnBasedExchange, IsNSString key, IsNSArray arguments, IsNSData data_) => gkTurnBasedExchange -> key -> arguments -> data_ -> Ptr () -> IO ()
replyWithLocalizableMessageKey_arguments_data_completionHandler gkTurnBasedExchange key arguments data_ completionHandler =
  sendMessage gkTurnBasedExchange replyWithLocalizableMessageKey_arguments_data_completionHandlerSelector (toNSString key) (toNSArray arguments) (toNSData data_) completionHandler

-- | @- exchangeID@
exchangeID :: IsGKTurnBasedExchange gkTurnBasedExchange => gkTurnBasedExchange -> IO (Id NSString)
exchangeID gkTurnBasedExchange =
  sendMessage gkTurnBasedExchange exchangeIDSelector

-- | @- sender@
sender :: IsGKTurnBasedExchange gkTurnBasedExchange => gkTurnBasedExchange -> IO (Id GKTurnBasedParticipant)
sender gkTurnBasedExchange =
  sendMessage gkTurnBasedExchange senderSelector

-- | @- recipients@
recipients :: IsGKTurnBasedExchange gkTurnBasedExchange => gkTurnBasedExchange -> IO (Id NSArray)
recipients gkTurnBasedExchange =
  sendMessage gkTurnBasedExchange recipientsSelector

-- | @- status@
status :: IsGKTurnBasedExchange gkTurnBasedExchange => gkTurnBasedExchange -> IO GKTurnBasedExchangeStatus
status gkTurnBasedExchange =
  sendMessage gkTurnBasedExchange statusSelector

-- | @- message@
message :: IsGKTurnBasedExchange gkTurnBasedExchange => gkTurnBasedExchange -> IO (Id NSString)
message gkTurnBasedExchange =
  sendMessage gkTurnBasedExchange messageSelector

-- | @- data@
data_ :: IsGKTurnBasedExchange gkTurnBasedExchange => gkTurnBasedExchange -> IO (Id NSData)
data_ gkTurnBasedExchange =
  sendMessage gkTurnBasedExchange dataSelector

-- | @- sendDate@
sendDate :: IsGKTurnBasedExchange gkTurnBasedExchange => gkTurnBasedExchange -> IO (Id NSDate)
sendDate gkTurnBasedExchange =
  sendMessage gkTurnBasedExchange sendDateSelector

-- | @- timeoutDate@
timeoutDate :: IsGKTurnBasedExchange gkTurnBasedExchange => gkTurnBasedExchange -> IO (Id NSDate)
timeoutDate gkTurnBasedExchange =
  sendMessage gkTurnBasedExchange timeoutDateSelector

-- | @- completionDate@
completionDate :: IsGKTurnBasedExchange gkTurnBasedExchange => gkTurnBasedExchange -> IO (Id NSDate)
completionDate gkTurnBasedExchange =
  sendMessage gkTurnBasedExchange completionDateSelector

-- | @- replies@
replies :: IsGKTurnBasedExchange gkTurnBasedExchange => gkTurnBasedExchange -> IO (Id NSArray)
replies gkTurnBasedExchange =
  sendMessage gkTurnBasedExchange repliesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cancelWithLocalizableMessageKey:arguments:completionHandler:@
cancelWithLocalizableMessageKey_arguments_completionHandlerSelector :: Selector '[Id NSString, Id NSArray, Ptr ()] ()
cancelWithLocalizableMessageKey_arguments_completionHandlerSelector = mkSelector "cancelWithLocalizableMessageKey:arguments:completionHandler:"

-- | @Selector@ for @replyWithLocalizableMessageKey:arguments:data:completionHandler:@
replyWithLocalizableMessageKey_arguments_data_completionHandlerSelector :: Selector '[Id NSString, Id NSArray, Id NSData, Ptr ()] ()
replyWithLocalizableMessageKey_arguments_data_completionHandlerSelector = mkSelector "replyWithLocalizableMessageKey:arguments:data:completionHandler:"

-- | @Selector@ for @exchangeID@
exchangeIDSelector :: Selector '[] (Id NSString)
exchangeIDSelector = mkSelector "exchangeID"

-- | @Selector@ for @sender@
senderSelector :: Selector '[] (Id GKTurnBasedParticipant)
senderSelector = mkSelector "sender"

-- | @Selector@ for @recipients@
recipientsSelector :: Selector '[] (Id NSArray)
recipientsSelector = mkSelector "recipients"

-- | @Selector@ for @status@
statusSelector :: Selector '[] GKTurnBasedExchangeStatus
statusSelector = mkSelector "status"

-- | @Selector@ for @message@
messageSelector :: Selector '[] (Id NSString)
messageSelector = mkSelector "message"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSData)
dataSelector = mkSelector "data"

-- | @Selector@ for @sendDate@
sendDateSelector :: Selector '[] (Id NSDate)
sendDateSelector = mkSelector "sendDate"

-- | @Selector@ for @timeoutDate@
timeoutDateSelector :: Selector '[] (Id NSDate)
timeoutDateSelector = mkSelector "timeoutDate"

-- | @Selector@ for @completionDate@
completionDateSelector :: Selector '[] (Id NSDate)
completionDateSelector = mkSelector "completionDate"

-- | @Selector@ for @replies@
repliesSelector :: Selector '[] (Id NSArray)
repliesSelector = mkSelector "replies"

