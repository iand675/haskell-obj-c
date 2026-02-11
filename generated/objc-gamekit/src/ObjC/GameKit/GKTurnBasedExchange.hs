{-# LANGUAGE PatternSynonyms #-}
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
  , replyWithLocalizableMessageKey_arguments_data_completionHandlerSelector
  , exchangeIDSelector
  , senderSelector
  , recipientsSelector
  , statusSelector
  , messageSelector
  , dataSelector
  , sendDateSelector
  , timeoutDateSelector
  , completionDateSelector
  , repliesSelector

  -- * Enum types
  , GKTurnBasedExchangeStatus(GKTurnBasedExchangeStatus)
  , pattern GKTurnBasedExchangeStatusUnknown
  , pattern GKTurnBasedExchangeStatusActive
  , pattern GKTurnBasedExchangeStatusComplete
  , pattern GKTurnBasedExchangeStatusResolved
  , pattern GKTurnBasedExchangeStatusCanceled

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameKit.Internal.Classes
import ObjC.GameKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- cancelWithLocalizableMessageKey:arguments:completionHandler:@
cancelWithLocalizableMessageKey_arguments_completionHandler :: (IsGKTurnBasedExchange gkTurnBasedExchange, IsNSString key, IsNSArray arguments) => gkTurnBasedExchange -> key -> arguments -> Ptr () -> IO ()
cancelWithLocalizableMessageKey_arguments_completionHandler gkTurnBasedExchange  key arguments completionHandler =
withObjCPtr key $ \raw_key ->
  withObjCPtr arguments $ \raw_arguments ->
      sendMsg gkTurnBasedExchange (mkSelector "cancelWithLocalizableMessageKey:arguments:completionHandler:") retVoid [argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_arguments :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- replyWithLocalizableMessageKey:arguments:data:completionHandler:@
replyWithLocalizableMessageKey_arguments_data_completionHandler :: (IsGKTurnBasedExchange gkTurnBasedExchange, IsNSString key, IsNSArray arguments, IsNSData data_) => gkTurnBasedExchange -> key -> arguments -> data_ -> Ptr () -> IO ()
replyWithLocalizableMessageKey_arguments_data_completionHandler gkTurnBasedExchange  key arguments data_ completionHandler =
withObjCPtr key $ \raw_key ->
  withObjCPtr arguments $ \raw_arguments ->
    withObjCPtr data_ $ \raw_data_ ->
        sendMsg gkTurnBasedExchange (mkSelector "replyWithLocalizableMessageKey:arguments:data:completionHandler:") retVoid [argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_arguments :: Ptr ()), argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- exchangeID@
exchangeID :: IsGKTurnBasedExchange gkTurnBasedExchange => gkTurnBasedExchange -> IO (Id NSString)
exchangeID gkTurnBasedExchange  =
  sendMsg gkTurnBasedExchange (mkSelector "exchangeID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sender@
sender :: IsGKTurnBasedExchange gkTurnBasedExchange => gkTurnBasedExchange -> IO (Id GKTurnBasedParticipant)
sender gkTurnBasedExchange  =
  sendMsg gkTurnBasedExchange (mkSelector "sender") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- recipients@
recipients :: IsGKTurnBasedExchange gkTurnBasedExchange => gkTurnBasedExchange -> IO (Id NSArray)
recipients gkTurnBasedExchange  =
  sendMsg gkTurnBasedExchange (mkSelector "recipients") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- status@
status :: IsGKTurnBasedExchange gkTurnBasedExchange => gkTurnBasedExchange -> IO GKTurnBasedExchangeStatus
status gkTurnBasedExchange  =
  fmap (coerce :: CSChar -> GKTurnBasedExchangeStatus) $ sendMsg gkTurnBasedExchange (mkSelector "status") retCSChar []

-- | @- message@
message :: IsGKTurnBasedExchange gkTurnBasedExchange => gkTurnBasedExchange -> IO (Id NSString)
message gkTurnBasedExchange  =
  sendMsg gkTurnBasedExchange (mkSelector "message") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- data@
data_ :: IsGKTurnBasedExchange gkTurnBasedExchange => gkTurnBasedExchange -> IO (Id NSData)
data_ gkTurnBasedExchange  =
  sendMsg gkTurnBasedExchange (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sendDate@
sendDate :: IsGKTurnBasedExchange gkTurnBasedExchange => gkTurnBasedExchange -> IO (Id NSDate)
sendDate gkTurnBasedExchange  =
  sendMsg gkTurnBasedExchange (mkSelector "sendDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- timeoutDate@
timeoutDate :: IsGKTurnBasedExchange gkTurnBasedExchange => gkTurnBasedExchange -> IO (Id NSDate)
timeoutDate gkTurnBasedExchange  =
  sendMsg gkTurnBasedExchange (mkSelector "timeoutDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- completionDate@
completionDate :: IsGKTurnBasedExchange gkTurnBasedExchange => gkTurnBasedExchange -> IO (Id NSDate)
completionDate gkTurnBasedExchange  =
  sendMsg gkTurnBasedExchange (mkSelector "completionDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- replies@
replies :: IsGKTurnBasedExchange gkTurnBasedExchange => gkTurnBasedExchange -> IO (Id NSArray)
replies gkTurnBasedExchange  =
  sendMsg gkTurnBasedExchange (mkSelector "replies") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cancelWithLocalizableMessageKey:arguments:completionHandler:@
cancelWithLocalizableMessageKey_arguments_completionHandlerSelector :: Selector
cancelWithLocalizableMessageKey_arguments_completionHandlerSelector = mkSelector "cancelWithLocalizableMessageKey:arguments:completionHandler:"

-- | @Selector@ for @replyWithLocalizableMessageKey:arguments:data:completionHandler:@
replyWithLocalizableMessageKey_arguments_data_completionHandlerSelector :: Selector
replyWithLocalizableMessageKey_arguments_data_completionHandlerSelector = mkSelector "replyWithLocalizableMessageKey:arguments:data:completionHandler:"

-- | @Selector@ for @exchangeID@
exchangeIDSelector :: Selector
exchangeIDSelector = mkSelector "exchangeID"

-- | @Selector@ for @sender@
senderSelector :: Selector
senderSelector = mkSelector "sender"

-- | @Selector@ for @recipients@
recipientsSelector :: Selector
recipientsSelector = mkSelector "recipients"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @message@
messageSelector :: Selector
messageSelector = mkSelector "message"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

-- | @Selector@ for @sendDate@
sendDateSelector :: Selector
sendDateSelector = mkSelector "sendDate"

-- | @Selector@ for @timeoutDate@
timeoutDateSelector :: Selector
timeoutDateSelector = mkSelector "timeoutDate"

-- | @Selector@ for @completionDate@
completionDateSelector :: Selector
completionDateSelector = mkSelector "completionDate"

-- | @Selector@ for @replies@
repliesSelector :: Selector
repliesSelector = mkSelector "replies"

