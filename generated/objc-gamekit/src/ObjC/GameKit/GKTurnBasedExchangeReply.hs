{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GKTurnBasedExchangeReply@.
module ObjC.GameKit.GKTurnBasedExchangeReply
  ( GKTurnBasedExchangeReply
  , IsGKTurnBasedExchangeReply(..)
  , recipient
  , message
  , data_
  , replyDate
  , recipientSelector
  , messageSelector
  , dataSelector
  , replyDateSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- recipient@
recipient :: IsGKTurnBasedExchangeReply gkTurnBasedExchangeReply => gkTurnBasedExchangeReply -> IO (Id GKTurnBasedParticipant)
recipient gkTurnBasedExchangeReply  =
  sendMsg gkTurnBasedExchangeReply (mkSelector "recipient") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- message@
message :: IsGKTurnBasedExchangeReply gkTurnBasedExchangeReply => gkTurnBasedExchangeReply -> IO (Id NSString)
message gkTurnBasedExchangeReply  =
  sendMsg gkTurnBasedExchangeReply (mkSelector "message") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- data@
data_ :: IsGKTurnBasedExchangeReply gkTurnBasedExchangeReply => gkTurnBasedExchangeReply -> IO (Id NSData)
data_ gkTurnBasedExchangeReply  =
  sendMsg gkTurnBasedExchangeReply (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- replyDate@
replyDate :: IsGKTurnBasedExchangeReply gkTurnBasedExchangeReply => gkTurnBasedExchangeReply -> IO (Id NSDate)
replyDate gkTurnBasedExchangeReply  =
  sendMsg gkTurnBasedExchangeReply (mkSelector "replyDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @recipient@
recipientSelector :: Selector
recipientSelector = mkSelector "recipient"

-- | @Selector@ for @message@
messageSelector :: Selector
messageSelector = mkSelector "message"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

-- | @Selector@ for @replyDate@
replyDateSelector :: Selector
replyDateSelector = mkSelector "replyDate"

