{-# LANGUAGE DataKinds #-}
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
  , dataSelector
  , messageSelector
  , recipientSelector
  , replyDateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- recipient@
recipient :: IsGKTurnBasedExchangeReply gkTurnBasedExchangeReply => gkTurnBasedExchangeReply -> IO (Id GKTurnBasedParticipant)
recipient gkTurnBasedExchangeReply =
  sendMessage gkTurnBasedExchangeReply recipientSelector

-- | @- message@
message :: IsGKTurnBasedExchangeReply gkTurnBasedExchangeReply => gkTurnBasedExchangeReply -> IO (Id NSString)
message gkTurnBasedExchangeReply =
  sendMessage gkTurnBasedExchangeReply messageSelector

-- | @- data@
data_ :: IsGKTurnBasedExchangeReply gkTurnBasedExchangeReply => gkTurnBasedExchangeReply -> IO (Id NSData)
data_ gkTurnBasedExchangeReply =
  sendMessage gkTurnBasedExchangeReply dataSelector

-- | @- replyDate@
replyDate :: IsGKTurnBasedExchangeReply gkTurnBasedExchangeReply => gkTurnBasedExchangeReply -> IO (Id NSDate)
replyDate gkTurnBasedExchangeReply =
  sendMessage gkTurnBasedExchangeReply replyDateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @recipient@
recipientSelector :: Selector '[] (Id GKTurnBasedParticipant)
recipientSelector = mkSelector "recipient"

-- | @Selector@ for @message@
messageSelector :: Selector '[] (Id NSString)
messageSelector = mkSelector "message"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSData)
dataSelector = mkSelector "data"

-- | @Selector@ for @replyDate@
replyDateSelector :: Selector '[] (Id NSDate)
replyDateSelector = mkSelector "replyDate"

