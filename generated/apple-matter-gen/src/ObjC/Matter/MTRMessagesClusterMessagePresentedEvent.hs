{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRMessagesClusterMessagePresentedEvent@.
module ObjC.Matter.MTRMessagesClusterMessagePresentedEvent
  ( MTRMessagesClusterMessagePresentedEvent
  , IsMTRMessagesClusterMessagePresentedEvent(..)
  , messageID
  , setMessageID
  , messageIDSelector
  , setMessageIDSelector


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
messageID :: IsMTRMessagesClusterMessagePresentedEvent mtrMessagesClusterMessagePresentedEvent => mtrMessagesClusterMessagePresentedEvent -> IO (Id NSData)
messageID mtrMessagesClusterMessagePresentedEvent =
  sendMessage mtrMessagesClusterMessagePresentedEvent messageIDSelector

-- | @- setMessageID:@
setMessageID :: (IsMTRMessagesClusterMessagePresentedEvent mtrMessagesClusterMessagePresentedEvent, IsNSData value) => mtrMessagesClusterMessagePresentedEvent -> value -> IO ()
setMessageID mtrMessagesClusterMessagePresentedEvent value =
  sendMessage mtrMessagesClusterMessagePresentedEvent setMessageIDSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @messageID@
messageIDSelector :: Selector '[] (Id NSData)
messageIDSelector = mkSelector "messageID"

-- | @Selector@ for @setMessageID:@
setMessageIDSelector :: Selector '[Id NSData] ()
setMessageIDSelector = mkSelector "setMessageID:"

