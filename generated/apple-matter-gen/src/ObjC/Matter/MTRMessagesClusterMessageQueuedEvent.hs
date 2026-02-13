{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRMessagesClusterMessageQueuedEvent@.
module ObjC.Matter.MTRMessagesClusterMessageQueuedEvent
  ( MTRMessagesClusterMessageQueuedEvent
  , IsMTRMessagesClusterMessageQueuedEvent(..)
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
messageID :: IsMTRMessagesClusterMessageQueuedEvent mtrMessagesClusterMessageQueuedEvent => mtrMessagesClusterMessageQueuedEvent -> IO (Id NSData)
messageID mtrMessagesClusterMessageQueuedEvent =
  sendMessage mtrMessagesClusterMessageQueuedEvent messageIDSelector

-- | @- setMessageID:@
setMessageID :: (IsMTRMessagesClusterMessageQueuedEvent mtrMessagesClusterMessageQueuedEvent, IsNSData value) => mtrMessagesClusterMessageQueuedEvent -> value -> IO ()
setMessageID mtrMessagesClusterMessageQueuedEvent value =
  sendMessage mtrMessagesClusterMessageQueuedEvent setMessageIDSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @messageID@
messageIDSelector :: Selector '[] (Id NSData)
messageIDSelector = mkSelector "messageID"

-- | @Selector@ for @setMessageID:@
setMessageIDSelector :: Selector '[Id NSData] ()
setMessageIDSelector = mkSelector "setMessageID:"

