{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPortMessage@.
module ObjC.Foundation.NSPortMessage
  ( NSPortMessage
  , IsNSPortMessage(..)
  , initWithSendPort_receivePort_components
  , sendBeforeDate
  , components
  , receivePort
  , sendPort
  , msgid
  , setMsgid
  , componentsSelector
  , initWithSendPort_receivePort_componentsSelector
  , msgidSelector
  , receivePortSelector
  , sendBeforeDateSelector
  , sendPortSelector
  , setMsgidSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- initWithSendPort:receivePort:components:@
initWithSendPort_receivePort_components :: (IsNSPortMessage nsPortMessage, IsNSPort sendPort, IsNSPort replyPort, IsNSArray components) => nsPortMessage -> sendPort -> replyPort -> components -> IO (Id NSPortMessage)
initWithSendPort_receivePort_components nsPortMessage sendPort replyPort components =
  sendOwnedMessage nsPortMessage initWithSendPort_receivePort_componentsSelector (toNSPort sendPort) (toNSPort replyPort) (toNSArray components)

-- | @- sendBeforeDate:@
sendBeforeDate :: (IsNSPortMessage nsPortMessage, IsNSDate date) => nsPortMessage -> date -> IO Bool
sendBeforeDate nsPortMessage date =
  sendMessage nsPortMessage sendBeforeDateSelector (toNSDate date)

-- | @- components@
components :: IsNSPortMessage nsPortMessage => nsPortMessage -> IO (Id NSArray)
components nsPortMessage =
  sendMessage nsPortMessage componentsSelector

-- | @- receivePort@
receivePort :: IsNSPortMessage nsPortMessage => nsPortMessage -> IO (Id NSPort)
receivePort nsPortMessage =
  sendMessage nsPortMessage receivePortSelector

-- | @- sendPort@
sendPort :: IsNSPortMessage nsPortMessage => nsPortMessage -> IO (Id NSPort)
sendPort nsPortMessage =
  sendMessage nsPortMessage sendPortSelector

-- | @- msgid@
msgid :: IsNSPortMessage nsPortMessage => nsPortMessage -> IO CUInt
msgid nsPortMessage =
  sendMessage nsPortMessage msgidSelector

-- | @- setMsgid:@
setMsgid :: IsNSPortMessage nsPortMessage => nsPortMessage -> CUInt -> IO ()
setMsgid nsPortMessage value =
  sendMessage nsPortMessage setMsgidSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSendPort:receivePort:components:@
initWithSendPort_receivePort_componentsSelector :: Selector '[Id NSPort, Id NSPort, Id NSArray] (Id NSPortMessage)
initWithSendPort_receivePort_componentsSelector = mkSelector "initWithSendPort:receivePort:components:"

-- | @Selector@ for @sendBeforeDate:@
sendBeforeDateSelector :: Selector '[Id NSDate] Bool
sendBeforeDateSelector = mkSelector "sendBeforeDate:"

-- | @Selector@ for @components@
componentsSelector :: Selector '[] (Id NSArray)
componentsSelector = mkSelector "components"

-- | @Selector@ for @receivePort@
receivePortSelector :: Selector '[] (Id NSPort)
receivePortSelector = mkSelector "receivePort"

-- | @Selector@ for @sendPort@
sendPortSelector :: Selector '[] (Id NSPort)
sendPortSelector = mkSelector "sendPort"

-- | @Selector@ for @msgid@
msgidSelector :: Selector '[] CUInt
msgidSelector = mkSelector "msgid"

-- | @Selector@ for @setMsgid:@
setMsgidSelector :: Selector '[CUInt] ()
setMsgidSelector = mkSelector "setMsgid:"

