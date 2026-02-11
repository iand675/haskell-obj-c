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
  , initWithSendPort_receivePort_componentsSelector
  , sendBeforeDateSelector
  , componentsSelector
  , receivePortSelector
  , sendPortSelector
  , msgidSelector
  , setMsgidSelector


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

import ObjC.Foundation.Internal.Classes

-- | @- initWithSendPort:receivePort:components:@
initWithSendPort_receivePort_components :: (IsNSPortMessage nsPortMessage, IsNSPort sendPort, IsNSPort replyPort, IsNSArray components) => nsPortMessage -> sendPort -> replyPort -> components -> IO (Id NSPortMessage)
initWithSendPort_receivePort_components nsPortMessage  sendPort replyPort components =
withObjCPtr sendPort $ \raw_sendPort ->
  withObjCPtr replyPort $ \raw_replyPort ->
    withObjCPtr components $ \raw_components ->
        sendMsg nsPortMessage (mkSelector "initWithSendPort:receivePort:components:") (retPtr retVoid) [argPtr (castPtr raw_sendPort :: Ptr ()), argPtr (castPtr raw_replyPort :: Ptr ()), argPtr (castPtr raw_components :: Ptr ())] >>= ownedObject . castPtr

-- | @- sendBeforeDate:@
sendBeforeDate :: (IsNSPortMessage nsPortMessage, IsNSDate date) => nsPortMessage -> date -> IO Bool
sendBeforeDate nsPortMessage  date =
withObjCPtr date $ \raw_date ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPortMessage (mkSelector "sendBeforeDate:") retCULong [argPtr (castPtr raw_date :: Ptr ())]

-- | @- components@
components :: IsNSPortMessage nsPortMessage => nsPortMessage -> IO (Id NSArray)
components nsPortMessage  =
  sendMsg nsPortMessage (mkSelector "components") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- receivePort@
receivePort :: IsNSPortMessage nsPortMessage => nsPortMessage -> IO (Id NSPort)
receivePort nsPortMessage  =
  sendMsg nsPortMessage (mkSelector "receivePort") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sendPort@
sendPort :: IsNSPortMessage nsPortMessage => nsPortMessage -> IO (Id NSPort)
sendPort nsPortMessage  =
  sendMsg nsPortMessage (mkSelector "sendPort") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- msgid@
msgid :: IsNSPortMessage nsPortMessage => nsPortMessage -> IO CUInt
msgid nsPortMessage  =
  sendMsg nsPortMessage (mkSelector "msgid") retCUInt []

-- | @- setMsgid:@
setMsgid :: IsNSPortMessage nsPortMessage => nsPortMessage -> CUInt -> IO ()
setMsgid nsPortMessage  value =
  sendMsg nsPortMessage (mkSelector "setMsgid:") retVoid [argCUInt (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSendPort:receivePort:components:@
initWithSendPort_receivePort_componentsSelector :: Selector
initWithSendPort_receivePort_componentsSelector = mkSelector "initWithSendPort:receivePort:components:"

-- | @Selector@ for @sendBeforeDate:@
sendBeforeDateSelector :: Selector
sendBeforeDateSelector = mkSelector "sendBeforeDate:"

-- | @Selector@ for @components@
componentsSelector :: Selector
componentsSelector = mkSelector "components"

-- | @Selector@ for @receivePort@
receivePortSelector :: Selector
receivePortSelector = mkSelector "receivePort"

-- | @Selector@ for @sendPort@
sendPortSelector :: Selector
sendPortSelector = mkSelector "sendPort"

-- | @Selector@ for @msgid@
msgidSelector :: Selector
msgidSelector = mkSelector "msgid"

-- | @Selector@ for @setMsgid:@
setMsgidSelector :: Selector
setMsgidSelector = mkSelector "setMsgid:"

