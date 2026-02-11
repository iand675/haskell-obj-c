{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSendMessageIntent@.
module ObjC.Intents.INSendMessageIntent
  ( INSendMessageIntent
  , IsINSendMessageIntent(..)
  , initWithRecipients_outgoingMessageType_content_speakableGroupName_conversationIdentifier_serviceName_sender_attachments
  , initWithRecipients_content_groupName_serviceName_sender
  , initWithRecipients_content_speakableGroupName_conversationIdentifier_serviceName_sender
  , initWithRecipients_outgoingMessageType_content_speakableGroupName_conversationIdentifier_serviceName_sender
  , recipients
  , outgoingMessageType
  , content
  , speakableGroupName
  , conversationIdentifier
  , serviceName
  , sender
  , attachments
  , groupName
  , initWithRecipients_outgoingMessageType_content_speakableGroupName_conversationIdentifier_serviceName_sender_attachmentsSelector
  , initWithRecipients_content_groupName_serviceName_senderSelector
  , initWithRecipients_content_speakableGroupName_conversationIdentifier_serviceName_senderSelector
  , initWithRecipients_outgoingMessageType_content_speakableGroupName_conversationIdentifier_serviceName_senderSelector
  , recipientsSelector
  , outgoingMessageTypeSelector
  , contentSelector
  , speakableGroupNameSelector
  , conversationIdentifierSelector
  , serviceNameSelector
  , senderSelector
  , attachmentsSelector
  , groupNameSelector

  -- * Enum types
  , INOutgoingMessageType(INOutgoingMessageType)
  , pattern INOutgoingMessageTypeUnknown
  , pattern INOutgoingMessageTypeOutgoingMessageText
  , pattern INOutgoingMessageTypeOutgoingMessageAudio

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

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithRecipients:outgoingMessageType:content:speakableGroupName:conversationIdentifier:serviceName:sender:attachments:@
initWithRecipients_outgoingMessageType_content_speakableGroupName_conversationIdentifier_serviceName_sender_attachments :: (IsINSendMessageIntent inSendMessageIntent, IsNSArray recipients, IsNSString content, IsINSpeakableString speakableGroupName, IsNSString conversationIdentifier, IsNSString serviceName, IsINPerson sender, IsNSArray attachments) => inSendMessageIntent -> recipients -> INOutgoingMessageType -> content -> speakableGroupName -> conversationIdentifier -> serviceName -> sender -> attachments -> IO (Id INSendMessageIntent)
initWithRecipients_outgoingMessageType_content_speakableGroupName_conversationIdentifier_serviceName_sender_attachments inSendMessageIntent  recipients outgoingMessageType content speakableGroupName conversationIdentifier serviceName sender attachments =
withObjCPtr recipients $ \raw_recipients ->
  withObjCPtr content $ \raw_content ->
    withObjCPtr speakableGroupName $ \raw_speakableGroupName ->
      withObjCPtr conversationIdentifier $ \raw_conversationIdentifier ->
        withObjCPtr serviceName $ \raw_serviceName ->
          withObjCPtr sender $ \raw_sender ->
            withObjCPtr attachments $ \raw_attachments ->
                sendMsg inSendMessageIntent (mkSelector "initWithRecipients:outgoingMessageType:content:speakableGroupName:conversationIdentifier:serviceName:sender:attachments:") (retPtr retVoid) [argPtr (castPtr raw_recipients :: Ptr ()), argCLong (coerce outgoingMessageType), argPtr (castPtr raw_content :: Ptr ()), argPtr (castPtr raw_speakableGroupName :: Ptr ()), argPtr (castPtr raw_conversationIdentifier :: Ptr ()), argPtr (castPtr raw_serviceName :: Ptr ()), argPtr (castPtr raw_sender :: Ptr ()), argPtr (castPtr raw_attachments :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithRecipients:content:groupName:serviceName:sender:@
initWithRecipients_content_groupName_serviceName_sender :: (IsINSendMessageIntent inSendMessageIntent, IsNSArray recipients, IsNSString content, IsNSString groupName, IsNSString serviceName, IsINPerson sender) => inSendMessageIntent -> recipients -> content -> groupName -> serviceName -> sender -> IO (Id INSendMessageIntent)
initWithRecipients_content_groupName_serviceName_sender inSendMessageIntent  recipients content groupName serviceName sender =
withObjCPtr recipients $ \raw_recipients ->
  withObjCPtr content $ \raw_content ->
    withObjCPtr groupName $ \raw_groupName ->
      withObjCPtr serviceName $ \raw_serviceName ->
        withObjCPtr sender $ \raw_sender ->
            sendMsg inSendMessageIntent (mkSelector "initWithRecipients:content:groupName:serviceName:sender:") (retPtr retVoid) [argPtr (castPtr raw_recipients :: Ptr ()), argPtr (castPtr raw_content :: Ptr ()), argPtr (castPtr raw_groupName :: Ptr ()), argPtr (castPtr raw_serviceName :: Ptr ()), argPtr (castPtr raw_sender :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithRecipients:content:speakableGroupName:conversationIdentifier:serviceName:sender:@
initWithRecipients_content_speakableGroupName_conversationIdentifier_serviceName_sender :: (IsINSendMessageIntent inSendMessageIntent, IsNSArray recipients, IsNSString content, IsINSpeakableString speakableGroupName, IsNSString conversationIdentifier, IsNSString serviceName, IsINPerson sender) => inSendMessageIntent -> recipients -> content -> speakableGroupName -> conversationIdentifier -> serviceName -> sender -> IO (Id INSendMessageIntent)
initWithRecipients_content_speakableGroupName_conversationIdentifier_serviceName_sender inSendMessageIntent  recipients content speakableGroupName conversationIdentifier serviceName sender =
withObjCPtr recipients $ \raw_recipients ->
  withObjCPtr content $ \raw_content ->
    withObjCPtr speakableGroupName $ \raw_speakableGroupName ->
      withObjCPtr conversationIdentifier $ \raw_conversationIdentifier ->
        withObjCPtr serviceName $ \raw_serviceName ->
          withObjCPtr sender $ \raw_sender ->
              sendMsg inSendMessageIntent (mkSelector "initWithRecipients:content:speakableGroupName:conversationIdentifier:serviceName:sender:") (retPtr retVoid) [argPtr (castPtr raw_recipients :: Ptr ()), argPtr (castPtr raw_content :: Ptr ()), argPtr (castPtr raw_speakableGroupName :: Ptr ()), argPtr (castPtr raw_conversationIdentifier :: Ptr ()), argPtr (castPtr raw_serviceName :: Ptr ()), argPtr (castPtr raw_sender :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithRecipients:outgoingMessageType:content:speakableGroupName:conversationIdentifier:serviceName:sender:@
initWithRecipients_outgoingMessageType_content_speakableGroupName_conversationIdentifier_serviceName_sender :: (IsINSendMessageIntent inSendMessageIntent, IsNSArray recipients, IsNSString content, IsINSpeakableString speakableGroupName, IsNSString conversationIdentifier, IsNSString serviceName, IsINPerson sender) => inSendMessageIntent -> recipients -> INOutgoingMessageType -> content -> speakableGroupName -> conversationIdentifier -> serviceName -> sender -> IO (Id INSendMessageIntent)
initWithRecipients_outgoingMessageType_content_speakableGroupName_conversationIdentifier_serviceName_sender inSendMessageIntent  recipients outgoingMessageType content speakableGroupName conversationIdentifier serviceName sender =
withObjCPtr recipients $ \raw_recipients ->
  withObjCPtr content $ \raw_content ->
    withObjCPtr speakableGroupName $ \raw_speakableGroupName ->
      withObjCPtr conversationIdentifier $ \raw_conversationIdentifier ->
        withObjCPtr serviceName $ \raw_serviceName ->
          withObjCPtr sender $ \raw_sender ->
              sendMsg inSendMessageIntent (mkSelector "initWithRecipients:outgoingMessageType:content:speakableGroupName:conversationIdentifier:serviceName:sender:") (retPtr retVoid) [argPtr (castPtr raw_recipients :: Ptr ()), argCLong (coerce outgoingMessageType), argPtr (castPtr raw_content :: Ptr ()), argPtr (castPtr raw_speakableGroupName :: Ptr ()), argPtr (castPtr raw_conversationIdentifier :: Ptr ()), argPtr (castPtr raw_serviceName :: Ptr ()), argPtr (castPtr raw_sender :: Ptr ())] >>= ownedObject . castPtr

-- | @- recipients@
recipients :: IsINSendMessageIntent inSendMessageIntent => inSendMessageIntent -> IO (Id NSArray)
recipients inSendMessageIntent  =
  sendMsg inSendMessageIntent (mkSelector "recipients") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- outgoingMessageType@
outgoingMessageType :: IsINSendMessageIntent inSendMessageIntent => inSendMessageIntent -> IO INOutgoingMessageType
outgoingMessageType inSendMessageIntent  =
  fmap (coerce :: CLong -> INOutgoingMessageType) $ sendMsg inSendMessageIntent (mkSelector "outgoingMessageType") retCLong []

-- | @- content@
content :: IsINSendMessageIntent inSendMessageIntent => inSendMessageIntent -> IO (Id NSString)
content inSendMessageIntent  =
  sendMsg inSendMessageIntent (mkSelector "content") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- speakableGroupName@
speakableGroupName :: IsINSendMessageIntent inSendMessageIntent => inSendMessageIntent -> IO (Id INSpeakableString)
speakableGroupName inSendMessageIntent  =
  sendMsg inSendMessageIntent (mkSelector "speakableGroupName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- conversationIdentifier@
conversationIdentifier :: IsINSendMessageIntent inSendMessageIntent => inSendMessageIntent -> IO (Id NSString)
conversationIdentifier inSendMessageIntent  =
  sendMsg inSendMessageIntent (mkSelector "conversationIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- serviceName@
serviceName :: IsINSendMessageIntent inSendMessageIntent => inSendMessageIntent -> IO (Id NSString)
serviceName inSendMessageIntent  =
  sendMsg inSendMessageIntent (mkSelector "serviceName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sender@
sender :: IsINSendMessageIntent inSendMessageIntent => inSendMessageIntent -> IO (Id INPerson)
sender inSendMessageIntent  =
  sendMsg inSendMessageIntent (mkSelector "sender") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- attachments@
attachments :: IsINSendMessageIntent inSendMessageIntent => inSendMessageIntent -> IO (Id NSArray)
attachments inSendMessageIntent  =
  sendMsg inSendMessageIntent (mkSelector "attachments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- groupName@
groupName :: IsINSendMessageIntent inSendMessageIntent => inSendMessageIntent -> IO (Id NSString)
groupName inSendMessageIntent  =
  sendMsg inSendMessageIntent (mkSelector "groupName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRecipients:outgoingMessageType:content:speakableGroupName:conversationIdentifier:serviceName:sender:attachments:@
initWithRecipients_outgoingMessageType_content_speakableGroupName_conversationIdentifier_serviceName_sender_attachmentsSelector :: Selector
initWithRecipients_outgoingMessageType_content_speakableGroupName_conversationIdentifier_serviceName_sender_attachmentsSelector = mkSelector "initWithRecipients:outgoingMessageType:content:speakableGroupName:conversationIdentifier:serviceName:sender:attachments:"

-- | @Selector@ for @initWithRecipients:content:groupName:serviceName:sender:@
initWithRecipients_content_groupName_serviceName_senderSelector :: Selector
initWithRecipients_content_groupName_serviceName_senderSelector = mkSelector "initWithRecipients:content:groupName:serviceName:sender:"

-- | @Selector@ for @initWithRecipients:content:speakableGroupName:conversationIdentifier:serviceName:sender:@
initWithRecipients_content_speakableGroupName_conversationIdentifier_serviceName_senderSelector :: Selector
initWithRecipients_content_speakableGroupName_conversationIdentifier_serviceName_senderSelector = mkSelector "initWithRecipients:content:speakableGroupName:conversationIdentifier:serviceName:sender:"

-- | @Selector@ for @initWithRecipients:outgoingMessageType:content:speakableGroupName:conversationIdentifier:serviceName:sender:@
initWithRecipients_outgoingMessageType_content_speakableGroupName_conversationIdentifier_serviceName_senderSelector :: Selector
initWithRecipients_outgoingMessageType_content_speakableGroupName_conversationIdentifier_serviceName_senderSelector = mkSelector "initWithRecipients:outgoingMessageType:content:speakableGroupName:conversationIdentifier:serviceName:sender:"

-- | @Selector@ for @recipients@
recipientsSelector :: Selector
recipientsSelector = mkSelector "recipients"

-- | @Selector@ for @outgoingMessageType@
outgoingMessageTypeSelector :: Selector
outgoingMessageTypeSelector = mkSelector "outgoingMessageType"

-- | @Selector@ for @content@
contentSelector :: Selector
contentSelector = mkSelector "content"

-- | @Selector@ for @speakableGroupName@
speakableGroupNameSelector :: Selector
speakableGroupNameSelector = mkSelector "speakableGroupName"

-- | @Selector@ for @conversationIdentifier@
conversationIdentifierSelector :: Selector
conversationIdentifierSelector = mkSelector "conversationIdentifier"

-- | @Selector@ for @serviceName@
serviceNameSelector :: Selector
serviceNameSelector = mkSelector "serviceName"

-- | @Selector@ for @sender@
senderSelector :: Selector
senderSelector = mkSelector "sender"

-- | @Selector@ for @attachments@
attachmentsSelector :: Selector
attachmentsSelector = mkSelector "attachments"

-- | @Selector@ for @groupName@
groupNameSelector :: Selector
groupNameSelector = mkSelector "groupName"

