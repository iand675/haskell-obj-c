{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , attachmentsSelector
  , contentSelector
  , conversationIdentifierSelector
  , groupNameSelector
  , initWithRecipients_content_groupName_serviceName_senderSelector
  , initWithRecipients_content_speakableGroupName_conversationIdentifier_serviceName_senderSelector
  , initWithRecipients_outgoingMessageType_content_speakableGroupName_conversationIdentifier_serviceName_senderSelector
  , initWithRecipients_outgoingMessageType_content_speakableGroupName_conversationIdentifier_serviceName_sender_attachmentsSelector
  , outgoingMessageTypeSelector
  , recipientsSelector
  , senderSelector
  , serviceNameSelector
  , speakableGroupNameSelector

  -- * Enum types
  , INOutgoingMessageType(INOutgoingMessageType)
  , pattern INOutgoingMessageTypeUnknown
  , pattern INOutgoingMessageTypeOutgoingMessageText
  , pattern INOutgoingMessageTypeOutgoingMessageAudio

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithRecipients:outgoingMessageType:content:speakableGroupName:conversationIdentifier:serviceName:sender:attachments:@
initWithRecipients_outgoingMessageType_content_speakableGroupName_conversationIdentifier_serviceName_sender_attachments :: (IsINSendMessageIntent inSendMessageIntent, IsNSArray recipients, IsNSString content, IsINSpeakableString speakableGroupName, IsNSString conversationIdentifier, IsNSString serviceName, IsINPerson sender, IsNSArray attachments) => inSendMessageIntent -> recipients -> INOutgoingMessageType -> content -> speakableGroupName -> conversationIdentifier -> serviceName -> sender -> attachments -> IO (Id INSendMessageIntent)
initWithRecipients_outgoingMessageType_content_speakableGroupName_conversationIdentifier_serviceName_sender_attachments inSendMessageIntent recipients outgoingMessageType content speakableGroupName conversationIdentifier serviceName sender attachments =
  sendOwnedMessage inSendMessageIntent initWithRecipients_outgoingMessageType_content_speakableGroupName_conversationIdentifier_serviceName_sender_attachmentsSelector (toNSArray recipients) outgoingMessageType (toNSString content) (toINSpeakableString speakableGroupName) (toNSString conversationIdentifier) (toNSString serviceName) (toINPerson sender) (toNSArray attachments)

-- | @- initWithRecipients:content:groupName:serviceName:sender:@
initWithRecipients_content_groupName_serviceName_sender :: (IsINSendMessageIntent inSendMessageIntent, IsNSArray recipients, IsNSString content, IsNSString groupName, IsNSString serviceName, IsINPerson sender) => inSendMessageIntent -> recipients -> content -> groupName -> serviceName -> sender -> IO (Id INSendMessageIntent)
initWithRecipients_content_groupName_serviceName_sender inSendMessageIntent recipients content groupName serviceName sender =
  sendOwnedMessage inSendMessageIntent initWithRecipients_content_groupName_serviceName_senderSelector (toNSArray recipients) (toNSString content) (toNSString groupName) (toNSString serviceName) (toINPerson sender)

-- | @- initWithRecipients:content:speakableGroupName:conversationIdentifier:serviceName:sender:@
initWithRecipients_content_speakableGroupName_conversationIdentifier_serviceName_sender :: (IsINSendMessageIntent inSendMessageIntent, IsNSArray recipients, IsNSString content, IsINSpeakableString speakableGroupName, IsNSString conversationIdentifier, IsNSString serviceName, IsINPerson sender) => inSendMessageIntent -> recipients -> content -> speakableGroupName -> conversationIdentifier -> serviceName -> sender -> IO (Id INSendMessageIntent)
initWithRecipients_content_speakableGroupName_conversationIdentifier_serviceName_sender inSendMessageIntent recipients content speakableGroupName conversationIdentifier serviceName sender =
  sendOwnedMessage inSendMessageIntent initWithRecipients_content_speakableGroupName_conversationIdentifier_serviceName_senderSelector (toNSArray recipients) (toNSString content) (toINSpeakableString speakableGroupName) (toNSString conversationIdentifier) (toNSString serviceName) (toINPerson sender)

-- | @- initWithRecipients:outgoingMessageType:content:speakableGroupName:conversationIdentifier:serviceName:sender:@
initWithRecipients_outgoingMessageType_content_speakableGroupName_conversationIdentifier_serviceName_sender :: (IsINSendMessageIntent inSendMessageIntent, IsNSArray recipients, IsNSString content, IsINSpeakableString speakableGroupName, IsNSString conversationIdentifier, IsNSString serviceName, IsINPerson sender) => inSendMessageIntent -> recipients -> INOutgoingMessageType -> content -> speakableGroupName -> conversationIdentifier -> serviceName -> sender -> IO (Id INSendMessageIntent)
initWithRecipients_outgoingMessageType_content_speakableGroupName_conversationIdentifier_serviceName_sender inSendMessageIntent recipients outgoingMessageType content speakableGroupName conversationIdentifier serviceName sender =
  sendOwnedMessage inSendMessageIntent initWithRecipients_outgoingMessageType_content_speakableGroupName_conversationIdentifier_serviceName_senderSelector (toNSArray recipients) outgoingMessageType (toNSString content) (toINSpeakableString speakableGroupName) (toNSString conversationIdentifier) (toNSString serviceName) (toINPerson sender)

-- | @- recipients@
recipients :: IsINSendMessageIntent inSendMessageIntent => inSendMessageIntent -> IO (Id NSArray)
recipients inSendMessageIntent =
  sendMessage inSendMessageIntent recipientsSelector

-- | @- outgoingMessageType@
outgoingMessageType :: IsINSendMessageIntent inSendMessageIntent => inSendMessageIntent -> IO INOutgoingMessageType
outgoingMessageType inSendMessageIntent =
  sendMessage inSendMessageIntent outgoingMessageTypeSelector

-- | @- content@
content :: IsINSendMessageIntent inSendMessageIntent => inSendMessageIntent -> IO (Id NSString)
content inSendMessageIntent =
  sendMessage inSendMessageIntent contentSelector

-- | @- speakableGroupName@
speakableGroupName :: IsINSendMessageIntent inSendMessageIntent => inSendMessageIntent -> IO (Id INSpeakableString)
speakableGroupName inSendMessageIntent =
  sendMessage inSendMessageIntent speakableGroupNameSelector

-- | @- conversationIdentifier@
conversationIdentifier :: IsINSendMessageIntent inSendMessageIntent => inSendMessageIntent -> IO (Id NSString)
conversationIdentifier inSendMessageIntent =
  sendMessage inSendMessageIntent conversationIdentifierSelector

-- | @- serviceName@
serviceName :: IsINSendMessageIntent inSendMessageIntent => inSendMessageIntent -> IO (Id NSString)
serviceName inSendMessageIntent =
  sendMessage inSendMessageIntent serviceNameSelector

-- | @- sender@
sender :: IsINSendMessageIntent inSendMessageIntent => inSendMessageIntent -> IO (Id INPerson)
sender inSendMessageIntent =
  sendMessage inSendMessageIntent senderSelector

-- | @- attachments@
attachments :: IsINSendMessageIntent inSendMessageIntent => inSendMessageIntent -> IO (Id NSArray)
attachments inSendMessageIntent =
  sendMessage inSendMessageIntent attachmentsSelector

-- | @- groupName@
groupName :: IsINSendMessageIntent inSendMessageIntent => inSendMessageIntent -> IO (Id NSString)
groupName inSendMessageIntent =
  sendMessage inSendMessageIntent groupNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRecipients:outgoingMessageType:content:speakableGroupName:conversationIdentifier:serviceName:sender:attachments:@
initWithRecipients_outgoingMessageType_content_speakableGroupName_conversationIdentifier_serviceName_sender_attachmentsSelector :: Selector '[Id NSArray, INOutgoingMessageType, Id NSString, Id INSpeakableString, Id NSString, Id NSString, Id INPerson, Id NSArray] (Id INSendMessageIntent)
initWithRecipients_outgoingMessageType_content_speakableGroupName_conversationIdentifier_serviceName_sender_attachmentsSelector = mkSelector "initWithRecipients:outgoingMessageType:content:speakableGroupName:conversationIdentifier:serviceName:sender:attachments:"

-- | @Selector@ for @initWithRecipients:content:groupName:serviceName:sender:@
initWithRecipients_content_groupName_serviceName_senderSelector :: Selector '[Id NSArray, Id NSString, Id NSString, Id NSString, Id INPerson] (Id INSendMessageIntent)
initWithRecipients_content_groupName_serviceName_senderSelector = mkSelector "initWithRecipients:content:groupName:serviceName:sender:"

-- | @Selector@ for @initWithRecipients:content:speakableGroupName:conversationIdentifier:serviceName:sender:@
initWithRecipients_content_speakableGroupName_conversationIdentifier_serviceName_senderSelector :: Selector '[Id NSArray, Id NSString, Id INSpeakableString, Id NSString, Id NSString, Id INPerson] (Id INSendMessageIntent)
initWithRecipients_content_speakableGroupName_conversationIdentifier_serviceName_senderSelector = mkSelector "initWithRecipients:content:speakableGroupName:conversationIdentifier:serviceName:sender:"

-- | @Selector@ for @initWithRecipients:outgoingMessageType:content:speakableGroupName:conversationIdentifier:serviceName:sender:@
initWithRecipients_outgoingMessageType_content_speakableGroupName_conversationIdentifier_serviceName_senderSelector :: Selector '[Id NSArray, INOutgoingMessageType, Id NSString, Id INSpeakableString, Id NSString, Id NSString, Id INPerson] (Id INSendMessageIntent)
initWithRecipients_outgoingMessageType_content_speakableGroupName_conversationIdentifier_serviceName_senderSelector = mkSelector "initWithRecipients:outgoingMessageType:content:speakableGroupName:conversationIdentifier:serviceName:sender:"

-- | @Selector@ for @recipients@
recipientsSelector :: Selector '[] (Id NSArray)
recipientsSelector = mkSelector "recipients"

-- | @Selector@ for @outgoingMessageType@
outgoingMessageTypeSelector :: Selector '[] INOutgoingMessageType
outgoingMessageTypeSelector = mkSelector "outgoingMessageType"

-- | @Selector@ for @content@
contentSelector :: Selector '[] (Id NSString)
contentSelector = mkSelector "content"

-- | @Selector@ for @speakableGroupName@
speakableGroupNameSelector :: Selector '[] (Id INSpeakableString)
speakableGroupNameSelector = mkSelector "speakableGroupName"

-- | @Selector@ for @conversationIdentifier@
conversationIdentifierSelector :: Selector '[] (Id NSString)
conversationIdentifierSelector = mkSelector "conversationIdentifier"

-- | @Selector@ for @serviceName@
serviceNameSelector :: Selector '[] (Id NSString)
serviceNameSelector = mkSelector "serviceName"

-- | @Selector@ for @sender@
senderSelector :: Selector '[] (Id INPerson)
senderSelector = mkSelector "sender"

-- | @Selector@ for @attachments@
attachmentsSelector :: Selector '[] (Id NSArray)
attachmentsSelector = mkSelector "attachments"

-- | @Selector@ for @groupName@
groupNameSelector :: Selector '[] (Id NSString)
groupNameSelector = mkSelector "groupName"

