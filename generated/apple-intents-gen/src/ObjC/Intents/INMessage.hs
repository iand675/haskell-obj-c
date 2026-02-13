{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INMessage@.
module ObjC.Intents.INMessage
  ( INMessage
  , IsINMessage(..)
  , init_
  , initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType_serviceName_attachmentFiles
  , initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType_serviceName_audioMessageFile
  , initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType_serviceName
  , initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType
  , initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_messageType
  , initWithIdentifier_content_dateSent_sender_recipients
  , initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_linkMetadata
  , initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_messageType_numberOfAttachments
  , initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_messageType_referencedMessage_reaction
  , initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_messageType_referencedMessage_sticker_reaction
  , identifier
  , conversationIdentifier
  , content
  , dateSent
  , sender
  , recipients
  , groupName
  , messageType
  , serviceName
  , attachmentFiles
  , numberOfAttachments
  , audioMessageFile
  , linkMetadata
  , sticker
  , setSticker
  , reaction
  , setReaction
  , attachmentFilesSelector
  , audioMessageFileSelector
  , contentSelector
  , conversationIdentifierSelector
  , dateSentSelector
  , groupNameSelector
  , identifierSelector
  , initSelector
  , initWithIdentifier_content_dateSent_sender_recipientsSelector
  , initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageTypeSelector
  , initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType_serviceNameSelector
  , initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType_serviceName_attachmentFilesSelector
  , initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType_serviceName_audioMessageFileSelector
  , initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_linkMetadataSelector
  , initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_messageType_numberOfAttachmentsSelector
  , initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_messageType_referencedMessage_reactionSelector
  , initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_messageType_referencedMessage_sticker_reactionSelector
  , initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_messageTypeSelector
  , linkMetadataSelector
  , messageTypeSelector
  , numberOfAttachmentsSelector
  , reactionSelector
  , recipientsSelector
  , senderSelector
  , serviceNameSelector
  , setReactionSelector
  , setStickerSelector
  , stickerSelector

  -- * Enum types
  , INMessageType(INMessageType)
  , pattern INMessageTypeUnspecified
  , pattern INMessageTypeText
  , pattern INMessageTypeAudio
  , pattern INMessageTypeDigitalTouch
  , pattern INMessageTypeHandwriting
  , pattern INMessageTypeSticker
  , pattern INMessageTypeTapbackLiked
  , pattern INMessageTypeTapbackDisliked
  , pattern INMessageTypeTapbackEmphasized
  , pattern INMessageTypeTapbackLoved
  , pattern INMessageTypeTapbackQuestioned
  , pattern INMessageTypeTapbackLaughed
  , pattern INMessageTypeMediaCalendar
  , pattern INMessageTypeMediaLocation
  , pattern INMessageTypeMediaAddressCard
  , pattern INMessageTypeMediaImage
  , pattern INMessageTypeMediaVideo
  , pattern INMessageTypeMediaPass
  , pattern INMessageTypeMediaAudio
  , pattern INMessageTypePaymentSent
  , pattern INMessageTypePaymentRequest
  , pattern INMessageTypePaymentNote
  , pattern INMessageTypeAnimoji
  , pattern INMessageTypeActivitySnippet
  , pattern INMessageTypeFile
  , pattern INMessageTypeLink
  , pattern INMessageTypeReaction
  , pattern INMessageTypeMediaAnimatedImage
  , pattern INMessageTypeThirdPartyAttachment

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

-- | @- init@
init_ :: IsINMessage inMessage => inMessage -> IO RawId
init_ inMessage =
  sendOwnedMessage inMessage initSelector

-- | @- initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:messageType:serviceName:attachmentFiles:@
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType_serviceName_attachmentFiles :: (IsINMessage inMessage, IsNSString identifier, IsNSString conversationIdentifier, IsNSString content, IsNSDate dateSent, IsINPerson sender, IsNSArray recipients, IsINSpeakableString groupName, IsNSString serviceName, IsNSArray attachmentFiles) => inMessage -> identifier -> conversationIdentifier -> content -> dateSent -> sender -> recipients -> groupName -> INMessageType -> serviceName -> attachmentFiles -> IO (Id INMessage)
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType_serviceName_attachmentFiles inMessage identifier conversationIdentifier content dateSent sender recipients groupName messageType serviceName attachmentFiles =
  sendOwnedMessage inMessage initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType_serviceName_attachmentFilesSelector (toNSString identifier) (toNSString conversationIdentifier) (toNSString content) (toNSDate dateSent) (toINPerson sender) (toNSArray recipients) (toINSpeakableString groupName) messageType (toNSString serviceName) (toNSArray attachmentFiles)

-- | @- initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:messageType:serviceName:audioMessageFile:@
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType_serviceName_audioMessageFile :: (IsINMessage inMessage, IsNSString identifier, IsNSString conversationIdentifier, IsNSString content, IsNSDate dateSent, IsINPerson sender, IsNSArray recipients, IsINSpeakableString groupName, IsNSString serviceName, IsINFile audioMessageFile) => inMessage -> identifier -> conversationIdentifier -> content -> dateSent -> sender -> recipients -> groupName -> INMessageType -> serviceName -> audioMessageFile -> IO (Id INMessage)
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType_serviceName_audioMessageFile inMessage identifier conversationIdentifier content dateSent sender recipients groupName messageType serviceName audioMessageFile =
  sendOwnedMessage inMessage initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType_serviceName_audioMessageFileSelector (toNSString identifier) (toNSString conversationIdentifier) (toNSString content) (toNSDate dateSent) (toINPerson sender) (toNSArray recipients) (toINSpeakableString groupName) messageType (toNSString serviceName) (toINFile audioMessageFile)

-- | @- initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:messageType:serviceName:@
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType_serviceName :: (IsINMessage inMessage, IsNSString identifier, IsNSString conversationIdentifier, IsNSString content, IsNSDate dateSent, IsINPerson sender, IsNSArray recipients, IsINSpeakableString groupName, IsNSString serviceName) => inMessage -> identifier -> conversationIdentifier -> content -> dateSent -> sender -> recipients -> groupName -> INMessageType -> serviceName -> IO (Id INMessage)
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType_serviceName inMessage identifier conversationIdentifier content dateSent sender recipients groupName messageType serviceName =
  sendOwnedMessage inMessage initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType_serviceNameSelector (toNSString identifier) (toNSString conversationIdentifier) (toNSString content) (toNSDate dateSent) (toINPerson sender) (toNSArray recipients) (toINSpeakableString groupName) messageType (toNSString serviceName)

-- | @- initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:messageType:@
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType :: (IsINMessage inMessage, IsNSString identifier, IsNSString conversationIdentifier, IsNSString content, IsNSDate dateSent, IsINPerson sender, IsNSArray recipients, IsINSpeakableString groupName) => inMessage -> identifier -> conversationIdentifier -> content -> dateSent -> sender -> recipients -> groupName -> INMessageType -> IO (Id INMessage)
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType inMessage identifier conversationIdentifier content dateSent sender recipients groupName messageType =
  sendOwnedMessage inMessage initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageTypeSelector (toNSString identifier) (toNSString conversationIdentifier) (toNSString content) (toNSDate dateSent) (toINPerson sender) (toNSArray recipients) (toINSpeakableString groupName) messageType

-- | @- initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:messageType:@
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_messageType :: (IsINMessage inMessage, IsNSString identifier, IsNSString conversationIdentifier, IsNSString content, IsNSDate dateSent, IsINPerson sender, IsNSArray recipients) => inMessage -> identifier -> conversationIdentifier -> content -> dateSent -> sender -> recipients -> INMessageType -> IO (Id INMessage)
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_messageType inMessage identifier conversationIdentifier content dateSent sender recipients messageType =
  sendOwnedMessage inMessage initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_messageTypeSelector (toNSString identifier) (toNSString conversationIdentifier) (toNSString content) (toNSDate dateSent) (toINPerson sender) (toNSArray recipients) messageType

-- | @- initWithIdentifier:content:dateSent:sender:recipients:@
initWithIdentifier_content_dateSent_sender_recipients :: (IsINMessage inMessage, IsNSString identifier, IsNSString content, IsNSDate dateSent, IsINPerson sender, IsNSArray recipients) => inMessage -> identifier -> content -> dateSent -> sender -> recipients -> IO (Id INMessage)
initWithIdentifier_content_dateSent_sender_recipients inMessage identifier content dateSent sender recipients =
  sendOwnedMessage inMessage initWithIdentifier_content_dateSent_sender_recipientsSelector (toNSString identifier) (toNSString content) (toNSDate dateSent) (toINPerson sender) (toNSArray recipients)

-- | @- initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:serviceName:linkMetadata:@
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_linkMetadata :: (IsINMessage inMessage, IsNSString identifier, IsNSString conversationIdentifier, IsNSString content, IsNSDate dateSent, IsINPerson sender, IsNSArray recipients, IsINSpeakableString groupName, IsNSString serviceName, IsINMessageLinkMetadata linkMetadata) => inMessage -> identifier -> conversationIdentifier -> content -> dateSent -> sender -> recipients -> groupName -> serviceName -> linkMetadata -> IO (Id INMessage)
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_linkMetadata inMessage identifier conversationIdentifier content dateSent sender recipients groupName serviceName linkMetadata =
  sendOwnedMessage inMessage initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_linkMetadataSelector (toNSString identifier) (toNSString conversationIdentifier) (toNSString content) (toNSDate dateSent) (toINPerson sender) (toNSArray recipients) (toINSpeakableString groupName) (toNSString serviceName) (toINMessageLinkMetadata linkMetadata)

-- | @- initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:serviceName:messageType:numberOfAttachments:@
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_messageType_numberOfAttachments :: (IsINMessage inMessage, IsNSString identifier, IsNSString conversationIdentifier, IsNSString content, IsNSDate dateSent, IsINPerson sender, IsNSArray recipients, IsINSpeakableString groupName, IsNSString serviceName, IsNSNumber numberOfAttachments) => inMessage -> identifier -> conversationIdentifier -> content -> dateSent -> sender -> recipients -> groupName -> serviceName -> INMessageType -> numberOfAttachments -> IO (Id INMessage)
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_messageType_numberOfAttachments inMessage identifier conversationIdentifier content dateSent sender recipients groupName serviceName messageType numberOfAttachments =
  sendOwnedMessage inMessage initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_messageType_numberOfAttachmentsSelector (toNSString identifier) (toNSString conversationIdentifier) (toNSString content) (toNSDate dateSent) (toINPerson sender) (toNSArray recipients) (toINSpeakableString groupName) (toNSString serviceName) messageType (toNSNumber numberOfAttachments)

-- | @- initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:serviceName:messageType:referencedMessage:reaction:@
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_messageType_referencedMessage_reaction :: (IsINMessage inMessage, IsNSString identifier, IsNSString conversationIdentifier, IsNSString content, IsNSDate dateSent, IsINPerson sender, IsNSArray recipients, IsINSpeakableString groupName, IsNSString serviceName, IsINMessage referencedMessage, IsINMessageReaction reaction) => inMessage -> identifier -> conversationIdentifier -> content -> dateSent -> sender -> recipients -> groupName -> serviceName -> INMessageType -> referencedMessage -> reaction -> IO (Id INMessage)
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_messageType_referencedMessage_reaction inMessage identifier conversationIdentifier content dateSent sender recipients groupName serviceName messageType referencedMessage reaction =
  sendOwnedMessage inMessage initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_messageType_referencedMessage_reactionSelector (toNSString identifier) (toNSString conversationIdentifier) (toNSString content) (toNSDate dateSent) (toINPerson sender) (toNSArray recipients) (toINSpeakableString groupName) (toNSString serviceName) messageType (toINMessage referencedMessage) (toINMessageReaction reaction)

-- | Creates a message that includes a reaction and references the original message for the reaction.
--
-- - Parameters:   - identifier: The message’s unique identifier.   - conversationIdentifier: The identifier of the conversation that contains this message.   - content: The text that Siri recites to the message recipient.   - dateSent: The date and time the app sent the message to each recipient.   - sender: The person who sent the message.   - recipients: The people who received the message.   - groupName: The name of the group conversation.   - serviceName: The name of the service that delivers the message.   - messageType: The type of content the message contains.   - referencedMessage: The referenced message that received a reaction if the message object itself was a reaction.   - sticker: The sticker that this message contains.   - reaction: The message reaction that this message contains.
--
-- ObjC selector: @- initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:serviceName:messageType:referencedMessage:sticker:reaction:@
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_messageType_referencedMessage_sticker_reaction :: (IsINMessage inMessage, IsNSString identifier, IsNSString conversationIdentifier, IsNSString content, IsNSDate dateSent, IsINPerson sender, IsNSArray recipients, IsINSpeakableString groupName, IsNSString serviceName, IsINMessage referencedMessage, IsINSticker sticker, IsINMessageReaction reaction) => inMessage -> identifier -> conversationIdentifier -> content -> dateSent -> sender -> recipients -> groupName -> serviceName -> INMessageType -> referencedMessage -> sticker -> reaction -> IO (Id INMessage)
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_messageType_referencedMessage_sticker_reaction inMessage identifier conversationIdentifier content dateSent sender recipients groupName serviceName messageType referencedMessage sticker reaction =
  sendOwnedMessage inMessage initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_messageType_referencedMessage_sticker_reactionSelector (toNSString identifier) (toNSString conversationIdentifier) (toNSString content) (toNSDate dateSent) (toINPerson sender) (toNSArray recipients) (toINSpeakableString groupName) (toNSString serviceName) messageType (toINMessage referencedMessage) (toINSticker sticker) (toINMessageReaction reaction)

-- | @- identifier@
identifier :: IsINMessage inMessage => inMessage -> IO (Id NSString)
identifier inMessage =
  sendMessage inMessage identifierSelector

-- | @- conversationIdentifier@
conversationIdentifier :: IsINMessage inMessage => inMessage -> IO (Id NSString)
conversationIdentifier inMessage =
  sendMessage inMessage conversationIdentifierSelector

-- | @- content@
content :: IsINMessage inMessage => inMessage -> IO (Id NSString)
content inMessage =
  sendMessage inMessage contentSelector

-- | @- dateSent@
dateSent :: IsINMessage inMessage => inMessage -> IO (Id NSDate)
dateSent inMessage =
  sendMessage inMessage dateSentSelector

-- | @- sender@
sender :: IsINMessage inMessage => inMessage -> IO (Id INPerson)
sender inMessage =
  sendMessage inMessage senderSelector

-- | @- recipients@
recipients :: IsINMessage inMessage => inMessage -> IO (Id NSArray)
recipients inMessage =
  sendMessage inMessage recipientsSelector

-- | @- groupName@
groupName :: IsINMessage inMessage => inMessage -> IO (Id INSpeakableString)
groupName inMessage =
  sendMessage inMessage groupNameSelector

-- | @- messageType@
messageType :: IsINMessage inMessage => inMessage -> IO INMessageType
messageType inMessage =
  sendMessage inMessage messageTypeSelector

-- | @- serviceName@
serviceName :: IsINMessage inMessage => inMessage -> IO (Id NSString)
serviceName inMessage =
  sendMessage inMessage serviceNameSelector

-- | @- attachmentFiles@
attachmentFiles :: IsINMessage inMessage => inMessage -> IO (Id NSArray)
attachmentFiles inMessage =
  sendMessage inMessage attachmentFilesSelector

-- | @- numberOfAttachments@
numberOfAttachments :: IsINMessage inMessage => inMessage -> IO (Id NSNumber)
numberOfAttachments inMessage =
  sendMessage inMessage numberOfAttachmentsSelector

-- | @- audioMessageFile@
audioMessageFile :: IsINMessage inMessage => inMessage -> IO (Id INFile)
audioMessageFile inMessage =
  sendMessage inMessage audioMessageFileSelector

-- | @- linkMetadata@
linkMetadata :: IsINMessage inMessage => inMessage -> IO (Id INMessageLinkMetadata)
linkMetadata inMessage =
  sendMessage inMessage linkMetadataSelector

-- | The sticker that this message contains.
--
-- ObjC selector: @- sticker@
sticker :: IsINMessage inMessage => inMessage -> IO (Id INSticker)
sticker inMessage =
  sendMessage inMessage stickerSelector

-- | The sticker that this message contains.
--
-- ObjC selector: @- setSticker:@
setSticker :: (IsINMessage inMessage, IsINSticker value) => inMessage -> value -> IO ()
setSticker inMessage value =
  sendMessage inMessage setStickerSelector (toINSticker value)

-- | The message reaction that this message contains.
--
-- ObjC selector: @- reaction@
reaction :: IsINMessage inMessage => inMessage -> IO (Id INMessageReaction)
reaction inMessage =
  sendMessage inMessage reactionSelector

-- | The message reaction that this message contains.
--
-- ObjC selector: @- setReaction:@
setReaction :: (IsINMessage inMessage, IsINMessageReaction value) => inMessage -> value -> IO ()
setReaction inMessage value =
  sendMessage inMessage setReactionSelector (toINMessageReaction value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:messageType:serviceName:attachmentFiles:@
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType_serviceName_attachmentFilesSelector :: Selector '[Id NSString, Id NSString, Id NSString, Id NSDate, Id INPerson, Id NSArray, Id INSpeakableString, INMessageType, Id NSString, Id NSArray] (Id INMessage)
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType_serviceName_attachmentFilesSelector = mkSelector "initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:messageType:serviceName:attachmentFiles:"

-- | @Selector@ for @initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:messageType:serviceName:audioMessageFile:@
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType_serviceName_audioMessageFileSelector :: Selector '[Id NSString, Id NSString, Id NSString, Id NSDate, Id INPerson, Id NSArray, Id INSpeakableString, INMessageType, Id NSString, Id INFile] (Id INMessage)
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType_serviceName_audioMessageFileSelector = mkSelector "initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:messageType:serviceName:audioMessageFile:"

-- | @Selector@ for @initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:messageType:serviceName:@
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType_serviceNameSelector :: Selector '[Id NSString, Id NSString, Id NSString, Id NSDate, Id INPerson, Id NSArray, Id INSpeakableString, INMessageType, Id NSString] (Id INMessage)
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType_serviceNameSelector = mkSelector "initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:messageType:serviceName:"

-- | @Selector@ for @initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:messageType:@
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageTypeSelector :: Selector '[Id NSString, Id NSString, Id NSString, Id NSDate, Id INPerson, Id NSArray, Id INSpeakableString, INMessageType] (Id INMessage)
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageTypeSelector = mkSelector "initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:messageType:"

-- | @Selector@ for @initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:messageType:@
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_messageTypeSelector :: Selector '[Id NSString, Id NSString, Id NSString, Id NSDate, Id INPerson, Id NSArray, INMessageType] (Id INMessage)
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_messageTypeSelector = mkSelector "initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:messageType:"

-- | @Selector@ for @initWithIdentifier:content:dateSent:sender:recipients:@
initWithIdentifier_content_dateSent_sender_recipientsSelector :: Selector '[Id NSString, Id NSString, Id NSDate, Id INPerson, Id NSArray] (Id INMessage)
initWithIdentifier_content_dateSent_sender_recipientsSelector = mkSelector "initWithIdentifier:content:dateSent:sender:recipients:"

-- | @Selector@ for @initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:serviceName:linkMetadata:@
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_linkMetadataSelector :: Selector '[Id NSString, Id NSString, Id NSString, Id NSDate, Id INPerson, Id NSArray, Id INSpeakableString, Id NSString, Id INMessageLinkMetadata] (Id INMessage)
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_linkMetadataSelector = mkSelector "initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:serviceName:linkMetadata:"

-- | @Selector@ for @initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:serviceName:messageType:numberOfAttachments:@
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_messageType_numberOfAttachmentsSelector :: Selector '[Id NSString, Id NSString, Id NSString, Id NSDate, Id INPerson, Id NSArray, Id INSpeakableString, Id NSString, INMessageType, Id NSNumber] (Id INMessage)
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_messageType_numberOfAttachmentsSelector = mkSelector "initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:serviceName:messageType:numberOfAttachments:"

-- | @Selector@ for @initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:serviceName:messageType:referencedMessage:reaction:@
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_messageType_referencedMessage_reactionSelector :: Selector '[Id NSString, Id NSString, Id NSString, Id NSDate, Id INPerson, Id NSArray, Id INSpeakableString, Id NSString, INMessageType, Id INMessage, Id INMessageReaction] (Id INMessage)
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_messageType_referencedMessage_reactionSelector = mkSelector "initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:serviceName:messageType:referencedMessage:reaction:"

-- | @Selector@ for @initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:serviceName:messageType:referencedMessage:sticker:reaction:@
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_messageType_referencedMessage_sticker_reactionSelector :: Selector '[Id NSString, Id NSString, Id NSString, Id NSDate, Id INPerson, Id NSArray, Id INSpeakableString, Id NSString, INMessageType, Id INMessage, Id INSticker, Id INMessageReaction] (Id INMessage)
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_messageType_referencedMessage_sticker_reactionSelector = mkSelector "initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:serviceName:messageType:referencedMessage:sticker:reaction:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @conversationIdentifier@
conversationIdentifierSelector :: Selector '[] (Id NSString)
conversationIdentifierSelector = mkSelector "conversationIdentifier"

-- | @Selector@ for @content@
contentSelector :: Selector '[] (Id NSString)
contentSelector = mkSelector "content"

-- | @Selector@ for @dateSent@
dateSentSelector :: Selector '[] (Id NSDate)
dateSentSelector = mkSelector "dateSent"

-- | @Selector@ for @sender@
senderSelector :: Selector '[] (Id INPerson)
senderSelector = mkSelector "sender"

-- | @Selector@ for @recipients@
recipientsSelector :: Selector '[] (Id NSArray)
recipientsSelector = mkSelector "recipients"

-- | @Selector@ for @groupName@
groupNameSelector :: Selector '[] (Id INSpeakableString)
groupNameSelector = mkSelector "groupName"

-- | @Selector@ for @messageType@
messageTypeSelector :: Selector '[] INMessageType
messageTypeSelector = mkSelector "messageType"

-- | @Selector@ for @serviceName@
serviceNameSelector :: Selector '[] (Id NSString)
serviceNameSelector = mkSelector "serviceName"

-- | @Selector@ for @attachmentFiles@
attachmentFilesSelector :: Selector '[] (Id NSArray)
attachmentFilesSelector = mkSelector "attachmentFiles"

-- | @Selector@ for @numberOfAttachments@
numberOfAttachmentsSelector :: Selector '[] (Id NSNumber)
numberOfAttachmentsSelector = mkSelector "numberOfAttachments"

-- | @Selector@ for @audioMessageFile@
audioMessageFileSelector :: Selector '[] (Id INFile)
audioMessageFileSelector = mkSelector "audioMessageFile"

-- | @Selector@ for @linkMetadata@
linkMetadataSelector :: Selector '[] (Id INMessageLinkMetadata)
linkMetadataSelector = mkSelector "linkMetadata"

-- | @Selector@ for @sticker@
stickerSelector :: Selector '[] (Id INSticker)
stickerSelector = mkSelector "sticker"

-- | @Selector@ for @setSticker:@
setStickerSelector :: Selector '[Id INSticker] ()
setStickerSelector = mkSelector "setSticker:"

-- | @Selector@ for @reaction@
reactionSelector :: Selector '[] (Id INMessageReaction)
reactionSelector = mkSelector "reaction"

-- | @Selector@ for @setReaction:@
setReactionSelector :: Selector '[Id INMessageReaction] ()
setReactionSelector = mkSelector "setReaction:"

