{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType_serviceName_attachmentFilesSelector
  , initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType_serviceName_audioMessageFileSelector
  , initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType_serviceNameSelector
  , initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageTypeSelector
  , initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_messageTypeSelector
  , initWithIdentifier_content_dateSent_sender_recipientsSelector
  , initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_linkMetadataSelector
  , initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_messageType_numberOfAttachmentsSelector
  , initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_messageType_referencedMessage_reactionSelector
  , initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_messageType_referencedMessage_sticker_reactionSelector
  , identifierSelector
  , conversationIdentifierSelector
  , contentSelector
  , dateSentSelector
  , senderSelector
  , recipientsSelector
  , groupNameSelector
  , messageTypeSelector
  , serviceNameSelector
  , attachmentFilesSelector
  , numberOfAttachmentsSelector
  , audioMessageFileSelector
  , linkMetadataSelector
  , stickerSelector
  , setStickerSelector
  , reactionSelector
  , setReactionSelector

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

-- | @- init@
init_ :: IsINMessage inMessage => inMessage -> IO RawId
init_ inMessage  =
    fmap (RawId . castPtr) $ sendMsg inMessage (mkSelector "init") (retPtr retVoid) []

-- | @- initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:messageType:serviceName:attachmentFiles:@
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType_serviceName_attachmentFiles :: (IsINMessage inMessage, IsNSString identifier, IsNSString conversationIdentifier, IsNSString content, IsNSDate dateSent, IsINPerson sender, IsNSArray recipients, IsINSpeakableString groupName, IsNSString serviceName, IsNSArray attachmentFiles) => inMessage -> identifier -> conversationIdentifier -> content -> dateSent -> sender -> recipients -> groupName -> INMessageType -> serviceName -> attachmentFiles -> IO (Id INMessage)
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType_serviceName_attachmentFiles inMessage  identifier conversationIdentifier content dateSent sender recipients groupName messageType serviceName attachmentFiles =
  withObjCPtr identifier $ \raw_identifier ->
    withObjCPtr conversationIdentifier $ \raw_conversationIdentifier ->
      withObjCPtr content $ \raw_content ->
        withObjCPtr dateSent $ \raw_dateSent ->
          withObjCPtr sender $ \raw_sender ->
            withObjCPtr recipients $ \raw_recipients ->
              withObjCPtr groupName $ \raw_groupName ->
                withObjCPtr serviceName $ \raw_serviceName ->
                  withObjCPtr attachmentFiles $ \raw_attachmentFiles ->
                      sendMsg inMessage (mkSelector "initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:messageType:serviceName:attachmentFiles:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_conversationIdentifier :: Ptr ()), argPtr (castPtr raw_content :: Ptr ()), argPtr (castPtr raw_dateSent :: Ptr ()), argPtr (castPtr raw_sender :: Ptr ()), argPtr (castPtr raw_recipients :: Ptr ()), argPtr (castPtr raw_groupName :: Ptr ()), argCLong (coerce messageType), argPtr (castPtr raw_serviceName :: Ptr ()), argPtr (castPtr raw_attachmentFiles :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:messageType:serviceName:audioMessageFile:@
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType_serviceName_audioMessageFile :: (IsINMessage inMessage, IsNSString identifier, IsNSString conversationIdentifier, IsNSString content, IsNSDate dateSent, IsINPerson sender, IsNSArray recipients, IsINSpeakableString groupName, IsNSString serviceName, IsINFile audioMessageFile) => inMessage -> identifier -> conversationIdentifier -> content -> dateSent -> sender -> recipients -> groupName -> INMessageType -> serviceName -> audioMessageFile -> IO (Id INMessage)
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType_serviceName_audioMessageFile inMessage  identifier conversationIdentifier content dateSent sender recipients groupName messageType serviceName audioMessageFile =
  withObjCPtr identifier $ \raw_identifier ->
    withObjCPtr conversationIdentifier $ \raw_conversationIdentifier ->
      withObjCPtr content $ \raw_content ->
        withObjCPtr dateSent $ \raw_dateSent ->
          withObjCPtr sender $ \raw_sender ->
            withObjCPtr recipients $ \raw_recipients ->
              withObjCPtr groupName $ \raw_groupName ->
                withObjCPtr serviceName $ \raw_serviceName ->
                  withObjCPtr audioMessageFile $ \raw_audioMessageFile ->
                      sendMsg inMessage (mkSelector "initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:messageType:serviceName:audioMessageFile:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_conversationIdentifier :: Ptr ()), argPtr (castPtr raw_content :: Ptr ()), argPtr (castPtr raw_dateSent :: Ptr ()), argPtr (castPtr raw_sender :: Ptr ()), argPtr (castPtr raw_recipients :: Ptr ()), argPtr (castPtr raw_groupName :: Ptr ()), argCLong (coerce messageType), argPtr (castPtr raw_serviceName :: Ptr ()), argPtr (castPtr raw_audioMessageFile :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:messageType:serviceName:@
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType_serviceName :: (IsINMessage inMessage, IsNSString identifier, IsNSString conversationIdentifier, IsNSString content, IsNSDate dateSent, IsINPerson sender, IsNSArray recipients, IsINSpeakableString groupName, IsNSString serviceName) => inMessage -> identifier -> conversationIdentifier -> content -> dateSent -> sender -> recipients -> groupName -> INMessageType -> serviceName -> IO (Id INMessage)
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType_serviceName inMessage  identifier conversationIdentifier content dateSent sender recipients groupName messageType serviceName =
  withObjCPtr identifier $ \raw_identifier ->
    withObjCPtr conversationIdentifier $ \raw_conversationIdentifier ->
      withObjCPtr content $ \raw_content ->
        withObjCPtr dateSent $ \raw_dateSent ->
          withObjCPtr sender $ \raw_sender ->
            withObjCPtr recipients $ \raw_recipients ->
              withObjCPtr groupName $ \raw_groupName ->
                withObjCPtr serviceName $ \raw_serviceName ->
                    sendMsg inMessage (mkSelector "initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:messageType:serviceName:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_conversationIdentifier :: Ptr ()), argPtr (castPtr raw_content :: Ptr ()), argPtr (castPtr raw_dateSent :: Ptr ()), argPtr (castPtr raw_sender :: Ptr ()), argPtr (castPtr raw_recipients :: Ptr ()), argPtr (castPtr raw_groupName :: Ptr ()), argCLong (coerce messageType), argPtr (castPtr raw_serviceName :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:messageType:@
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType :: (IsINMessage inMessage, IsNSString identifier, IsNSString conversationIdentifier, IsNSString content, IsNSDate dateSent, IsINPerson sender, IsNSArray recipients, IsINSpeakableString groupName) => inMessage -> identifier -> conversationIdentifier -> content -> dateSent -> sender -> recipients -> groupName -> INMessageType -> IO (Id INMessage)
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType inMessage  identifier conversationIdentifier content dateSent sender recipients groupName messageType =
  withObjCPtr identifier $ \raw_identifier ->
    withObjCPtr conversationIdentifier $ \raw_conversationIdentifier ->
      withObjCPtr content $ \raw_content ->
        withObjCPtr dateSent $ \raw_dateSent ->
          withObjCPtr sender $ \raw_sender ->
            withObjCPtr recipients $ \raw_recipients ->
              withObjCPtr groupName $ \raw_groupName ->
                  sendMsg inMessage (mkSelector "initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:messageType:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_conversationIdentifier :: Ptr ()), argPtr (castPtr raw_content :: Ptr ()), argPtr (castPtr raw_dateSent :: Ptr ()), argPtr (castPtr raw_sender :: Ptr ()), argPtr (castPtr raw_recipients :: Ptr ()), argPtr (castPtr raw_groupName :: Ptr ()), argCLong (coerce messageType)] >>= ownedObject . castPtr

-- | @- initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:messageType:@
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_messageType :: (IsINMessage inMessage, IsNSString identifier, IsNSString conversationIdentifier, IsNSString content, IsNSDate dateSent, IsINPerson sender, IsNSArray recipients) => inMessage -> identifier -> conversationIdentifier -> content -> dateSent -> sender -> recipients -> INMessageType -> IO (Id INMessage)
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_messageType inMessage  identifier conversationIdentifier content dateSent sender recipients messageType =
  withObjCPtr identifier $ \raw_identifier ->
    withObjCPtr conversationIdentifier $ \raw_conversationIdentifier ->
      withObjCPtr content $ \raw_content ->
        withObjCPtr dateSent $ \raw_dateSent ->
          withObjCPtr sender $ \raw_sender ->
            withObjCPtr recipients $ \raw_recipients ->
                sendMsg inMessage (mkSelector "initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:messageType:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_conversationIdentifier :: Ptr ()), argPtr (castPtr raw_content :: Ptr ()), argPtr (castPtr raw_dateSent :: Ptr ()), argPtr (castPtr raw_sender :: Ptr ()), argPtr (castPtr raw_recipients :: Ptr ()), argCLong (coerce messageType)] >>= ownedObject . castPtr

-- | @- initWithIdentifier:content:dateSent:sender:recipients:@
initWithIdentifier_content_dateSent_sender_recipients :: (IsINMessage inMessage, IsNSString identifier, IsNSString content, IsNSDate dateSent, IsINPerson sender, IsNSArray recipients) => inMessage -> identifier -> content -> dateSent -> sender -> recipients -> IO (Id INMessage)
initWithIdentifier_content_dateSent_sender_recipients inMessage  identifier content dateSent sender recipients =
  withObjCPtr identifier $ \raw_identifier ->
    withObjCPtr content $ \raw_content ->
      withObjCPtr dateSent $ \raw_dateSent ->
        withObjCPtr sender $ \raw_sender ->
          withObjCPtr recipients $ \raw_recipients ->
              sendMsg inMessage (mkSelector "initWithIdentifier:content:dateSent:sender:recipients:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_content :: Ptr ()), argPtr (castPtr raw_dateSent :: Ptr ()), argPtr (castPtr raw_sender :: Ptr ()), argPtr (castPtr raw_recipients :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:serviceName:linkMetadata:@
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_linkMetadata :: (IsINMessage inMessage, IsNSString identifier, IsNSString conversationIdentifier, IsNSString content, IsNSDate dateSent, IsINPerson sender, IsNSArray recipients, IsINSpeakableString groupName, IsNSString serviceName, IsINMessageLinkMetadata linkMetadata) => inMessage -> identifier -> conversationIdentifier -> content -> dateSent -> sender -> recipients -> groupName -> serviceName -> linkMetadata -> IO (Id INMessage)
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_linkMetadata inMessage  identifier conversationIdentifier content dateSent sender recipients groupName serviceName linkMetadata =
  withObjCPtr identifier $ \raw_identifier ->
    withObjCPtr conversationIdentifier $ \raw_conversationIdentifier ->
      withObjCPtr content $ \raw_content ->
        withObjCPtr dateSent $ \raw_dateSent ->
          withObjCPtr sender $ \raw_sender ->
            withObjCPtr recipients $ \raw_recipients ->
              withObjCPtr groupName $ \raw_groupName ->
                withObjCPtr serviceName $ \raw_serviceName ->
                  withObjCPtr linkMetadata $ \raw_linkMetadata ->
                      sendMsg inMessage (mkSelector "initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:serviceName:linkMetadata:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_conversationIdentifier :: Ptr ()), argPtr (castPtr raw_content :: Ptr ()), argPtr (castPtr raw_dateSent :: Ptr ()), argPtr (castPtr raw_sender :: Ptr ()), argPtr (castPtr raw_recipients :: Ptr ()), argPtr (castPtr raw_groupName :: Ptr ()), argPtr (castPtr raw_serviceName :: Ptr ()), argPtr (castPtr raw_linkMetadata :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:serviceName:messageType:numberOfAttachments:@
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_messageType_numberOfAttachments :: (IsINMessage inMessage, IsNSString identifier, IsNSString conversationIdentifier, IsNSString content, IsNSDate dateSent, IsINPerson sender, IsNSArray recipients, IsINSpeakableString groupName, IsNSString serviceName, IsNSNumber numberOfAttachments) => inMessage -> identifier -> conversationIdentifier -> content -> dateSent -> sender -> recipients -> groupName -> serviceName -> INMessageType -> numberOfAttachments -> IO (Id INMessage)
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_messageType_numberOfAttachments inMessage  identifier conversationIdentifier content dateSent sender recipients groupName serviceName messageType numberOfAttachments =
  withObjCPtr identifier $ \raw_identifier ->
    withObjCPtr conversationIdentifier $ \raw_conversationIdentifier ->
      withObjCPtr content $ \raw_content ->
        withObjCPtr dateSent $ \raw_dateSent ->
          withObjCPtr sender $ \raw_sender ->
            withObjCPtr recipients $ \raw_recipients ->
              withObjCPtr groupName $ \raw_groupName ->
                withObjCPtr serviceName $ \raw_serviceName ->
                  withObjCPtr numberOfAttachments $ \raw_numberOfAttachments ->
                      sendMsg inMessage (mkSelector "initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:serviceName:messageType:numberOfAttachments:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_conversationIdentifier :: Ptr ()), argPtr (castPtr raw_content :: Ptr ()), argPtr (castPtr raw_dateSent :: Ptr ()), argPtr (castPtr raw_sender :: Ptr ()), argPtr (castPtr raw_recipients :: Ptr ()), argPtr (castPtr raw_groupName :: Ptr ()), argPtr (castPtr raw_serviceName :: Ptr ()), argCLong (coerce messageType), argPtr (castPtr raw_numberOfAttachments :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:serviceName:messageType:referencedMessage:reaction:@
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_messageType_referencedMessage_reaction :: (IsINMessage inMessage, IsNSString identifier, IsNSString conversationIdentifier, IsNSString content, IsNSDate dateSent, IsINPerson sender, IsNSArray recipients, IsINSpeakableString groupName, IsNSString serviceName, IsINMessage referencedMessage, IsINMessageReaction reaction) => inMessage -> identifier -> conversationIdentifier -> content -> dateSent -> sender -> recipients -> groupName -> serviceName -> INMessageType -> referencedMessage -> reaction -> IO (Id INMessage)
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_messageType_referencedMessage_reaction inMessage  identifier conversationIdentifier content dateSent sender recipients groupName serviceName messageType referencedMessage reaction =
  withObjCPtr identifier $ \raw_identifier ->
    withObjCPtr conversationIdentifier $ \raw_conversationIdentifier ->
      withObjCPtr content $ \raw_content ->
        withObjCPtr dateSent $ \raw_dateSent ->
          withObjCPtr sender $ \raw_sender ->
            withObjCPtr recipients $ \raw_recipients ->
              withObjCPtr groupName $ \raw_groupName ->
                withObjCPtr serviceName $ \raw_serviceName ->
                  withObjCPtr referencedMessage $ \raw_referencedMessage ->
                    withObjCPtr reaction $ \raw_reaction ->
                        sendMsg inMessage (mkSelector "initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:serviceName:messageType:referencedMessage:reaction:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_conversationIdentifier :: Ptr ()), argPtr (castPtr raw_content :: Ptr ()), argPtr (castPtr raw_dateSent :: Ptr ()), argPtr (castPtr raw_sender :: Ptr ()), argPtr (castPtr raw_recipients :: Ptr ()), argPtr (castPtr raw_groupName :: Ptr ()), argPtr (castPtr raw_serviceName :: Ptr ()), argCLong (coerce messageType), argPtr (castPtr raw_referencedMessage :: Ptr ()), argPtr (castPtr raw_reaction :: Ptr ())] >>= ownedObject . castPtr

-- | Creates a message that includes a reaction and references the original message for the reaction.
--
-- - Parameters:   - identifier: The message’s unique identifier.   - conversationIdentifier: The identifier of the conversation that contains this message.   - content: The text that Siri recites to the message recipient.   - dateSent: The date and time the app sent the message to each recipient.   - sender: The person who sent the message.   - recipients: The people who received the message.   - groupName: The name of the group conversation.   - serviceName: The name of the service that delivers the message.   - messageType: The type of content the message contains.   - referencedMessage: The referenced message that received a reaction if the message object itself was a reaction.   - sticker: The sticker that this message contains.   - reaction: The message reaction that this message contains.
--
-- ObjC selector: @- initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:serviceName:messageType:referencedMessage:sticker:reaction:@
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_messageType_referencedMessage_sticker_reaction :: (IsINMessage inMessage, IsNSString identifier, IsNSString conversationIdentifier, IsNSString content, IsNSDate dateSent, IsINPerson sender, IsNSArray recipients, IsINSpeakableString groupName, IsNSString serviceName, IsINMessage referencedMessage, IsINSticker sticker, IsINMessageReaction reaction) => inMessage -> identifier -> conversationIdentifier -> content -> dateSent -> sender -> recipients -> groupName -> serviceName -> INMessageType -> referencedMessage -> sticker -> reaction -> IO (Id INMessage)
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_messageType_referencedMessage_sticker_reaction inMessage  identifier conversationIdentifier content dateSent sender recipients groupName serviceName messageType referencedMessage sticker reaction =
  withObjCPtr identifier $ \raw_identifier ->
    withObjCPtr conversationIdentifier $ \raw_conversationIdentifier ->
      withObjCPtr content $ \raw_content ->
        withObjCPtr dateSent $ \raw_dateSent ->
          withObjCPtr sender $ \raw_sender ->
            withObjCPtr recipients $ \raw_recipients ->
              withObjCPtr groupName $ \raw_groupName ->
                withObjCPtr serviceName $ \raw_serviceName ->
                  withObjCPtr referencedMessage $ \raw_referencedMessage ->
                    withObjCPtr sticker $ \raw_sticker ->
                      withObjCPtr reaction $ \raw_reaction ->
                          sendMsg inMessage (mkSelector "initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:serviceName:messageType:referencedMessage:sticker:reaction:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_conversationIdentifier :: Ptr ()), argPtr (castPtr raw_content :: Ptr ()), argPtr (castPtr raw_dateSent :: Ptr ()), argPtr (castPtr raw_sender :: Ptr ()), argPtr (castPtr raw_recipients :: Ptr ()), argPtr (castPtr raw_groupName :: Ptr ()), argPtr (castPtr raw_serviceName :: Ptr ()), argCLong (coerce messageType), argPtr (castPtr raw_referencedMessage :: Ptr ()), argPtr (castPtr raw_sticker :: Ptr ()), argPtr (castPtr raw_reaction :: Ptr ())] >>= ownedObject . castPtr

-- | @- identifier@
identifier :: IsINMessage inMessage => inMessage -> IO (Id NSString)
identifier inMessage  =
    sendMsg inMessage (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- conversationIdentifier@
conversationIdentifier :: IsINMessage inMessage => inMessage -> IO (Id NSString)
conversationIdentifier inMessage  =
    sendMsg inMessage (mkSelector "conversationIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- content@
content :: IsINMessage inMessage => inMessage -> IO (Id NSString)
content inMessage  =
    sendMsg inMessage (mkSelector "content") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- dateSent@
dateSent :: IsINMessage inMessage => inMessage -> IO (Id NSDate)
dateSent inMessage  =
    sendMsg inMessage (mkSelector "dateSent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sender@
sender :: IsINMessage inMessage => inMessage -> IO (Id INPerson)
sender inMessage  =
    sendMsg inMessage (mkSelector "sender") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- recipients@
recipients :: IsINMessage inMessage => inMessage -> IO (Id NSArray)
recipients inMessage  =
    sendMsg inMessage (mkSelector "recipients") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- groupName@
groupName :: IsINMessage inMessage => inMessage -> IO (Id INSpeakableString)
groupName inMessage  =
    sendMsg inMessage (mkSelector "groupName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- messageType@
messageType :: IsINMessage inMessage => inMessage -> IO INMessageType
messageType inMessage  =
    fmap (coerce :: CLong -> INMessageType) $ sendMsg inMessage (mkSelector "messageType") retCLong []

-- | @- serviceName@
serviceName :: IsINMessage inMessage => inMessage -> IO (Id NSString)
serviceName inMessage  =
    sendMsg inMessage (mkSelector "serviceName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- attachmentFiles@
attachmentFiles :: IsINMessage inMessage => inMessage -> IO (Id NSArray)
attachmentFiles inMessage  =
    sendMsg inMessage (mkSelector "attachmentFiles") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- numberOfAttachments@
numberOfAttachments :: IsINMessage inMessage => inMessage -> IO (Id NSNumber)
numberOfAttachments inMessage  =
    sendMsg inMessage (mkSelector "numberOfAttachments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- audioMessageFile@
audioMessageFile :: IsINMessage inMessage => inMessage -> IO (Id INFile)
audioMessageFile inMessage  =
    sendMsg inMessage (mkSelector "audioMessageFile") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- linkMetadata@
linkMetadata :: IsINMessage inMessage => inMessage -> IO (Id INMessageLinkMetadata)
linkMetadata inMessage  =
    sendMsg inMessage (mkSelector "linkMetadata") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The sticker that this message contains.
--
-- ObjC selector: @- sticker@
sticker :: IsINMessage inMessage => inMessage -> IO (Id INSticker)
sticker inMessage  =
    sendMsg inMessage (mkSelector "sticker") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The sticker that this message contains.
--
-- ObjC selector: @- setSticker:@
setSticker :: (IsINMessage inMessage, IsINSticker value) => inMessage -> value -> IO ()
setSticker inMessage  value =
  withObjCPtr value $ \raw_value ->
      sendMsg inMessage (mkSelector "setSticker:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The message reaction that this message contains.
--
-- ObjC selector: @- reaction@
reaction :: IsINMessage inMessage => inMessage -> IO (Id INMessageReaction)
reaction inMessage  =
    sendMsg inMessage (mkSelector "reaction") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The message reaction that this message contains.
--
-- ObjC selector: @- setReaction:@
setReaction :: (IsINMessage inMessage, IsINMessageReaction value) => inMessage -> value -> IO ()
setReaction inMessage  value =
  withObjCPtr value $ \raw_value ->
      sendMsg inMessage (mkSelector "setReaction:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:messageType:serviceName:attachmentFiles:@
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType_serviceName_attachmentFilesSelector :: Selector
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType_serviceName_attachmentFilesSelector = mkSelector "initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:messageType:serviceName:attachmentFiles:"

-- | @Selector@ for @initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:messageType:serviceName:audioMessageFile:@
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType_serviceName_audioMessageFileSelector :: Selector
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType_serviceName_audioMessageFileSelector = mkSelector "initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:messageType:serviceName:audioMessageFile:"

-- | @Selector@ for @initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:messageType:serviceName:@
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType_serviceNameSelector :: Selector
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageType_serviceNameSelector = mkSelector "initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:messageType:serviceName:"

-- | @Selector@ for @initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:messageType:@
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageTypeSelector :: Selector
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_messageTypeSelector = mkSelector "initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:messageType:"

-- | @Selector@ for @initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:messageType:@
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_messageTypeSelector :: Selector
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_messageTypeSelector = mkSelector "initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:messageType:"

-- | @Selector@ for @initWithIdentifier:content:dateSent:sender:recipients:@
initWithIdentifier_content_dateSent_sender_recipientsSelector :: Selector
initWithIdentifier_content_dateSent_sender_recipientsSelector = mkSelector "initWithIdentifier:content:dateSent:sender:recipients:"

-- | @Selector@ for @initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:serviceName:linkMetadata:@
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_linkMetadataSelector :: Selector
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_linkMetadataSelector = mkSelector "initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:serviceName:linkMetadata:"

-- | @Selector@ for @initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:serviceName:messageType:numberOfAttachments:@
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_messageType_numberOfAttachmentsSelector :: Selector
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_messageType_numberOfAttachmentsSelector = mkSelector "initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:serviceName:messageType:numberOfAttachments:"

-- | @Selector@ for @initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:serviceName:messageType:referencedMessage:reaction:@
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_messageType_referencedMessage_reactionSelector :: Selector
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_messageType_referencedMessage_reactionSelector = mkSelector "initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:serviceName:messageType:referencedMessage:reaction:"

-- | @Selector@ for @initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:serviceName:messageType:referencedMessage:sticker:reaction:@
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_messageType_referencedMessage_sticker_reactionSelector :: Selector
initWithIdentifier_conversationIdentifier_content_dateSent_sender_recipients_groupName_serviceName_messageType_referencedMessage_sticker_reactionSelector = mkSelector "initWithIdentifier:conversationIdentifier:content:dateSent:sender:recipients:groupName:serviceName:messageType:referencedMessage:sticker:reaction:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @conversationIdentifier@
conversationIdentifierSelector :: Selector
conversationIdentifierSelector = mkSelector "conversationIdentifier"

-- | @Selector@ for @content@
contentSelector :: Selector
contentSelector = mkSelector "content"

-- | @Selector@ for @dateSent@
dateSentSelector :: Selector
dateSentSelector = mkSelector "dateSent"

-- | @Selector@ for @sender@
senderSelector :: Selector
senderSelector = mkSelector "sender"

-- | @Selector@ for @recipients@
recipientsSelector :: Selector
recipientsSelector = mkSelector "recipients"

-- | @Selector@ for @groupName@
groupNameSelector :: Selector
groupNameSelector = mkSelector "groupName"

-- | @Selector@ for @messageType@
messageTypeSelector :: Selector
messageTypeSelector = mkSelector "messageType"

-- | @Selector@ for @serviceName@
serviceNameSelector :: Selector
serviceNameSelector = mkSelector "serviceName"

-- | @Selector@ for @attachmentFiles@
attachmentFilesSelector :: Selector
attachmentFilesSelector = mkSelector "attachmentFiles"

-- | @Selector@ for @numberOfAttachments@
numberOfAttachmentsSelector :: Selector
numberOfAttachmentsSelector = mkSelector "numberOfAttachments"

-- | @Selector@ for @audioMessageFile@
audioMessageFileSelector :: Selector
audioMessageFileSelector = mkSelector "audioMessageFile"

-- | @Selector@ for @linkMetadata@
linkMetadataSelector :: Selector
linkMetadataSelector = mkSelector "linkMetadata"

-- | @Selector@ for @sticker@
stickerSelector :: Selector
stickerSelector = mkSelector "sticker"

-- | @Selector@ for @setSticker:@
setStickerSelector :: Selector
setStickerSelector = mkSelector "setSticker:"

-- | @Selector@ for @reaction@
reactionSelector :: Selector
reactionSelector = mkSelector "reaction"

-- | @Selector@ for @setReaction:@
setReactionSelector :: Selector
setReactionSelector = mkSelector "setReaction:"

