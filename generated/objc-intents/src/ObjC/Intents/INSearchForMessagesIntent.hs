{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSearchForMessagesIntent@.
module ObjC.Intents.INSearchForMessagesIntent
  ( INSearchForMessagesIntent
  , IsINSearchForMessagesIntent(..)
  , initWithRecipients_senders_searchTerms_attributes_dateTimeRange_identifiers_notificationIdentifiers_speakableGroupNames_conversationIdentifiers
  , initWithRecipients_senders_searchTerms_attributes_dateTimeRange_identifiers_notificationIdentifiers_groupNames
  , initWithRecipients_senders_searchTerms_attributes_dateTimeRange_identifiers_notificationIdentifiers_speakableGroupNames
  , recipients
  , recipientsOperator
  , senders
  , sendersOperator
  , searchTerms
  , searchTermsOperator
  , attributes
  , dateTimeRange
  , identifiers
  , identifiersOperator
  , notificationIdentifiers
  , notificationIdentifiersOperator
  , speakableGroupNamesOperator
  , conversationIdentifiersOperator
  , groupNames
  , groupNamesOperator
  , initWithRecipients_senders_searchTerms_attributes_dateTimeRange_identifiers_notificationIdentifiers_speakableGroupNames_conversationIdentifiersSelector
  , initWithRecipients_senders_searchTerms_attributes_dateTimeRange_identifiers_notificationIdentifiers_groupNamesSelector
  , initWithRecipients_senders_searchTerms_attributes_dateTimeRange_identifiers_notificationIdentifiers_speakableGroupNamesSelector
  , recipientsSelector
  , recipientsOperatorSelector
  , sendersSelector
  , sendersOperatorSelector
  , searchTermsSelector
  , searchTermsOperatorSelector
  , attributesSelector
  , dateTimeRangeSelector
  , identifiersSelector
  , identifiersOperatorSelector
  , notificationIdentifiersSelector
  , notificationIdentifiersOperatorSelector
  , speakableGroupNamesOperatorSelector
  , conversationIdentifiersOperatorSelector
  , groupNamesSelector
  , groupNamesOperatorSelector

  -- * Enum types
  , INConditionalOperator(INConditionalOperator)
  , pattern INConditionalOperatorAll
  , pattern INConditionalOperatorAny
  , pattern INConditionalOperatorNone
  , INMessageAttributeOptions(INMessageAttributeOptions)
  , pattern INMessageAttributeOptionRead
  , pattern INMessageAttributeOptionUnread
  , pattern INMessageAttributeOptionFlagged
  , pattern INMessageAttributeOptionUnflagged
  , pattern INMessageAttributeOptionPlayed

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

-- | @- initWithRecipients:senders:searchTerms:attributes:dateTimeRange:identifiers:notificationIdentifiers:speakableGroupNames:conversationIdentifiers:@
initWithRecipients_senders_searchTerms_attributes_dateTimeRange_identifiers_notificationIdentifiers_speakableGroupNames_conversationIdentifiers :: (IsINSearchForMessagesIntent inSearchForMessagesIntent, IsNSArray recipients, IsNSArray senders, IsNSArray searchTerms, IsINDateComponentsRange dateTimeRange, IsNSArray identifiers, IsNSArray notificationIdentifiers, IsNSArray speakableGroupNames, IsNSArray conversationIdentifiers) => inSearchForMessagesIntent -> recipients -> senders -> searchTerms -> INMessageAttributeOptions -> dateTimeRange -> identifiers -> notificationIdentifiers -> speakableGroupNames -> conversationIdentifiers -> IO (Id INSearchForMessagesIntent)
initWithRecipients_senders_searchTerms_attributes_dateTimeRange_identifiers_notificationIdentifiers_speakableGroupNames_conversationIdentifiers inSearchForMessagesIntent  recipients senders searchTerms attributes dateTimeRange identifiers notificationIdentifiers speakableGroupNames conversationIdentifiers =
withObjCPtr recipients $ \raw_recipients ->
  withObjCPtr senders $ \raw_senders ->
    withObjCPtr searchTerms $ \raw_searchTerms ->
      withObjCPtr dateTimeRange $ \raw_dateTimeRange ->
        withObjCPtr identifiers $ \raw_identifiers ->
          withObjCPtr notificationIdentifiers $ \raw_notificationIdentifiers ->
            withObjCPtr speakableGroupNames $ \raw_speakableGroupNames ->
              withObjCPtr conversationIdentifiers $ \raw_conversationIdentifiers ->
                  sendMsg inSearchForMessagesIntent (mkSelector "initWithRecipients:senders:searchTerms:attributes:dateTimeRange:identifiers:notificationIdentifiers:speakableGroupNames:conversationIdentifiers:") (retPtr retVoid) [argPtr (castPtr raw_recipients :: Ptr ()), argPtr (castPtr raw_senders :: Ptr ()), argPtr (castPtr raw_searchTerms :: Ptr ()), argCULong (coerce attributes), argPtr (castPtr raw_dateTimeRange :: Ptr ()), argPtr (castPtr raw_identifiers :: Ptr ()), argPtr (castPtr raw_notificationIdentifiers :: Ptr ()), argPtr (castPtr raw_speakableGroupNames :: Ptr ()), argPtr (castPtr raw_conversationIdentifiers :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithRecipients:senders:searchTerms:attributes:dateTimeRange:identifiers:notificationIdentifiers:groupNames:@
initWithRecipients_senders_searchTerms_attributes_dateTimeRange_identifiers_notificationIdentifiers_groupNames :: (IsINSearchForMessagesIntent inSearchForMessagesIntent, IsNSArray recipients, IsNSArray senders, IsNSArray searchTerms, IsINDateComponentsRange dateTimeRange, IsNSArray identifiers, IsNSArray notificationIdentifiers, IsNSArray groupNames) => inSearchForMessagesIntent -> recipients -> senders -> searchTerms -> INMessageAttributeOptions -> dateTimeRange -> identifiers -> notificationIdentifiers -> groupNames -> IO (Id INSearchForMessagesIntent)
initWithRecipients_senders_searchTerms_attributes_dateTimeRange_identifiers_notificationIdentifiers_groupNames inSearchForMessagesIntent  recipients senders searchTerms attributes dateTimeRange identifiers notificationIdentifiers groupNames =
withObjCPtr recipients $ \raw_recipients ->
  withObjCPtr senders $ \raw_senders ->
    withObjCPtr searchTerms $ \raw_searchTerms ->
      withObjCPtr dateTimeRange $ \raw_dateTimeRange ->
        withObjCPtr identifiers $ \raw_identifiers ->
          withObjCPtr notificationIdentifiers $ \raw_notificationIdentifiers ->
            withObjCPtr groupNames $ \raw_groupNames ->
                sendMsg inSearchForMessagesIntent (mkSelector "initWithRecipients:senders:searchTerms:attributes:dateTimeRange:identifiers:notificationIdentifiers:groupNames:") (retPtr retVoid) [argPtr (castPtr raw_recipients :: Ptr ()), argPtr (castPtr raw_senders :: Ptr ()), argPtr (castPtr raw_searchTerms :: Ptr ()), argCULong (coerce attributes), argPtr (castPtr raw_dateTimeRange :: Ptr ()), argPtr (castPtr raw_identifiers :: Ptr ()), argPtr (castPtr raw_notificationIdentifiers :: Ptr ()), argPtr (castPtr raw_groupNames :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithRecipients:senders:searchTerms:attributes:dateTimeRange:identifiers:notificationIdentifiers:speakableGroupNames:@
initWithRecipients_senders_searchTerms_attributes_dateTimeRange_identifiers_notificationIdentifiers_speakableGroupNames :: (IsINSearchForMessagesIntent inSearchForMessagesIntent, IsNSArray recipients, IsNSArray senders, IsNSArray searchTerms, IsINDateComponentsRange dateTimeRange, IsNSArray identifiers, IsNSArray notificationIdentifiers, IsNSArray speakableGroupNames) => inSearchForMessagesIntent -> recipients -> senders -> searchTerms -> INMessageAttributeOptions -> dateTimeRange -> identifiers -> notificationIdentifiers -> speakableGroupNames -> IO (Id INSearchForMessagesIntent)
initWithRecipients_senders_searchTerms_attributes_dateTimeRange_identifiers_notificationIdentifiers_speakableGroupNames inSearchForMessagesIntent  recipients senders searchTerms attributes dateTimeRange identifiers notificationIdentifiers speakableGroupNames =
withObjCPtr recipients $ \raw_recipients ->
  withObjCPtr senders $ \raw_senders ->
    withObjCPtr searchTerms $ \raw_searchTerms ->
      withObjCPtr dateTimeRange $ \raw_dateTimeRange ->
        withObjCPtr identifiers $ \raw_identifiers ->
          withObjCPtr notificationIdentifiers $ \raw_notificationIdentifiers ->
            withObjCPtr speakableGroupNames $ \raw_speakableGroupNames ->
                sendMsg inSearchForMessagesIntent (mkSelector "initWithRecipients:senders:searchTerms:attributes:dateTimeRange:identifiers:notificationIdentifiers:speakableGroupNames:") (retPtr retVoid) [argPtr (castPtr raw_recipients :: Ptr ()), argPtr (castPtr raw_senders :: Ptr ()), argPtr (castPtr raw_searchTerms :: Ptr ()), argCULong (coerce attributes), argPtr (castPtr raw_dateTimeRange :: Ptr ()), argPtr (castPtr raw_identifiers :: Ptr ()), argPtr (castPtr raw_notificationIdentifiers :: Ptr ()), argPtr (castPtr raw_speakableGroupNames :: Ptr ())] >>= ownedObject . castPtr

-- | @- recipients@
recipients :: IsINSearchForMessagesIntent inSearchForMessagesIntent => inSearchForMessagesIntent -> IO (Id NSArray)
recipients inSearchForMessagesIntent  =
  sendMsg inSearchForMessagesIntent (mkSelector "recipients") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- recipientsOperator@
recipientsOperator :: IsINSearchForMessagesIntent inSearchForMessagesIntent => inSearchForMessagesIntent -> IO INConditionalOperator
recipientsOperator inSearchForMessagesIntent  =
  fmap (coerce :: CLong -> INConditionalOperator) $ sendMsg inSearchForMessagesIntent (mkSelector "recipientsOperator") retCLong []

-- | @- senders@
senders :: IsINSearchForMessagesIntent inSearchForMessagesIntent => inSearchForMessagesIntent -> IO (Id NSArray)
senders inSearchForMessagesIntent  =
  sendMsg inSearchForMessagesIntent (mkSelector "senders") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sendersOperator@
sendersOperator :: IsINSearchForMessagesIntent inSearchForMessagesIntent => inSearchForMessagesIntent -> IO INConditionalOperator
sendersOperator inSearchForMessagesIntent  =
  fmap (coerce :: CLong -> INConditionalOperator) $ sendMsg inSearchForMessagesIntent (mkSelector "sendersOperator") retCLong []

-- | @- searchTerms@
searchTerms :: IsINSearchForMessagesIntent inSearchForMessagesIntent => inSearchForMessagesIntent -> IO (Id NSArray)
searchTerms inSearchForMessagesIntent  =
  sendMsg inSearchForMessagesIntent (mkSelector "searchTerms") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- searchTermsOperator@
searchTermsOperator :: IsINSearchForMessagesIntent inSearchForMessagesIntent => inSearchForMessagesIntent -> IO INConditionalOperator
searchTermsOperator inSearchForMessagesIntent  =
  fmap (coerce :: CLong -> INConditionalOperator) $ sendMsg inSearchForMessagesIntent (mkSelector "searchTermsOperator") retCLong []

-- | @- attributes@
attributes :: IsINSearchForMessagesIntent inSearchForMessagesIntent => inSearchForMessagesIntent -> IO INMessageAttributeOptions
attributes inSearchForMessagesIntent  =
  fmap (coerce :: CULong -> INMessageAttributeOptions) $ sendMsg inSearchForMessagesIntent (mkSelector "attributes") retCULong []

-- | @- dateTimeRange@
dateTimeRange :: IsINSearchForMessagesIntent inSearchForMessagesIntent => inSearchForMessagesIntent -> IO (Id INDateComponentsRange)
dateTimeRange inSearchForMessagesIntent  =
  sendMsg inSearchForMessagesIntent (mkSelector "dateTimeRange") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- identifiers@
identifiers :: IsINSearchForMessagesIntent inSearchForMessagesIntent => inSearchForMessagesIntent -> IO (Id NSArray)
identifiers inSearchForMessagesIntent  =
  sendMsg inSearchForMessagesIntent (mkSelector "identifiers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- identifiersOperator@
identifiersOperator :: IsINSearchForMessagesIntent inSearchForMessagesIntent => inSearchForMessagesIntent -> IO INConditionalOperator
identifiersOperator inSearchForMessagesIntent  =
  fmap (coerce :: CLong -> INConditionalOperator) $ sendMsg inSearchForMessagesIntent (mkSelector "identifiersOperator") retCLong []

-- | @- notificationIdentifiers@
notificationIdentifiers :: IsINSearchForMessagesIntent inSearchForMessagesIntent => inSearchForMessagesIntent -> IO (Id NSArray)
notificationIdentifiers inSearchForMessagesIntent  =
  sendMsg inSearchForMessagesIntent (mkSelector "notificationIdentifiers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- notificationIdentifiersOperator@
notificationIdentifiersOperator :: IsINSearchForMessagesIntent inSearchForMessagesIntent => inSearchForMessagesIntent -> IO INConditionalOperator
notificationIdentifiersOperator inSearchForMessagesIntent  =
  fmap (coerce :: CLong -> INConditionalOperator) $ sendMsg inSearchForMessagesIntent (mkSelector "notificationIdentifiersOperator") retCLong []

-- | @- speakableGroupNamesOperator@
speakableGroupNamesOperator :: IsINSearchForMessagesIntent inSearchForMessagesIntent => inSearchForMessagesIntent -> IO INConditionalOperator
speakableGroupNamesOperator inSearchForMessagesIntent  =
  fmap (coerce :: CLong -> INConditionalOperator) $ sendMsg inSearchForMessagesIntent (mkSelector "speakableGroupNamesOperator") retCLong []

-- | @- conversationIdentifiersOperator@
conversationIdentifiersOperator :: IsINSearchForMessagesIntent inSearchForMessagesIntent => inSearchForMessagesIntent -> IO INConditionalOperator
conversationIdentifiersOperator inSearchForMessagesIntent  =
  fmap (coerce :: CLong -> INConditionalOperator) $ sendMsg inSearchForMessagesIntent (mkSelector "conversationIdentifiersOperator") retCLong []

-- | @- groupNames@
groupNames :: IsINSearchForMessagesIntent inSearchForMessagesIntent => inSearchForMessagesIntent -> IO (Id NSArray)
groupNames inSearchForMessagesIntent  =
  sendMsg inSearchForMessagesIntent (mkSelector "groupNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- groupNamesOperator@
groupNamesOperator :: IsINSearchForMessagesIntent inSearchForMessagesIntent => inSearchForMessagesIntent -> IO INConditionalOperator
groupNamesOperator inSearchForMessagesIntent  =
  fmap (coerce :: CLong -> INConditionalOperator) $ sendMsg inSearchForMessagesIntent (mkSelector "groupNamesOperator") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRecipients:senders:searchTerms:attributes:dateTimeRange:identifiers:notificationIdentifiers:speakableGroupNames:conversationIdentifiers:@
initWithRecipients_senders_searchTerms_attributes_dateTimeRange_identifiers_notificationIdentifiers_speakableGroupNames_conversationIdentifiersSelector :: Selector
initWithRecipients_senders_searchTerms_attributes_dateTimeRange_identifiers_notificationIdentifiers_speakableGroupNames_conversationIdentifiersSelector = mkSelector "initWithRecipients:senders:searchTerms:attributes:dateTimeRange:identifiers:notificationIdentifiers:speakableGroupNames:conversationIdentifiers:"

-- | @Selector@ for @initWithRecipients:senders:searchTerms:attributes:dateTimeRange:identifiers:notificationIdentifiers:groupNames:@
initWithRecipients_senders_searchTerms_attributes_dateTimeRange_identifiers_notificationIdentifiers_groupNamesSelector :: Selector
initWithRecipients_senders_searchTerms_attributes_dateTimeRange_identifiers_notificationIdentifiers_groupNamesSelector = mkSelector "initWithRecipients:senders:searchTerms:attributes:dateTimeRange:identifiers:notificationIdentifiers:groupNames:"

-- | @Selector@ for @initWithRecipients:senders:searchTerms:attributes:dateTimeRange:identifiers:notificationIdentifiers:speakableGroupNames:@
initWithRecipients_senders_searchTerms_attributes_dateTimeRange_identifiers_notificationIdentifiers_speakableGroupNamesSelector :: Selector
initWithRecipients_senders_searchTerms_attributes_dateTimeRange_identifiers_notificationIdentifiers_speakableGroupNamesSelector = mkSelector "initWithRecipients:senders:searchTerms:attributes:dateTimeRange:identifiers:notificationIdentifiers:speakableGroupNames:"

-- | @Selector@ for @recipients@
recipientsSelector :: Selector
recipientsSelector = mkSelector "recipients"

-- | @Selector@ for @recipientsOperator@
recipientsOperatorSelector :: Selector
recipientsOperatorSelector = mkSelector "recipientsOperator"

-- | @Selector@ for @senders@
sendersSelector :: Selector
sendersSelector = mkSelector "senders"

-- | @Selector@ for @sendersOperator@
sendersOperatorSelector :: Selector
sendersOperatorSelector = mkSelector "sendersOperator"

-- | @Selector@ for @searchTerms@
searchTermsSelector :: Selector
searchTermsSelector = mkSelector "searchTerms"

-- | @Selector@ for @searchTermsOperator@
searchTermsOperatorSelector :: Selector
searchTermsOperatorSelector = mkSelector "searchTermsOperator"

-- | @Selector@ for @attributes@
attributesSelector :: Selector
attributesSelector = mkSelector "attributes"

-- | @Selector@ for @dateTimeRange@
dateTimeRangeSelector :: Selector
dateTimeRangeSelector = mkSelector "dateTimeRange"

-- | @Selector@ for @identifiers@
identifiersSelector :: Selector
identifiersSelector = mkSelector "identifiers"

-- | @Selector@ for @identifiersOperator@
identifiersOperatorSelector :: Selector
identifiersOperatorSelector = mkSelector "identifiersOperator"

-- | @Selector@ for @notificationIdentifiers@
notificationIdentifiersSelector :: Selector
notificationIdentifiersSelector = mkSelector "notificationIdentifiers"

-- | @Selector@ for @notificationIdentifiersOperator@
notificationIdentifiersOperatorSelector :: Selector
notificationIdentifiersOperatorSelector = mkSelector "notificationIdentifiersOperator"

-- | @Selector@ for @speakableGroupNamesOperator@
speakableGroupNamesOperatorSelector :: Selector
speakableGroupNamesOperatorSelector = mkSelector "speakableGroupNamesOperator"

-- | @Selector@ for @conversationIdentifiersOperator@
conversationIdentifiersOperatorSelector :: Selector
conversationIdentifiersOperatorSelector = mkSelector "conversationIdentifiersOperator"

-- | @Selector@ for @groupNames@
groupNamesSelector :: Selector
groupNamesSelector = mkSelector "groupNames"

-- | @Selector@ for @groupNamesOperator@
groupNamesOperatorSelector :: Selector
groupNamesOperatorSelector = mkSelector "groupNamesOperator"

