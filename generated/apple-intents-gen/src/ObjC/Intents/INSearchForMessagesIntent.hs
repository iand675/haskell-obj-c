{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , speakableGroupNames
  , speakableGroupNamesOperator
  , conversationIdentifiers
  , conversationIdentifiersOperator
  , groupNames
  , groupNamesOperator
  , attributesSelector
  , conversationIdentifiersOperatorSelector
  , conversationIdentifiersSelector
  , dateTimeRangeSelector
  , groupNamesOperatorSelector
  , groupNamesSelector
  , identifiersOperatorSelector
  , identifiersSelector
  , initWithRecipients_senders_searchTerms_attributes_dateTimeRange_identifiers_notificationIdentifiers_groupNamesSelector
  , initWithRecipients_senders_searchTerms_attributes_dateTimeRange_identifiers_notificationIdentifiers_speakableGroupNamesSelector
  , initWithRecipients_senders_searchTerms_attributes_dateTimeRange_identifiers_notificationIdentifiers_speakableGroupNames_conversationIdentifiersSelector
  , notificationIdentifiersOperatorSelector
  , notificationIdentifiersSelector
  , recipientsOperatorSelector
  , recipientsSelector
  , searchTermsOperatorSelector
  , searchTermsSelector
  , sendersOperatorSelector
  , sendersSelector
  , speakableGroupNamesOperatorSelector
  , speakableGroupNamesSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithRecipients:senders:searchTerms:attributes:dateTimeRange:identifiers:notificationIdentifiers:speakableGroupNames:conversationIdentifiers:@
initWithRecipients_senders_searchTerms_attributes_dateTimeRange_identifiers_notificationIdentifiers_speakableGroupNames_conversationIdentifiers :: (IsINSearchForMessagesIntent inSearchForMessagesIntent, IsNSArray recipients, IsNSArray senders, IsNSArray searchTerms, IsINDateComponentsRange dateTimeRange, IsNSArray identifiers, IsNSArray notificationIdentifiers, IsNSArray speakableGroupNames, IsNSArray conversationIdentifiers) => inSearchForMessagesIntent -> recipients -> senders -> searchTerms -> INMessageAttributeOptions -> dateTimeRange -> identifiers -> notificationIdentifiers -> speakableGroupNames -> conversationIdentifiers -> IO (Id INSearchForMessagesIntent)
initWithRecipients_senders_searchTerms_attributes_dateTimeRange_identifiers_notificationIdentifiers_speakableGroupNames_conversationIdentifiers inSearchForMessagesIntent recipients senders searchTerms attributes dateTimeRange identifiers notificationIdentifiers speakableGroupNames conversationIdentifiers =
  sendOwnedMessage inSearchForMessagesIntent initWithRecipients_senders_searchTerms_attributes_dateTimeRange_identifiers_notificationIdentifiers_speakableGroupNames_conversationIdentifiersSelector (toNSArray recipients) (toNSArray senders) (toNSArray searchTerms) attributes (toINDateComponentsRange dateTimeRange) (toNSArray identifiers) (toNSArray notificationIdentifiers) (toNSArray speakableGroupNames) (toNSArray conversationIdentifiers)

-- | @- initWithRecipients:senders:searchTerms:attributes:dateTimeRange:identifiers:notificationIdentifiers:groupNames:@
initWithRecipients_senders_searchTerms_attributes_dateTimeRange_identifiers_notificationIdentifiers_groupNames :: (IsINSearchForMessagesIntent inSearchForMessagesIntent, IsNSArray recipients, IsNSArray senders, IsNSArray searchTerms, IsINDateComponentsRange dateTimeRange, IsNSArray identifiers, IsNSArray notificationIdentifiers, IsNSArray groupNames) => inSearchForMessagesIntent -> recipients -> senders -> searchTerms -> INMessageAttributeOptions -> dateTimeRange -> identifiers -> notificationIdentifiers -> groupNames -> IO (Id INSearchForMessagesIntent)
initWithRecipients_senders_searchTerms_attributes_dateTimeRange_identifiers_notificationIdentifiers_groupNames inSearchForMessagesIntent recipients senders searchTerms attributes dateTimeRange identifiers notificationIdentifiers groupNames =
  sendOwnedMessage inSearchForMessagesIntent initWithRecipients_senders_searchTerms_attributes_dateTimeRange_identifiers_notificationIdentifiers_groupNamesSelector (toNSArray recipients) (toNSArray senders) (toNSArray searchTerms) attributes (toINDateComponentsRange dateTimeRange) (toNSArray identifiers) (toNSArray notificationIdentifiers) (toNSArray groupNames)

-- | @- initWithRecipients:senders:searchTerms:attributes:dateTimeRange:identifiers:notificationIdentifiers:speakableGroupNames:@
initWithRecipients_senders_searchTerms_attributes_dateTimeRange_identifiers_notificationIdentifiers_speakableGroupNames :: (IsINSearchForMessagesIntent inSearchForMessagesIntent, IsNSArray recipients, IsNSArray senders, IsNSArray searchTerms, IsINDateComponentsRange dateTimeRange, IsNSArray identifiers, IsNSArray notificationIdentifiers, IsNSArray speakableGroupNames) => inSearchForMessagesIntent -> recipients -> senders -> searchTerms -> INMessageAttributeOptions -> dateTimeRange -> identifiers -> notificationIdentifiers -> speakableGroupNames -> IO (Id INSearchForMessagesIntent)
initWithRecipients_senders_searchTerms_attributes_dateTimeRange_identifiers_notificationIdentifiers_speakableGroupNames inSearchForMessagesIntent recipients senders searchTerms attributes dateTimeRange identifiers notificationIdentifiers speakableGroupNames =
  sendOwnedMessage inSearchForMessagesIntent initWithRecipients_senders_searchTerms_attributes_dateTimeRange_identifiers_notificationIdentifiers_speakableGroupNamesSelector (toNSArray recipients) (toNSArray senders) (toNSArray searchTerms) attributes (toINDateComponentsRange dateTimeRange) (toNSArray identifiers) (toNSArray notificationIdentifiers) (toNSArray speakableGroupNames)

-- | @- recipients@
recipients :: IsINSearchForMessagesIntent inSearchForMessagesIntent => inSearchForMessagesIntent -> IO (Id NSArray)
recipients inSearchForMessagesIntent =
  sendMessage inSearchForMessagesIntent recipientsSelector

-- | @- recipientsOperator@
recipientsOperator :: IsINSearchForMessagesIntent inSearchForMessagesIntent => inSearchForMessagesIntent -> IO INConditionalOperator
recipientsOperator inSearchForMessagesIntent =
  sendMessage inSearchForMessagesIntent recipientsOperatorSelector

-- | @- senders@
senders :: IsINSearchForMessagesIntent inSearchForMessagesIntent => inSearchForMessagesIntent -> IO (Id NSArray)
senders inSearchForMessagesIntent =
  sendMessage inSearchForMessagesIntent sendersSelector

-- | @- sendersOperator@
sendersOperator :: IsINSearchForMessagesIntent inSearchForMessagesIntent => inSearchForMessagesIntent -> IO INConditionalOperator
sendersOperator inSearchForMessagesIntent =
  sendMessage inSearchForMessagesIntent sendersOperatorSelector

-- | @- searchTerms@
searchTerms :: IsINSearchForMessagesIntent inSearchForMessagesIntent => inSearchForMessagesIntent -> IO (Id NSArray)
searchTerms inSearchForMessagesIntent =
  sendMessage inSearchForMessagesIntent searchTermsSelector

-- | @- searchTermsOperator@
searchTermsOperator :: IsINSearchForMessagesIntent inSearchForMessagesIntent => inSearchForMessagesIntent -> IO INConditionalOperator
searchTermsOperator inSearchForMessagesIntent =
  sendMessage inSearchForMessagesIntent searchTermsOperatorSelector

-- | @- attributes@
attributes :: IsINSearchForMessagesIntent inSearchForMessagesIntent => inSearchForMessagesIntent -> IO INMessageAttributeOptions
attributes inSearchForMessagesIntent =
  sendMessage inSearchForMessagesIntent attributesSelector

-- | @- dateTimeRange@
dateTimeRange :: IsINSearchForMessagesIntent inSearchForMessagesIntent => inSearchForMessagesIntent -> IO (Id INDateComponentsRange)
dateTimeRange inSearchForMessagesIntent =
  sendMessage inSearchForMessagesIntent dateTimeRangeSelector

-- | @- identifiers@
identifiers :: IsINSearchForMessagesIntent inSearchForMessagesIntent => inSearchForMessagesIntent -> IO (Id NSArray)
identifiers inSearchForMessagesIntent =
  sendMessage inSearchForMessagesIntent identifiersSelector

-- | @- identifiersOperator@
identifiersOperator :: IsINSearchForMessagesIntent inSearchForMessagesIntent => inSearchForMessagesIntent -> IO INConditionalOperator
identifiersOperator inSearchForMessagesIntent =
  sendMessage inSearchForMessagesIntent identifiersOperatorSelector

-- | @- notificationIdentifiers@
notificationIdentifiers :: IsINSearchForMessagesIntent inSearchForMessagesIntent => inSearchForMessagesIntent -> IO (Id NSArray)
notificationIdentifiers inSearchForMessagesIntent =
  sendMessage inSearchForMessagesIntent notificationIdentifiersSelector

-- | @- notificationIdentifiersOperator@
notificationIdentifiersOperator :: IsINSearchForMessagesIntent inSearchForMessagesIntent => inSearchForMessagesIntent -> IO INConditionalOperator
notificationIdentifiersOperator inSearchForMessagesIntent =
  sendMessage inSearchForMessagesIntent notificationIdentifiersOperatorSelector

-- | @- speakableGroupNames@
speakableGroupNames :: IsINSearchForMessagesIntent inSearchForMessagesIntent => inSearchForMessagesIntent -> IO (Id NSArray)
speakableGroupNames inSearchForMessagesIntent =
  sendMessage inSearchForMessagesIntent speakableGroupNamesSelector

-- | @- speakableGroupNamesOperator@
speakableGroupNamesOperator :: IsINSearchForMessagesIntent inSearchForMessagesIntent => inSearchForMessagesIntent -> IO INConditionalOperator
speakableGroupNamesOperator inSearchForMessagesIntent =
  sendMessage inSearchForMessagesIntent speakableGroupNamesOperatorSelector

-- | @- conversationIdentifiers@
conversationIdentifiers :: IsINSearchForMessagesIntent inSearchForMessagesIntent => inSearchForMessagesIntent -> IO (Id NSArray)
conversationIdentifiers inSearchForMessagesIntent =
  sendMessage inSearchForMessagesIntent conversationIdentifiersSelector

-- | @- conversationIdentifiersOperator@
conversationIdentifiersOperator :: IsINSearchForMessagesIntent inSearchForMessagesIntent => inSearchForMessagesIntent -> IO INConditionalOperator
conversationIdentifiersOperator inSearchForMessagesIntent =
  sendMessage inSearchForMessagesIntent conversationIdentifiersOperatorSelector

-- | @- groupNames@
groupNames :: IsINSearchForMessagesIntent inSearchForMessagesIntent => inSearchForMessagesIntent -> IO (Id NSArray)
groupNames inSearchForMessagesIntent =
  sendMessage inSearchForMessagesIntent groupNamesSelector

-- | @- groupNamesOperator@
groupNamesOperator :: IsINSearchForMessagesIntent inSearchForMessagesIntent => inSearchForMessagesIntent -> IO INConditionalOperator
groupNamesOperator inSearchForMessagesIntent =
  sendMessage inSearchForMessagesIntent groupNamesOperatorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRecipients:senders:searchTerms:attributes:dateTimeRange:identifiers:notificationIdentifiers:speakableGroupNames:conversationIdentifiers:@
initWithRecipients_senders_searchTerms_attributes_dateTimeRange_identifiers_notificationIdentifiers_speakableGroupNames_conversationIdentifiersSelector :: Selector '[Id NSArray, Id NSArray, Id NSArray, INMessageAttributeOptions, Id INDateComponentsRange, Id NSArray, Id NSArray, Id NSArray, Id NSArray] (Id INSearchForMessagesIntent)
initWithRecipients_senders_searchTerms_attributes_dateTimeRange_identifiers_notificationIdentifiers_speakableGroupNames_conversationIdentifiersSelector = mkSelector "initWithRecipients:senders:searchTerms:attributes:dateTimeRange:identifiers:notificationIdentifiers:speakableGroupNames:conversationIdentifiers:"

-- | @Selector@ for @initWithRecipients:senders:searchTerms:attributes:dateTimeRange:identifiers:notificationIdentifiers:groupNames:@
initWithRecipients_senders_searchTerms_attributes_dateTimeRange_identifiers_notificationIdentifiers_groupNamesSelector :: Selector '[Id NSArray, Id NSArray, Id NSArray, INMessageAttributeOptions, Id INDateComponentsRange, Id NSArray, Id NSArray, Id NSArray] (Id INSearchForMessagesIntent)
initWithRecipients_senders_searchTerms_attributes_dateTimeRange_identifiers_notificationIdentifiers_groupNamesSelector = mkSelector "initWithRecipients:senders:searchTerms:attributes:dateTimeRange:identifiers:notificationIdentifiers:groupNames:"

-- | @Selector@ for @initWithRecipients:senders:searchTerms:attributes:dateTimeRange:identifiers:notificationIdentifiers:speakableGroupNames:@
initWithRecipients_senders_searchTerms_attributes_dateTimeRange_identifiers_notificationIdentifiers_speakableGroupNamesSelector :: Selector '[Id NSArray, Id NSArray, Id NSArray, INMessageAttributeOptions, Id INDateComponentsRange, Id NSArray, Id NSArray, Id NSArray] (Id INSearchForMessagesIntent)
initWithRecipients_senders_searchTerms_attributes_dateTimeRange_identifiers_notificationIdentifiers_speakableGroupNamesSelector = mkSelector "initWithRecipients:senders:searchTerms:attributes:dateTimeRange:identifiers:notificationIdentifiers:speakableGroupNames:"

-- | @Selector@ for @recipients@
recipientsSelector :: Selector '[] (Id NSArray)
recipientsSelector = mkSelector "recipients"

-- | @Selector@ for @recipientsOperator@
recipientsOperatorSelector :: Selector '[] INConditionalOperator
recipientsOperatorSelector = mkSelector "recipientsOperator"

-- | @Selector@ for @senders@
sendersSelector :: Selector '[] (Id NSArray)
sendersSelector = mkSelector "senders"

-- | @Selector@ for @sendersOperator@
sendersOperatorSelector :: Selector '[] INConditionalOperator
sendersOperatorSelector = mkSelector "sendersOperator"

-- | @Selector@ for @searchTerms@
searchTermsSelector :: Selector '[] (Id NSArray)
searchTermsSelector = mkSelector "searchTerms"

-- | @Selector@ for @searchTermsOperator@
searchTermsOperatorSelector :: Selector '[] INConditionalOperator
searchTermsOperatorSelector = mkSelector "searchTermsOperator"

-- | @Selector@ for @attributes@
attributesSelector :: Selector '[] INMessageAttributeOptions
attributesSelector = mkSelector "attributes"

-- | @Selector@ for @dateTimeRange@
dateTimeRangeSelector :: Selector '[] (Id INDateComponentsRange)
dateTimeRangeSelector = mkSelector "dateTimeRange"

-- | @Selector@ for @identifiers@
identifiersSelector :: Selector '[] (Id NSArray)
identifiersSelector = mkSelector "identifiers"

-- | @Selector@ for @identifiersOperator@
identifiersOperatorSelector :: Selector '[] INConditionalOperator
identifiersOperatorSelector = mkSelector "identifiersOperator"

-- | @Selector@ for @notificationIdentifiers@
notificationIdentifiersSelector :: Selector '[] (Id NSArray)
notificationIdentifiersSelector = mkSelector "notificationIdentifiers"

-- | @Selector@ for @notificationIdentifiersOperator@
notificationIdentifiersOperatorSelector :: Selector '[] INConditionalOperator
notificationIdentifiersOperatorSelector = mkSelector "notificationIdentifiersOperator"

-- | @Selector@ for @speakableGroupNames@
speakableGroupNamesSelector :: Selector '[] (Id NSArray)
speakableGroupNamesSelector = mkSelector "speakableGroupNames"

-- | @Selector@ for @speakableGroupNamesOperator@
speakableGroupNamesOperatorSelector :: Selector '[] INConditionalOperator
speakableGroupNamesOperatorSelector = mkSelector "speakableGroupNamesOperator"

-- | @Selector@ for @conversationIdentifiers@
conversationIdentifiersSelector :: Selector '[] (Id NSArray)
conversationIdentifiersSelector = mkSelector "conversationIdentifiers"

-- | @Selector@ for @conversationIdentifiersOperator@
conversationIdentifiersOperatorSelector :: Selector '[] INConditionalOperator
conversationIdentifiersOperatorSelector = mkSelector "conversationIdentifiersOperator"

-- | @Selector@ for @groupNames@
groupNamesSelector :: Selector '[] (Id NSArray)
groupNamesSelector = mkSelector "groupNames"

-- | @Selector@ for @groupNamesOperator@
groupNamesOperatorSelector :: Selector '[] INConditionalOperator
groupNamesOperatorSelector = mkSelector "groupNamesOperator"

