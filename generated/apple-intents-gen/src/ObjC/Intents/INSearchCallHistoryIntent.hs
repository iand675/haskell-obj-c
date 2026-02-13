{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSearchCallHistoryIntent@.
module ObjC.Intents.INSearchCallHistoryIntent
  ( INSearchCallHistoryIntent
  , IsINSearchCallHistoryIntent(..)
  , initWithDateCreated_recipient_callCapabilities_callTypes_unseen
  , initWithCallType_dateCreated_recipient_callCapabilities
  , dateCreated
  , recipient
  , callCapabilities
  , callTypes
  , unseen
  , callType
  , callCapabilitiesSelector
  , callTypeSelector
  , callTypesSelector
  , dateCreatedSelector
  , initWithCallType_dateCreated_recipient_callCapabilitiesSelector
  , initWithDateCreated_recipient_callCapabilities_callTypes_unseenSelector
  , recipientSelector
  , unseenSelector

  -- * Enum types
  , INCallCapabilityOptions(INCallCapabilityOptions)
  , pattern INCallCapabilityOptionAudioCall
  , pattern INCallCapabilityOptionVideoCall
  , INCallRecordType(INCallRecordType)
  , pattern INCallRecordTypeUnknown
  , pattern INCallRecordTypeOutgoing
  , pattern INCallRecordTypeMissed
  , pattern INCallRecordTypeReceived
  , pattern INCallRecordTypeLatest
  , pattern INCallRecordTypeVoicemail
  , pattern INCallRecordTypeRinging
  , pattern INCallRecordTypeInProgress
  , pattern INCallRecordTypeOnHold
  , INCallRecordTypeOptions(INCallRecordTypeOptions)
  , pattern INCallRecordTypeOptionOutgoing
  , pattern INCallRecordTypeOptionMissed
  , pattern INCallRecordTypeOptionReceived
  , pattern INCallRecordTypeOptionLatest
  , pattern INCallRecordTypeOptionVoicemail
  , pattern INCallRecordTypeOptionRinging
  , pattern INCallRecordTypeOptionInProgress
  , pattern INCallRecordTypeOptionOnHold

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

-- | @- initWithDateCreated:recipient:callCapabilities:callTypes:unseen:@
initWithDateCreated_recipient_callCapabilities_callTypes_unseen :: (IsINSearchCallHistoryIntent inSearchCallHistoryIntent, IsINDateComponentsRange dateCreated, IsINPerson recipient, IsNSNumber unseen) => inSearchCallHistoryIntent -> dateCreated -> recipient -> INCallCapabilityOptions -> INCallRecordTypeOptions -> unseen -> IO (Id INSearchCallHistoryIntent)
initWithDateCreated_recipient_callCapabilities_callTypes_unseen inSearchCallHistoryIntent dateCreated recipient callCapabilities callTypes unseen =
  sendOwnedMessage inSearchCallHistoryIntent initWithDateCreated_recipient_callCapabilities_callTypes_unseenSelector (toINDateComponentsRange dateCreated) (toINPerson recipient) callCapabilities callTypes (toNSNumber unseen)

-- | @- initWithCallType:dateCreated:recipient:callCapabilities:@
initWithCallType_dateCreated_recipient_callCapabilities :: (IsINSearchCallHistoryIntent inSearchCallHistoryIntent, IsINDateComponentsRange dateCreated, IsINPerson recipient) => inSearchCallHistoryIntent -> INCallRecordType -> dateCreated -> recipient -> INCallCapabilityOptions -> IO (Id INSearchCallHistoryIntent)
initWithCallType_dateCreated_recipient_callCapabilities inSearchCallHistoryIntent callType dateCreated recipient callCapabilities =
  sendOwnedMessage inSearchCallHistoryIntent initWithCallType_dateCreated_recipient_callCapabilitiesSelector callType (toINDateComponentsRange dateCreated) (toINPerson recipient) callCapabilities

-- | @- dateCreated@
dateCreated :: IsINSearchCallHistoryIntent inSearchCallHistoryIntent => inSearchCallHistoryIntent -> IO (Id INDateComponentsRange)
dateCreated inSearchCallHistoryIntent =
  sendMessage inSearchCallHistoryIntent dateCreatedSelector

-- | @- recipient@
recipient :: IsINSearchCallHistoryIntent inSearchCallHistoryIntent => inSearchCallHistoryIntent -> IO (Id INPerson)
recipient inSearchCallHistoryIntent =
  sendMessage inSearchCallHistoryIntent recipientSelector

-- | @- callCapabilities@
callCapabilities :: IsINSearchCallHistoryIntent inSearchCallHistoryIntent => inSearchCallHistoryIntent -> IO INCallCapabilityOptions
callCapabilities inSearchCallHistoryIntent =
  sendMessage inSearchCallHistoryIntent callCapabilitiesSelector

-- | @- callTypes@
callTypes :: IsINSearchCallHistoryIntent inSearchCallHistoryIntent => inSearchCallHistoryIntent -> IO INCallRecordTypeOptions
callTypes inSearchCallHistoryIntent =
  sendMessage inSearchCallHistoryIntent callTypesSelector

-- | @- unseen@
unseen :: IsINSearchCallHistoryIntent inSearchCallHistoryIntent => inSearchCallHistoryIntent -> IO (Id NSNumber)
unseen inSearchCallHistoryIntent =
  sendMessage inSearchCallHistoryIntent unseenSelector

-- | @- callType@
callType :: IsINSearchCallHistoryIntent inSearchCallHistoryIntent => inSearchCallHistoryIntent -> IO INCallRecordType
callType inSearchCallHistoryIntent =
  sendMessage inSearchCallHistoryIntent callTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDateCreated:recipient:callCapabilities:callTypes:unseen:@
initWithDateCreated_recipient_callCapabilities_callTypes_unseenSelector :: Selector '[Id INDateComponentsRange, Id INPerson, INCallCapabilityOptions, INCallRecordTypeOptions, Id NSNumber] (Id INSearchCallHistoryIntent)
initWithDateCreated_recipient_callCapabilities_callTypes_unseenSelector = mkSelector "initWithDateCreated:recipient:callCapabilities:callTypes:unseen:"

-- | @Selector@ for @initWithCallType:dateCreated:recipient:callCapabilities:@
initWithCallType_dateCreated_recipient_callCapabilitiesSelector :: Selector '[INCallRecordType, Id INDateComponentsRange, Id INPerson, INCallCapabilityOptions] (Id INSearchCallHistoryIntent)
initWithCallType_dateCreated_recipient_callCapabilitiesSelector = mkSelector "initWithCallType:dateCreated:recipient:callCapabilities:"

-- | @Selector@ for @dateCreated@
dateCreatedSelector :: Selector '[] (Id INDateComponentsRange)
dateCreatedSelector = mkSelector "dateCreated"

-- | @Selector@ for @recipient@
recipientSelector :: Selector '[] (Id INPerson)
recipientSelector = mkSelector "recipient"

-- | @Selector@ for @callCapabilities@
callCapabilitiesSelector :: Selector '[] INCallCapabilityOptions
callCapabilitiesSelector = mkSelector "callCapabilities"

-- | @Selector@ for @callTypes@
callTypesSelector :: Selector '[] INCallRecordTypeOptions
callTypesSelector = mkSelector "callTypes"

-- | @Selector@ for @unseen@
unseenSelector :: Selector '[] (Id NSNumber)
unseenSelector = mkSelector "unseen"

-- | @Selector@ for @callType@
callTypeSelector :: Selector '[] INCallRecordType
callTypeSelector = mkSelector "callType"

