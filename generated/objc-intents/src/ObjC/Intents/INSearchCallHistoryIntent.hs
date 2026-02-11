{-# LANGUAGE PatternSynonyms #-}
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
  , callType
  , initWithDateCreated_recipient_callCapabilities_callTypes_unseenSelector
  , initWithCallType_dateCreated_recipient_callCapabilitiesSelector
  , dateCreatedSelector
  , recipientSelector
  , callCapabilitiesSelector
  , callTypesSelector
  , callTypeSelector

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

-- | @- initWithDateCreated:recipient:callCapabilities:callTypes:unseen:@
initWithDateCreated_recipient_callCapabilities_callTypes_unseen :: (IsINSearchCallHistoryIntent inSearchCallHistoryIntent, IsINDateComponentsRange dateCreated, IsINPerson recipient, IsNSNumber unseen) => inSearchCallHistoryIntent -> dateCreated -> recipient -> INCallCapabilityOptions -> INCallRecordTypeOptions -> unseen -> IO (Id INSearchCallHistoryIntent)
initWithDateCreated_recipient_callCapabilities_callTypes_unseen inSearchCallHistoryIntent  dateCreated recipient callCapabilities callTypes unseen =
withObjCPtr dateCreated $ \raw_dateCreated ->
  withObjCPtr recipient $ \raw_recipient ->
    withObjCPtr unseen $ \raw_unseen ->
        sendMsg inSearchCallHistoryIntent (mkSelector "initWithDateCreated:recipient:callCapabilities:callTypes:unseen:") (retPtr retVoid) [argPtr (castPtr raw_dateCreated :: Ptr ()), argPtr (castPtr raw_recipient :: Ptr ()), argCULong (coerce callCapabilities), argCULong (coerce callTypes), argPtr (castPtr raw_unseen :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCallType:dateCreated:recipient:callCapabilities:@
initWithCallType_dateCreated_recipient_callCapabilities :: (IsINSearchCallHistoryIntent inSearchCallHistoryIntent, IsINDateComponentsRange dateCreated, IsINPerson recipient) => inSearchCallHistoryIntent -> INCallRecordType -> dateCreated -> recipient -> INCallCapabilityOptions -> IO (Id INSearchCallHistoryIntent)
initWithCallType_dateCreated_recipient_callCapabilities inSearchCallHistoryIntent  callType dateCreated recipient callCapabilities =
withObjCPtr dateCreated $ \raw_dateCreated ->
  withObjCPtr recipient $ \raw_recipient ->
      sendMsg inSearchCallHistoryIntent (mkSelector "initWithCallType:dateCreated:recipient:callCapabilities:") (retPtr retVoid) [argCLong (coerce callType), argPtr (castPtr raw_dateCreated :: Ptr ()), argPtr (castPtr raw_recipient :: Ptr ()), argCULong (coerce callCapabilities)] >>= ownedObject . castPtr

-- | @- dateCreated@
dateCreated :: IsINSearchCallHistoryIntent inSearchCallHistoryIntent => inSearchCallHistoryIntent -> IO (Id INDateComponentsRange)
dateCreated inSearchCallHistoryIntent  =
  sendMsg inSearchCallHistoryIntent (mkSelector "dateCreated") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- recipient@
recipient :: IsINSearchCallHistoryIntent inSearchCallHistoryIntent => inSearchCallHistoryIntent -> IO (Id INPerson)
recipient inSearchCallHistoryIntent  =
  sendMsg inSearchCallHistoryIntent (mkSelector "recipient") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- callCapabilities@
callCapabilities :: IsINSearchCallHistoryIntent inSearchCallHistoryIntent => inSearchCallHistoryIntent -> IO INCallCapabilityOptions
callCapabilities inSearchCallHistoryIntent  =
  fmap (coerce :: CULong -> INCallCapabilityOptions) $ sendMsg inSearchCallHistoryIntent (mkSelector "callCapabilities") retCULong []

-- | @- callTypes@
callTypes :: IsINSearchCallHistoryIntent inSearchCallHistoryIntent => inSearchCallHistoryIntent -> IO INCallRecordTypeOptions
callTypes inSearchCallHistoryIntent  =
  fmap (coerce :: CULong -> INCallRecordTypeOptions) $ sendMsg inSearchCallHistoryIntent (mkSelector "callTypes") retCULong []

-- | @- callType@
callType :: IsINSearchCallHistoryIntent inSearchCallHistoryIntent => inSearchCallHistoryIntent -> IO INCallRecordType
callType inSearchCallHistoryIntent  =
  fmap (coerce :: CLong -> INCallRecordType) $ sendMsg inSearchCallHistoryIntent (mkSelector "callType") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDateCreated:recipient:callCapabilities:callTypes:unseen:@
initWithDateCreated_recipient_callCapabilities_callTypes_unseenSelector :: Selector
initWithDateCreated_recipient_callCapabilities_callTypes_unseenSelector = mkSelector "initWithDateCreated:recipient:callCapabilities:callTypes:unseen:"

-- | @Selector@ for @initWithCallType:dateCreated:recipient:callCapabilities:@
initWithCallType_dateCreated_recipient_callCapabilitiesSelector :: Selector
initWithCallType_dateCreated_recipient_callCapabilitiesSelector = mkSelector "initWithCallType:dateCreated:recipient:callCapabilities:"

-- | @Selector@ for @dateCreated@
dateCreatedSelector :: Selector
dateCreatedSelector = mkSelector "dateCreated"

-- | @Selector@ for @recipient@
recipientSelector :: Selector
recipientSelector = mkSelector "recipient"

-- | @Selector@ for @callCapabilities@
callCapabilitiesSelector :: Selector
callCapabilitiesSelector = mkSelector "callCapabilities"

-- | @Selector@ for @callTypes@
callTypesSelector :: Selector
callTypesSelector = mkSelector "callTypes"

-- | @Selector@ for @callType@
callTypeSelector :: Selector
callTypeSelector = mkSelector "callType"

