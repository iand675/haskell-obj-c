{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INStartCallIntent@.
module ObjC.Intents.INStartCallIntent
  ( INStartCallIntent
  , IsINStartCallIntent(..)
  , initWithCallRecordFilter_callRecordToCallBack_audioRoute_destinationType_contacts_callCapability
  , initWithAudioRoute_destinationType_contacts_recordTypeForRedialing_callCapability
  , callRecordFilter
  , callRecordToCallBack
  , audioRoute
  , destinationType
  , contacts
  , callCapability
  , recordTypeForRedialing
  , audioRouteSelector
  , callCapabilitySelector
  , callRecordFilterSelector
  , callRecordToCallBackSelector
  , contactsSelector
  , destinationTypeSelector
  , initWithAudioRoute_destinationType_contacts_recordTypeForRedialing_callCapabilitySelector
  , initWithCallRecordFilter_callRecordToCallBack_audioRoute_destinationType_contacts_callCapabilitySelector
  , recordTypeForRedialingSelector

  -- * Enum types
  , INCallAudioRoute(INCallAudioRoute)
  , pattern INCallAudioRouteUnknown
  , pattern INCallAudioRouteSpeakerphoneAudioRoute
  , pattern INCallAudioRouteBluetoothAudioRoute
  , INCallCapability(INCallCapability)
  , pattern INCallCapabilityUnknown
  , pattern INCallCapabilityAudioCall
  , pattern INCallCapabilityVideoCall
  , INCallDestinationType(INCallDestinationType)
  , pattern INCallDestinationTypeUnknown
  , pattern INCallDestinationTypeNormal
  , pattern INCallDestinationTypeEmergency
  , pattern INCallDestinationTypeVoicemail
  , pattern INCallDestinationTypeRedial
  , pattern INCallDestinationTypeCallBack
  , pattern INCallDestinationTypeNormalDestination
  , pattern INCallDestinationTypeEmergencyDestination
  , pattern INCallDestinationTypeVoicemailDestination
  , pattern INCallDestinationTypeRedialDestination
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

-- | @- initWithCallRecordFilter:callRecordToCallBack:audioRoute:destinationType:contacts:callCapability:@
initWithCallRecordFilter_callRecordToCallBack_audioRoute_destinationType_contacts_callCapability :: (IsINStartCallIntent inStartCallIntent, IsINCallRecordFilter callRecordFilter, IsINCallRecord callRecordToCallBack, IsNSArray contacts) => inStartCallIntent -> callRecordFilter -> callRecordToCallBack -> INCallAudioRoute -> INCallDestinationType -> contacts -> INCallCapability -> IO (Id INStartCallIntent)
initWithCallRecordFilter_callRecordToCallBack_audioRoute_destinationType_contacts_callCapability inStartCallIntent callRecordFilter callRecordToCallBack audioRoute destinationType contacts callCapability =
  sendOwnedMessage inStartCallIntent initWithCallRecordFilter_callRecordToCallBack_audioRoute_destinationType_contacts_callCapabilitySelector (toINCallRecordFilter callRecordFilter) (toINCallRecord callRecordToCallBack) audioRoute destinationType (toNSArray contacts) callCapability

-- | @- initWithAudioRoute:destinationType:contacts:recordTypeForRedialing:callCapability:@
initWithAudioRoute_destinationType_contacts_recordTypeForRedialing_callCapability :: (IsINStartCallIntent inStartCallIntent, IsNSArray contacts) => inStartCallIntent -> INCallAudioRoute -> INCallDestinationType -> contacts -> INCallRecordType -> INCallCapability -> IO (Id INStartCallIntent)
initWithAudioRoute_destinationType_contacts_recordTypeForRedialing_callCapability inStartCallIntent audioRoute destinationType contacts recordTypeForRedialing callCapability =
  sendOwnedMessage inStartCallIntent initWithAudioRoute_destinationType_contacts_recordTypeForRedialing_callCapabilitySelector audioRoute destinationType (toNSArray contacts) recordTypeForRedialing callCapability

-- | @- callRecordFilter@
callRecordFilter :: IsINStartCallIntent inStartCallIntent => inStartCallIntent -> IO (Id INCallRecordFilter)
callRecordFilter inStartCallIntent =
  sendMessage inStartCallIntent callRecordFilterSelector

-- | @- callRecordToCallBack@
callRecordToCallBack :: IsINStartCallIntent inStartCallIntent => inStartCallIntent -> IO (Id INCallRecord)
callRecordToCallBack inStartCallIntent =
  sendMessage inStartCallIntent callRecordToCallBackSelector

-- | @- audioRoute@
audioRoute :: IsINStartCallIntent inStartCallIntent => inStartCallIntent -> IO INCallAudioRoute
audioRoute inStartCallIntent =
  sendMessage inStartCallIntent audioRouteSelector

-- | @- destinationType@
destinationType :: IsINStartCallIntent inStartCallIntent => inStartCallIntent -> IO INCallDestinationType
destinationType inStartCallIntent =
  sendMessage inStartCallIntent destinationTypeSelector

-- | @- contacts@
contacts :: IsINStartCallIntent inStartCallIntent => inStartCallIntent -> IO (Id NSArray)
contacts inStartCallIntent =
  sendMessage inStartCallIntent contactsSelector

-- | @- callCapability@
callCapability :: IsINStartCallIntent inStartCallIntent => inStartCallIntent -> IO INCallCapability
callCapability inStartCallIntent =
  sendMessage inStartCallIntent callCapabilitySelector

-- | @- recordTypeForRedialing@
recordTypeForRedialing :: IsINStartCallIntent inStartCallIntent => inStartCallIntent -> IO INCallRecordType
recordTypeForRedialing inStartCallIntent =
  sendMessage inStartCallIntent recordTypeForRedialingSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCallRecordFilter:callRecordToCallBack:audioRoute:destinationType:contacts:callCapability:@
initWithCallRecordFilter_callRecordToCallBack_audioRoute_destinationType_contacts_callCapabilitySelector :: Selector '[Id INCallRecordFilter, Id INCallRecord, INCallAudioRoute, INCallDestinationType, Id NSArray, INCallCapability] (Id INStartCallIntent)
initWithCallRecordFilter_callRecordToCallBack_audioRoute_destinationType_contacts_callCapabilitySelector = mkSelector "initWithCallRecordFilter:callRecordToCallBack:audioRoute:destinationType:contacts:callCapability:"

-- | @Selector@ for @initWithAudioRoute:destinationType:contacts:recordTypeForRedialing:callCapability:@
initWithAudioRoute_destinationType_contacts_recordTypeForRedialing_callCapabilitySelector :: Selector '[INCallAudioRoute, INCallDestinationType, Id NSArray, INCallRecordType, INCallCapability] (Id INStartCallIntent)
initWithAudioRoute_destinationType_contacts_recordTypeForRedialing_callCapabilitySelector = mkSelector "initWithAudioRoute:destinationType:contacts:recordTypeForRedialing:callCapability:"

-- | @Selector@ for @callRecordFilter@
callRecordFilterSelector :: Selector '[] (Id INCallRecordFilter)
callRecordFilterSelector = mkSelector "callRecordFilter"

-- | @Selector@ for @callRecordToCallBack@
callRecordToCallBackSelector :: Selector '[] (Id INCallRecord)
callRecordToCallBackSelector = mkSelector "callRecordToCallBack"

-- | @Selector@ for @audioRoute@
audioRouteSelector :: Selector '[] INCallAudioRoute
audioRouteSelector = mkSelector "audioRoute"

-- | @Selector@ for @destinationType@
destinationTypeSelector :: Selector '[] INCallDestinationType
destinationTypeSelector = mkSelector "destinationType"

-- | @Selector@ for @contacts@
contactsSelector :: Selector '[] (Id NSArray)
contactsSelector = mkSelector "contacts"

-- | @Selector@ for @callCapability@
callCapabilitySelector :: Selector '[] INCallCapability
callCapabilitySelector = mkSelector "callCapability"

-- | @Selector@ for @recordTypeForRedialing@
recordTypeForRedialingSelector :: Selector '[] INCallRecordType
recordTypeForRedialingSelector = mkSelector "recordTypeForRedialing"

