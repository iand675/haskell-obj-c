{-# LANGUAGE PatternSynonyms #-}
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
  , initWithCallRecordFilter_callRecordToCallBack_audioRoute_destinationType_contacts_callCapabilitySelector
  , initWithAudioRoute_destinationType_contacts_recordTypeForRedialing_callCapabilitySelector
  , callRecordFilterSelector
  , callRecordToCallBackSelector
  , audioRouteSelector
  , destinationTypeSelector
  , contactsSelector
  , callCapabilitySelector
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

-- | @- initWithCallRecordFilter:callRecordToCallBack:audioRoute:destinationType:contacts:callCapability:@
initWithCallRecordFilter_callRecordToCallBack_audioRoute_destinationType_contacts_callCapability :: (IsINStartCallIntent inStartCallIntent, IsINCallRecordFilter callRecordFilter, IsINCallRecord callRecordToCallBack, IsNSArray contacts) => inStartCallIntent -> callRecordFilter -> callRecordToCallBack -> INCallAudioRoute -> INCallDestinationType -> contacts -> INCallCapability -> IO (Id INStartCallIntent)
initWithCallRecordFilter_callRecordToCallBack_audioRoute_destinationType_contacts_callCapability inStartCallIntent  callRecordFilter callRecordToCallBack audioRoute destinationType contacts callCapability =
withObjCPtr callRecordFilter $ \raw_callRecordFilter ->
  withObjCPtr callRecordToCallBack $ \raw_callRecordToCallBack ->
    withObjCPtr contacts $ \raw_contacts ->
        sendMsg inStartCallIntent (mkSelector "initWithCallRecordFilter:callRecordToCallBack:audioRoute:destinationType:contacts:callCapability:") (retPtr retVoid) [argPtr (castPtr raw_callRecordFilter :: Ptr ()), argPtr (castPtr raw_callRecordToCallBack :: Ptr ()), argCLong (coerce audioRoute), argCLong (coerce destinationType), argPtr (castPtr raw_contacts :: Ptr ()), argCLong (coerce callCapability)] >>= ownedObject . castPtr

-- | @- initWithAudioRoute:destinationType:contacts:recordTypeForRedialing:callCapability:@
initWithAudioRoute_destinationType_contacts_recordTypeForRedialing_callCapability :: (IsINStartCallIntent inStartCallIntent, IsNSArray contacts) => inStartCallIntent -> INCallAudioRoute -> INCallDestinationType -> contacts -> INCallRecordType -> INCallCapability -> IO (Id INStartCallIntent)
initWithAudioRoute_destinationType_contacts_recordTypeForRedialing_callCapability inStartCallIntent  audioRoute destinationType contacts recordTypeForRedialing callCapability =
withObjCPtr contacts $ \raw_contacts ->
    sendMsg inStartCallIntent (mkSelector "initWithAudioRoute:destinationType:contacts:recordTypeForRedialing:callCapability:") (retPtr retVoid) [argCLong (coerce audioRoute), argCLong (coerce destinationType), argPtr (castPtr raw_contacts :: Ptr ()), argCLong (coerce recordTypeForRedialing), argCLong (coerce callCapability)] >>= ownedObject . castPtr

-- | @- callRecordFilter@
callRecordFilter :: IsINStartCallIntent inStartCallIntent => inStartCallIntent -> IO (Id INCallRecordFilter)
callRecordFilter inStartCallIntent  =
  sendMsg inStartCallIntent (mkSelector "callRecordFilter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- callRecordToCallBack@
callRecordToCallBack :: IsINStartCallIntent inStartCallIntent => inStartCallIntent -> IO (Id INCallRecord)
callRecordToCallBack inStartCallIntent  =
  sendMsg inStartCallIntent (mkSelector "callRecordToCallBack") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- audioRoute@
audioRoute :: IsINStartCallIntent inStartCallIntent => inStartCallIntent -> IO INCallAudioRoute
audioRoute inStartCallIntent  =
  fmap (coerce :: CLong -> INCallAudioRoute) $ sendMsg inStartCallIntent (mkSelector "audioRoute") retCLong []

-- | @- destinationType@
destinationType :: IsINStartCallIntent inStartCallIntent => inStartCallIntent -> IO INCallDestinationType
destinationType inStartCallIntent  =
  fmap (coerce :: CLong -> INCallDestinationType) $ sendMsg inStartCallIntent (mkSelector "destinationType") retCLong []

-- | @- contacts@
contacts :: IsINStartCallIntent inStartCallIntent => inStartCallIntent -> IO (Id NSArray)
contacts inStartCallIntent  =
  sendMsg inStartCallIntent (mkSelector "contacts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- callCapability@
callCapability :: IsINStartCallIntent inStartCallIntent => inStartCallIntent -> IO INCallCapability
callCapability inStartCallIntent  =
  fmap (coerce :: CLong -> INCallCapability) $ sendMsg inStartCallIntent (mkSelector "callCapability") retCLong []

-- | @- recordTypeForRedialing@
recordTypeForRedialing :: IsINStartCallIntent inStartCallIntent => inStartCallIntent -> IO INCallRecordType
recordTypeForRedialing inStartCallIntent  =
  fmap (coerce :: CLong -> INCallRecordType) $ sendMsg inStartCallIntent (mkSelector "recordTypeForRedialing") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCallRecordFilter:callRecordToCallBack:audioRoute:destinationType:contacts:callCapability:@
initWithCallRecordFilter_callRecordToCallBack_audioRoute_destinationType_contacts_callCapabilitySelector :: Selector
initWithCallRecordFilter_callRecordToCallBack_audioRoute_destinationType_contacts_callCapabilitySelector = mkSelector "initWithCallRecordFilter:callRecordToCallBack:audioRoute:destinationType:contacts:callCapability:"

-- | @Selector@ for @initWithAudioRoute:destinationType:contacts:recordTypeForRedialing:callCapability:@
initWithAudioRoute_destinationType_contacts_recordTypeForRedialing_callCapabilitySelector :: Selector
initWithAudioRoute_destinationType_contacts_recordTypeForRedialing_callCapabilitySelector = mkSelector "initWithAudioRoute:destinationType:contacts:recordTypeForRedialing:callCapability:"

-- | @Selector@ for @callRecordFilter@
callRecordFilterSelector :: Selector
callRecordFilterSelector = mkSelector "callRecordFilter"

-- | @Selector@ for @callRecordToCallBack@
callRecordToCallBackSelector :: Selector
callRecordToCallBackSelector = mkSelector "callRecordToCallBack"

-- | @Selector@ for @audioRoute@
audioRouteSelector :: Selector
audioRouteSelector = mkSelector "audioRoute"

-- | @Selector@ for @destinationType@
destinationTypeSelector :: Selector
destinationTypeSelector = mkSelector "destinationType"

-- | @Selector@ for @contacts@
contactsSelector :: Selector
contactsSelector = mkSelector "contacts"

-- | @Selector@ for @callCapability@
callCapabilitySelector :: Selector
callCapabilitySelector = mkSelector "callCapability"

-- | @Selector@ for @recordTypeForRedialing@
recordTypeForRedialingSelector :: Selector
recordTypeForRedialingSelector = mkSelector "recordTypeForRedialing"

