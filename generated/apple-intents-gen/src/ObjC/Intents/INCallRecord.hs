{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INCallRecord@.
module ObjC.Intents.INCallRecord
  ( INCallRecord
  , IsINCallRecord(..)
  , init_
  , initWithIdentifier_dateCreated_callRecordType_callCapability_callDuration_unseen_participants_numberOfCalls_isCallerIdBlocked
  , initWithIdentifier_dateCreated_callRecordType_callCapability_callDuration_unseen
  , initWithIdentifier_dateCreated_callRecordType_callCapability_callDuration_unseen_numberOfCalls
  , initWithIdentifier_dateCreated_caller_callRecordType_callCapability_callDuration_unseen
  , initWithIdentifier_dateCreated_caller_callRecordType_callCapability_callDuration_unseen_numberOfCalls
  , identifier
  , dateCreated
  , callRecordType
  , callDuration
  , unseen
  , callCapability
  , numberOfCalls
  , isCallerIdBlocked
  , participants
  , caller
  , callCapabilitySelector
  , callDurationSelector
  , callRecordTypeSelector
  , callerSelector
  , dateCreatedSelector
  , identifierSelector
  , initSelector
  , initWithIdentifier_dateCreated_callRecordType_callCapability_callDuration_unseenSelector
  , initWithIdentifier_dateCreated_callRecordType_callCapability_callDuration_unseen_numberOfCallsSelector
  , initWithIdentifier_dateCreated_callRecordType_callCapability_callDuration_unseen_participants_numberOfCalls_isCallerIdBlockedSelector
  , initWithIdentifier_dateCreated_caller_callRecordType_callCapability_callDuration_unseenSelector
  , initWithIdentifier_dateCreated_caller_callRecordType_callCapability_callDuration_unseen_numberOfCallsSelector
  , isCallerIdBlockedSelector
  , numberOfCallsSelector
  , participantsSelector
  , unseenSelector

  -- * Enum types
  , INCallCapability(INCallCapability)
  , pattern INCallCapabilityUnknown
  , pattern INCallCapabilityAudioCall
  , pattern INCallCapabilityVideoCall
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

-- | @- init@
init_ :: IsINCallRecord inCallRecord => inCallRecord -> IO (Id INCallRecord)
init_ inCallRecord =
  sendOwnedMessage inCallRecord initSelector

-- | @- initWithIdentifier:dateCreated:callRecordType:callCapability:callDuration:unseen:participants:numberOfCalls:isCallerIdBlocked:@
initWithIdentifier_dateCreated_callRecordType_callCapability_callDuration_unseen_participants_numberOfCalls_isCallerIdBlocked :: (IsINCallRecord inCallRecord, IsNSString identifier, IsNSDate dateCreated, IsNSNumber callDuration, IsNSNumber unseen, IsNSArray participants, IsNSNumber numberOfCalls, IsNSNumber isCallerIdBlocked) => inCallRecord -> identifier -> dateCreated -> INCallRecordType -> INCallCapability -> callDuration -> unseen -> participants -> numberOfCalls -> isCallerIdBlocked -> IO (Id INCallRecord)
initWithIdentifier_dateCreated_callRecordType_callCapability_callDuration_unseen_participants_numberOfCalls_isCallerIdBlocked inCallRecord identifier dateCreated callRecordType callCapability callDuration unseen participants numberOfCalls isCallerIdBlocked =
  sendOwnedMessage inCallRecord initWithIdentifier_dateCreated_callRecordType_callCapability_callDuration_unseen_participants_numberOfCalls_isCallerIdBlockedSelector (toNSString identifier) (toNSDate dateCreated) callRecordType callCapability (toNSNumber callDuration) (toNSNumber unseen) (toNSArray participants) (toNSNumber numberOfCalls) (toNSNumber isCallerIdBlocked)

-- | @- initWithIdentifier:dateCreated:callRecordType:callCapability:callDuration:unseen:@
initWithIdentifier_dateCreated_callRecordType_callCapability_callDuration_unseen :: (IsINCallRecord inCallRecord, IsNSString identifier, IsNSDate dateCreated, IsNSNumber callDuration, IsNSNumber unseen) => inCallRecord -> identifier -> dateCreated -> INCallRecordType -> INCallCapability -> callDuration -> unseen -> IO (Id INCallRecord)
initWithIdentifier_dateCreated_callRecordType_callCapability_callDuration_unseen inCallRecord identifier dateCreated callRecordType callCapability callDuration unseen =
  sendOwnedMessage inCallRecord initWithIdentifier_dateCreated_callRecordType_callCapability_callDuration_unseenSelector (toNSString identifier) (toNSDate dateCreated) callRecordType callCapability (toNSNumber callDuration) (toNSNumber unseen)

-- | @- initWithIdentifier:dateCreated:callRecordType:callCapability:callDuration:unseen:numberOfCalls:@
initWithIdentifier_dateCreated_callRecordType_callCapability_callDuration_unseen_numberOfCalls :: (IsINCallRecord inCallRecord, IsNSString identifier, IsNSDate dateCreated, IsNSNumber callDuration, IsNSNumber unseen, IsNSNumber numberOfCalls) => inCallRecord -> identifier -> dateCreated -> INCallRecordType -> INCallCapability -> callDuration -> unseen -> numberOfCalls -> IO (Id INCallRecord)
initWithIdentifier_dateCreated_callRecordType_callCapability_callDuration_unseen_numberOfCalls inCallRecord identifier dateCreated callRecordType callCapability callDuration unseen numberOfCalls =
  sendOwnedMessage inCallRecord initWithIdentifier_dateCreated_callRecordType_callCapability_callDuration_unseen_numberOfCallsSelector (toNSString identifier) (toNSDate dateCreated) callRecordType callCapability (toNSNumber callDuration) (toNSNumber unseen) (toNSNumber numberOfCalls)

-- | @- initWithIdentifier:dateCreated:caller:callRecordType:callCapability:callDuration:unseen:@
initWithIdentifier_dateCreated_caller_callRecordType_callCapability_callDuration_unseen :: (IsINCallRecord inCallRecord, IsNSString identifier, IsNSDate dateCreated, IsINPerson caller, IsNSNumber callDuration, IsNSNumber unseen) => inCallRecord -> identifier -> dateCreated -> caller -> INCallRecordType -> INCallCapability -> callDuration -> unseen -> IO (Id INCallRecord)
initWithIdentifier_dateCreated_caller_callRecordType_callCapability_callDuration_unseen inCallRecord identifier dateCreated caller callRecordType callCapability callDuration unseen =
  sendOwnedMessage inCallRecord initWithIdentifier_dateCreated_caller_callRecordType_callCapability_callDuration_unseenSelector (toNSString identifier) (toNSDate dateCreated) (toINPerson caller) callRecordType callCapability (toNSNumber callDuration) (toNSNumber unseen)

-- | @- initWithIdentifier:dateCreated:caller:callRecordType:callCapability:callDuration:unseen:numberOfCalls:@
initWithIdentifier_dateCreated_caller_callRecordType_callCapability_callDuration_unseen_numberOfCalls :: (IsINCallRecord inCallRecord, IsNSString identifier, IsNSDate dateCreated, IsINPerson caller, IsNSNumber callDuration, IsNSNumber unseen, IsNSNumber numberOfCalls) => inCallRecord -> identifier -> dateCreated -> caller -> INCallRecordType -> INCallCapability -> callDuration -> unseen -> numberOfCalls -> IO (Id INCallRecord)
initWithIdentifier_dateCreated_caller_callRecordType_callCapability_callDuration_unseen_numberOfCalls inCallRecord identifier dateCreated caller callRecordType callCapability callDuration unseen numberOfCalls =
  sendOwnedMessage inCallRecord initWithIdentifier_dateCreated_caller_callRecordType_callCapability_callDuration_unseen_numberOfCallsSelector (toNSString identifier) (toNSDate dateCreated) (toINPerson caller) callRecordType callCapability (toNSNumber callDuration) (toNSNumber unseen) (toNSNumber numberOfCalls)

-- | @- identifier@
identifier :: IsINCallRecord inCallRecord => inCallRecord -> IO (Id NSString)
identifier inCallRecord =
  sendMessage inCallRecord identifierSelector

-- | @- dateCreated@
dateCreated :: IsINCallRecord inCallRecord => inCallRecord -> IO (Id NSDate)
dateCreated inCallRecord =
  sendMessage inCallRecord dateCreatedSelector

-- | @- callRecordType@
callRecordType :: IsINCallRecord inCallRecord => inCallRecord -> IO INCallRecordType
callRecordType inCallRecord =
  sendMessage inCallRecord callRecordTypeSelector

-- | @- callDuration@
callDuration :: IsINCallRecord inCallRecord => inCallRecord -> IO (Id NSNumber)
callDuration inCallRecord =
  sendMessage inCallRecord callDurationSelector

-- | @- unseen@
unseen :: IsINCallRecord inCallRecord => inCallRecord -> IO (Id NSNumber)
unseen inCallRecord =
  sendMessage inCallRecord unseenSelector

-- | @- callCapability@
callCapability :: IsINCallRecord inCallRecord => inCallRecord -> IO INCallCapability
callCapability inCallRecord =
  sendMessage inCallRecord callCapabilitySelector

-- | @- numberOfCalls@
numberOfCalls :: IsINCallRecord inCallRecord => inCallRecord -> IO (Id NSNumber)
numberOfCalls inCallRecord =
  sendMessage inCallRecord numberOfCallsSelector

-- | @- isCallerIdBlocked@
isCallerIdBlocked :: IsINCallRecord inCallRecord => inCallRecord -> IO (Id NSNumber)
isCallerIdBlocked inCallRecord =
  sendMessage inCallRecord isCallerIdBlockedSelector

-- | @- participants@
participants :: IsINCallRecord inCallRecord => inCallRecord -> IO (Id NSArray)
participants inCallRecord =
  sendMessage inCallRecord participantsSelector

-- | @- caller@
caller :: IsINCallRecord inCallRecord => inCallRecord -> IO (Id INPerson)
caller inCallRecord =
  sendMessage inCallRecord callerSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INCallRecord)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithIdentifier:dateCreated:callRecordType:callCapability:callDuration:unseen:participants:numberOfCalls:isCallerIdBlocked:@
initWithIdentifier_dateCreated_callRecordType_callCapability_callDuration_unseen_participants_numberOfCalls_isCallerIdBlockedSelector :: Selector '[Id NSString, Id NSDate, INCallRecordType, INCallCapability, Id NSNumber, Id NSNumber, Id NSArray, Id NSNumber, Id NSNumber] (Id INCallRecord)
initWithIdentifier_dateCreated_callRecordType_callCapability_callDuration_unseen_participants_numberOfCalls_isCallerIdBlockedSelector = mkSelector "initWithIdentifier:dateCreated:callRecordType:callCapability:callDuration:unseen:participants:numberOfCalls:isCallerIdBlocked:"

-- | @Selector@ for @initWithIdentifier:dateCreated:callRecordType:callCapability:callDuration:unseen:@
initWithIdentifier_dateCreated_callRecordType_callCapability_callDuration_unseenSelector :: Selector '[Id NSString, Id NSDate, INCallRecordType, INCallCapability, Id NSNumber, Id NSNumber] (Id INCallRecord)
initWithIdentifier_dateCreated_callRecordType_callCapability_callDuration_unseenSelector = mkSelector "initWithIdentifier:dateCreated:callRecordType:callCapability:callDuration:unseen:"

-- | @Selector@ for @initWithIdentifier:dateCreated:callRecordType:callCapability:callDuration:unseen:numberOfCalls:@
initWithIdentifier_dateCreated_callRecordType_callCapability_callDuration_unseen_numberOfCallsSelector :: Selector '[Id NSString, Id NSDate, INCallRecordType, INCallCapability, Id NSNumber, Id NSNumber, Id NSNumber] (Id INCallRecord)
initWithIdentifier_dateCreated_callRecordType_callCapability_callDuration_unseen_numberOfCallsSelector = mkSelector "initWithIdentifier:dateCreated:callRecordType:callCapability:callDuration:unseen:numberOfCalls:"

-- | @Selector@ for @initWithIdentifier:dateCreated:caller:callRecordType:callCapability:callDuration:unseen:@
initWithIdentifier_dateCreated_caller_callRecordType_callCapability_callDuration_unseenSelector :: Selector '[Id NSString, Id NSDate, Id INPerson, INCallRecordType, INCallCapability, Id NSNumber, Id NSNumber] (Id INCallRecord)
initWithIdentifier_dateCreated_caller_callRecordType_callCapability_callDuration_unseenSelector = mkSelector "initWithIdentifier:dateCreated:caller:callRecordType:callCapability:callDuration:unseen:"

-- | @Selector@ for @initWithIdentifier:dateCreated:caller:callRecordType:callCapability:callDuration:unseen:numberOfCalls:@
initWithIdentifier_dateCreated_caller_callRecordType_callCapability_callDuration_unseen_numberOfCallsSelector :: Selector '[Id NSString, Id NSDate, Id INPerson, INCallRecordType, INCallCapability, Id NSNumber, Id NSNumber, Id NSNumber] (Id INCallRecord)
initWithIdentifier_dateCreated_caller_callRecordType_callCapability_callDuration_unseen_numberOfCallsSelector = mkSelector "initWithIdentifier:dateCreated:caller:callRecordType:callCapability:callDuration:unseen:numberOfCalls:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @dateCreated@
dateCreatedSelector :: Selector '[] (Id NSDate)
dateCreatedSelector = mkSelector "dateCreated"

-- | @Selector@ for @callRecordType@
callRecordTypeSelector :: Selector '[] INCallRecordType
callRecordTypeSelector = mkSelector "callRecordType"

-- | @Selector@ for @callDuration@
callDurationSelector :: Selector '[] (Id NSNumber)
callDurationSelector = mkSelector "callDuration"

-- | @Selector@ for @unseen@
unseenSelector :: Selector '[] (Id NSNumber)
unseenSelector = mkSelector "unseen"

-- | @Selector@ for @callCapability@
callCapabilitySelector :: Selector '[] INCallCapability
callCapabilitySelector = mkSelector "callCapability"

-- | @Selector@ for @numberOfCalls@
numberOfCallsSelector :: Selector '[] (Id NSNumber)
numberOfCallsSelector = mkSelector "numberOfCalls"

-- | @Selector@ for @isCallerIdBlocked@
isCallerIdBlockedSelector :: Selector '[] (Id NSNumber)
isCallerIdBlockedSelector = mkSelector "isCallerIdBlocked"

-- | @Selector@ for @participants@
participantsSelector :: Selector '[] (Id NSArray)
participantsSelector = mkSelector "participants"

-- | @Selector@ for @caller@
callerSelector :: Selector '[] (Id INPerson)
callerSelector = mkSelector "caller"

