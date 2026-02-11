{-# LANGUAGE PatternSynonyms #-}
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
  , callCapability
  , participants
  , caller
  , initSelector
  , initWithIdentifier_dateCreated_callRecordType_callCapability_callDuration_unseen_participants_numberOfCalls_isCallerIdBlockedSelector
  , initWithIdentifier_dateCreated_callRecordType_callCapability_callDuration_unseenSelector
  , initWithIdentifier_dateCreated_callRecordType_callCapability_callDuration_unseen_numberOfCallsSelector
  , initWithIdentifier_dateCreated_caller_callRecordType_callCapability_callDuration_unseenSelector
  , initWithIdentifier_dateCreated_caller_callRecordType_callCapability_callDuration_unseen_numberOfCallsSelector
  , identifierSelector
  , dateCreatedSelector
  , callRecordTypeSelector
  , callCapabilitySelector
  , participantsSelector
  , callerSelector

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
init_ :: IsINCallRecord inCallRecord => inCallRecord -> IO (Id INCallRecord)
init_ inCallRecord  =
  sendMsg inCallRecord (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithIdentifier:dateCreated:callRecordType:callCapability:callDuration:unseen:participants:numberOfCalls:isCallerIdBlocked:@
initWithIdentifier_dateCreated_callRecordType_callCapability_callDuration_unseen_participants_numberOfCalls_isCallerIdBlocked :: (IsINCallRecord inCallRecord, IsNSString identifier, IsNSDate dateCreated, IsNSNumber callDuration, IsNSNumber unseen, IsNSArray participants, IsNSNumber numberOfCalls, IsNSNumber isCallerIdBlocked) => inCallRecord -> identifier -> dateCreated -> INCallRecordType -> INCallCapability -> callDuration -> unseen -> participants -> numberOfCalls -> isCallerIdBlocked -> IO (Id INCallRecord)
initWithIdentifier_dateCreated_callRecordType_callCapability_callDuration_unseen_participants_numberOfCalls_isCallerIdBlocked inCallRecord  identifier dateCreated callRecordType callCapability callDuration unseen participants numberOfCalls isCallerIdBlocked =
withObjCPtr identifier $ \raw_identifier ->
  withObjCPtr dateCreated $ \raw_dateCreated ->
    withObjCPtr callDuration $ \raw_callDuration ->
      withObjCPtr unseen $ \raw_unseen ->
        withObjCPtr participants $ \raw_participants ->
          withObjCPtr numberOfCalls $ \raw_numberOfCalls ->
            withObjCPtr isCallerIdBlocked $ \raw_isCallerIdBlocked ->
                sendMsg inCallRecord (mkSelector "initWithIdentifier:dateCreated:callRecordType:callCapability:callDuration:unseen:participants:numberOfCalls:isCallerIdBlocked:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_dateCreated :: Ptr ()), argCLong (coerce callRecordType), argCLong (coerce callCapability), argPtr (castPtr raw_callDuration :: Ptr ()), argPtr (castPtr raw_unseen :: Ptr ()), argPtr (castPtr raw_participants :: Ptr ()), argPtr (castPtr raw_numberOfCalls :: Ptr ()), argPtr (castPtr raw_isCallerIdBlocked :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithIdentifier:dateCreated:callRecordType:callCapability:callDuration:unseen:@
initWithIdentifier_dateCreated_callRecordType_callCapability_callDuration_unseen :: (IsINCallRecord inCallRecord, IsNSString identifier, IsNSDate dateCreated, IsNSNumber callDuration, IsNSNumber unseen) => inCallRecord -> identifier -> dateCreated -> INCallRecordType -> INCallCapability -> callDuration -> unseen -> IO (Id INCallRecord)
initWithIdentifier_dateCreated_callRecordType_callCapability_callDuration_unseen inCallRecord  identifier dateCreated callRecordType callCapability callDuration unseen =
withObjCPtr identifier $ \raw_identifier ->
  withObjCPtr dateCreated $ \raw_dateCreated ->
    withObjCPtr callDuration $ \raw_callDuration ->
      withObjCPtr unseen $ \raw_unseen ->
          sendMsg inCallRecord (mkSelector "initWithIdentifier:dateCreated:callRecordType:callCapability:callDuration:unseen:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_dateCreated :: Ptr ()), argCLong (coerce callRecordType), argCLong (coerce callCapability), argPtr (castPtr raw_callDuration :: Ptr ()), argPtr (castPtr raw_unseen :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithIdentifier:dateCreated:callRecordType:callCapability:callDuration:unseen:numberOfCalls:@
initWithIdentifier_dateCreated_callRecordType_callCapability_callDuration_unseen_numberOfCalls :: (IsINCallRecord inCallRecord, IsNSString identifier, IsNSDate dateCreated, IsNSNumber callDuration, IsNSNumber unseen, IsNSNumber numberOfCalls) => inCallRecord -> identifier -> dateCreated -> INCallRecordType -> INCallCapability -> callDuration -> unseen -> numberOfCalls -> IO (Id INCallRecord)
initWithIdentifier_dateCreated_callRecordType_callCapability_callDuration_unseen_numberOfCalls inCallRecord  identifier dateCreated callRecordType callCapability callDuration unseen numberOfCalls =
withObjCPtr identifier $ \raw_identifier ->
  withObjCPtr dateCreated $ \raw_dateCreated ->
    withObjCPtr callDuration $ \raw_callDuration ->
      withObjCPtr unseen $ \raw_unseen ->
        withObjCPtr numberOfCalls $ \raw_numberOfCalls ->
            sendMsg inCallRecord (mkSelector "initWithIdentifier:dateCreated:callRecordType:callCapability:callDuration:unseen:numberOfCalls:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_dateCreated :: Ptr ()), argCLong (coerce callRecordType), argCLong (coerce callCapability), argPtr (castPtr raw_callDuration :: Ptr ()), argPtr (castPtr raw_unseen :: Ptr ()), argPtr (castPtr raw_numberOfCalls :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithIdentifier:dateCreated:caller:callRecordType:callCapability:callDuration:unseen:@
initWithIdentifier_dateCreated_caller_callRecordType_callCapability_callDuration_unseen :: (IsINCallRecord inCallRecord, IsNSString identifier, IsNSDate dateCreated, IsINPerson caller, IsNSNumber callDuration, IsNSNumber unseen) => inCallRecord -> identifier -> dateCreated -> caller -> INCallRecordType -> INCallCapability -> callDuration -> unseen -> IO (Id INCallRecord)
initWithIdentifier_dateCreated_caller_callRecordType_callCapability_callDuration_unseen inCallRecord  identifier dateCreated caller callRecordType callCapability callDuration unseen =
withObjCPtr identifier $ \raw_identifier ->
  withObjCPtr dateCreated $ \raw_dateCreated ->
    withObjCPtr caller $ \raw_caller ->
      withObjCPtr callDuration $ \raw_callDuration ->
        withObjCPtr unseen $ \raw_unseen ->
            sendMsg inCallRecord (mkSelector "initWithIdentifier:dateCreated:caller:callRecordType:callCapability:callDuration:unseen:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_dateCreated :: Ptr ()), argPtr (castPtr raw_caller :: Ptr ()), argCLong (coerce callRecordType), argCLong (coerce callCapability), argPtr (castPtr raw_callDuration :: Ptr ()), argPtr (castPtr raw_unseen :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithIdentifier:dateCreated:caller:callRecordType:callCapability:callDuration:unseen:numberOfCalls:@
initWithIdentifier_dateCreated_caller_callRecordType_callCapability_callDuration_unseen_numberOfCalls :: (IsINCallRecord inCallRecord, IsNSString identifier, IsNSDate dateCreated, IsINPerson caller, IsNSNumber callDuration, IsNSNumber unseen, IsNSNumber numberOfCalls) => inCallRecord -> identifier -> dateCreated -> caller -> INCallRecordType -> INCallCapability -> callDuration -> unseen -> numberOfCalls -> IO (Id INCallRecord)
initWithIdentifier_dateCreated_caller_callRecordType_callCapability_callDuration_unseen_numberOfCalls inCallRecord  identifier dateCreated caller callRecordType callCapability callDuration unseen numberOfCalls =
withObjCPtr identifier $ \raw_identifier ->
  withObjCPtr dateCreated $ \raw_dateCreated ->
    withObjCPtr caller $ \raw_caller ->
      withObjCPtr callDuration $ \raw_callDuration ->
        withObjCPtr unseen $ \raw_unseen ->
          withObjCPtr numberOfCalls $ \raw_numberOfCalls ->
              sendMsg inCallRecord (mkSelector "initWithIdentifier:dateCreated:caller:callRecordType:callCapability:callDuration:unseen:numberOfCalls:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_dateCreated :: Ptr ()), argPtr (castPtr raw_caller :: Ptr ()), argCLong (coerce callRecordType), argCLong (coerce callCapability), argPtr (castPtr raw_callDuration :: Ptr ()), argPtr (castPtr raw_unseen :: Ptr ()), argPtr (castPtr raw_numberOfCalls :: Ptr ())] >>= ownedObject . castPtr

-- | @- identifier@
identifier :: IsINCallRecord inCallRecord => inCallRecord -> IO (Id NSString)
identifier inCallRecord  =
  sendMsg inCallRecord (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- dateCreated@
dateCreated :: IsINCallRecord inCallRecord => inCallRecord -> IO (Id NSDate)
dateCreated inCallRecord  =
  sendMsg inCallRecord (mkSelector "dateCreated") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- callRecordType@
callRecordType :: IsINCallRecord inCallRecord => inCallRecord -> IO INCallRecordType
callRecordType inCallRecord  =
  fmap (coerce :: CLong -> INCallRecordType) $ sendMsg inCallRecord (mkSelector "callRecordType") retCLong []

-- | @- callCapability@
callCapability :: IsINCallRecord inCallRecord => inCallRecord -> IO INCallCapability
callCapability inCallRecord  =
  fmap (coerce :: CLong -> INCallCapability) $ sendMsg inCallRecord (mkSelector "callCapability") retCLong []

-- | @- participants@
participants :: IsINCallRecord inCallRecord => inCallRecord -> IO (Id NSArray)
participants inCallRecord  =
  sendMsg inCallRecord (mkSelector "participants") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- caller@
caller :: IsINCallRecord inCallRecord => inCallRecord -> IO (Id INPerson)
caller inCallRecord  =
  sendMsg inCallRecord (mkSelector "caller") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithIdentifier:dateCreated:callRecordType:callCapability:callDuration:unseen:participants:numberOfCalls:isCallerIdBlocked:@
initWithIdentifier_dateCreated_callRecordType_callCapability_callDuration_unseen_participants_numberOfCalls_isCallerIdBlockedSelector :: Selector
initWithIdentifier_dateCreated_callRecordType_callCapability_callDuration_unseen_participants_numberOfCalls_isCallerIdBlockedSelector = mkSelector "initWithIdentifier:dateCreated:callRecordType:callCapability:callDuration:unseen:participants:numberOfCalls:isCallerIdBlocked:"

-- | @Selector@ for @initWithIdentifier:dateCreated:callRecordType:callCapability:callDuration:unseen:@
initWithIdentifier_dateCreated_callRecordType_callCapability_callDuration_unseenSelector :: Selector
initWithIdentifier_dateCreated_callRecordType_callCapability_callDuration_unseenSelector = mkSelector "initWithIdentifier:dateCreated:callRecordType:callCapability:callDuration:unseen:"

-- | @Selector@ for @initWithIdentifier:dateCreated:callRecordType:callCapability:callDuration:unseen:numberOfCalls:@
initWithIdentifier_dateCreated_callRecordType_callCapability_callDuration_unseen_numberOfCallsSelector :: Selector
initWithIdentifier_dateCreated_callRecordType_callCapability_callDuration_unseen_numberOfCallsSelector = mkSelector "initWithIdentifier:dateCreated:callRecordType:callCapability:callDuration:unseen:numberOfCalls:"

-- | @Selector@ for @initWithIdentifier:dateCreated:caller:callRecordType:callCapability:callDuration:unseen:@
initWithIdentifier_dateCreated_caller_callRecordType_callCapability_callDuration_unseenSelector :: Selector
initWithIdentifier_dateCreated_caller_callRecordType_callCapability_callDuration_unseenSelector = mkSelector "initWithIdentifier:dateCreated:caller:callRecordType:callCapability:callDuration:unseen:"

-- | @Selector@ for @initWithIdentifier:dateCreated:caller:callRecordType:callCapability:callDuration:unseen:numberOfCalls:@
initWithIdentifier_dateCreated_caller_callRecordType_callCapability_callDuration_unseen_numberOfCallsSelector :: Selector
initWithIdentifier_dateCreated_caller_callRecordType_callCapability_callDuration_unseen_numberOfCallsSelector = mkSelector "initWithIdentifier:dateCreated:caller:callRecordType:callCapability:callDuration:unseen:numberOfCalls:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @dateCreated@
dateCreatedSelector :: Selector
dateCreatedSelector = mkSelector "dateCreated"

-- | @Selector@ for @callRecordType@
callRecordTypeSelector :: Selector
callRecordTypeSelector = mkSelector "callRecordType"

-- | @Selector@ for @callCapability@
callCapabilitySelector :: Selector
callCapabilitySelector = mkSelector "callCapability"

-- | @Selector@ for @participants@
participantsSelector :: Selector
participantsSelector = mkSelector "participants"

-- | @Selector@ for @caller@
callerSelector :: Selector
callerSelector = mkSelector "caller"

