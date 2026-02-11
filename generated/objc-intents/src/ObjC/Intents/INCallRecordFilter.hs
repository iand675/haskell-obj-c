{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INCallRecordFilter@.
module ObjC.Intents.INCallRecordFilter
  ( INCallRecordFilter
  , IsINCallRecordFilter(..)
  , init_
  , initWithParticipants_callTypes_callCapability
  , participants
  , callTypes
  , callCapability
  , initSelector
  , initWithParticipants_callTypes_callCapabilitySelector
  , participantsSelector
  , callTypesSelector
  , callCapabilitySelector

  -- * Enum types
  , INCallCapability(INCallCapability)
  , pattern INCallCapabilityUnknown
  , pattern INCallCapabilityAudioCall
  , pattern INCallCapabilityVideoCall
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

-- | @- init@
init_ :: IsINCallRecordFilter inCallRecordFilter => inCallRecordFilter -> IO (Id INCallRecordFilter)
init_ inCallRecordFilter  =
  sendMsg inCallRecordFilter (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithParticipants:callTypes:callCapability:@
initWithParticipants_callTypes_callCapability :: (IsINCallRecordFilter inCallRecordFilter, IsNSArray participants) => inCallRecordFilter -> participants -> INCallRecordTypeOptions -> INCallCapability -> IO (Id INCallRecordFilter)
initWithParticipants_callTypes_callCapability inCallRecordFilter  participants callTypes callCapability =
withObjCPtr participants $ \raw_participants ->
    sendMsg inCallRecordFilter (mkSelector "initWithParticipants:callTypes:callCapability:") (retPtr retVoid) [argPtr (castPtr raw_participants :: Ptr ()), argCULong (coerce callTypes), argCLong (coerce callCapability)] >>= ownedObject . castPtr

-- | @- participants@
participants :: IsINCallRecordFilter inCallRecordFilter => inCallRecordFilter -> IO (Id NSArray)
participants inCallRecordFilter  =
  sendMsg inCallRecordFilter (mkSelector "participants") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- callTypes@
callTypes :: IsINCallRecordFilter inCallRecordFilter => inCallRecordFilter -> IO INCallRecordTypeOptions
callTypes inCallRecordFilter  =
  fmap (coerce :: CULong -> INCallRecordTypeOptions) $ sendMsg inCallRecordFilter (mkSelector "callTypes") retCULong []

-- | @- callCapability@
callCapability :: IsINCallRecordFilter inCallRecordFilter => inCallRecordFilter -> IO INCallCapability
callCapability inCallRecordFilter  =
  fmap (coerce :: CLong -> INCallCapability) $ sendMsg inCallRecordFilter (mkSelector "callCapability") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithParticipants:callTypes:callCapability:@
initWithParticipants_callTypes_callCapabilitySelector :: Selector
initWithParticipants_callTypes_callCapabilitySelector = mkSelector "initWithParticipants:callTypes:callCapability:"

-- | @Selector@ for @participants@
participantsSelector :: Selector
participantsSelector = mkSelector "participants"

-- | @Selector@ for @callTypes@
callTypesSelector :: Selector
callTypesSelector = mkSelector "callTypes"

-- | @Selector@ for @callCapability@
callCapabilitySelector :: Selector
callCapabilitySelector = mkSelector "callCapability"

