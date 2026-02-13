{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , callCapabilitySelector
  , callTypesSelector
  , initSelector
  , initWithParticipants_callTypes_callCapabilitySelector
  , participantsSelector

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
init_ :: IsINCallRecordFilter inCallRecordFilter => inCallRecordFilter -> IO (Id INCallRecordFilter)
init_ inCallRecordFilter =
  sendOwnedMessage inCallRecordFilter initSelector

-- | @- initWithParticipants:callTypes:callCapability:@
initWithParticipants_callTypes_callCapability :: (IsINCallRecordFilter inCallRecordFilter, IsNSArray participants) => inCallRecordFilter -> participants -> INCallRecordTypeOptions -> INCallCapability -> IO (Id INCallRecordFilter)
initWithParticipants_callTypes_callCapability inCallRecordFilter participants callTypes callCapability =
  sendOwnedMessage inCallRecordFilter initWithParticipants_callTypes_callCapabilitySelector (toNSArray participants) callTypes callCapability

-- | @- participants@
participants :: IsINCallRecordFilter inCallRecordFilter => inCallRecordFilter -> IO (Id NSArray)
participants inCallRecordFilter =
  sendMessage inCallRecordFilter participantsSelector

-- | @- callTypes@
callTypes :: IsINCallRecordFilter inCallRecordFilter => inCallRecordFilter -> IO INCallRecordTypeOptions
callTypes inCallRecordFilter =
  sendMessage inCallRecordFilter callTypesSelector

-- | @- callCapability@
callCapability :: IsINCallRecordFilter inCallRecordFilter => inCallRecordFilter -> IO INCallCapability
callCapability inCallRecordFilter =
  sendMessage inCallRecordFilter callCapabilitySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INCallRecordFilter)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithParticipants:callTypes:callCapability:@
initWithParticipants_callTypes_callCapabilitySelector :: Selector '[Id NSArray, INCallRecordTypeOptions, INCallCapability] (Id INCallRecordFilter)
initWithParticipants_callTypes_callCapabilitySelector = mkSelector "initWithParticipants:callTypes:callCapability:"

-- | @Selector@ for @participants@
participantsSelector :: Selector '[] (Id NSArray)
participantsSelector = mkSelector "participants"

-- | @Selector@ for @callTypes@
callTypesSelector :: Selector '[] INCallRecordTypeOptions
callTypesSelector = mkSelector "callTypes"

-- | @Selector@ for @callCapability@
callCapabilitySelector :: Selector '[] INCallCapability
callCapabilitySelector = mkSelector "callCapability"

