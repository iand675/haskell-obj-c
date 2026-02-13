{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GKChallenge@.
module ObjC.GameKit.GKChallenge
  ( GKChallenge
  , IsGKChallenge(..)
  , decline
  , issuingPlayer
  , receivingPlayer
  , state
  , issueDate
  , completionDate
  , message
  , issuingPlayerID
  , receivingPlayerID
  , completionDateSelector
  , declineSelector
  , issueDateSelector
  , issuingPlayerIDSelector
  , issuingPlayerSelector
  , messageSelector
  , receivingPlayerIDSelector
  , receivingPlayerSelector
  , stateSelector

  -- * Enum types
  , GKChallengeState(GKChallengeState)
  , pattern GKChallengeStateInvalid
  , pattern GKChallengeStatePending
  , pattern GKChallengeStateCompleted
  , pattern GKChallengeStateDeclined

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameKit.Internal.Classes
import ObjC.GameKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Any GKChallenge object to be declined must be in a state of GKChallengeStatePending in order to be successfully cancelled
--
-- ObjC selector: @- decline@
decline :: IsGKChallenge gkChallenge => gkChallenge -> IO ()
decline gkChallenge =
  sendMessage gkChallenge declineSelector

-- | The GKPlayer who issued the challenge
--
-- ObjC selector: @- issuingPlayer@
issuingPlayer :: IsGKChallenge gkChallenge => gkChallenge -> IO (Id GKPlayer)
issuingPlayer gkChallenge =
  sendMessage gkChallenge issuingPlayerSelector

-- | The GKPlayer who has received the challenge
--
-- ObjC selector: @- receivingPlayer@
receivingPlayer :: IsGKChallenge gkChallenge => gkChallenge -> IO (Id GKPlayer)
receivingPlayer gkChallenge =
  sendMessage gkChallenge receivingPlayerSelector

-- | Current state of the challenge
--
-- ObjC selector: @- state@
state :: IsGKChallenge gkChallenge => gkChallenge -> IO GKChallengeState
state gkChallenge =
  sendMessage gkChallenge stateSelector

-- | Date the challenge was issued
--
-- ObjC selector: @- issueDate@
issueDate :: IsGKChallenge gkChallenge => gkChallenge -> IO (Id NSDate)
issueDate gkChallenge =
  sendMessage gkChallenge issueDateSelector

-- | Date the challenge was completed or aborted
--
-- ObjC selector: @- completionDate@
completionDate :: IsGKChallenge gkChallenge => gkChallenge -> IO (Id NSDate)
completionDate gkChallenge =
  sendMessage gkChallenge completionDateSelector

-- | The message sent to receivers of this challenge
--
-- ObjC selector: @- message@
message :: IsGKChallenge gkChallenge => gkChallenge -> IO (Id NSString)
message gkChallenge =
  sendMessage gkChallenge messageSelector

-- | * This property is obsolete. **
--
-- ObjC selector: @- issuingPlayerID@
issuingPlayerID :: IsGKChallenge gkChallenge => gkChallenge -> IO (Id NSString)
issuingPlayerID gkChallenge =
  sendMessage gkChallenge issuingPlayerIDSelector

-- | * This property is obsolete. **
--
-- ObjC selector: @- receivingPlayerID@
receivingPlayerID :: IsGKChallenge gkChallenge => gkChallenge -> IO (Id NSString)
receivingPlayerID gkChallenge =
  sendMessage gkChallenge receivingPlayerIDSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @decline@
declineSelector :: Selector '[] ()
declineSelector = mkSelector "decline"

-- | @Selector@ for @issuingPlayer@
issuingPlayerSelector :: Selector '[] (Id GKPlayer)
issuingPlayerSelector = mkSelector "issuingPlayer"

-- | @Selector@ for @receivingPlayer@
receivingPlayerSelector :: Selector '[] (Id GKPlayer)
receivingPlayerSelector = mkSelector "receivingPlayer"

-- | @Selector@ for @state@
stateSelector :: Selector '[] GKChallengeState
stateSelector = mkSelector "state"

-- | @Selector@ for @issueDate@
issueDateSelector :: Selector '[] (Id NSDate)
issueDateSelector = mkSelector "issueDate"

-- | @Selector@ for @completionDate@
completionDateSelector :: Selector '[] (Id NSDate)
completionDateSelector = mkSelector "completionDate"

-- | @Selector@ for @message@
messageSelector :: Selector '[] (Id NSString)
messageSelector = mkSelector "message"

-- | @Selector@ for @issuingPlayerID@
issuingPlayerIDSelector :: Selector '[] (Id NSString)
issuingPlayerIDSelector = mkSelector "issuingPlayerID"

-- | @Selector@ for @receivingPlayerID@
receivingPlayerIDSelector :: Selector '[] (Id NSString)
receivingPlayerIDSelector = mkSelector "receivingPlayerID"

