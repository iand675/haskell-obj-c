{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | GKTurnBasedMatch represents an ongoing turn-based game among the matched group of participants Existing matches can be shown and new matches created using GKTurnBasedMatchmakerViewController A list of existing matches can be retrieved using +loadMatchesWithCompletionHandler:
--
-- By default turn based events will badge your app.  To opt out of this add GKGameCenterBadgingDisabled  with a boolean value of YES to your info plist
--
-- Generated bindings for @GKTurnBasedParticipant@.
module ObjC.GameKit.GKTurnBasedParticipant
  ( GKTurnBasedParticipant
  , IsGKTurnBasedParticipant(..)
  , player
  , lastTurnDate
  , status
  , matchOutcome
  , setMatchOutcome
  , timeoutDate
  , playerID
  , lastTurnDateSelector
  , matchOutcomeSelector
  , playerIDSelector
  , playerSelector
  , setMatchOutcomeSelector
  , statusSelector
  , timeoutDateSelector

  -- * Enum types
  , GKTurnBasedMatchOutcome(GKTurnBasedMatchOutcome)
  , pattern GKTurnBasedMatchOutcomeNone
  , pattern GKTurnBasedMatchOutcomeQuit
  , pattern GKTurnBasedMatchOutcomeWon
  , pattern GKTurnBasedMatchOutcomeLost
  , pattern GKTurnBasedMatchOutcomeTied
  , pattern GKTurnBasedMatchOutcomeTimeExpired
  , pattern GKTurnBasedMatchOutcomeFirst
  , pattern GKTurnBasedMatchOutcomeSecond
  , pattern GKTurnBasedMatchOutcomeThird
  , pattern GKTurnBasedMatchOutcomeFourth
  , pattern GKTurnBasedMatchOutcomeCustomRange
  , GKTurnBasedParticipantStatus(GKTurnBasedParticipantStatus)
  , pattern GKTurnBasedParticipantStatusUnknown
  , pattern GKTurnBasedParticipantStatusInvited
  , pattern GKTurnBasedParticipantStatusDeclined
  , pattern GKTurnBasedParticipantStatusMatching
  , pattern GKTurnBasedParticipantStatusActive
  , pattern GKTurnBasedParticipantStatusDone

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

-- | @- player@
player :: IsGKTurnBasedParticipant gkTurnBasedParticipant => gkTurnBasedParticipant -> IO (Id GKPlayer)
player gkTurnBasedParticipant =
  sendMessage gkTurnBasedParticipant playerSelector

-- | @- lastTurnDate@
lastTurnDate :: IsGKTurnBasedParticipant gkTurnBasedParticipant => gkTurnBasedParticipant -> IO (Id NSDate)
lastTurnDate gkTurnBasedParticipant =
  sendMessage gkTurnBasedParticipant lastTurnDateSelector

-- | @- status@
status :: IsGKTurnBasedParticipant gkTurnBasedParticipant => gkTurnBasedParticipant -> IO GKTurnBasedParticipantStatus
status gkTurnBasedParticipant =
  sendMessage gkTurnBasedParticipant statusSelector

-- | @- matchOutcome@
matchOutcome :: IsGKTurnBasedParticipant gkTurnBasedParticipant => gkTurnBasedParticipant -> IO GKTurnBasedMatchOutcome
matchOutcome gkTurnBasedParticipant =
  sendMessage gkTurnBasedParticipant matchOutcomeSelector

-- | @- setMatchOutcome:@
setMatchOutcome :: IsGKTurnBasedParticipant gkTurnBasedParticipant => gkTurnBasedParticipant -> GKTurnBasedMatchOutcome -> IO ()
setMatchOutcome gkTurnBasedParticipant value =
  sendMessage gkTurnBasedParticipant setMatchOutcomeSelector value

-- | @- timeoutDate@
timeoutDate :: IsGKTurnBasedParticipant gkTurnBasedParticipant => gkTurnBasedParticipant -> IO (Id NSDate)
timeoutDate gkTurnBasedParticipant =
  sendMessage gkTurnBasedParticipant timeoutDateSelector

-- | * This property is obsolete. **
--
-- ObjC selector: @- playerID@
playerID :: IsGKTurnBasedParticipant gkTurnBasedParticipant => gkTurnBasedParticipant -> IO (Id NSString)
playerID gkTurnBasedParticipant =
  sendMessage gkTurnBasedParticipant playerIDSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @player@
playerSelector :: Selector '[] (Id GKPlayer)
playerSelector = mkSelector "player"

-- | @Selector@ for @lastTurnDate@
lastTurnDateSelector :: Selector '[] (Id NSDate)
lastTurnDateSelector = mkSelector "lastTurnDate"

-- | @Selector@ for @status@
statusSelector :: Selector '[] GKTurnBasedParticipantStatus
statusSelector = mkSelector "status"

-- | @Selector@ for @matchOutcome@
matchOutcomeSelector :: Selector '[] GKTurnBasedMatchOutcome
matchOutcomeSelector = mkSelector "matchOutcome"

-- | @Selector@ for @setMatchOutcome:@
setMatchOutcomeSelector :: Selector '[GKTurnBasedMatchOutcome] ()
setMatchOutcomeSelector = mkSelector "setMatchOutcome:"

-- | @Selector@ for @timeoutDate@
timeoutDateSelector :: Selector '[] (Id NSDate)
timeoutDateSelector = mkSelector "timeoutDate"

-- | @Selector@ for @playerID@
playerIDSelector :: Selector '[] (Id NSString)
playerIDSelector = mkSelector "playerID"

