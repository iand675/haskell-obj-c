{-# LANGUAGE PatternSynonyms #-}
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
  , playerSelector
  , lastTurnDateSelector
  , statusSelector
  , matchOutcomeSelector
  , setMatchOutcomeSelector
  , timeoutDateSelector
  , playerIDSelector

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

import ObjC.GameKit.Internal.Classes
import ObjC.GameKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- player@
player :: IsGKTurnBasedParticipant gkTurnBasedParticipant => gkTurnBasedParticipant -> IO (Id GKPlayer)
player gkTurnBasedParticipant  =
    sendMsg gkTurnBasedParticipant (mkSelector "player") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- lastTurnDate@
lastTurnDate :: IsGKTurnBasedParticipant gkTurnBasedParticipant => gkTurnBasedParticipant -> IO (Id NSDate)
lastTurnDate gkTurnBasedParticipant  =
    sendMsg gkTurnBasedParticipant (mkSelector "lastTurnDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- status@
status :: IsGKTurnBasedParticipant gkTurnBasedParticipant => gkTurnBasedParticipant -> IO GKTurnBasedParticipantStatus
status gkTurnBasedParticipant  =
    fmap (coerce :: CLong -> GKTurnBasedParticipantStatus) $ sendMsg gkTurnBasedParticipant (mkSelector "status") retCLong []

-- | @- matchOutcome@
matchOutcome :: IsGKTurnBasedParticipant gkTurnBasedParticipant => gkTurnBasedParticipant -> IO GKTurnBasedMatchOutcome
matchOutcome gkTurnBasedParticipant  =
    fmap (coerce :: CLong -> GKTurnBasedMatchOutcome) $ sendMsg gkTurnBasedParticipant (mkSelector "matchOutcome") retCLong []

-- | @- setMatchOutcome:@
setMatchOutcome :: IsGKTurnBasedParticipant gkTurnBasedParticipant => gkTurnBasedParticipant -> GKTurnBasedMatchOutcome -> IO ()
setMatchOutcome gkTurnBasedParticipant  value =
    sendMsg gkTurnBasedParticipant (mkSelector "setMatchOutcome:") retVoid [argCLong (coerce value)]

-- | @- timeoutDate@
timeoutDate :: IsGKTurnBasedParticipant gkTurnBasedParticipant => gkTurnBasedParticipant -> IO (Id NSDate)
timeoutDate gkTurnBasedParticipant  =
    sendMsg gkTurnBasedParticipant (mkSelector "timeoutDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | * This property is obsolete. **
--
-- ObjC selector: @- playerID@
playerID :: IsGKTurnBasedParticipant gkTurnBasedParticipant => gkTurnBasedParticipant -> IO (Id NSString)
playerID gkTurnBasedParticipant  =
    sendMsg gkTurnBasedParticipant (mkSelector "playerID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @player@
playerSelector :: Selector
playerSelector = mkSelector "player"

-- | @Selector@ for @lastTurnDate@
lastTurnDateSelector :: Selector
lastTurnDateSelector = mkSelector "lastTurnDate"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @matchOutcome@
matchOutcomeSelector :: Selector
matchOutcomeSelector = mkSelector "matchOutcome"

-- | @Selector@ for @setMatchOutcome:@
setMatchOutcomeSelector :: Selector
setMatchOutcomeSelector = mkSelector "setMatchOutcome:"

-- | @Selector@ for @timeoutDate@
timeoutDateSelector :: Selector
timeoutDateSelector = mkSelector "timeoutDate"

-- | @Selector@ for @playerID@
playerIDSelector :: Selector
playerIDSelector = mkSelector "playerID"

