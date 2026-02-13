{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GKScoreChallenge@.
module ObjC.GameKit.GKScoreChallenge
  ( GKScoreChallenge
  , IsGKScoreChallenge(..)
  , score
  , leaderboardEntry
  , leaderboardEntrySelector
  , scoreSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The score to meet to satisfy this challenge
--
-- ObjC selector: @- score@
score :: IsGKScoreChallenge gkScoreChallenge => gkScoreChallenge -> IO (Id GKScore)
score gkScoreChallenge =
  sendMessage gkScoreChallenge scoreSelector

-- | The leaderboard entry to meet to satisfy this challenge
--
-- ObjC selector: @- leaderboardEntry@
leaderboardEntry :: IsGKScoreChallenge gkScoreChallenge => gkScoreChallenge -> IO (Id GKLeaderboardEntry)
leaderboardEntry gkScoreChallenge =
  sendMessage gkScoreChallenge leaderboardEntrySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @score@
scoreSelector :: Selector '[] (Id GKScore)
scoreSelector = mkSelector "score"

-- | @Selector@ for @leaderboardEntry@
leaderboardEntrySelector :: Selector '[] (Id GKLeaderboardEntry)
leaderboardEntrySelector = mkSelector "leaderboardEntry"

