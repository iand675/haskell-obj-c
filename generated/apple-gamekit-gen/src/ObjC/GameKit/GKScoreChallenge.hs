{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GKScoreChallenge@.
module ObjC.GameKit.GKScoreChallenge
  ( GKScoreChallenge
  , IsGKScoreChallenge(..)
  , score
  , leaderboardEntry
  , scoreSelector
  , leaderboardEntrySelector


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
import ObjC.Foundation.Internal.Classes

-- | The score to meet to satisfy this challenge
--
-- ObjC selector: @- score@
score :: IsGKScoreChallenge gkScoreChallenge => gkScoreChallenge -> IO (Id GKScore)
score gkScoreChallenge  =
    sendMsg gkScoreChallenge (mkSelector "score") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The leaderboard entry to meet to satisfy this challenge
--
-- ObjC selector: @- leaderboardEntry@
leaderboardEntry :: IsGKScoreChallenge gkScoreChallenge => gkScoreChallenge -> IO (Id GKLeaderboardEntry)
leaderboardEntry gkScoreChallenge  =
    sendMsg gkScoreChallenge (mkSelector "leaderboardEntry") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @score@
scoreSelector :: Selector
scoreSelector = mkSelector "score"

-- | @Selector@ for @leaderboardEntry@
leaderboardEntrySelector :: Selector
leaderboardEntrySelector = mkSelector "leaderboardEntry"

