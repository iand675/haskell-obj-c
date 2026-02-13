{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A @GKLeaderboardScore@ object represents a score on a leaderboard for scores you report for challenges or turn-based games.
--
-- Generated bindings for @GKLeaderboardScore@.
module ObjC.GameKit.GKLeaderboardScore
  ( GKLeaderboardScore
  , IsGKLeaderboardScore(..)
  , player
  , setPlayer
  , value
  , setValue
  , context
  , setContext
  , leaderboardID
  , setLeaderboardID
  , contextSelector
  , leaderboardIDSelector
  , playerSelector
  , setContextSelector
  , setLeaderboardIDSelector
  , setPlayerSelector
  , setValueSelector
  , valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The player who earns the score.
--
-- ObjC selector: @- player@
player :: IsGKLeaderboardScore gkLeaderboardScore => gkLeaderboardScore -> IO (Id GKPlayer)
player gkLeaderboardScore =
  sendMessage gkLeaderboardScore playerSelector

-- | The player who earns the score.
--
-- ObjC selector: @- setPlayer:@
setPlayer :: (IsGKLeaderboardScore gkLeaderboardScore, IsGKPlayer value) => gkLeaderboardScore -> value -> IO ()
setPlayer gkLeaderboardScore value =
  sendMessage gkLeaderboardScore setPlayerSelector (toGKPlayer value)

-- | The score that the player earns.
--
-- ObjC selector: @- value@
value :: IsGKLeaderboardScore gkLeaderboardScore => gkLeaderboardScore -> IO CLong
value gkLeaderboardScore =
  sendMessage gkLeaderboardScore valueSelector

-- | The score that the player earns.
--
-- ObjC selector: @- setValue:@
setValue :: IsGKLeaderboardScore gkLeaderboardScore => gkLeaderboardScore -> CLong -> IO ()
setValue gkLeaderboardScore value =
  sendMessage gkLeaderboardScore setValueSelector value

-- | An integer value that your game uses.
--
-- ObjC selector: @- context@
context :: IsGKLeaderboardScore gkLeaderboardScore => gkLeaderboardScore -> IO CULong
context gkLeaderboardScore =
  sendMessage gkLeaderboardScore contextSelector

-- | An integer value that your game uses.
--
-- ObjC selector: @- setContext:@
setContext :: IsGKLeaderboardScore gkLeaderboardScore => gkLeaderboardScore -> CULong -> IO ()
setContext gkLeaderboardScore value =
  sendMessage gkLeaderboardScore setContextSelector value

-- | The ID that Game Center uses for the leaderboard.
--
-- ObjC selector: @- leaderboardID@
leaderboardID :: IsGKLeaderboardScore gkLeaderboardScore => gkLeaderboardScore -> IO (Id NSString)
leaderboardID gkLeaderboardScore =
  sendMessage gkLeaderboardScore leaderboardIDSelector

-- | The ID that Game Center uses for the leaderboard.
--
-- ObjC selector: @- setLeaderboardID:@
setLeaderboardID :: (IsGKLeaderboardScore gkLeaderboardScore, IsNSString value) => gkLeaderboardScore -> value -> IO ()
setLeaderboardID gkLeaderboardScore value =
  sendMessage gkLeaderboardScore setLeaderboardIDSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @player@
playerSelector :: Selector '[] (Id GKPlayer)
playerSelector = mkSelector "player"

-- | @Selector@ for @setPlayer:@
setPlayerSelector :: Selector '[Id GKPlayer] ()
setPlayerSelector = mkSelector "setPlayer:"

-- | @Selector@ for @value@
valueSelector :: Selector '[] CLong
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector '[CLong] ()
setValueSelector = mkSelector "setValue:"

-- | @Selector@ for @context@
contextSelector :: Selector '[] CULong
contextSelector = mkSelector "context"

-- | @Selector@ for @setContext:@
setContextSelector :: Selector '[CULong] ()
setContextSelector = mkSelector "setContext:"

-- | @Selector@ for @leaderboardID@
leaderboardIDSelector :: Selector '[] (Id NSString)
leaderboardIDSelector = mkSelector "leaderboardID"

-- | @Selector@ for @setLeaderboardID:@
setLeaderboardIDSelector :: Selector '[Id NSString] ()
setLeaderboardIDSelector = mkSelector "setLeaderboardID:"

