{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The Minmax Strategist is a generic AI that selects a game model update for a given player that maximises  potential gain, while minimising potential loss. It does this by examining all of the updates available  to the player in question, extrapolating the potential moves opposing players may take, projecting out  maxLookAheadDepth number of turns. The selected update will result in the greatest potential gain, balanced  against the potential gain of other players.
--
-- Generated bindings for @GKMinmaxStrategist@.
module ObjC.GameplayKit.GKMinmaxStrategist
  ( GKMinmaxStrategist
  , IsGKMinmaxStrategist(..)
  , bestMoveForPlayer
  , randomMoveForPlayer_fromNumberOfBestMoves
  , maxLookAheadDepth
  , setMaxLookAheadDepth
  , bestMoveForPlayerSelector
  , maxLookAheadDepthSelector
  , randomMoveForPlayer_fromNumberOfBestMovesSelector
  , setMaxLookAheadDepthSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Selects the best move for the specified player. If randomSource is not nil, it will randomly select which move to use if there are one or more ties for the best. Returns nil if the player is invalid, the player is not a part of the game model, or the player has no valid moves available.
--
-- ObjC selector: @- bestMoveForPlayer:@
bestMoveForPlayer :: IsGKMinmaxStrategist gkMinmaxStrategist => gkMinmaxStrategist -> RawId -> IO RawId
bestMoveForPlayer gkMinmaxStrategist player =
  sendMessage gkMinmaxStrategist bestMoveForPlayerSelector player

-- | Selects one move from the set of N best moves for the specified player, where N is equal to  numMovesToConsider. If randomSource is nil, it will not randomly select, but will behave like  bestMoveForPlayer and return the first best move. Returns nil if the player is invalid, the  player is not a part of the game model, or the player has no valid moves available.
--
-- ObjC selector: @- randomMoveForPlayer:fromNumberOfBestMoves:@
randomMoveForPlayer_fromNumberOfBestMoves :: IsGKMinmaxStrategist gkMinmaxStrategist => gkMinmaxStrategist -> RawId -> CLong -> IO RawId
randomMoveForPlayer_fromNumberOfBestMoves gkMinmaxStrategist player numMovesToConsider =
  sendMessage gkMinmaxStrategist randomMoveForPlayer_fromNumberOfBestMovesSelector player numMovesToConsider

-- | The maximum number of future turns that will be processed when searching for a move.
--
-- ObjC selector: @- maxLookAheadDepth@
maxLookAheadDepth :: IsGKMinmaxStrategist gkMinmaxStrategist => gkMinmaxStrategist -> IO CLong
maxLookAheadDepth gkMinmaxStrategist =
  sendMessage gkMinmaxStrategist maxLookAheadDepthSelector

-- | The maximum number of future turns that will be processed when searching for a move.
--
-- ObjC selector: @- setMaxLookAheadDepth:@
setMaxLookAheadDepth :: IsGKMinmaxStrategist gkMinmaxStrategist => gkMinmaxStrategist -> CLong -> IO ()
setMaxLookAheadDepth gkMinmaxStrategist value =
  sendMessage gkMinmaxStrategist setMaxLookAheadDepthSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bestMoveForPlayer:@
bestMoveForPlayerSelector :: Selector '[RawId] RawId
bestMoveForPlayerSelector = mkSelector "bestMoveForPlayer:"

-- | @Selector@ for @randomMoveForPlayer:fromNumberOfBestMoves:@
randomMoveForPlayer_fromNumberOfBestMovesSelector :: Selector '[RawId, CLong] RawId
randomMoveForPlayer_fromNumberOfBestMovesSelector = mkSelector "randomMoveForPlayer:fromNumberOfBestMoves:"

-- | @Selector@ for @maxLookAheadDepth@
maxLookAheadDepthSelector :: Selector '[] CLong
maxLookAheadDepthSelector = mkSelector "maxLookAheadDepth"

-- | @Selector@ for @setMaxLookAheadDepth:@
setMaxLookAheadDepthSelector :: Selector '[CLong] ()
setMaxLookAheadDepthSelector = mkSelector "setMaxLookAheadDepth:"

