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
  , playerSelector
  , setPlayerSelector
  , valueSelector
  , setValueSelector
  , contextSelector
  , setContextSelector
  , leaderboardIDSelector
  , setLeaderboardIDSelector


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

-- | The player who earns the score.
--
-- ObjC selector: @- player@
player :: IsGKLeaderboardScore gkLeaderboardScore => gkLeaderboardScore -> IO (Id GKPlayer)
player gkLeaderboardScore  =
  sendMsg gkLeaderboardScore (mkSelector "player") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The player who earns the score.
--
-- ObjC selector: @- setPlayer:@
setPlayer :: (IsGKLeaderboardScore gkLeaderboardScore, IsGKPlayer value) => gkLeaderboardScore -> value -> IO ()
setPlayer gkLeaderboardScore  value =
withObjCPtr value $ \raw_value ->
    sendMsg gkLeaderboardScore (mkSelector "setPlayer:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The score that the player earns.
--
-- ObjC selector: @- value@
value :: IsGKLeaderboardScore gkLeaderboardScore => gkLeaderboardScore -> IO CLong
value gkLeaderboardScore  =
  sendMsg gkLeaderboardScore (mkSelector "value") retCLong []

-- | The score that the player earns.
--
-- ObjC selector: @- setValue:@
setValue :: IsGKLeaderboardScore gkLeaderboardScore => gkLeaderboardScore -> CLong -> IO ()
setValue gkLeaderboardScore  value =
  sendMsg gkLeaderboardScore (mkSelector "setValue:") retVoid [argCLong (fromIntegral value)]

-- | An integer value that your game uses.
--
-- ObjC selector: @- context@
context :: IsGKLeaderboardScore gkLeaderboardScore => gkLeaderboardScore -> IO CULong
context gkLeaderboardScore  =
  sendMsg gkLeaderboardScore (mkSelector "context") retCULong []

-- | An integer value that your game uses.
--
-- ObjC selector: @- setContext:@
setContext :: IsGKLeaderboardScore gkLeaderboardScore => gkLeaderboardScore -> CULong -> IO ()
setContext gkLeaderboardScore  value =
  sendMsg gkLeaderboardScore (mkSelector "setContext:") retVoid [argCULong (fromIntegral value)]

-- | The ID that Game Center uses for the leaderboard.
--
-- ObjC selector: @- leaderboardID@
leaderboardID :: IsGKLeaderboardScore gkLeaderboardScore => gkLeaderboardScore -> IO (Id NSString)
leaderboardID gkLeaderboardScore  =
  sendMsg gkLeaderboardScore (mkSelector "leaderboardID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The ID that Game Center uses for the leaderboard.
--
-- ObjC selector: @- setLeaderboardID:@
setLeaderboardID :: (IsGKLeaderboardScore gkLeaderboardScore, IsNSString value) => gkLeaderboardScore -> value -> IO ()
setLeaderboardID gkLeaderboardScore  value =
withObjCPtr value $ \raw_value ->
    sendMsg gkLeaderboardScore (mkSelector "setLeaderboardID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @player@
playerSelector :: Selector
playerSelector = mkSelector "player"

-- | @Selector@ for @setPlayer:@
setPlayerSelector :: Selector
setPlayerSelector = mkSelector "setPlayer:"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector
setValueSelector = mkSelector "setValue:"

-- | @Selector@ for @context@
contextSelector :: Selector
contextSelector = mkSelector "context"

-- | @Selector@ for @setContext:@
setContextSelector :: Selector
setContextSelector = mkSelector "setContext:"

-- | @Selector@ for @leaderboardID@
leaderboardIDSelector :: Selector
leaderboardIDSelector = mkSelector "leaderboardID"

-- | @Selector@ for @setLeaderboardID:@
setLeaderboardIDSelector :: Selector
setLeaderboardIDSelector = mkSelector "setLeaderboardID:"

