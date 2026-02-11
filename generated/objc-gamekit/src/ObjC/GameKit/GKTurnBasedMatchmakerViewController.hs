{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | View controller to manage turn-based matches, invite friends and perform automatching. Present modally from the top view controller.
--
-- Generated bindings for @GKTurnBasedMatchmakerViewController@.
module ObjC.GameKit.GKTurnBasedMatchmakerViewController
  ( GKTurnBasedMatchmakerViewController
  , IsGKTurnBasedMatchmakerViewController(..)
  , initWithMatchRequest
  , showExistingMatches
  , setShowExistingMatches
  , matchmakingMode
  , setMatchmakingMode
  , initWithMatchRequestSelector
  , showExistingMatchesSelector
  , setShowExistingMatchesSelector
  , matchmakingModeSelector
  , setMatchmakingModeSelector

  -- * Enum types
  , GKMatchmakingMode(GKMatchmakingMode)
  , pattern GKMatchmakingModeDefault
  , pattern GKMatchmakingModeNearbyOnly
  , pattern GKMatchmakingModeAutomatchOnly
  , pattern GKMatchmakingModeInviteOnly

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
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithMatchRequest:@
initWithMatchRequest :: (IsGKTurnBasedMatchmakerViewController gkTurnBasedMatchmakerViewController, IsGKMatchRequest request) => gkTurnBasedMatchmakerViewController -> request -> IO RawId
initWithMatchRequest gkTurnBasedMatchmakerViewController  request =
withObjCPtr request $ \raw_request ->
    fmap (RawId . castPtr) $ sendMsg gkTurnBasedMatchmakerViewController (mkSelector "initWithMatchRequest:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ())]

-- | @- showExistingMatches@
showExistingMatches :: IsGKTurnBasedMatchmakerViewController gkTurnBasedMatchmakerViewController => gkTurnBasedMatchmakerViewController -> IO Bool
showExistingMatches gkTurnBasedMatchmakerViewController  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkTurnBasedMatchmakerViewController (mkSelector "showExistingMatches") retCULong []

-- | @- setShowExistingMatches:@
setShowExistingMatches :: IsGKTurnBasedMatchmakerViewController gkTurnBasedMatchmakerViewController => gkTurnBasedMatchmakerViewController -> Bool -> IO ()
setShowExistingMatches gkTurnBasedMatchmakerViewController  value =
  sendMsg gkTurnBasedMatchmakerViewController (mkSelector "setShowExistingMatches:") retVoid [argCULong (if value then 1 else 0)]

-- | This controls the mode of matchmaking to support in the UI (all, nearby only, automatch only, invite only). Throws an exception if you can not set to the desired mode (due to restrictions)
--
-- ObjC selector: @- matchmakingMode@
matchmakingMode :: IsGKTurnBasedMatchmakerViewController gkTurnBasedMatchmakerViewController => gkTurnBasedMatchmakerViewController -> IO GKMatchmakingMode
matchmakingMode gkTurnBasedMatchmakerViewController  =
  fmap (coerce :: CLong -> GKMatchmakingMode) $ sendMsg gkTurnBasedMatchmakerViewController (mkSelector "matchmakingMode") retCLong []

-- | This controls the mode of matchmaking to support in the UI (all, nearby only, automatch only, invite only). Throws an exception if you can not set to the desired mode (due to restrictions)
--
-- ObjC selector: @- setMatchmakingMode:@
setMatchmakingMode :: IsGKTurnBasedMatchmakerViewController gkTurnBasedMatchmakerViewController => gkTurnBasedMatchmakerViewController -> GKMatchmakingMode -> IO ()
setMatchmakingMode gkTurnBasedMatchmakerViewController  value =
  sendMsg gkTurnBasedMatchmakerViewController (mkSelector "setMatchmakingMode:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMatchRequest:@
initWithMatchRequestSelector :: Selector
initWithMatchRequestSelector = mkSelector "initWithMatchRequest:"

-- | @Selector@ for @showExistingMatches@
showExistingMatchesSelector :: Selector
showExistingMatchesSelector = mkSelector "showExistingMatches"

-- | @Selector@ for @setShowExistingMatches:@
setShowExistingMatchesSelector :: Selector
setShowExistingMatchesSelector = mkSelector "setShowExistingMatches:"

-- | @Selector@ for @matchmakingMode@
matchmakingModeSelector :: Selector
matchmakingModeSelector = mkSelector "matchmakingMode"

-- | @Selector@ for @setMatchmakingMode:@
setMatchmakingModeSelector :: Selector
setMatchmakingModeSelector = mkSelector "setMatchmakingMode:"

