{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , turnBasedMatchmakerDelegate
  , setTurnBasedMatchmakerDelegate
  , showExistingMatches
  , setShowExistingMatches
  , matchmakingMode
  , setMatchmakingMode
  , initWithMatchRequestSelector
  , matchmakingModeSelector
  , setMatchmakingModeSelector
  , setShowExistingMatchesSelector
  , setTurnBasedMatchmakerDelegateSelector
  , showExistingMatchesSelector
  , turnBasedMatchmakerDelegateSelector

  -- * Enum types
  , GKMatchmakingMode(GKMatchmakingMode)
  , pattern GKMatchmakingModeDefault
  , pattern GKMatchmakingModeNearbyOnly
  , pattern GKMatchmakingModeAutomatchOnly
  , pattern GKMatchmakingModeInviteOnly

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameKit.Internal.Classes
import ObjC.GameKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithMatchRequest:@
initWithMatchRequest :: (IsGKTurnBasedMatchmakerViewController gkTurnBasedMatchmakerViewController, IsGKMatchRequest request) => gkTurnBasedMatchmakerViewController -> request -> IO RawId
initWithMatchRequest gkTurnBasedMatchmakerViewController request =
  sendOwnedMessage gkTurnBasedMatchmakerViewController initWithMatchRequestSelector (toGKMatchRequest request)

-- | @- turnBasedMatchmakerDelegate@
turnBasedMatchmakerDelegate :: IsGKTurnBasedMatchmakerViewController gkTurnBasedMatchmakerViewController => gkTurnBasedMatchmakerViewController -> IO RawId
turnBasedMatchmakerDelegate gkTurnBasedMatchmakerViewController =
  sendMessage gkTurnBasedMatchmakerViewController turnBasedMatchmakerDelegateSelector

-- | @- setTurnBasedMatchmakerDelegate:@
setTurnBasedMatchmakerDelegate :: IsGKTurnBasedMatchmakerViewController gkTurnBasedMatchmakerViewController => gkTurnBasedMatchmakerViewController -> RawId -> IO ()
setTurnBasedMatchmakerDelegate gkTurnBasedMatchmakerViewController value =
  sendMessage gkTurnBasedMatchmakerViewController setTurnBasedMatchmakerDelegateSelector value

-- | @- showExistingMatches@
showExistingMatches :: IsGKTurnBasedMatchmakerViewController gkTurnBasedMatchmakerViewController => gkTurnBasedMatchmakerViewController -> IO Bool
showExistingMatches gkTurnBasedMatchmakerViewController =
  sendMessage gkTurnBasedMatchmakerViewController showExistingMatchesSelector

-- | @- setShowExistingMatches:@
setShowExistingMatches :: IsGKTurnBasedMatchmakerViewController gkTurnBasedMatchmakerViewController => gkTurnBasedMatchmakerViewController -> Bool -> IO ()
setShowExistingMatches gkTurnBasedMatchmakerViewController value =
  sendMessage gkTurnBasedMatchmakerViewController setShowExistingMatchesSelector value

-- | This controls the mode of matchmaking to support in the UI (all, nearby only, automatch only, invite only). Throws an exception if you can not set to the desired mode (due to restrictions)
--
-- ObjC selector: @- matchmakingMode@
matchmakingMode :: IsGKTurnBasedMatchmakerViewController gkTurnBasedMatchmakerViewController => gkTurnBasedMatchmakerViewController -> IO GKMatchmakingMode
matchmakingMode gkTurnBasedMatchmakerViewController =
  sendMessage gkTurnBasedMatchmakerViewController matchmakingModeSelector

-- | This controls the mode of matchmaking to support in the UI (all, nearby only, automatch only, invite only). Throws an exception if you can not set to the desired mode (due to restrictions)
--
-- ObjC selector: @- setMatchmakingMode:@
setMatchmakingMode :: IsGKTurnBasedMatchmakerViewController gkTurnBasedMatchmakerViewController => gkTurnBasedMatchmakerViewController -> GKMatchmakingMode -> IO ()
setMatchmakingMode gkTurnBasedMatchmakerViewController value =
  sendMessage gkTurnBasedMatchmakerViewController setMatchmakingModeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMatchRequest:@
initWithMatchRequestSelector :: Selector '[Id GKMatchRequest] RawId
initWithMatchRequestSelector = mkSelector "initWithMatchRequest:"

-- | @Selector@ for @turnBasedMatchmakerDelegate@
turnBasedMatchmakerDelegateSelector :: Selector '[] RawId
turnBasedMatchmakerDelegateSelector = mkSelector "turnBasedMatchmakerDelegate"

-- | @Selector@ for @setTurnBasedMatchmakerDelegate:@
setTurnBasedMatchmakerDelegateSelector :: Selector '[RawId] ()
setTurnBasedMatchmakerDelegateSelector = mkSelector "setTurnBasedMatchmakerDelegate:"

-- | @Selector@ for @showExistingMatches@
showExistingMatchesSelector :: Selector '[] Bool
showExistingMatchesSelector = mkSelector "showExistingMatches"

-- | @Selector@ for @setShowExistingMatches:@
setShowExistingMatchesSelector :: Selector '[Bool] ()
setShowExistingMatchesSelector = mkSelector "setShowExistingMatches:"

-- | @Selector@ for @matchmakingMode@
matchmakingModeSelector :: Selector '[] GKMatchmakingMode
matchmakingModeSelector = mkSelector "matchmakingMode"

-- | @Selector@ for @setMatchmakingMode:@
setMatchmakingModeSelector :: Selector '[GKMatchmakingMode] ()
setMatchmakingModeSelector = mkSelector "setMatchmakingMode:"

