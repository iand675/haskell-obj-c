{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GKLeaderboardEntry@.
module ObjC.GameKit.GKLeaderboardEntry
  ( GKLeaderboardEntry
  , IsGKLeaderboardEntry(..)
  , init_
  , challengeComposeControllerWithMessage_players_completionHandler
  , challengeComposeControllerWithMessage_players_completion
  , player
  , rank
  , score
  , formattedScore
  , context
  , date
  , challengeComposeControllerWithMessage_players_completionHandlerSelector
  , challengeComposeControllerWithMessage_players_completionSelector
  , contextSelector
  , dateSelector
  , formattedScoreSelector
  , initSelector
  , playerSelector
  , rankSelector
  , scoreSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsGKLeaderboardEntry gkLeaderboardEntry => gkLeaderboardEntry -> IO (Id GKLeaderboardEntry)
init_ gkLeaderboardEntry =
  sendOwnedMessage gkLeaderboardEntry initSelector

-- | @- challengeComposeControllerWithMessage:players:completionHandler:@
challengeComposeControllerWithMessage_players_completionHandler :: (IsGKLeaderboardEntry gkLeaderboardEntry, IsNSString message, IsNSArray players) => gkLeaderboardEntry -> message -> players -> Ptr () -> IO (Id NSViewController)
challengeComposeControllerWithMessage_players_completionHandler gkLeaderboardEntry message players completionHandler =
  sendMessage gkLeaderboardEntry challengeComposeControllerWithMessage_players_completionHandlerSelector (toNSString message) (toNSArray players) completionHandler

-- | @- challengeComposeControllerWithMessage:players:completion:@
challengeComposeControllerWithMessage_players_completion :: (IsGKLeaderboardEntry gkLeaderboardEntry, IsNSString message, IsNSArray players) => gkLeaderboardEntry -> message -> players -> Ptr () -> IO (Id NSViewController)
challengeComposeControllerWithMessage_players_completion gkLeaderboardEntry message players completionHandler =
  sendMessage gkLeaderboardEntry challengeComposeControllerWithMessage_players_completionSelector (toNSString message) (toNSArray players) completionHandler

-- | @- player@
player :: IsGKLeaderboardEntry gkLeaderboardEntry => gkLeaderboardEntry -> IO (Id GKPlayer)
player gkLeaderboardEntry =
  sendMessage gkLeaderboardEntry playerSelector

-- | @- rank@
rank :: IsGKLeaderboardEntry gkLeaderboardEntry => gkLeaderboardEntry -> IO CLong
rank gkLeaderboardEntry =
  sendMessage gkLeaderboardEntry rankSelector

-- | @- score@
score :: IsGKLeaderboardEntry gkLeaderboardEntry => gkLeaderboardEntry -> IO CLong
score gkLeaderboardEntry =
  sendMessage gkLeaderboardEntry scoreSelector

-- | @- formattedScore@
formattedScore :: IsGKLeaderboardEntry gkLeaderboardEntry => gkLeaderboardEntry -> IO (Id NSString)
formattedScore gkLeaderboardEntry =
  sendMessage gkLeaderboardEntry formattedScoreSelector

-- | @- context@
context :: IsGKLeaderboardEntry gkLeaderboardEntry => gkLeaderboardEntry -> IO CULong
context gkLeaderboardEntry =
  sendMessage gkLeaderboardEntry contextSelector

-- | @- date@
date :: IsGKLeaderboardEntry gkLeaderboardEntry => gkLeaderboardEntry -> IO (Id NSDate)
date gkLeaderboardEntry =
  sendMessage gkLeaderboardEntry dateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id GKLeaderboardEntry)
initSelector = mkSelector "init"

-- | @Selector@ for @challengeComposeControllerWithMessage:players:completionHandler:@
challengeComposeControllerWithMessage_players_completionHandlerSelector :: Selector '[Id NSString, Id NSArray, Ptr ()] (Id NSViewController)
challengeComposeControllerWithMessage_players_completionHandlerSelector = mkSelector "challengeComposeControllerWithMessage:players:completionHandler:"

-- | @Selector@ for @challengeComposeControllerWithMessage:players:completion:@
challengeComposeControllerWithMessage_players_completionSelector :: Selector '[Id NSString, Id NSArray, Ptr ()] (Id NSViewController)
challengeComposeControllerWithMessage_players_completionSelector = mkSelector "challengeComposeControllerWithMessage:players:completion:"

-- | @Selector@ for @player@
playerSelector :: Selector '[] (Id GKPlayer)
playerSelector = mkSelector "player"

-- | @Selector@ for @rank@
rankSelector :: Selector '[] CLong
rankSelector = mkSelector "rank"

-- | @Selector@ for @score@
scoreSelector :: Selector '[] CLong
scoreSelector = mkSelector "score"

-- | @Selector@ for @formattedScore@
formattedScoreSelector :: Selector '[] (Id NSString)
formattedScoreSelector = mkSelector "formattedScore"

-- | @Selector@ for @context@
contextSelector :: Selector '[] CULong
contextSelector = mkSelector "context"

-- | @Selector@ for @date@
dateSelector :: Selector '[] (Id NSDate)
dateSelector = mkSelector "date"

