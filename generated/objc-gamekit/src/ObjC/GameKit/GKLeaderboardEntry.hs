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
  , initSelector
  , challengeComposeControllerWithMessage_players_completionHandlerSelector
  , challengeComposeControllerWithMessage_players_completionSelector
  , playerSelector
  , rankSelector
  , scoreSelector
  , formattedScoreSelector
  , contextSelector
  , dateSelector


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
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsGKLeaderboardEntry gkLeaderboardEntry => gkLeaderboardEntry -> IO (Id GKLeaderboardEntry)
init_ gkLeaderboardEntry  =
  sendMsg gkLeaderboardEntry (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- challengeComposeControllerWithMessage:players:completionHandler:@
challengeComposeControllerWithMessage_players_completionHandler :: (IsGKLeaderboardEntry gkLeaderboardEntry, IsNSString message, IsNSArray players) => gkLeaderboardEntry -> message -> players -> Ptr () -> IO (Id NSViewController)
challengeComposeControllerWithMessage_players_completionHandler gkLeaderboardEntry  message players completionHandler =
withObjCPtr message $ \raw_message ->
  withObjCPtr players $ \raw_players ->
      sendMsg gkLeaderboardEntry (mkSelector "challengeComposeControllerWithMessage:players:completionHandler:") (retPtr retVoid) [argPtr (castPtr raw_message :: Ptr ()), argPtr (castPtr raw_players :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= retainedObject . castPtr

-- | @- challengeComposeControllerWithMessage:players:completion:@
challengeComposeControllerWithMessage_players_completion :: (IsGKLeaderboardEntry gkLeaderboardEntry, IsNSString message, IsNSArray players) => gkLeaderboardEntry -> message -> players -> Ptr () -> IO (Id NSViewController)
challengeComposeControllerWithMessage_players_completion gkLeaderboardEntry  message players completionHandler =
withObjCPtr message $ \raw_message ->
  withObjCPtr players $ \raw_players ->
      sendMsg gkLeaderboardEntry (mkSelector "challengeComposeControllerWithMessage:players:completion:") (retPtr retVoid) [argPtr (castPtr raw_message :: Ptr ()), argPtr (castPtr raw_players :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= retainedObject . castPtr

-- | @- player@
player :: IsGKLeaderboardEntry gkLeaderboardEntry => gkLeaderboardEntry -> IO (Id GKPlayer)
player gkLeaderboardEntry  =
  sendMsg gkLeaderboardEntry (mkSelector "player") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- rank@
rank :: IsGKLeaderboardEntry gkLeaderboardEntry => gkLeaderboardEntry -> IO CLong
rank gkLeaderboardEntry  =
  sendMsg gkLeaderboardEntry (mkSelector "rank") retCLong []

-- | @- score@
score :: IsGKLeaderboardEntry gkLeaderboardEntry => gkLeaderboardEntry -> IO CLong
score gkLeaderboardEntry  =
  sendMsg gkLeaderboardEntry (mkSelector "score") retCLong []

-- | @- formattedScore@
formattedScore :: IsGKLeaderboardEntry gkLeaderboardEntry => gkLeaderboardEntry -> IO (Id NSString)
formattedScore gkLeaderboardEntry  =
  sendMsg gkLeaderboardEntry (mkSelector "formattedScore") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- context@
context :: IsGKLeaderboardEntry gkLeaderboardEntry => gkLeaderboardEntry -> IO CULong
context gkLeaderboardEntry  =
  sendMsg gkLeaderboardEntry (mkSelector "context") retCULong []

-- | @- date@
date :: IsGKLeaderboardEntry gkLeaderboardEntry => gkLeaderboardEntry -> IO (Id NSDate)
date gkLeaderboardEntry  =
  sendMsg gkLeaderboardEntry (mkSelector "date") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @challengeComposeControllerWithMessage:players:completionHandler:@
challengeComposeControllerWithMessage_players_completionHandlerSelector :: Selector
challengeComposeControllerWithMessage_players_completionHandlerSelector = mkSelector "challengeComposeControllerWithMessage:players:completionHandler:"

-- | @Selector@ for @challengeComposeControllerWithMessage:players:completion:@
challengeComposeControllerWithMessage_players_completionSelector :: Selector
challengeComposeControllerWithMessage_players_completionSelector = mkSelector "challengeComposeControllerWithMessage:players:completion:"

-- | @Selector@ for @player@
playerSelector :: Selector
playerSelector = mkSelector "player"

-- | @Selector@ for @rank@
rankSelector :: Selector
rankSelector = mkSelector "rank"

-- | @Selector@ for @score@
scoreSelector :: Selector
scoreSelector = mkSelector "score"

-- | @Selector@ for @formattedScore@
formattedScoreSelector :: Selector
formattedScoreSelector = mkSelector "formattedScore"

-- | @Selector@ for @context@
contextSelector :: Selector
contextSelector = mkSelector "context"

-- | @Selector@ for @date@
dateSelector :: Selector
dateSelector = mkSelector "date"

