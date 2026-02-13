{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | GKLeaderboardSet represents the sets that leaderboards can be broken out into.
--
-- Generated bindings for @GKLeaderboardSet@.
module ObjC.GameKit.GKLeaderboardSet
  ( GKLeaderboardSet
  , IsGKLeaderboardSet(..)
  , loadImageWithCompletionHandler
  , title
  , groupIdentifier
  , identifier
  , setIdentifier
  , groupIdentifierSelector
  , identifierSelector
  , loadImageWithCompletionHandlerSelector
  , setIdentifierSelector
  , titleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Asynchronously load the image. Error will be nil on success.
--
-- ObjC selector: @- loadImageWithCompletionHandler:@
loadImageWithCompletionHandler :: IsGKLeaderboardSet gkLeaderboardSet => gkLeaderboardSet -> Ptr () -> IO ()
loadImageWithCompletionHandler gkLeaderboardSet completionHandler =
  sendMessage gkLeaderboardSet loadImageWithCompletionHandlerSelector completionHandler

-- | Localized set title.
--
-- ObjC selector: @- title@
title :: IsGKLeaderboardSet gkLeaderboardSet => gkLeaderboardSet -> IO (Id NSString)
title gkLeaderboardSet =
  sendMessage gkLeaderboardSet titleSelector

-- | set when leaderboardSets have been designated a game group; set when loadLeaderboardSetsWithCompletionHandler has been called for leaderboards that support game groups
--
-- ObjC selector: @- groupIdentifier@
groupIdentifier :: IsGKLeaderboardSet gkLeaderboardSet => gkLeaderboardSet -> IO (Id NSString)
groupIdentifier gkLeaderboardSet =
  sendMessage gkLeaderboardSet groupIdentifierSelector

-- | leaderboard set.
--
-- ObjC selector: @- identifier@
identifier :: IsGKLeaderboardSet gkLeaderboardSet => gkLeaderboardSet -> IO (Id NSString)
identifier gkLeaderboardSet =
  sendMessage gkLeaderboardSet identifierSelector

-- | leaderboard set.
--
-- ObjC selector: @- setIdentifier:@
setIdentifier :: (IsGKLeaderboardSet gkLeaderboardSet, IsNSString value) => gkLeaderboardSet -> value -> IO ()
setIdentifier gkLeaderboardSet value =
  sendMessage gkLeaderboardSet setIdentifierSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @loadImageWithCompletionHandler:@
loadImageWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
loadImageWithCompletionHandlerSelector = mkSelector "loadImageWithCompletionHandler:"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @groupIdentifier@
groupIdentifierSelector :: Selector '[] (Id NSString)
groupIdentifierSelector = mkSelector "groupIdentifier"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector '[Id NSString] ()
setIdentifierSelector = mkSelector "setIdentifier:"

