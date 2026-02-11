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
  , loadImageWithCompletionHandlerSelector
  , titleSelector
  , groupIdentifierSelector
  , identifierSelector
  , setIdentifierSelector


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

-- | Asynchronously load the image. Error will be nil on success.
--
-- ObjC selector: @- loadImageWithCompletionHandler:@
loadImageWithCompletionHandler :: IsGKLeaderboardSet gkLeaderboardSet => gkLeaderboardSet -> Ptr () -> IO ()
loadImageWithCompletionHandler gkLeaderboardSet  completionHandler =
  sendMsg gkLeaderboardSet (mkSelector "loadImageWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | Localized set title.
--
-- ObjC selector: @- title@
title :: IsGKLeaderboardSet gkLeaderboardSet => gkLeaderboardSet -> IO (Id NSString)
title gkLeaderboardSet  =
  sendMsg gkLeaderboardSet (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | set when leaderboardSets have been designated a game group; set when loadLeaderboardSetsWithCompletionHandler has been called for leaderboards that support game groups
--
-- ObjC selector: @- groupIdentifier@
groupIdentifier :: IsGKLeaderboardSet gkLeaderboardSet => gkLeaderboardSet -> IO (Id NSString)
groupIdentifier gkLeaderboardSet  =
  sendMsg gkLeaderboardSet (mkSelector "groupIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | leaderboard set.
--
-- ObjC selector: @- identifier@
identifier :: IsGKLeaderboardSet gkLeaderboardSet => gkLeaderboardSet -> IO (Id NSString)
identifier gkLeaderboardSet  =
  sendMsg gkLeaderboardSet (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | leaderboard set.
--
-- ObjC selector: @- setIdentifier:@
setIdentifier :: (IsGKLeaderboardSet gkLeaderboardSet, IsNSString value) => gkLeaderboardSet -> value -> IO ()
setIdentifier gkLeaderboardSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg gkLeaderboardSet (mkSelector "setIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @loadImageWithCompletionHandler:@
loadImageWithCompletionHandlerSelector :: Selector
loadImageWithCompletionHandlerSelector = mkSelector "loadImageWithCompletionHandler:"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @groupIdentifier@
groupIdentifierSelector :: Selector
groupIdentifierSelector = mkSelector "groupIdentifier"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector
setIdentifierSelector = mkSelector "setIdentifier:"

