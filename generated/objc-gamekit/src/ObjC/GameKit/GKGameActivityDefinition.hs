{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GKGameActivityDefinition@.
module ObjC.GameKit.GKGameActivityDefinition
  ( GKGameActivityDefinition
  , IsGKGameActivityDefinition(..)
  , init_
  , loadImageWithCompletionHandler
  , identifier
  , groupIdentifier
  , title
  , details
  , defaultProperties
  , fallbackURL
  , supportsPartyCode
  , supportsUnlimitedPlayers
  , playStyle
  , releaseState
  , initSelector
  , loadImageWithCompletionHandlerSelector
  , identifierSelector
  , groupIdentifierSelector
  , titleSelector
  , detailsSelector
  , defaultPropertiesSelector
  , fallbackURLSelector
  , supportsPartyCodeSelector
  , supportsUnlimitedPlayersSelector
  , playStyleSelector
  , releaseStateSelector

  -- * Enum types
  , GKGameActivityPlayStyle(GKGameActivityPlayStyle)
  , pattern GKGameActivityPlayStyleUnspecified
  , pattern GKGameActivityPlayStyleSynchronous
  , pattern GKGameActivityPlayStyleAsynchronous
  , GKReleaseState(GKReleaseState)
  , pattern GKReleaseStateUnknown
  , pattern GKReleaseStateReleased
  , pattern GKReleaseStatePrereleased

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
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsGKGameActivityDefinition gkGameActivityDefinition => gkGameActivityDefinition -> IO (Id GKGameActivityDefinition)
init_ gkGameActivityDefinition  =
  sendMsg gkGameActivityDefinition (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Asynchronously load the image. Error will be nil on success.
--
-- ObjC selector: @- loadImageWithCompletionHandler:@
loadImageWithCompletionHandler :: IsGKGameActivityDefinition gkGameActivityDefinition => gkGameActivityDefinition -> Ptr () -> IO ()
loadImageWithCompletionHandler gkGameActivityDefinition  completionHandler =
  sendMsg gkGameActivityDefinition (mkSelector "loadImageWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | The developer defined identifier for a given game activity.
--
-- ObjC selector: @- identifier@
identifier :: IsGKGameActivityDefinition gkGameActivityDefinition => gkGameActivityDefinition -> IO (Id NSString)
identifier gkGameActivityDefinition  =
  sendMsg gkGameActivityDefinition (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The group identifier for the activity, if one exists.
--
-- ObjC selector: @- groupIdentifier@
groupIdentifier :: IsGKGameActivityDefinition gkGameActivityDefinition => gkGameActivityDefinition -> IO (Id NSString)
groupIdentifier gkGameActivityDefinition  =
  sendMsg gkGameActivityDefinition (mkSelector "groupIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A short title for the game activity.
--
-- ObjC selector: @- title@
title :: IsGKGameActivityDefinition gkGameActivityDefinition => gkGameActivityDefinition -> IO (Id NSString)
title gkGameActivityDefinition  =
  sendMsg gkGameActivityDefinition (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A more detailed description of the game activity.
--
-- ObjC selector: @- details@
details :: IsGKGameActivityDefinition gkGameActivityDefinition => gkGameActivityDefinition -> IO (Id NSString)
details gkGameActivityDefinition  =
  sendMsg gkGameActivityDefinition (mkSelector "details") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Default properties defined by the developer for this type of game activity.
--
-- ObjC selector: @- defaultProperties@
defaultProperties :: IsGKGameActivityDefinition gkGameActivityDefinition => gkGameActivityDefinition -> IO (Id NSDictionary)
defaultProperties gkGameActivityDefinition  =
  sendMsg gkGameActivityDefinition (mkSelector "defaultProperties") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A fallback URL that can be used to construct a game-specific URL for players to share or join, if the joining device does not support the default URL.
--
-- ObjC selector: @- fallbackURL@
fallbackURL :: IsGKGameActivityDefinition gkGameActivityDefinition => gkGameActivityDefinition -> IO (Id NSURL)
fallbackURL gkGameActivityDefinition  =
  sendMsg gkGameActivityDefinition (mkSelector "fallbackURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Whether the activity can be joined by others via a party code. - SeeAlso: ``-[GKGameActivityListener player:wantsToPlayGameActivity:completionHandler:]`` where you can receive and handle game activities that players want to play in a party with friends.
--
-- ObjC selector: @- supportsPartyCode@
supportsPartyCode :: IsGKGameActivityDefinition gkGameActivityDefinition => gkGameActivityDefinition -> IO Bool
supportsPartyCode gkGameActivityDefinition  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkGameActivityDefinition (mkSelector "supportsPartyCode") retCULong []

-- | True if the activity supports an unlimited number of players. False if maxPlayers is set to a defined limit or if no player range is provided.
--
-- ObjC selector: @- supportsUnlimitedPlayers@
supportsUnlimitedPlayers :: IsGKGameActivityDefinition gkGameActivityDefinition => gkGameActivityDefinition -> IO Bool
supportsUnlimitedPlayers gkGameActivityDefinition  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkGameActivityDefinition (mkSelector "supportsUnlimitedPlayers") retCULong []

-- | The play style of the game activity.
--
-- ObjC selector: @- playStyle@
playStyle :: IsGKGameActivityDefinition gkGameActivityDefinition => gkGameActivityDefinition -> IO GKGameActivityPlayStyle
playStyle gkGameActivityDefinition  =
  fmap (coerce :: CLong -> GKGameActivityPlayStyle) $ sendMsg gkGameActivityDefinition (mkSelector "playStyle") retCLong []

-- | The release state of the game activity definition in App Store Connect.
--
-- ObjC selector: @- releaseState@
releaseState :: IsGKGameActivityDefinition gkGameActivityDefinition => gkGameActivityDefinition -> IO GKReleaseState
releaseState gkGameActivityDefinition  =
  fmap (coerce :: CULong -> GKReleaseState) $ sendMsg gkGameActivityDefinition (mkSelector "releaseState") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @loadImageWithCompletionHandler:@
loadImageWithCompletionHandlerSelector :: Selector
loadImageWithCompletionHandlerSelector = mkSelector "loadImageWithCompletionHandler:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @groupIdentifier@
groupIdentifierSelector :: Selector
groupIdentifierSelector = mkSelector "groupIdentifier"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @details@
detailsSelector :: Selector
detailsSelector = mkSelector "details"

-- | @Selector@ for @defaultProperties@
defaultPropertiesSelector :: Selector
defaultPropertiesSelector = mkSelector "defaultProperties"

-- | @Selector@ for @fallbackURL@
fallbackURLSelector :: Selector
fallbackURLSelector = mkSelector "fallbackURL"

-- | @Selector@ for @supportsPartyCode@
supportsPartyCodeSelector :: Selector
supportsPartyCodeSelector = mkSelector "supportsPartyCode"

-- | @Selector@ for @supportsUnlimitedPlayers@
supportsUnlimitedPlayersSelector :: Selector
supportsUnlimitedPlayersSelector = mkSelector "supportsUnlimitedPlayers"

-- | @Selector@ for @playStyle@
playStyleSelector :: Selector
playStyleSelector = mkSelector "playStyle"

-- | @Selector@ for @releaseState@
releaseStateSelector :: Selector
releaseStateSelector = mkSelector "releaseState"

