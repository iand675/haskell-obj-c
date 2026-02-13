{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , maxPlayers
  , minPlayers
  , supportsUnlimitedPlayers
  , playStyle
  , releaseState
  , defaultPropertiesSelector
  , detailsSelector
  , fallbackURLSelector
  , groupIdentifierSelector
  , identifierSelector
  , initSelector
  , loadImageWithCompletionHandlerSelector
  , maxPlayersSelector
  , minPlayersSelector
  , playStyleSelector
  , releaseStateSelector
  , supportsPartyCodeSelector
  , supportsUnlimitedPlayersSelector
  , titleSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameKit.Internal.Classes
import ObjC.GameKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsGKGameActivityDefinition gkGameActivityDefinition => gkGameActivityDefinition -> IO (Id GKGameActivityDefinition)
init_ gkGameActivityDefinition =
  sendOwnedMessage gkGameActivityDefinition initSelector

-- | Asynchronously load the image. Error will be nil on success.
--
-- ObjC selector: @- loadImageWithCompletionHandler:@
loadImageWithCompletionHandler :: IsGKGameActivityDefinition gkGameActivityDefinition => gkGameActivityDefinition -> Ptr () -> IO ()
loadImageWithCompletionHandler gkGameActivityDefinition completionHandler =
  sendMessage gkGameActivityDefinition loadImageWithCompletionHandlerSelector completionHandler

-- | The developer defined identifier for a given game activity.
--
-- ObjC selector: @- identifier@
identifier :: IsGKGameActivityDefinition gkGameActivityDefinition => gkGameActivityDefinition -> IO (Id NSString)
identifier gkGameActivityDefinition =
  sendMessage gkGameActivityDefinition identifierSelector

-- | The group identifier for the activity, if one exists.
--
-- ObjC selector: @- groupIdentifier@
groupIdentifier :: IsGKGameActivityDefinition gkGameActivityDefinition => gkGameActivityDefinition -> IO (Id NSString)
groupIdentifier gkGameActivityDefinition =
  sendMessage gkGameActivityDefinition groupIdentifierSelector

-- | A short title for the game activity.
--
-- ObjC selector: @- title@
title :: IsGKGameActivityDefinition gkGameActivityDefinition => gkGameActivityDefinition -> IO (Id NSString)
title gkGameActivityDefinition =
  sendMessage gkGameActivityDefinition titleSelector

-- | A more detailed description of the game activity.
--
-- ObjC selector: @- details@
details :: IsGKGameActivityDefinition gkGameActivityDefinition => gkGameActivityDefinition -> IO (Id NSString)
details gkGameActivityDefinition =
  sendMessage gkGameActivityDefinition detailsSelector

-- | Default properties defined by the developer for this type of game activity.
--
-- ObjC selector: @- defaultProperties@
defaultProperties :: IsGKGameActivityDefinition gkGameActivityDefinition => gkGameActivityDefinition -> IO (Id NSDictionary)
defaultProperties gkGameActivityDefinition =
  sendMessage gkGameActivityDefinition defaultPropertiesSelector

-- | A fallback URL that can be used to construct a game-specific URL for players to share or join, if the joining device does not support the default URL.
--
-- ObjC selector: @- fallbackURL@
fallbackURL :: IsGKGameActivityDefinition gkGameActivityDefinition => gkGameActivityDefinition -> IO (Id NSURL)
fallbackURL gkGameActivityDefinition =
  sendMessage gkGameActivityDefinition fallbackURLSelector

-- | Whether the activity can be joined by others via a party code. - SeeAlso: ``-[GKGameActivityListener player:wantsToPlayGameActivity:completionHandler:]`` where you can receive and handle game activities that players want to play in a party with friends.
--
-- ObjC selector: @- supportsPartyCode@
supportsPartyCode :: IsGKGameActivityDefinition gkGameActivityDefinition => gkGameActivityDefinition -> IO Bool
supportsPartyCode gkGameActivityDefinition =
  sendMessage gkGameActivityDefinition supportsPartyCodeSelector

-- | The maximum number of participants that can join the activity. Returns nil when no maximum is set (unlimited players) or when player range is undefined. When not nil, the value is always greater than or equal to @minPlayers@.
--
-- ObjC selector: @- maxPlayers@
maxPlayers :: IsGKGameActivityDefinition gkGameActivityDefinition => gkGameActivityDefinition -> IO (Id NSNumber)
maxPlayers gkGameActivityDefinition =
  sendMessage gkGameActivityDefinition maxPlayersSelector

-- | The minimum number of participants that can join the activity.
--
-- ObjC selector: @- minPlayers@
minPlayers :: IsGKGameActivityDefinition gkGameActivityDefinition => gkGameActivityDefinition -> IO (Id NSNumber)
minPlayers gkGameActivityDefinition =
  sendMessage gkGameActivityDefinition minPlayersSelector

-- | True if the activity supports an unlimited number of players. False if maxPlayers is set to a defined limit or if no player range is provided.
--
-- ObjC selector: @- supportsUnlimitedPlayers@
supportsUnlimitedPlayers :: IsGKGameActivityDefinition gkGameActivityDefinition => gkGameActivityDefinition -> IO Bool
supportsUnlimitedPlayers gkGameActivityDefinition =
  sendMessage gkGameActivityDefinition supportsUnlimitedPlayersSelector

-- | The play style of the game activity.
--
-- ObjC selector: @- playStyle@
playStyle :: IsGKGameActivityDefinition gkGameActivityDefinition => gkGameActivityDefinition -> IO GKGameActivityPlayStyle
playStyle gkGameActivityDefinition =
  sendMessage gkGameActivityDefinition playStyleSelector

-- | The release state of the game activity definition in App Store Connect.
--
-- ObjC selector: @- releaseState@
releaseState :: IsGKGameActivityDefinition gkGameActivityDefinition => gkGameActivityDefinition -> IO GKReleaseState
releaseState gkGameActivityDefinition =
  sendMessage gkGameActivityDefinition releaseStateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id GKGameActivityDefinition)
initSelector = mkSelector "init"

-- | @Selector@ for @loadImageWithCompletionHandler:@
loadImageWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
loadImageWithCompletionHandlerSelector = mkSelector "loadImageWithCompletionHandler:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @groupIdentifier@
groupIdentifierSelector :: Selector '[] (Id NSString)
groupIdentifierSelector = mkSelector "groupIdentifier"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @details@
detailsSelector :: Selector '[] (Id NSString)
detailsSelector = mkSelector "details"

-- | @Selector@ for @defaultProperties@
defaultPropertiesSelector :: Selector '[] (Id NSDictionary)
defaultPropertiesSelector = mkSelector "defaultProperties"

-- | @Selector@ for @fallbackURL@
fallbackURLSelector :: Selector '[] (Id NSURL)
fallbackURLSelector = mkSelector "fallbackURL"

-- | @Selector@ for @supportsPartyCode@
supportsPartyCodeSelector :: Selector '[] Bool
supportsPartyCodeSelector = mkSelector "supportsPartyCode"

-- | @Selector@ for @maxPlayers@
maxPlayersSelector :: Selector '[] (Id NSNumber)
maxPlayersSelector = mkSelector "maxPlayers"

-- | @Selector@ for @minPlayers@
minPlayersSelector :: Selector '[] (Id NSNumber)
minPlayersSelector = mkSelector "minPlayers"

-- | @Selector@ for @supportsUnlimitedPlayers@
supportsUnlimitedPlayersSelector :: Selector '[] Bool
supportsUnlimitedPlayersSelector = mkSelector "supportsUnlimitedPlayers"

-- | @Selector@ for @playStyle@
playStyleSelector :: Selector '[] GKGameActivityPlayStyle
playStyleSelector = mkSelector "playStyle"

-- | @Selector@ for @releaseState@
releaseStateSelector :: Selector '[] GKReleaseState
releaseStateSelector = mkSelector "releaseState"

