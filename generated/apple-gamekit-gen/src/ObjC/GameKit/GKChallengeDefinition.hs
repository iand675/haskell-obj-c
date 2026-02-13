{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GKChallengeDefinition@.
module ObjC.GameKit.GKChallengeDefinition
  ( GKChallengeDefinition
  , IsGKChallengeDefinition(..)
  , loadImageWithCompletionHandler
  , hasActiveChallengesWithCompletionHandler
  , identifier
  , groupIdentifier
  , title
  , details
  , durationOptions
  , isRepeatable
  , leaderboard
  , releaseState
  , detailsSelector
  , durationOptionsSelector
  , groupIdentifierSelector
  , hasActiveChallengesWithCompletionHandlerSelector
  , identifierSelector
  , isRepeatableSelector
  , leaderboardSelector
  , loadImageWithCompletionHandlerSelector
  , releaseStateSelector
  , titleSelector

  -- * Enum types
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

-- | Loads the image set on the challenge definition, which may be @nil@ if none was set.
--
-- ObjC selector: @- loadImageWithCompletionHandler:@
loadImageWithCompletionHandler :: IsGKChallengeDefinition gkChallengeDefinition => gkChallengeDefinition -> Ptr () -> IO ()
loadImageWithCompletionHandler gkChallengeDefinition completionHandler =
  sendMessage gkChallengeDefinition loadImageWithCompletionHandlerSelector completionHandler

-- | Indicates if this definition has active challenges associated with it.
--
-- ObjC selector: @- hasActiveChallengesWithCompletionHandler:@
hasActiveChallengesWithCompletionHandler :: IsGKChallengeDefinition gkChallengeDefinition => gkChallengeDefinition -> Ptr () -> IO ()
hasActiveChallengesWithCompletionHandler gkChallengeDefinition completionHandler =
  sendMessage gkChallengeDefinition hasActiveChallengesWithCompletionHandlerSelector completionHandler

-- | The developer defined identifier for a given challenge definition.
--
-- ObjC selector: @- identifier@
identifier :: IsGKChallengeDefinition gkChallengeDefinition => gkChallengeDefinition -> IO (Id NSString)
identifier gkChallengeDefinition =
  sendMessage gkChallengeDefinition identifierSelector

-- | The group identifier for the challenge definition, if one exists.
--
-- ObjC selector: @- groupIdentifier@
groupIdentifier :: IsGKChallengeDefinition gkChallengeDefinition => gkChallengeDefinition -> IO (Id NSString)
groupIdentifier gkChallengeDefinition =
  sendMessage gkChallengeDefinition groupIdentifierSelector

-- | A short title for the challenge definition.
--
-- ObjC selector: @- title@
title :: IsGKChallengeDefinition gkChallengeDefinition => gkChallengeDefinition -> IO (Id NSString)
title gkChallengeDefinition =
  sendMessage gkChallengeDefinition titleSelector

-- | A more detailed description of the challenge definition.
--
-- ObjC selector: @- details@
details :: IsGKChallengeDefinition gkChallengeDefinition => gkChallengeDefinition -> IO (Id NSString)
details gkChallengeDefinition =
  sendMessage gkChallengeDefinition detailsSelector

-- | The duration options for the challenge, like @1 day@ or @1 week@.  - Note: If set, the amount of weeks is stored in the @weekOfYear@ field. - Important: The actual duration of the challenge may be dynamically adjusted              in order to accommodate different factors like players' timezones.
--
-- ObjC selector: @- durationOptions@
durationOptions :: IsGKChallengeDefinition gkChallengeDefinition => gkChallengeDefinition -> IO (Id NSArray)
durationOptions gkChallengeDefinition =
  sendMessage gkChallengeDefinition durationOptionsSelector

-- | Indicates if a challenge can be attempted more than once.
--
-- ObjC selector: @- isRepeatable@
isRepeatable :: IsGKChallengeDefinition gkChallengeDefinition => gkChallengeDefinition -> IO Bool
isRepeatable gkChallengeDefinition =
  sendMessage gkChallengeDefinition isRepeatableSelector

-- | Scores submitted to this leaderboard will also be submitted as scores in this challenge.
--
-- ObjC selector: @- leaderboard@
leaderboard :: IsGKChallengeDefinition gkChallengeDefinition => gkChallengeDefinition -> IO (Id GKLeaderboard)
leaderboard gkChallengeDefinition =
  sendMessage gkChallengeDefinition leaderboardSelector

-- | The release state of the challenge definition in App Store Connect.
--
-- ObjC selector: @- releaseState@
releaseState :: IsGKChallengeDefinition gkChallengeDefinition => gkChallengeDefinition -> IO GKReleaseState
releaseState gkChallengeDefinition =
  sendMessage gkChallengeDefinition releaseStateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @loadImageWithCompletionHandler:@
loadImageWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
loadImageWithCompletionHandlerSelector = mkSelector "loadImageWithCompletionHandler:"

-- | @Selector@ for @hasActiveChallengesWithCompletionHandler:@
hasActiveChallengesWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
hasActiveChallengesWithCompletionHandlerSelector = mkSelector "hasActiveChallengesWithCompletionHandler:"

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

-- | @Selector@ for @durationOptions@
durationOptionsSelector :: Selector '[] (Id NSArray)
durationOptionsSelector = mkSelector "durationOptions"

-- | @Selector@ for @isRepeatable@
isRepeatableSelector :: Selector '[] Bool
isRepeatableSelector = mkSelector "isRepeatable"

-- | @Selector@ for @leaderboard@
leaderboardSelector :: Selector '[] (Id GKLeaderboard)
leaderboardSelector = mkSelector "leaderboard"

-- | @Selector@ for @releaseState@
releaseStateSelector :: Selector '[] GKReleaseState
releaseStateSelector = mkSelector "releaseState"

