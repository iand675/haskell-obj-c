{-# LANGUAGE PatternSynonyms #-}
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
  , loadImageWithCompletionHandlerSelector
  , hasActiveChallengesWithCompletionHandlerSelector
  , identifierSelector
  , groupIdentifierSelector
  , titleSelector
  , detailsSelector
  , durationOptionsSelector
  , isRepeatableSelector
  , leaderboardSelector
  , releaseStateSelector

  -- * Enum types
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

-- | Loads the image set on the challenge definition, which may be @nil@ if none was set.
--
-- ObjC selector: @- loadImageWithCompletionHandler:@
loadImageWithCompletionHandler :: IsGKChallengeDefinition gkChallengeDefinition => gkChallengeDefinition -> Ptr () -> IO ()
loadImageWithCompletionHandler gkChallengeDefinition  completionHandler =
  sendMsg gkChallengeDefinition (mkSelector "loadImageWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | Indicates if this definition has active challenges associated with it.
--
-- ObjC selector: @- hasActiveChallengesWithCompletionHandler:@
hasActiveChallengesWithCompletionHandler :: IsGKChallengeDefinition gkChallengeDefinition => gkChallengeDefinition -> Ptr () -> IO ()
hasActiveChallengesWithCompletionHandler gkChallengeDefinition  completionHandler =
  sendMsg gkChallengeDefinition (mkSelector "hasActiveChallengesWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | The developer defined identifier for a given challenge definition.
--
-- ObjC selector: @- identifier@
identifier :: IsGKChallengeDefinition gkChallengeDefinition => gkChallengeDefinition -> IO (Id NSString)
identifier gkChallengeDefinition  =
  sendMsg gkChallengeDefinition (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The group identifier for the challenge definition, if one exists.
--
-- ObjC selector: @- groupIdentifier@
groupIdentifier :: IsGKChallengeDefinition gkChallengeDefinition => gkChallengeDefinition -> IO (Id NSString)
groupIdentifier gkChallengeDefinition  =
  sendMsg gkChallengeDefinition (mkSelector "groupIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A short title for the challenge definition.
--
-- ObjC selector: @- title@
title :: IsGKChallengeDefinition gkChallengeDefinition => gkChallengeDefinition -> IO (Id NSString)
title gkChallengeDefinition  =
  sendMsg gkChallengeDefinition (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A more detailed description of the challenge definition.
--
-- ObjC selector: @- details@
details :: IsGKChallengeDefinition gkChallengeDefinition => gkChallengeDefinition -> IO (Id NSString)
details gkChallengeDefinition  =
  sendMsg gkChallengeDefinition (mkSelector "details") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The duration options for the challenge, like @1 day@ or @1 week@.  - Note: If set, the amount of weeks is stored in the @weekOfYear@ field. - Important: The actual duration of the challenge may be dynamically adjusted              in order to accommodate different factors like players' timezones.
--
-- ObjC selector: @- durationOptions@
durationOptions :: IsGKChallengeDefinition gkChallengeDefinition => gkChallengeDefinition -> IO (Id NSArray)
durationOptions gkChallengeDefinition  =
  sendMsg gkChallengeDefinition (mkSelector "durationOptions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Indicates if a challenge can be attempted more than once.
--
-- ObjC selector: @- isRepeatable@
isRepeatable :: IsGKChallengeDefinition gkChallengeDefinition => gkChallengeDefinition -> IO Bool
isRepeatable gkChallengeDefinition  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkChallengeDefinition (mkSelector "isRepeatable") retCULong []

-- | Scores submitted to this leaderboard will also be submitted as scores in this challenge.
--
-- ObjC selector: @- leaderboard@
leaderboard :: IsGKChallengeDefinition gkChallengeDefinition => gkChallengeDefinition -> IO (Id GKLeaderboard)
leaderboard gkChallengeDefinition  =
  sendMsg gkChallengeDefinition (mkSelector "leaderboard") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The release state of the challenge definition in App Store Connect.
--
-- ObjC selector: @- releaseState@
releaseState :: IsGKChallengeDefinition gkChallengeDefinition => gkChallengeDefinition -> IO GKReleaseState
releaseState gkChallengeDefinition  =
  fmap (coerce :: CULong -> GKReleaseState) $ sendMsg gkChallengeDefinition (mkSelector "releaseState") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @loadImageWithCompletionHandler:@
loadImageWithCompletionHandlerSelector :: Selector
loadImageWithCompletionHandlerSelector = mkSelector "loadImageWithCompletionHandler:"

-- | @Selector@ for @hasActiveChallengesWithCompletionHandler:@
hasActiveChallengesWithCompletionHandlerSelector :: Selector
hasActiveChallengesWithCompletionHandlerSelector = mkSelector "hasActiveChallengesWithCompletionHandler:"

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

-- | @Selector@ for @durationOptions@
durationOptionsSelector :: Selector
durationOptionsSelector = mkSelector "durationOptions"

-- | @Selector@ for @isRepeatable@
isRepeatableSelector :: Selector
isRepeatableSelector = mkSelector "isRepeatable"

-- | @Selector@ for @leaderboard@
leaderboardSelector :: Selector
leaderboardSelector = mkSelector "leaderboard"

-- | @Selector@ for @releaseState@
releaseStateSelector :: Selector
releaseStateSelector = mkSelector "releaseState"

