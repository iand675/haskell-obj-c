{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.GameKit.Internal.Classes (
    module ObjC.GameKit.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- GKAccessPoint ----------

-- | Phantom type for @GKAccessPoint@.
data GKAccessPoint

instance IsObjCObject (Id GKAccessPoint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKAccessPoint"

class IsNSObject a => IsGKAccessPoint a where
  toGKAccessPoint :: a -> Id GKAccessPoint

instance IsGKAccessPoint (Id GKAccessPoint) where
  toGKAccessPoint = unsafeCastId

instance IsNSObject (Id GKAccessPoint) where
  toNSObject = unsafeCastId

-- ---------- GKAchievement ----------

-- | Phantom type for @GKAchievement@.
data GKAchievement

instance IsObjCObject (Id GKAchievement) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKAchievement"

class IsNSObject a => IsGKAchievement a where
  toGKAchievement :: a -> Id GKAchievement

instance IsGKAchievement (Id GKAchievement) where
  toGKAchievement = unsafeCastId

instance IsNSObject (Id GKAchievement) where
  toNSObject = unsafeCastId

-- ---------- GKAchievementDescription ----------

-- | GKAchievementDescription is a full description of the achievement as defined before app submission in App Store Connect.
-- 
-- Phantom type for @GKAchievementDescription@.
data GKAchievementDescription

instance IsObjCObject (Id GKAchievementDescription) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKAchievementDescription"

class IsNSObject a => IsGKAchievementDescription a where
  toGKAchievementDescription :: a -> Id GKAchievementDescription

instance IsGKAchievementDescription (Id GKAchievementDescription) where
  toGKAchievementDescription = unsafeCastId

instance IsNSObject (Id GKAchievementDescription) where
  toNSObject = unsafeCastId

-- ---------- GKBasePlayer ----------

-- | Phantom type for @GKBasePlayer@.
data GKBasePlayer

instance IsObjCObject (Id GKBasePlayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKBasePlayer"

class IsNSObject a => IsGKBasePlayer a where
  toGKBasePlayer :: a -> Id GKBasePlayer

instance IsGKBasePlayer (Id GKBasePlayer) where
  toGKBasePlayer = unsafeCastId

instance IsNSObject (Id GKBasePlayer) where
  toNSObject = unsafeCastId

-- ---------- GKChallenge ----------

-- | Phantom type for @GKChallenge@.
data GKChallenge

instance IsObjCObject (Id GKChallenge) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKChallenge"

class IsNSObject a => IsGKChallenge a where
  toGKChallenge :: a -> Id GKChallenge

instance IsGKChallenge (Id GKChallenge) where
  toGKChallenge = unsafeCastId

instance IsNSObject (Id GKChallenge) where
  toNSObject = unsafeCastId

-- ---------- GKChallengeDefinition ----------

-- | Phantom type for @GKChallengeDefinition@.
data GKChallengeDefinition

instance IsObjCObject (Id GKChallengeDefinition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKChallengeDefinition"

class IsNSObject a => IsGKChallengeDefinition a where
  toGKChallengeDefinition :: a -> Id GKChallengeDefinition

instance IsGKChallengeDefinition (Id GKChallengeDefinition) where
  toGKChallengeDefinition = unsafeCastId

instance IsNSObject (Id GKChallengeDefinition) where
  toNSObject = unsafeCastId

-- ---------- GKChallengeEventHandler ----------

-- | A singleton object responsible for dispatching challenge-related events to its delegate
-- 
-- Phantom type for @GKChallengeEventHandler@.
data GKChallengeEventHandler

instance IsObjCObject (Id GKChallengeEventHandler) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKChallengeEventHandler"

class IsNSObject a => IsGKChallengeEventHandler a where
  toGKChallengeEventHandler :: a -> Id GKChallengeEventHandler

instance IsGKChallengeEventHandler (Id GKChallengeEventHandler) where
  toGKChallengeEventHandler = unsafeCastId

instance IsNSObject (Id GKChallengeEventHandler) where
  toNSObject = unsafeCastId

-- ---------- GKGameActivity ----------

-- | An object that represents a single instance of a game activity for the current game.
-- 
-- Phantom type for @GKGameActivity@.
data GKGameActivity

instance IsObjCObject (Id GKGameActivity) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKGameActivity"

class IsNSObject a => IsGKGameActivity a where
  toGKGameActivity :: a -> Id GKGameActivity

instance IsGKGameActivity (Id GKGameActivity) where
  toGKGameActivity = unsafeCastId

instance IsNSObject (Id GKGameActivity) where
  toNSObject = unsafeCastId

-- ---------- GKGameActivityDefinition ----------

-- | Phantom type for @GKGameActivityDefinition@.
data GKGameActivityDefinition

instance IsObjCObject (Id GKGameActivityDefinition) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKGameActivityDefinition"

class IsNSObject a => IsGKGameActivityDefinition a where
  toGKGameActivityDefinition :: a -> Id GKGameActivityDefinition

instance IsGKGameActivityDefinition (Id GKGameActivityDefinition) where
  toGKGameActivityDefinition = unsafeCastId

instance IsNSObject (Id GKGameActivityDefinition) where
  toNSObject = unsafeCastId

-- ---------- GKGameSession ----------

-- | Phantom type for @GKGameSession@.
data GKGameSession

instance IsObjCObject (Id GKGameSession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKGameSession"

class IsNSObject a => IsGKGameSession a where
  toGKGameSession :: a -> Id GKGameSession

instance IsGKGameSession (Id GKGameSession) where
  toGKGameSession = unsafeCastId

instance IsNSObject (Id GKGameSession) where
  toNSObject = unsafeCastId

-- ---------- GKInvite ----------

-- | GKInvite represents an accepted game invite, it is used to create a GKMatchmakerViewController
-- 
-- Phantom type for @GKInvite@.
data GKInvite

instance IsObjCObject (Id GKInvite) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKInvite"

class IsNSObject a => IsGKInvite a where
  toGKInvite :: a -> Id GKInvite

instance IsGKInvite (Id GKInvite) where
  toGKInvite = unsafeCastId

instance IsNSObject (Id GKInvite) where
  toNSObject = unsafeCastId

-- ---------- GKLeaderboard ----------

-- | GKLeaderboard represents a single instance of a leaderboard for the current game. Leaderboards can be of the following types:      1. Classic - Traditional, non-expiring leaderboards      2. Recurring - Periodic timed leaderboards that follow a recurrence rule defined in App Store Connect.
-- 
-- Phantom type for @GKLeaderboard@.
data GKLeaderboard

instance IsObjCObject (Id GKLeaderboard) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKLeaderboard"

class IsNSObject a => IsGKLeaderboard a where
  toGKLeaderboard :: a -> Id GKLeaderboard

instance IsGKLeaderboard (Id GKLeaderboard) where
  toGKLeaderboard = unsafeCastId

instance IsNSObject (Id GKLeaderboard) where
  toNSObject = unsafeCastId

-- ---------- GKLeaderboardEntry ----------

-- | Phantom type for @GKLeaderboardEntry@.
data GKLeaderboardEntry

instance IsObjCObject (Id GKLeaderboardEntry) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKLeaderboardEntry"

class IsNSObject a => IsGKLeaderboardEntry a where
  toGKLeaderboardEntry :: a -> Id GKLeaderboardEntry

instance IsGKLeaderboardEntry (Id GKLeaderboardEntry) where
  toGKLeaderboardEntry = unsafeCastId

instance IsNSObject (Id GKLeaderboardEntry) where
  toNSObject = unsafeCastId

-- ---------- GKLeaderboardScore ----------

-- | A @GKLeaderboardScore@ object represents a score on a leaderboard for scores you report for challenges or turn-based games.
-- 
-- Phantom type for @GKLeaderboardScore@.
data GKLeaderboardScore

instance IsObjCObject (Id GKLeaderboardScore) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKLeaderboardScore"

class IsNSObject a => IsGKLeaderboardScore a where
  toGKLeaderboardScore :: a -> Id GKLeaderboardScore

instance IsGKLeaderboardScore (Id GKLeaderboardScore) where
  toGKLeaderboardScore = unsafeCastId

instance IsNSObject (Id GKLeaderboardScore) where
  toNSObject = unsafeCastId

-- ---------- GKLeaderboardSet ----------

-- | GKLeaderboardSet represents the sets that leaderboards can be broken out into.
-- 
-- Phantom type for @GKLeaderboardSet@.
data GKLeaderboardSet

instance IsObjCObject (Id GKLeaderboardSet) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKLeaderboardSet"

class IsNSObject a => IsGKLeaderboardSet a where
  toGKLeaderboardSet :: a -> Id GKLeaderboardSet

instance IsGKLeaderboardSet (Id GKLeaderboardSet) where
  toGKLeaderboardSet = unsafeCastId

instance IsNSObject (Id GKLeaderboardSet) where
  toNSObject = unsafeCastId

-- ---------- GKMatch ----------

-- | GKMatch represents an active networking sessions between players. It handles network communications and can report player connection status. All matches are created by a GKMatchmaker.
-- 
-- Phantom type for @GKMatch@.
data GKMatch

instance IsObjCObject (Id GKMatch) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKMatch"

class IsNSObject a => IsGKMatch a where
  toGKMatch :: a -> Id GKMatch

instance IsGKMatch (Id GKMatch) where
  toGKMatch = unsafeCastId

instance IsNSObject (Id GKMatch) where
  toNSObject = unsafeCastId

-- ---------- GKMatchRequest ----------

-- | GKMatchRequest represents the parameters needed to create the match.
-- 
-- Phantom type for @GKMatchRequest@.
data GKMatchRequest

instance IsObjCObject (Id GKMatchRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKMatchRequest"

class IsNSObject a => IsGKMatchRequest a where
  toGKMatchRequest :: a -> Id GKMatchRequest

instance IsGKMatchRequest (Id GKMatchRequest) where
  toGKMatchRequest = unsafeCastId

instance IsNSObject (Id GKMatchRequest) where
  toNSObject = unsafeCastId

-- ---------- GKMatchedPlayers ----------

-- | Phantom type for @GKMatchedPlayers@.
data GKMatchedPlayers

instance IsObjCObject (Id GKMatchedPlayers) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKMatchedPlayers"

class IsNSObject a => IsGKMatchedPlayers a where
  toGKMatchedPlayers :: a -> Id GKMatchedPlayers

instance IsGKMatchedPlayers (Id GKMatchedPlayers) where
  toGKMatchedPlayers = unsafeCastId

instance IsNSObject (Id GKMatchedPlayers) where
  toNSObject = unsafeCastId

-- ---------- GKMatchmaker ----------

-- | GKMatchmaker is a singleton object to manage match creation from invites and automatching.
-- 
-- Phantom type for @GKMatchmaker@.
data GKMatchmaker

instance IsObjCObject (Id GKMatchmaker) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKMatchmaker"

class IsNSObject a => IsGKMatchmaker a where
  toGKMatchmaker :: a -> Id GKMatchmaker

instance IsGKMatchmaker (Id GKMatchmaker) where
  toGKMatchmaker = unsafeCastId

instance IsNSObject (Id GKMatchmaker) where
  toNSObject = unsafeCastId

-- ---------- GKNotificationBanner ----------

-- | Asynchronously shows a notification banner like the one used for Game Center’s “Welcome Back” message. If a banner is already being displayed, additional banners will be shown in sequence. Use this to notify the user of game events, high scores, completed achievements, etc.
-- 
-- Phantom type for @GKNotificationBanner@.
data GKNotificationBanner

instance IsObjCObject (Id GKNotificationBanner) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKNotificationBanner"

class IsNSObject a => IsGKNotificationBanner a where
  toGKNotificationBanner :: a -> Id GKNotificationBanner

instance IsGKNotificationBanner (Id GKNotificationBanner) where
  toGKNotificationBanner = unsafeCastId

instance IsNSObject (Id GKNotificationBanner) where
  toNSObject = unsafeCastId

-- ---------- GKSavedGame ----------

-- | Class representing a saved game for the local player, or a version of a saved game when in conflict
-- 
-- Phantom type for @GKSavedGame@.
data GKSavedGame

instance IsObjCObject (Id GKSavedGame) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKSavedGame"

class IsNSObject a => IsGKSavedGame a where
  toGKSavedGame :: a -> Id GKSavedGame

instance IsGKSavedGame (Id GKSavedGame) where
  toGKSavedGame = unsafeCastId

instance IsNSObject (Id GKSavedGame) where
  toNSObject = unsafeCastId

-- ---------- GKScore ----------

-- | GKScore represents a score in the leaderboards.
-- 
-- Phantom type for @GKScore@.
data GKScore

instance IsObjCObject (Id GKScore) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKScore"

class IsNSObject a => IsGKScore a where
  toGKScore :: a -> Id GKScore

instance IsGKScore (Id GKScore) where
  toGKScore = unsafeCastId

instance IsNSObject (Id GKScore) where
  toNSObject = unsafeCastId

-- ---------- GKSession ----------

-- | The GKSession handles networking between peers for a game, which includes establishing and maintaining connections over a game network, and network data transport.
--
-- This a not a Game Center feature. To support Game Center and online play, see GKMatch.
-- 
-- Phantom type for @GKSession@.
data GKSession

instance IsObjCObject (Id GKSession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKSession"

class IsNSObject a => IsGKSession a where
  toGKSession :: a -> Id GKSession

instance IsGKSession (Id GKSession) where
  toGKSession = unsafeCastId

instance IsNSObject (Id GKSession) where
  toNSObject = unsafeCastId

-- ---------- GKTurnBasedEventHandler ----------

-- | Phantom type for @GKTurnBasedEventHandler@.
data GKTurnBasedEventHandler

instance IsObjCObject (Id GKTurnBasedEventHandler) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKTurnBasedEventHandler"

class IsNSObject a => IsGKTurnBasedEventHandler a where
  toGKTurnBasedEventHandler :: a -> Id GKTurnBasedEventHandler

instance IsGKTurnBasedEventHandler (Id GKTurnBasedEventHandler) where
  toGKTurnBasedEventHandler = unsafeCastId

instance IsNSObject (Id GKTurnBasedEventHandler) where
  toNSObject = unsafeCastId

-- ---------- GKTurnBasedExchange ----------

-- | Phantom type for @GKTurnBasedExchange@.
data GKTurnBasedExchange

instance IsObjCObject (Id GKTurnBasedExchange) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKTurnBasedExchange"

class IsNSObject a => IsGKTurnBasedExchange a where
  toGKTurnBasedExchange :: a -> Id GKTurnBasedExchange

instance IsGKTurnBasedExchange (Id GKTurnBasedExchange) where
  toGKTurnBasedExchange = unsafeCastId

instance IsNSObject (Id GKTurnBasedExchange) where
  toNSObject = unsafeCastId

-- ---------- GKTurnBasedExchangeReply ----------

-- | Phantom type for @GKTurnBasedExchangeReply@.
data GKTurnBasedExchangeReply

instance IsObjCObject (Id GKTurnBasedExchangeReply) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKTurnBasedExchangeReply"

class IsNSObject a => IsGKTurnBasedExchangeReply a where
  toGKTurnBasedExchangeReply :: a -> Id GKTurnBasedExchangeReply

instance IsGKTurnBasedExchangeReply (Id GKTurnBasedExchangeReply) where
  toGKTurnBasedExchangeReply = unsafeCastId

instance IsNSObject (Id GKTurnBasedExchangeReply) where
  toNSObject = unsafeCastId

-- ---------- GKTurnBasedMatch ----------

-- | Phantom type for @GKTurnBasedMatch@.
data GKTurnBasedMatch

instance IsObjCObject (Id GKTurnBasedMatch) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKTurnBasedMatch"

class IsNSObject a => IsGKTurnBasedMatch a where
  toGKTurnBasedMatch :: a -> Id GKTurnBasedMatch

instance IsGKTurnBasedMatch (Id GKTurnBasedMatch) where
  toGKTurnBasedMatch = unsafeCastId

instance IsNSObject (Id GKTurnBasedMatch) where
  toNSObject = unsafeCastId

-- ---------- GKTurnBasedParticipant ----------

-- | GKTurnBasedMatch represents an ongoing turn-based game among the matched group of participants Existing matches can be shown and new matches created using GKTurnBasedMatchmakerViewController A list of existing matches can be retrieved using +loadMatchesWithCompletionHandler:
--
-- By default turn based events will badge your app.  To opt out of this add GKGameCenterBadgingDisabled  with a boolean value of YES to your info plist
-- 
-- Phantom type for @GKTurnBasedParticipant@.
data GKTurnBasedParticipant

instance IsObjCObject (Id GKTurnBasedParticipant) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKTurnBasedParticipant"

class IsNSObject a => IsGKTurnBasedParticipant a where
  toGKTurnBasedParticipant :: a -> Id GKTurnBasedParticipant

instance IsGKTurnBasedParticipant (Id GKTurnBasedParticipant) where
  toGKTurnBasedParticipant = unsafeCastId

instance IsNSObject (Id GKTurnBasedParticipant) where
  toNSObject = unsafeCastId

-- ---------- GKVoiceChat ----------

-- | GKVoiceChat represents an instance of a named voice communications channel
-- 
-- Phantom type for @GKVoiceChat@.
data GKVoiceChat

instance IsObjCObject (Id GKVoiceChat) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKVoiceChat"

class IsNSObject a => IsGKVoiceChat a where
  toGKVoiceChat :: a -> Id GKVoiceChat

instance IsGKVoiceChat (Id GKVoiceChat) where
  toGKVoiceChat = unsafeCastId

instance IsNSObject (Id GKVoiceChat) where
  toNSObject = unsafeCastId

-- ---------- GKVoiceChatService ----------

-- | This a not a Game Center feature. To support voice chat as part of Game Center online play, see GKVoiceChat.
-- 
-- Phantom type for @GKVoiceChatService@.
data GKVoiceChatService

instance IsObjCObject (Id GKVoiceChatService) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKVoiceChatService"

class IsNSObject a => IsGKVoiceChatService a where
  toGKVoiceChatService :: a -> Id GKVoiceChatService

instance IsGKVoiceChatService (Id GKVoiceChatService) where
  toGKVoiceChatService = unsafeCastId

instance IsNSObject (Id GKVoiceChatService) where
  toNSObject = unsafeCastId

-- ---------- GKCloudPlayer ----------

-- | Phantom type for @GKCloudPlayer@.
data GKCloudPlayer

instance IsObjCObject (Id GKCloudPlayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKCloudPlayer"

class IsGKBasePlayer a => IsGKCloudPlayer a where
  toGKCloudPlayer :: a -> Id GKCloudPlayer

instance IsGKCloudPlayer (Id GKCloudPlayer) where
  toGKCloudPlayer = unsafeCastId

instance IsGKBasePlayer (Id GKCloudPlayer) where
  toGKBasePlayer = unsafeCastId

instance IsNSObject (Id GKCloudPlayer) where
  toNSObject = unsafeCastId

-- ---------- GKPlayer ----------

-- | Phantom type for @GKPlayer@.
data GKPlayer

instance IsObjCObject (Id GKPlayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKPlayer"

class IsGKBasePlayer a => IsGKPlayer a where
  toGKPlayer :: a -> Id GKPlayer

instance IsGKPlayer (Id GKPlayer) where
  toGKPlayer = unsafeCastId

instance IsGKBasePlayer (Id GKPlayer) where
  toGKBasePlayer = unsafeCastId

instance IsNSObject (Id GKPlayer) where
  toNSObject = unsafeCastId

-- ---------- GKAchievementChallenge ----------

-- | Phantom type for @GKAchievementChallenge@.
data GKAchievementChallenge

instance IsObjCObject (Id GKAchievementChallenge) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKAchievementChallenge"

class IsGKChallenge a => IsGKAchievementChallenge a where
  toGKAchievementChallenge :: a -> Id GKAchievementChallenge

instance IsGKAchievementChallenge (Id GKAchievementChallenge) where
  toGKAchievementChallenge = unsafeCastId

instance IsGKChallenge (Id GKAchievementChallenge) where
  toGKChallenge = unsafeCastId

instance IsNSObject (Id GKAchievementChallenge) where
  toNSObject = unsafeCastId

-- ---------- GKScoreChallenge ----------

-- | Phantom type for @GKScoreChallenge@.
data GKScoreChallenge

instance IsObjCObject (Id GKScoreChallenge) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKScoreChallenge"

class IsGKChallenge a => IsGKScoreChallenge a where
  toGKScoreChallenge :: a -> Id GKScoreChallenge

instance IsGKScoreChallenge (Id GKScoreChallenge) where
  toGKScoreChallenge = unsafeCastId

instance IsGKChallenge (Id GKScoreChallenge) where
  toGKChallenge = unsafeCastId

instance IsNSObject (Id GKScoreChallenge) where
  toNSObject = unsafeCastId

-- ---------- GKDialogController ----------

-- | Phantom type for @GKDialogController@.
data GKDialogController

instance IsObjCObject (Id GKDialogController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKDialogController"

class IsNSResponder a => IsGKDialogController a where
  toGKDialogController :: a -> Id GKDialogController

instance IsGKDialogController (Id GKDialogController) where
  toGKDialogController = unsafeCastId

instance IsNSObject (Id GKDialogController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id GKDialogController) where
  toNSResponder = unsafeCastId

-- ---------- GKLocalPlayer ----------

-- | Phantom type for @GKLocalPlayer@.
data GKLocalPlayer

instance IsObjCObject (Id GKLocalPlayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKLocalPlayer"

class IsGKPlayer a => IsGKLocalPlayer a where
  toGKLocalPlayer :: a -> Id GKLocalPlayer

instance IsGKLocalPlayer (Id GKLocalPlayer) where
  toGKLocalPlayer = unsafeCastId

instance IsGKBasePlayer (Id GKLocalPlayer) where
  toGKBasePlayer = unsafeCastId

instance IsGKPlayer (Id GKLocalPlayer) where
  toGKPlayer = unsafeCastId

instance IsNSObject (Id GKLocalPlayer) where
  toNSObject = unsafeCastId

-- ---------- GKChallengesViewController ----------

-- | Phantom type for @GKChallengesViewController@.
data GKChallengesViewController

instance IsObjCObject (Id GKChallengesViewController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKChallengesViewController"

class IsNSViewController a => IsGKChallengesViewController a where
  toGKChallengesViewController :: a -> Id GKChallengesViewController

instance IsGKChallengesViewController (Id GKChallengesViewController) where
  toGKChallengesViewController = unsafeCastId

instance IsNSObject (Id GKChallengesViewController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id GKChallengesViewController) where
  toNSResponder = unsafeCastId

instance IsNSViewController (Id GKChallengesViewController) where
  toNSViewController = unsafeCastId

-- ---------- GKFriendRequestComposeViewController ----------

-- | Standard view controller for sending friend requests to other players. Present modally from the top view controller.
-- 
-- Phantom type for @GKFriendRequestComposeViewController@.
data GKFriendRequestComposeViewController

instance IsObjCObject (Id GKFriendRequestComposeViewController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKFriendRequestComposeViewController"

class IsNSViewController a => IsGKFriendRequestComposeViewController a where
  toGKFriendRequestComposeViewController :: a -> Id GKFriendRequestComposeViewController

instance IsGKFriendRequestComposeViewController (Id GKFriendRequestComposeViewController) where
  toGKFriendRequestComposeViewController = unsafeCastId

instance IsNSObject (Id GKFriendRequestComposeViewController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id GKFriendRequestComposeViewController) where
  toNSResponder = unsafeCastId

instance IsNSViewController (Id GKFriendRequestComposeViewController) where
  toNSViewController = unsafeCastId

-- ---------- GKGameCenterViewController ----------

-- | View controller that provides the standard user interface for leaderboards, achievements, and challenges. Present modally from the top view controller.
-- 
-- Phantom type for @GKGameCenterViewController@.
data GKGameCenterViewController

instance IsObjCObject (Id GKGameCenterViewController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKGameCenterViewController"

class IsNSViewController a => IsGKGameCenterViewController a where
  toGKGameCenterViewController :: a -> Id GKGameCenterViewController

instance IsGKGameCenterViewController (Id GKGameCenterViewController) where
  toGKGameCenterViewController = unsafeCastId

instance IsNSObject (Id GKGameCenterViewController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id GKGameCenterViewController) where
  toNSResponder = unsafeCastId

instance IsNSViewController (Id GKGameCenterViewController) where
  toNSViewController = unsafeCastId

-- ---------- GKMatchmakerViewController ----------

-- | Phantom type for @GKMatchmakerViewController@.
data GKMatchmakerViewController

instance IsObjCObject (Id GKMatchmakerViewController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKMatchmakerViewController"

class IsNSViewController a => IsGKMatchmakerViewController a where
  toGKMatchmakerViewController :: a -> Id GKMatchmakerViewController

instance IsGKMatchmakerViewController (Id GKMatchmakerViewController) where
  toGKMatchmakerViewController = unsafeCastId

instance IsNSObject (Id GKMatchmakerViewController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id GKMatchmakerViewController) where
  toNSResponder = unsafeCastId

instance IsNSViewController (Id GKMatchmakerViewController) where
  toNSViewController = unsafeCastId

-- ---------- GKTurnBasedMatchmakerViewController ----------

-- | View controller to manage turn-based matches, invite friends and perform automatching. Present modally from the top view controller.
-- 
-- Phantom type for @GKTurnBasedMatchmakerViewController@.
data GKTurnBasedMatchmakerViewController

instance IsObjCObject (Id GKTurnBasedMatchmakerViewController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKTurnBasedMatchmakerViewController"

class IsNSViewController a => IsGKTurnBasedMatchmakerViewController a where
  toGKTurnBasedMatchmakerViewController :: a -> Id GKTurnBasedMatchmakerViewController

instance IsGKTurnBasedMatchmakerViewController (Id GKTurnBasedMatchmakerViewController) where
  toGKTurnBasedMatchmakerViewController = unsafeCastId

instance IsNSObject (Id GKTurnBasedMatchmakerViewController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id GKTurnBasedMatchmakerViewController) where
  toNSResponder = unsafeCastId

instance IsNSViewController (Id GKTurnBasedMatchmakerViewController) where
  toNSViewController = unsafeCastId

-- ---------- GKAchievementViewController ----------

-- | View controller that provides the standard user interface for achievements. Present modally from the top view controller.
-- 
-- Phantom type for @GKAchievementViewController@.
data GKAchievementViewController

instance IsObjCObject (Id GKAchievementViewController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKAchievementViewController"

class IsGKGameCenterViewController a => IsGKAchievementViewController a where
  toGKAchievementViewController :: a -> Id GKAchievementViewController

instance IsGKAchievementViewController (Id GKAchievementViewController) where
  toGKAchievementViewController = unsafeCastId

instance IsGKGameCenterViewController (Id GKAchievementViewController) where
  toGKGameCenterViewController = unsafeCastId

instance IsNSObject (Id GKAchievementViewController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id GKAchievementViewController) where
  toNSResponder = unsafeCastId

instance IsNSViewController (Id GKAchievementViewController) where
  toNSViewController = unsafeCastId

-- ---------- GKLeaderboardViewController ----------

-- | View controller that provides the standard user interface for leaderboards.  Present modally from the top view controller.
-- 
-- Phantom type for @GKLeaderboardViewController@.
data GKLeaderboardViewController

instance IsObjCObject (Id GKLeaderboardViewController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKLeaderboardViewController"

class IsGKGameCenterViewController a => IsGKLeaderboardViewController a where
  toGKLeaderboardViewController :: a -> Id GKLeaderboardViewController

instance IsGKLeaderboardViewController (Id GKLeaderboardViewController) where
  toGKLeaderboardViewController = unsafeCastId

instance IsGKGameCenterViewController (Id GKLeaderboardViewController) where
  toGKGameCenterViewController = unsafeCastId

instance IsNSObject (Id GKLeaderboardViewController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id GKLeaderboardViewController) where
  toNSResponder = unsafeCastId

instance IsNSViewController (Id GKLeaderboardViewController) where
  toNSViewController = unsafeCastId
