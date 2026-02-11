{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | GKMatchRequest represents the parameters needed to create the match.
--
-- Generated bindings for @GKMatchRequest@.
module ObjC.GameKit.GKMatchRequest
  ( GKMatchRequest
  , IsGKMatchRequest(..)
  , maxPlayersAllowedForMatchOfType
  , minPlayers
  , setMinPlayers
  , maxPlayers
  , setMaxPlayers
  , playerGroup
  , setPlayerGroup
  , playerAttributes
  , setPlayerAttributes
  , recipients
  , setRecipients
  , inviteMessage
  , setInviteMessage
  , defaultNumberOfPlayers
  , setDefaultNumberOfPlayers
  , restrictToAutomatch
  , setRestrictToAutomatch
  , recipientResponseHandler
  , setRecipientResponseHandler
  , inviteeResponseHandler
  , setInviteeResponseHandler
  , playersToInvite
  , setPlayersToInvite
  , queueName
  , setQueueName
  , properties
  , setProperties
  , recipientProperties
  , setRecipientProperties
  , maxPlayersAllowedForMatchOfTypeSelector
  , minPlayersSelector
  , setMinPlayersSelector
  , maxPlayersSelector
  , setMaxPlayersSelector
  , playerGroupSelector
  , setPlayerGroupSelector
  , playerAttributesSelector
  , setPlayerAttributesSelector
  , recipientsSelector
  , setRecipientsSelector
  , inviteMessageSelector
  , setInviteMessageSelector
  , defaultNumberOfPlayersSelector
  , setDefaultNumberOfPlayersSelector
  , restrictToAutomatchSelector
  , setRestrictToAutomatchSelector
  , recipientResponseHandlerSelector
  , setRecipientResponseHandlerSelector
  , inviteeResponseHandlerSelector
  , setInviteeResponseHandlerSelector
  , playersToInviteSelector
  , setPlayersToInviteSelector
  , queueNameSelector
  , setQueueNameSelector
  , propertiesSelector
  , setPropertiesSelector
  , recipientPropertiesSelector
  , setRecipientPropertiesSelector

  -- * Enum types
  , GKMatchType(GKMatchType)
  , pattern GKMatchTypePeerToPeer
  , pattern GKMatchTypeHosted
  , pattern GKMatchTypeTurnBased

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

-- | To determine the maximum allowed players for each type of match supported.
--
-- ObjC selector: @+ maxPlayersAllowedForMatchOfType:@
maxPlayersAllowedForMatchOfType :: GKMatchType -> IO CULong
maxPlayersAllowedForMatchOfType matchType =
  do
    cls' <- getRequiredClass "GKMatchRequest"
    sendClassMsg cls' (mkSelector "maxPlayersAllowedForMatchOfType:") retCULong [argCULong (coerce matchType)]

-- | Minimum number of players for the match
--
-- ObjC selector: @- minPlayers@
minPlayers :: IsGKMatchRequest gkMatchRequest => gkMatchRequest -> IO CULong
minPlayers gkMatchRequest  =
    sendMsg gkMatchRequest (mkSelector "minPlayers") retCULong []

-- | Minimum number of players for the match
--
-- ObjC selector: @- setMinPlayers:@
setMinPlayers :: IsGKMatchRequest gkMatchRequest => gkMatchRequest -> CULong -> IO ()
setMinPlayers gkMatchRequest  value =
    sendMsg gkMatchRequest (mkSelector "setMinPlayers:") retVoid [argCULong value]

-- | Maximum number of players for the match
--
-- ObjC selector: @- maxPlayers@
maxPlayers :: IsGKMatchRequest gkMatchRequest => gkMatchRequest -> IO CULong
maxPlayers gkMatchRequest  =
    sendMsg gkMatchRequest (mkSelector "maxPlayers") retCULong []

-- | Maximum number of players for the match
--
-- ObjC selector: @- setMaxPlayers:@
setMaxPlayers :: IsGKMatchRequest gkMatchRequest => gkMatchRequest -> CULong -> IO ()
setMaxPlayers gkMatchRequest  value =
    sendMsg gkMatchRequest (mkSelector "setMaxPlayers:") retVoid [argCULong value]

-- | The player group identifier. Matchmaking will only take place between players in the same group.
--
-- ObjC selector: @- playerGroup@
playerGroup :: IsGKMatchRequest gkMatchRequest => gkMatchRequest -> IO CULong
playerGroup gkMatchRequest  =
    sendMsg gkMatchRequest (mkSelector "playerGroup") retCULong []

-- | The player group identifier. Matchmaking will only take place between players in the same group.
--
-- ObjC selector: @- setPlayerGroup:@
setPlayerGroup :: IsGKMatchRequest gkMatchRequest => gkMatchRequest -> CULong -> IO ()
setPlayerGroup gkMatchRequest  value =
    sendMsg gkMatchRequest (mkSelector "setPlayerGroup:") retVoid [argCULong value]

-- | optional mask that specifies the role that the local player would like to play in the game.  If this value is 0, it will be set to 0xFFFFFFFF (the default), and this property will be ignored. If the value is nonzero, then automatching uses the value as a mask that restricts the role the player can play in the group. Automatching with player attributes matches new players into the game so that the bitwise OR of the masks of all the players in the resulting match equals 0xFFFFFFFF.
--
-- ObjC selector: @- playerAttributes@
playerAttributes :: IsGKMatchRequest gkMatchRequest => gkMatchRequest -> IO CUInt
playerAttributes gkMatchRequest  =
    sendMsg gkMatchRequest (mkSelector "playerAttributes") retCUInt []

-- | optional mask that specifies the role that the local player would like to play in the game.  If this value is 0, it will be set to 0xFFFFFFFF (the default), and this property will be ignored. If the value is nonzero, then automatching uses the value as a mask that restricts the role the player can play in the group. Automatching with player attributes matches new players into the game so that the bitwise OR of the masks of all the players in the resulting match equals 0xFFFFFFFF.
--
-- ObjC selector: @- setPlayerAttributes:@
setPlayerAttributes :: IsGKMatchRequest gkMatchRequest => gkMatchRequest -> CUInt -> IO ()
setPlayerAttributes gkMatchRequest  value =
    sendMsg gkMatchRequest (mkSelector "setPlayerAttributes:") retVoid [argCUInt value]

-- | Array of GKPlayers to invite, or nil if none. This array can also include local guest players.
--
-- ObjC selector: @- recipients@
recipients :: IsGKMatchRequest gkMatchRequest => gkMatchRequest -> IO (Id NSArray)
recipients gkMatchRequest  =
    sendMsg gkMatchRequest (mkSelector "recipients") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Array of GKPlayers to invite, or nil if none. This array can also include local guest players.
--
-- ObjC selector: @- setRecipients:@
setRecipients :: (IsGKMatchRequest gkMatchRequest, IsNSArray value) => gkMatchRequest -> value -> IO ()
setRecipients gkMatchRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg gkMatchRequest (mkSelector "setRecipients:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Message sent to invited players, may be modified if using GKMatchmakerViewController Will return nil if the player is underage or restricted.
--
-- ObjC selector: @- inviteMessage@
inviteMessage :: IsGKMatchRequest gkMatchRequest => gkMatchRequest -> IO (Id NSString)
inviteMessage gkMatchRequest  =
    sendMsg gkMatchRequest (mkSelector "inviteMessage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Message sent to invited players, may be modified if using GKMatchmakerViewController Will return nil if the player is underage or restricted.
--
-- ObjC selector: @- setInviteMessage:@
setInviteMessage :: (IsGKMatchRequest gkMatchRequest, IsNSString value) => gkMatchRequest -> value -> IO ()
setInviteMessage gkMatchRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg gkMatchRequest (mkSelector "setInviteMessage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Default number of players to use during matchmaking. If not set we will default to the number that the player previously set for this game, or maxPlayers.
--
-- ObjC selector: @- defaultNumberOfPlayers@
defaultNumberOfPlayers :: IsGKMatchRequest gkMatchRequest => gkMatchRequest -> IO CULong
defaultNumberOfPlayers gkMatchRequest  =
    sendMsg gkMatchRequest (mkSelector "defaultNumberOfPlayers") retCULong []

-- | Default number of players to use during matchmaking. If not set we will default to the number that the player previously set for this game, or maxPlayers.
--
-- ObjC selector: @- setDefaultNumberOfPlayers:@
setDefaultNumberOfPlayers :: IsGKMatchRequest gkMatchRequest => gkMatchRequest -> CULong -> IO ()
setDefaultNumberOfPlayers gkMatchRequest  value =
    sendMsg gkMatchRequest (mkSelector "setDefaultNumberOfPlayers:") retVoid [argCULong value]

-- | Whether or not a match will be created only using automatch.  If YES, then a player will not be able to invite anyone (including contacts, friends, and nearby players) to the match, but rely on automatching to find players for the match.  Default is NO.
--
-- ObjC selector: @- restrictToAutomatch@
restrictToAutomatch :: IsGKMatchRequest gkMatchRequest => gkMatchRequest -> IO Bool
restrictToAutomatch gkMatchRequest  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkMatchRequest (mkSelector "restrictToAutomatch") retCULong []

-- | Whether or not a match will be created only using automatch.  If YES, then a player will not be able to invite anyone (including contacts, friends, and nearby players) to the match, but rely on automatching to find players for the match.  Default is NO.
--
-- ObjC selector: @- setRestrictToAutomatch:@
setRestrictToAutomatch :: IsGKMatchRequest gkMatchRequest => gkMatchRequest -> Bool -> IO ()
setRestrictToAutomatch gkMatchRequest  value =
    sendMsg gkMatchRequest (mkSelector "setRestrictToAutomatch:") retVoid [argCULong (if value then 1 else 0)]

-- | An recipientResponseHandler can be set in order to receive responses from programmatically invited players.
--
-- ObjC selector: @- recipientResponseHandler@
recipientResponseHandler :: IsGKMatchRequest gkMatchRequest => gkMatchRequest -> IO (Ptr ())
recipientResponseHandler gkMatchRequest  =
    fmap castPtr $ sendMsg gkMatchRequest (mkSelector "recipientResponseHandler") (retPtr retVoid) []

-- | An recipientResponseHandler can be set in order to receive responses from programmatically invited players.
--
-- ObjC selector: @- setRecipientResponseHandler:@
setRecipientResponseHandler :: IsGKMatchRequest gkMatchRequest => gkMatchRequest -> Ptr () -> IO ()
setRecipientResponseHandler gkMatchRequest  value =
    sendMsg gkMatchRequest (mkSelector "setRecipientResponseHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | @- inviteeResponseHandler@
inviteeResponseHandler :: IsGKMatchRequest gkMatchRequest => gkMatchRequest -> IO (Ptr ())
inviteeResponseHandler gkMatchRequest  =
    fmap castPtr $ sendMsg gkMatchRequest (mkSelector "inviteeResponseHandler") (retPtr retVoid) []

-- | @- setInviteeResponseHandler:@
setInviteeResponseHandler :: IsGKMatchRequest gkMatchRequest => gkMatchRequest -> Ptr () -> IO ()
setInviteeResponseHandler gkMatchRequest  value =
    sendMsg gkMatchRequest (mkSelector "setInviteeResponseHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | @- playersToInvite@
playersToInvite :: IsGKMatchRequest gkMatchRequest => gkMatchRequest -> IO (Id NSArray)
playersToInvite gkMatchRequest  =
    sendMsg gkMatchRequest (mkSelector "playersToInvite") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPlayersToInvite:@
setPlayersToInvite :: (IsGKMatchRequest gkMatchRequest, IsNSArray value) => gkMatchRequest -> value -> IO ()
setPlayersToInvite gkMatchRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg gkMatchRequest (mkSelector "setPlayersToInvite:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The name of the queue, if rule-based matchmaking is used.
--
-- ObjC selector: @- queueName@
queueName :: IsGKMatchRequest gkMatchRequest => gkMatchRequest -> IO (Id NSString)
queueName gkMatchRequest  =
    sendMsg gkMatchRequest (mkSelector "queueName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The name of the queue, if rule-based matchmaking is used.
--
-- ObjC selector: @- setQueueName:@
setQueueName :: (IsGKMatchRequest gkMatchRequest, IsNSString value) => gkMatchRequest -> value -> IO ()
setQueueName gkMatchRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg gkMatchRequest (mkSelector "setQueueName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The match properties, if rule-based matchmaking is used.
--
-- ObjC selector: @- properties@
properties :: IsGKMatchRequest gkMatchRequest => gkMatchRequest -> IO RawId
properties gkMatchRequest  =
    fmap (RawId . castPtr) $ sendMsg gkMatchRequest (mkSelector "properties") (retPtr retVoid) []

-- | The match properties, if rule-based matchmaking is used.
--
-- ObjC selector: @- setProperties:@
setProperties :: IsGKMatchRequest gkMatchRequest => gkMatchRequest -> RawId -> IO ()
setProperties gkMatchRequest  value =
    sendMsg gkMatchRequest (mkSelector "setProperties:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | The recipient specific match properties, if rule-based matchmaking is used when inviting players.
--
-- ObjC selector: @- recipientProperties@
recipientProperties :: IsGKMatchRequest gkMatchRequest => gkMatchRequest -> IO (Id NSDictionary)
recipientProperties gkMatchRequest  =
    sendMsg gkMatchRequest (mkSelector "recipientProperties") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The recipient specific match properties, if rule-based matchmaking is used when inviting players.
--
-- ObjC selector: @- setRecipientProperties:@
setRecipientProperties :: (IsGKMatchRequest gkMatchRequest, IsNSDictionary value) => gkMatchRequest -> value -> IO ()
setRecipientProperties gkMatchRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg gkMatchRequest (mkSelector "setRecipientProperties:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @maxPlayersAllowedForMatchOfType:@
maxPlayersAllowedForMatchOfTypeSelector :: Selector
maxPlayersAllowedForMatchOfTypeSelector = mkSelector "maxPlayersAllowedForMatchOfType:"

-- | @Selector@ for @minPlayers@
minPlayersSelector :: Selector
minPlayersSelector = mkSelector "minPlayers"

-- | @Selector@ for @setMinPlayers:@
setMinPlayersSelector :: Selector
setMinPlayersSelector = mkSelector "setMinPlayers:"

-- | @Selector@ for @maxPlayers@
maxPlayersSelector :: Selector
maxPlayersSelector = mkSelector "maxPlayers"

-- | @Selector@ for @setMaxPlayers:@
setMaxPlayersSelector :: Selector
setMaxPlayersSelector = mkSelector "setMaxPlayers:"

-- | @Selector@ for @playerGroup@
playerGroupSelector :: Selector
playerGroupSelector = mkSelector "playerGroup"

-- | @Selector@ for @setPlayerGroup:@
setPlayerGroupSelector :: Selector
setPlayerGroupSelector = mkSelector "setPlayerGroup:"

-- | @Selector@ for @playerAttributes@
playerAttributesSelector :: Selector
playerAttributesSelector = mkSelector "playerAttributes"

-- | @Selector@ for @setPlayerAttributes:@
setPlayerAttributesSelector :: Selector
setPlayerAttributesSelector = mkSelector "setPlayerAttributes:"

-- | @Selector@ for @recipients@
recipientsSelector :: Selector
recipientsSelector = mkSelector "recipients"

-- | @Selector@ for @setRecipients:@
setRecipientsSelector :: Selector
setRecipientsSelector = mkSelector "setRecipients:"

-- | @Selector@ for @inviteMessage@
inviteMessageSelector :: Selector
inviteMessageSelector = mkSelector "inviteMessage"

-- | @Selector@ for @setInviteMessage:@
setInviteMessageSelector :: Selector
setInviteMessageSelector = mkSelector "setInviteMessage:"

-- | @Selector@ for @defaultNumberOfPlayers@
defaultNumberOfPlayersSelector :: Selector
defaultNumberOfPlayersSelector = mkSelector "defaultNumberOfPlayers"

-- | @Selector@ for @setDefaultNumberOfPlayers:@
setDefaultNumberOfPlayersSelector :: Selector
setDefaultNumberOfPlayersSelector = mkSelector "setDefaultNumberOfPlayers:"

-- | @Selector@ for @restrictToAutomatch@
restrictToAutomatchSelector :: Selector
restrictToAutomatchSelector = mkSelector "restrictToAutomatch"

-- | @Selector@ for @setRestrictToAutomatch:@
setRestrictToAutomatchSelector :: Selector
setRestrictToAutomatchSelector = mkSelector "setRestrictToAutomatch:"

-- | @Selector@ for @recipientResponseHandler@
recipientResponseHandlerSelector :: Selector
recipientResponseHandlerSelector = mkSelector "recipientResponseHandler"

-- | @Selector@ for @setRecipientResponseHandler:@
setRecipientResponseHandlerSelector :: Selector
setRecipientResponseHandlerSelector = mkSelector "setRecipientResponseHandler:"

-- | @Selector@ for @inviteeResponseHandler@
inviteeResponseHandlerSelector :: Selector
inviteeResponseHandlerSelector = mkSelector "inviteeResponseHandler"

-- | @Selector@ for @setInviteeResponseHandler:@
setInviteeResponseHandlerSelector :: Selector
setInviteeResponseHandlerSelector = mkSelector "setInviteeResponseHandler:"

-- | @Selector@ for @playersToInvite@
playersToInviteSelector :: Selector
playersToInviteSelector = mkSelector "playersToInvite"

-- | @Selector@ for @setPlayersToInvite:@
setPlayersToInviteSelector :: Selector
setPlayersToInviteSelector = mkSelector "setPlayersToInvite:"

-- | @Selector@ for @queueName@
queueNameSelector :: Selector
queueNameSelector = mkSelector "queueName"

-- | @Selector@ for @setQueueName:@
setQueueNameSelector :: Selector
setQueueNameSelector = mkSelector "setQueueName:"

-- | @Selector@ for @properties@
propertiesSelector :: Selector
propertiesSelector = mkSelector "properties"

-- | @Selector@ for @setProperties:@
setPropertiesSelector :: Selector
setPropertiesSelector = mkSelector "setProperties:"

-- | @Selector@ for @recipientProperties@
recipientPropertiesSelector :: Selector
recipientPropertiesSelector = mkSelector "recipientProperties"

-- | @Selector@ for @setRecipientProperties:@
setRecipientPropertiesSelector :: Selector
setRecipientPropertiesSelector = mkSelector "setRecipientProperties:"

