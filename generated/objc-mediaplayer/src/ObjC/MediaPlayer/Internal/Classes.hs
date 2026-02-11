{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.MediaPlayer.Internal.Classes (
    module ObjC.MediaPlayer.Internal.Classes,
    module ObjC.AVFoundation.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AVFoundation.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- MPAdTimeRange ----------

-- | Phantom type for @MPAdTimeRange@.
data MPAdTimeRange

instance IsObjCObject (Id MPAdTimeRange) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPAdTimeRange"

class IsNSObject a => IsMPAdTimeRange a where
  toMPAdTimeRange :: a -> Id MPAdTimeRange

instance IsMPAdTimeRange (Id MPAdTimeRange) where
  toMPAdTimeRange = unsafeCastId

instance IsNSObject (Id MPAdTimeRange) where
  toNSObject = unsafeCastId

-- ---------- MPContentItem ----------

-- | MPContentItem represents high-level metadata for a particular media item for representation outside the client application. Examples of media items that a developer might want to represent include song files, streaming audio URLs, or radio stations.
-- 
-- Phantom type for @MPContentItem@.
data MPContentItem

instance IsObjCObject (Id MPContentItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPContentItem"

class IsNSObject a => IsMPContentItem a where
  toMPContentItem :: a -> Id MPContentItem

instance IsMPContentItem (Id MPContentItem) where
  toMPContentItem = unsafeCastId

instance IsNSObject (Id MPContentItem) where
  toNSObject = unsafeCastId

-- ---------- MPMediaEntity ----------

-- | Phantom type for @MPMediaEntity@.
data MPMediaEntity

instance IsObjCObject (Id MPMediaEntity) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPMediaEntity"

class IsNSObject a => IsMPMediaEntity a where
  toMPMediaEntity :: a -> Id MPMediaEntity

instance IsMPMediaEntity (Id MPMediaEntity) where
  toMPMediaEntity = unsafeCastId

instance IsNSObject (Id MPMediaEntity) where
  toNSObject = unsafeCastId

-- ---------- MPMediaItemAnimatedArtwork ----------

-- | An animated image, such as an animated music album cover art, for a media item.
--
-- A single instance of animated artwork is comprised of two assets: an artwork video asset, and a preview image which should match the first frame of the artwork video. The preview image may be used when displaying the animated artwork whilst the video becomes available.
--
-- Both the preview image and artwork video can be fetched asynchronously and will only be requested when required at point of display. Aim to provide preview images as quickly as possible once requested, and ideally synchronously.
--
-- Video asset @URL@s you provide must be local file @URL@s. You should make the associated assets available locally before providing them via the relevant handler, for example by fetching the associated video asset over the network. The @URL@s should remain valid for the lifetime of the ``MPMediaItemAnimatedArtwork``, once provided.
--
-- ``MPMediaItemAnimatedArtwork`` should not be subclassed.
-- 
-- Phantom type for @MPMediaItemAnimatedArtwork@.
data MPMediaItemAnimatedArtwork

instance IsObjCObject (Id MPMediaItemAnimatedArtwork) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPMediaItemAnimatedArtwork"

class IsNSObject a => IsMPMediaItemAnimatedArtwork a where
  toMPMediaItemAnimatedArtwork :: a -> Id MPMediaItemAnimatedArtwork

instance IsMPMediaItemAnimatedArtwork (Id MPMediaItemAnimatedArtwork) where
  toMPMediaItemAnimatedArtwork = unsafeCastId

instance IsNSObject (Id MPMediaItemAnimatedArtwork) where
  toNSObject = unsafeCastId

-- ---------- MPMediaItemArtwork ----------

-- | Phantom type for @MPMediaItemArtwork@.
data MPMediaItemArtwork

instance IsObjCObject (Id MPMediaItemArtwork) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPMediaItemArtwork"

class IsNSObject a => IsMPMediaItemArtwork a where
  toMPMediaItemArtwork :: a -> Id MPMediaItemArtwork

instance IsMPMediaItemArtwork (Id MPMediaItemArtwork) where
  toMPMediaItemArtwork = unsafeCastId

instance IsNSObject (Id MPMediaItemArtwork) where
  toNSObject = unsafeCastId

-- ---------- MPMediaLibrary ----------

-- | Phantom type for @MPMediaLibrary@.
data MPMediaLibrary

instance IsObjCObject (Id MPMediaLibrary) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPMediaLibrary"

class IsNSObject a => IsMPMediaLibrary a where
  toMPMediaLibrary :: a -> Id MPMediaLibrary

instance IsMPMediaLibrary (Id MPMediaLibrary) where
  toMPMediaLibrary = unsafeCastId

instance IsNSObject (Id MPMediaLibrary) where
  toNSObject = unsafeCastId

-- ---------- MPMediaPlaylistCreationMetadata ----------

-- | Phantom type for @MPMediaPlaylistCreationMetadata@.
data MPMediaPlaylistCreationMetadata

instance IsObjCObject (Id MPMediaPlaylistCreationMetadata) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPMediaPlaylistCreationMetadata"

class IsNSObject a => IsMPMediaPlaylistCreationMetadata a where
  toMPMediaPlaylistCreationMetadata :: a -> Id MPMediaPlaylistCreationMetadata

instance IsMPMediaPlaylistCreationMetadata (Id MPMediaPlaylistCreationMetadata) where
  toMPMediaPlaylistCreationMetadata = unsafeCastId

instance IsNSObject (Id MPMediaPlaylistCreationMetadata) where
  toNSObject = unsafeCastId

-- ---------- MPMediaPredicate ----------

-- | Phantom type for @MPMediaPredicate@.
data MPMediaPredicate

instance IsObjCObject (Id MPMediaPredicate) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPMediaPredicate"

class IsNSObject a => IsMPMediaPredicate a where
  toMPMediaPredicate :: a -> Id MPMediaPredicate

instance IsMPMediaPredicate (Id MPMediaPredicate) where
  toMPMediaPredicate = unsafeCastId

instance IsNSObject (Id MPMediaPredicate) where
  toNSObject = unsafeCastId

-- ---------- MPMediaQuery ----------

-- | Phantom type for @MPMediaQuery@.
data MPMediaQuery

instance IsObjCObject (Id MPMediaQuery) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPMediaQuery"

class IsNSObject a => IsMPMediaQuery a where
  toMPMediaQuery :: a -> Id MPMediaQuery

instance IsMPMediaQuery (Id MPMediaQuery) where
  toMPMediaQuery = unsafeCastId

instance IsNSObject (Id MPMediaQuery) where
  toNSObject = unsafeCastId

-- ---------- MPMediaQuerySection ----------

-- | Phantom type for @MPMediaQuerySection@.
data MPMediaQuerySection

instance IsObjCObject (Id MPMediaQuerySection) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPMediaQuerySection"

class IsNSObject a => IsMPMediaQuerySection a where
  toMPMediaQuerySection :: a -> Id MPMediaQuerySection

instance IsMPMediaQuerySection (Id MPMediaQuerySection) where
  toMPMediaQuerySection = unsafeCastId

instance IsNSObject (Id MPMediaQuerySection) where
  toNSObject = unsafeCastId

-- ---------- MPMusicPlayerController ----------

-- | Phantom type for @MPMusicPlayerController@.
data MPMusicPlayerController

instance IsObjCObject (Id MPMusicPlayerController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPMusicPlayerController"

class IsNSObject a => IsMPMusicPlayerController a where
  toMPMusicPlayerController :: a -> Id MPMusicPlayerController

instance IsMPMusicPlayerController (Id MPMusicPlayerController) where
  toMPMusicPlayerController = unsafeCastId

instance IsNSObject (Id MPMusicPlayerController) where
  toNSObject = unsafeCastId

-- ---------- MPMusicPlayerControllerQueue ----------

-- | Phantom type for @MPMusicPlayerControllerQueue@.
data MPMusicPlayerControllerQueue

instance IsObjCObject (Id MPMusicPlayerControllerQueue) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPMusicPlayerControllerQueue"

class IsNSObject a => IsMPMusicPlayerControllerQueue a where
  toMPMusicPlayerControllerQueue :: a -> Id MPMusicPlayerControllerQueue

instance IsMPMusicPlayerControllerQueue (Id MPMusicPlayerControllerQueue) where
  toMPMusicPlayerControllerQueue = unsafeCastId

instance IsNSObject (Id MPMusicPlayerControllerQueue) where
  toNSObject = unsafeCastId

-- ---------- MPMusicPlayerPlayParameters ----------

-- | Phantom type for @MPMusicPlayerPlayParameters@.
data MPMusicPlayerPlayParameters

instance IsObjCObject (Id MPMusicPlayerPlayParameters) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPMusicPlayerPlayParameters"

class IsNSObject a => IsMPMusicPlayerPlayParameters a where
  toMPMusicPlayerPlayParameters :: a -> Id MPMusicPlayerPlayParameters

instance IsMPMusicPlayerPlayParameters (Id MPMusicPlayerPlayParameters) where
  toMPMusicPlayerPlayParameters = unsafeCastId

instance IsNSObject (Id MPMusicPlayerPlayParameters) where
  toNSObject = unsafeCastId

-- ---------- MPMusicPlayerQueueDescriptor ----------

-- | Phantom type for @MPMusicPlayerQueueDescriptor@.
data MPMusicPlayerQueueDescriptor

instance IsObjCObject (Id MPMusicPlayerQueueDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPMusicPlayerQueueDescriptor"

class IsNSObject a => IsMPMusicPlayerQueueDescriptor a where
  toMPMusicPlayerQueueDescriptor :: a -> Id MPMusicPlayerQueueDescriptor

instance IsMPMusicPlayerQueueDescriptor (Id MPMusicPlayerQueueDescriptor) where
  toMPMusicPlayerQueueDescriptor = unsafeCastId

instance IsNSObject (Id MPMusicPlayerQueueDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPNowPlayingInfoCenter ----------

-- | Phantom type for @MPNowPlayingInfoCenter@.
data MPNowPlayingInfoCenter

instance IsObjCObject (Id MPNowPlayingInfoCenter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPNowPlayingInfoCenter"

class IsNSObject a => IsMPNowPlayingInfoCenter a where
  toMPNowPlayingInfoCenter :: a -> Id MPNowPlayingInfoCenter

instance IsMPNowPlayingInfoCenter (Id MPNowPlayingInfoCenter) where
  toMPNowPlayingInfoCenter = unsafeCastId

instance IsNSObject (Id MPNowPlayingInfoCenter) where
  toNSObject = unsafeCastId

-- ---------- MPNowPlayingInfoLanguageOption ----------

-- | Represents a single language option option.
-- 
-- Phantom type for @MPNowPlayingInfoLanguageOption@.
data MPNowPlayingInfoLanguageOption

instance IsObjCObject (Id MPNowPlayingInfoLanguageOption) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPNowPlayingInfoLanguageOption"

class IsNSObject a => IsMPNowPlayingInfoLanguageOption a where
  toMPNowPlayingInfoLanguageOption :: a -> Id MPNowPlayingInfoLanguageOption

instance IsMPNowPlayingInfoLanguageOption (Id MPNowPlayingInfoLanguageOption) where
  toMPNowPlayingInfoLanguageOption = unsafeCastId

instance IsNSObject (Id MPNowPlayingInfoLanguageOption) where
  toNSObject = unsafeCastId

-- ---------- MPNowPlayingInfoLanguageOptionGroup ----------

-- | Phantom type for @MPNowPlayingInfoLanguageOptionGroup@.
data MPNowPlayingInfoLanguageOptionGroup

instance IsObjCObject (Id MPNowPlayingInfoLanguageOptionGroup) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPNowPlayingInfoLanguageOptionGroup"

class IsNSObject a => IsMPNowPlayingInfoLanguageOptionGroup a where
  toMPNowPlayingInfoLanguageOptionGroup :: a -> Id MPNowPlayingInfoLanguageOptionGroup

instance IsMPNowPlayingInfoLanguageOptionGroup (Id MPNowPlayingInfoLanguageOptionGroup) where
  toMPNowPlayingInfoLanguageOptionGroup = unsafeCastId

instance IsNSObject (Id MPNowPlayingInfoLanguageOptionGroup) where
  toNSObject = unsafeCastId

-- ---------- MPNowPlayingSession ----------

-- | Phantom type for @MPNowPlayingSession@.
data MPNowPlayingSession

instance IsObjCObject (Id MPNowPlayingSession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPNowPlayingSession"

class IsNSObject a => IsMPNowPlayingSession a where
  toMPNowPlayingSession :: a -> Id MPNowPlayingSession

instance IsMPNowPlayingSession (Id MPNowPlayingSession) where
  toMPNowPlayingSession = unsafeCastId

instance IsNSObject (Id MPNowPlayingSession) where
  toNSObject = unsafeCastId

-- ---------- MPPlayableContentManager ----------

-- | MPPlayableContentManager is a class that manages the interactions between a media application and an external media player interface. The application provides the content manager with a data source, which allows the media player to browse the media content offered by the application, as well as a delegate, which allows the media player to relay non-media remote playback commands to the application.
-- 
-- Phantom type for @MPPlayableContentManager@.
data MPPlayableContentManager

instance IsObjCObject (Id MPPlayableContentManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPPlayableContentManager"

class IsNSObject a => IsMPPlayableContentManager a where
  toMPPlayableContentManager :: a -> Id MPPlayableContentManager

instance IsMPPlayableContentManager (Id MPPlayableContentManager) where
  toMPPlayableContentManager = unsafeCastId

instance IsNSObject (Id MPPlayableContentManager) where
  toNSObject = unsafeCastId

-- ---------- MPPlayableContentManagerContext ----------

-- | MPPlayableContentManagerContext represents the current state of the playable content endpoint. A context is retrievable from an instance of MPPlayableContentManager.
-- 
-- Phantom type for @MPPlayableContentManagerContext@.
data MPPlayableContentManagerContext

instance IsObjCObject (Id MPPlayableContentManagerContext) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPPlayableContentManagerContext"

class IsNSObject a => IsMPPlayableContentManagerContext a where
  toMPPlayableContentManagerContext :: a -> Id MPPlayableContentManagerContext

instance IsMPPlayableContentManagerContext (Id MPPlayableContentManagerContext) where
  toMPPlayableContentManagerContext = unsafeCastId

instance IsNSObject (Id MPPlayableContentManagerContext) where
  toNSObject = unsafeCastId

-- ---------- MPRemoteCommand ----------

-- | Phantom type for @MPRemoteCommand@.
data MPRemoteCommand

instance IsObjCObject (Id MPRemoteCommand) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPRemoteCommand"

class IsNSObject a => IsMPRemoteCommand a where
  toMPRemoteCommand :: a -> Id MPRemoteCommand

instance IsMPRemoteCommand (Id MPRemoteCommand) where
  toMPRemoteCommand = unsafeCastId

instance IsNSObject (Id MPRemoteCommand) where
  toNSObject = unsafeCastId

-- ---------- MPRemoteCommandCenter ----------

-- | Phantom type for @MPRemoteCommandCenter@.
data MPRemoteCommandCenter

instance IsObjCObject (Id MPRemoteCommandCenter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPRemoteCommandCenter"

class IsNSObject a => IsMPRemoteCommandCenter a where
  toMPRemoteCommandCenter :: a -> Id MPRemoteCommandCenter

instance IsMPRemoteCommandCenter (Id MPRemoteCommandCenter) where
  toMPRemoteCommandCenter = unsafeCastId

instance IsNSObject (Id MPRemoteCommandCenter) where
  toNSObject = unsafeCastId

-- ---------- MPRemoteCommandEvent ----------

-- | Phantom type for @MPRemoteCommandEvent@.
data MPRemoteCommandEvent

instance IsObjCObject (Id MPRemoteCommandEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPRemoteCommandEvent"

class IsNSObject a => IsMPRemoteCommandEvent a where
  toMPRemoteCommandEvent :: a -> Id MPRemoteCommandEvent

instance IsMPRemoteCommandEvent (Id MPRemoteCommandEvent) where
  toMPRemoteCommandEvent = unsafeCastId

instance IsNSObject (Id MPRemoteCommandEvent) where
  toNSObject = unsafeCastId

-- ---------- MPMediaItem ----------

-- | Phantom type for @MPMediaItem@.
data MPMediaItem

instance IsObjCObject (Id MPMediaItem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPMediaItem"

class IsMPMediaEntity a => IsMPMediaItem a where
  toMPMediaItem :: a -> Id MPMediaItem

instance IsMPMediaItem (Id MPMediaItem) where
  toMPMediaItem = unsafeCastId

instance IsMPMediaEntity (Id MPMediaItem) where
  toMPMediaEntity = unsafeCastId

instance IsNSObject (Id MPMediaItem) where
  toNSObject = unsafeCastId

-- ---------- MPMediaItemCollection ----------

-- | Phantom type for @MPMediaItemCollection@.
data MPMediaItemCollection

instance IsObjCObject (Id MPMediaItemCollection) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPMediaItemCollection"

class IsMPMediaEntity a => IsMPMediaItemCollection a where
  toMPMediaItemCollection :: a -> Id MPMediaItemCollection

instance IsMPMediaItemCollection (Id MPMediaItemCollection) where
  toMPMediaItemCollection = unsafeCastId

instance IsMPMediaEntity (Id MPMediaItemCollection) where
  toMPMediaEntity = unsafeCastId

instance IsNSObject (Id MPMediaItemCollection) where
  toNSObject = unsafeCastId

-- ---------- MPMediaPropertyPredicate ----------

-- | Phantom type for @MPMediaPropertyPredicate@.
data MPMediaPropertyPredicate

instance IsObjCObject (Id MPMediaPropertyPredicate) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPMediaPropertyPredicate"

class IsMPMediaPredicate a => IsMPMediaPropertyPredicate a where
  toMPMediaPropertyPredicate :: a -> Id MPMediaPropertyPredicate

instance IsMPMediaPropertyPredicate (Id MPMediaPropertyPredicate) where
  toMPMediaPropertyPredicate = unsafeCastId

instance IsMPMediaPredicate (Id MPMediaPropertyPredicate) where
  toMPMediaPredicate = unsafeCastId

instance IsNSObject (Id MPMediaPropertyPredicate) where
  toNSObject = unsafeCastId

-- ---------- MPMusicPlayerApplicationController ----------

-- | Phantom type for @MPMusicPlayerApplicationController@.
data MPMusicPlayerApplicationController

instance IsObjCObject (Id MPMusicPlayerApplicationController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPMusicPlayerApplicationController"

class IsMPMusicPlayerController a => IsMPMusicPlayerApplicationController a where
  toMPMusicPlayerApplicationController :: a -> Id MPMusicPlayerApplicationController

instance IsMPMusicPlayerApplicationController (Id MPMusicPlayerApplicationController) where
  toMPMusicPlayerApplicationController = unsafeCastId

instance IsMPMusicPlayerController (Id MPMusicPlayerApplicationController) where
  toMPMusicPlayerController = unsafeCastId

instance IsNSObject (Id MPMusicPlayerApplicationController) where
  toNSObject = unsafeCastId

-- ---------- MPMusicPlayerControllerMutableQueue ----------

-- | Phantom type for @MPMusicPlayerControllerMutableQueue@.
data MPMusicPlayerControllerMutableQueue

instance IsObjCObject (Id MPMusicPlayerControllerMutableQueue) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPMusicPlayerControllerMutableQueue"

class IsMPMusicPlayerControllerQueue a => IsMPMusicPlayerControllerMutableQueue a where
  toMPMusicPlayerControllerMutableQueue :: a -> Id MPMusicPlayerControllerMutableQueue

instance IsMPMusicPlayerControllerMutableQueue (Id MPMusicPlayerControllerMutableQueue) where
  toMPMusicPlayerControllerMutableQueue = unsafeCastId

instance IsMPMusicPlayerControllerQueue (Id MPMusicPlayerControllerMutableQueue) where
  toMPMusicPlayerControllerQueue = unsafeCastId

instance IsNSObject (Id MPMusicPlayerControllerMutableQueue) where
  toNSObject = unsafeCastId

-- ---------- MPMusicPlayerMediaItemQueueDescriptor ----------

-- | Phantom type for @MPMusicPlayerMediaItemQueueDescriptor@.
data MPMusicPlayerMediaItemQueueDescriptor

instance IsObjCObject (Id MPMusicPlayerMediaItemQueueDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPMusicPlayerMediaItemQueueDescriptor"

class IsMPMusicPlayerQueueDescriptor a => IsMPMusicPlayerMediaItemQueueDescriptor a where
  toMPMusicPlayerMediaItemQueueDescriptor :: a -> Id MPMusicPlayerMediaItemQueueDescriptor

instance IsMPMusicPlayerMediaItemQueueDescriptor (Id MPMusicPlayerMediaItemQueueDescriptor) where
  toMPMusicPlayerMediaItemQueueDescriptor = unsafeCastId

instance IsMPMusicPlayerQueueDescriptor (Id MPMusicPlayerMediaItemQueueDescriptor) where
  toMPMusicPlayerQueueDescriptor = unsafeCastId

instance IsNSObject (Id MPMusicPlayerMediaItemQueueDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPMusicPlayerPlayParametersQueueDescriptor ----------

-- | Phantom type for @MPMusicPlayerPlayParametersQueueDescriptor@.
data MPMusicPlayerPlayParametersQueueDescriptor

instance IsObjCObject (Id MPMusicPlayerPlayParametersQueueDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPMusicPlayerPlayParametersQueueDescriptor"

class IsMPMusicPlayerQueueDescriptor a => IsMPMusicPlayerPlayParametersQueueDescriptor a where
  toMPMusicPlayerPlayParametersQueueDescriptor :: a -> Id MPMusicPlayerPlayParametersQueueDescriptor

instance IsMPMusicPlayerPlayParametersQueueDescriptor (Id MPMusicPlayerPlayParametersQueueDescriptor) where
  toMPMusicPlayerPlayParametersQueueDescriptor = unsafeCastId

instance IsMPMusicPlayerQueueDescriptor (Id MPMusicPlayerPlayParametersQueueDescriptor) where
  toMPMusicPlayerQueueDescriptor = unsafeCastId

instance IsNSObject (Id MPMusicPlayerPlayParametersQueueDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPMusicPlayerStoreQueueDescriptor ----------

-- | Phantom type for @MPMusicPlayerStoreQueueDescriptor@.
data MPMusicPlayerStoreQueueDescriptor

instance IsObjCObject (Id MPMusicPlayerStoreQueueDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPMusicPlayerStoreQueueDescriptor"

class IsMPMusicPlayerQueueDescriptor a => IsMPMusicPlayerStoreQueueDescriptor a where
  toMPMusicPlayerStoreQueueDescriptor :: a -> Id MPMusicPlayerStoreQueueDescriptor

instance IsMPMusicPlayerStoreQueueDescriptor (Id MPMusicPlayerStoreQueueDescriptor) where
  toMPMusicPlayerStoreQueueDescriptor = unsafeCastId

instance IsMPMusicPlayerQueueDescriptor (Id MPMusicPlayerStoreQueueDescriptor) where
  toMPMusicPlayerQueueDescriptor = unsafeCastId

instance IsNSObject (Id MPMusicPlayerStoreQueueDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPChangePlaybackPositionCommand ----------

-- | Command for changing the current playback position in a now playing item. Sends out MPChangePlaybackPositionCommandEvents.
-- 
-- Phantom type for @MPChangePlaybackPositionCommand@.
data MPChangePlaybackPositionCommand

instance IsObjCObject (Id MPChangePlaybackPositionCommand) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPChangePlaybackPositionCommand"

class IsMPRemoteCommand a => IsMPChangePlaybackPositionCommand a where
  toMPChangePlaybackPositionCommand :: a -> Id MPChangePlaybackPositionCommand

instance IsMPChangePlaybackPositionCommand (Id MPChangePlaybackPositionCommand) where
  toMPChangePlaybackPositionCommand = unsafeCastId

instance IsMPRemoteCommand (Id MPChangePlaybackPositionCommand) where
  toMPRemoteCommand = unsafeCastId

instance IsNSObject (Id MPChangePlaybackPositionCommand) where
  toNSObject = unsafeCastId

-- ---------- MPChangePlaybackRateCommand ----------

-- | Phantom type for @MPChangePlaybackRateCommand@.
data MPChangePlaybackRateCommand

instance IsObjCObject (Id MPChangePlaybackRateCommand) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPChangePlaybackRateCommand"

class IsMPRemoteCommand a => IsMPChangePlaybackRateCommand a where
  toMPChangePlaybackRateCommand :: a -> Id MPChangePlaybackRateCommand

instance IsMPChangePlaybackRateCommand (Id MPChangePlaybackRateCommand) where
  toMPChangePlaybackRateCommand = unsafeCastId

instance IsMPRemoteCommand (Id MPChangePlaybackRateCommand) where
  toMPRemoteCommand = unsafeCastId

instance IsNSObject (Id MPChangePlaybackRateCommand) where
  toNSObject = unsafeCastId

-- ---------- MPChangeRepeatModeCommand ----------

-- | Command for changing the current repeat mode to use during playback. To update the system's current representation of your app's repeat mode, set the currentRepeatType property on this command to the proper repeat type value.
-- 
-- Phantom type for @MPChangeRepeatModeCommand@.
data MPChangeRepeatModeCommand

instance IsObjCObject (Id MPChangeRepeatModeCommand) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPChangeRepeatModeCommand"

class IsMPRemoteCommand a => IsMPChangeRepeatModeCommand a where
  toMPChangeRepeatModeCommand :: a -> Id MPChangeRepeatModeCommand

instance IsMPChangeRepeatModeCommand (Id MPChangeRepeatModeCommand) where
  toMPChangeRepeatModeCommand = unsafeCastId

instance IsMPRemoteCommand (Id MPChangeRepeatModeCommand) where
  toMPRemoteCommand = unsafeCastId

instance IsNSObject (Id MPChangeRepeatModeCommand) where
  toNSObject = unsafeCastId

-- ---------- MPChangeShuffleModeCommand ----------

-- | Command for changing the current shuffle mode to use during playback. To update the system's current representation of your app's shuffle mode, set the currentShuffleType property on this command to the proper shuffle type value.
-- 
-- Phantom type for @MPChangeShuffleModeCommand@.
data MPChangeShuffleModeCommand

instance IsObjCObject (Id MPChangeShuffleModeCommand) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPChangeShuffleModeCommand"

class IsMPRemoteCommand a => IsMPChangeShuffleModeCommand a where
  toMPChangeShuffleModeCommand :: a -> Id MPChangeShuffleModeCommand

instance IsMPChangeShuffleModeCommand (Id MPChangeShuffleModeCommand) where
  toMPChangeShuffleModeCommand = unsafeCastId

instance IsMPRemoteCommand (Id MPChangeShuffleModeCommand) where
  toMPRemoteCommand = unsafeCastId

instance IsNSObject (Id MPChangeShuffleModeCommand) where
  toNSObject = unsafeCastId

-- ---------- MPFeedbackCommand ----------

-- | Phantom type for @MPFeedbackCommand@.
data MPFeedbackCommand

instance IsObjCObject (Id MPFeedbackCommand) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPFeedbackCommand"

class IsMPRemoteCommand a => IsMPFeedbackCommand a where
  toMPFeedbackCommand :: a -> Id MPFeedbackCommand

instance IsMPFeedbackCommand (Id MPFeedbackCommand) where
  toMPFeedbackCommand = unsafeCastId

instance IsMPRemoteCommand (Id MPFeedbackCommand) where
  toMPRemoteCommand = unsafeCastId

instance IsNSObject (Id MPFeedbackCommand) where
  toNSObject = unsafeCastId

-- ---------- MPRatingCommand ----------

-- | Phantom type for @MPRatingCommand@.
data MPRatingCommand

instance IsObjCObject (Id MPRatingCommand) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPRatingCommand"

class IsMPRemoteCommand a => IsMPRatingCommand a where
  toMPRatingCommand :: a -> Id MPRatingCommand

instance IsMPRatingCommand (Id MPRatingCommand) where
  toMPRatingCommand = unsafeCastId

instance IsMPRemoteCommand (Id MPRatingCommand) where
  toMPRemoteCommand = unsafeCastId

instance IsNSObject (Id MPRatingCommand) where
  toNSObject = unsafeCastId

-- ---------- MPSkipIntervalCommand ----------

-- | Phantom type for @MPSkipIntervalCommand@.
data MPSkipIntervalCommand

instance IsObjCObject (Id MPSkipIntervalCommand) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSkipIntervalCommand"

class IsMPRemoteCommand a => IsMPSkipIntervalCommand a where
  toMPSkipIntervalCommand :: a -> Id MPSkipIntervalCommand

instance IsMPSkipIntervalCommand (Id MPSkipIntervalCommand) where
  toMPSkipIntervalCommand = unsafeCastId

instance IsMPRemoteCommand (Id MPSkipIntervalCommand) where
  toMPRemoteCommand = unsafeCastId

instance IsNSObject (Id MPSkipIntervalCommand) where
  toNSObject = unsafeCastId

-- ---------- MPChangeLanguageOptionCommandEvent ----------

-- | Phantom type for @MPChangeLanguageOptionCommandEvent@.
data MPChangeLanguageOptionCommandEvent

instance IsObjCObject (Id MPChangeLanguageOptionCommandEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPChangeLanguageOptionCommandEvent"

class IsMPRemoteCommandEvent a => IsMPChangeLanguageOptionCommandEvent a where
  toMPChangeLanguageOptionCommandEvent :: a -> Id MPChangeLanguageOptionCommandEvent

instance IsMPChangeLanguageOptionCommandEvent (Id MPChangeLanguageOptionCommandEvent) where
  toMPChangeLanguageOptionCommandEvent = unsafeCastId

instance IsMPRemoteCommandEvent (Id MPChangeLanguageOptionCommandEvent) where
  toMPRemoteCommandEvent = unsafeCastId

instance IsNSObject (Id MPChangeLanguageOptionCommandEvent) where
  toNSObject = unsafeCastId

-- ---------- MPChangePlaybackPositionCommandEvent ----------

-- | Phantom type for @MPChangePlaybackPositionCommandEvent@.
data MPChangePlaybackPositionCommandEvent

instance IsObjCObject (Id MPChangePlaybackPositionCommandEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPChangePlaybackPositionCommandEvent"

class IsMPRemoteCommandEvent a => IsMPChangePlaybackPositionCommandEvent a where
  toMPChangePlaybackPositionCommandEvent :: a -> Id MPChangePlaybackPositionCommandEvent

instance IsMPChangePlaybackPositionCommandEvent (Id MPChangePlaybackPositionCommandEvent) where
  toMPChangePlaybackPositionCommandEvent = unsafeCastId

instance IsMPRemoteCommandEvent (Id MPChangePlaybackPositionCommandEvent) where
  toMPRemoteCommandEvent = unsafeCastId

instance IsNSObject (Id MPChangePlaybackPositionCommandEvent) where
  toNSObject = unsafeCastId

-- ---------- MPChangePlaybackRateCommandEvent ----------

-- | Phantom type for @MPChangePlaybackRateCommandEvent@.
data MPChangePlaybackRateCommandEvent

instance IsObjCObject (Id MPChangePlaybackRateCommandEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPChangePlaybackRateCommandEvent"

class IsMPRemoteCommandEvent a => IsMPChangePlaybackRateCommandEvent a where
  toMPChangePlaybackRateCommandEvent :: a -> Id MPChangePlaybackRateCommandEvent

instance IsMPChangePlaybackRateCommandEvent (Id MPChangePlaybackRateCommandEvent) where
  toMPChangePlaybackRateCommandEvent = unsafeCastId

instance IsMPRemoteCommandEvent (Id MPChangePlaybackRateCommandEvent) where
  toMPRemoteCommandEvent = unsafeCastId

instance IsNSObject (Id MPChangePlaybackRateCommandEvent) where
  toNSObject = unsafeCastId

-- ---------- MPChangeRepeatModeCommandEvent ----------

-- | Phantom type for @MPChangeRepeatModeCommandEvent@.
data MPChangeRepeatModeCommandEvent

instance IsObjCObject (Id MPChangeRepeatModeCommandEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPChangeRepeatModeCommandEvent"

class IsMPRemoteCommandEvent a => IsMPChangeRepeatModeCommandEvent a where
  toMPChangeRepeatModeCommandEvent :: a -> Id MPChangeRepeatModeCommandEvent

instance IsMPChangeRepeatModeCommandEvent (Id MPChangeRepeatModeCommandEvent) where
  toMPChangeRepeatModeCommandEvent = unsafeCastId

instance IsMPRemoteCommandEvent (Id MPChangeRepeatModeCommandEvent) where
  toMPRemoteCommandEvent = unsafeCastId

instance IsNSObject (Id MPChangeRepeatModeCommandEvent) where
  toNSObject = unsafeCastId

-- ---------- MPChangeShuffleModeCommandEvent ----------

-- | Phantom type for @MPChangeShuffleModeCommandEvent@.
data MPChangeShuffleModeCommandEvent

instance IsObjCObject (Id MPChangeShuffleModeCommandEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPChangeShuffleModeCommandEvent"

class IsMPRemoteCommandEvent a => IsMPChangeShuffleModeCommandEvent a where
  toMPChangeShuffleModeCommandEvent :: a -> Id MPChangeShuffleModeCommandEvent

instance IsMPChangeShuffleModeCommandEvent (Id MPChangeShuffleModeCommandEvent) where
  toMPChangeShuffleModeCommandEvent = unsafeCastId

instance IsMPRemoteCommandEvent (Id MPChangeShuffleModeCommandEvent) where
  toMPRemoteCommandEvent = unsafeCastId

instance IsNSObject (Id MPChangeShuffleModeCommandEvent) where
  toNSObject = unsafeCastId

-- ---------- MPFeedbackCommandEvent ----------

-- | Phantom type for @MPFeedbackCommandEvent@.
data MPFeedbackCommandEvent

instance IsObjCObject (Id MPFeedbackCommandEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPFeedbackCommandEvent"

class IsMPRemoteCommandEvent a => IsMPFeedbackCommandEvent a where
  toMPFeedbackCommandEvent :: a -> Id MPFeedbackCommandEvent

instance IsMPFeedbackCommandEvent (Id MPFeedbackCommandEvent) where
  toMPFeedbackCommandEvent = unsafeCastId

instance IsMPRemoteCommandEvent (Id MPFeedbackCommandEvent) where
  toMPRemoteCommandEvent = unsafeCastId

instance IsNSObject (Id MPFeedbackCommandEvent) where
  toNSObject = unsafeCastId

-- ---------- MPRatingCommandEvent ----------

-- | Phantom type for @MPRatingCommandEvent@.
data MPRatingCommandEvent

instance IsObjCObject (Id MPRatingCommandEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPRatingCommandEvent"

class IsMPRemoteCommandEvent a => IsMPRatingCommandEvent a where
  toMPRatingCommandEvent :: a -> Id MPRatingCommandEvent

instance IsMPRatingCommandEvent (Id MPRatingCommandEvent) where
  toMPRatingCommandEvent = unsafeCastId

instance IsMPRemoteCommandEvent (Id MPRatingCommandEvent) where
  toMPRemoteCommandEvent = unsafeCastId

instance IsNSObject (Id MPRatingCommandEvent) where
  toNSObject = unsafeCastId

-- ---------- MPSeekCommandEvent ----------

-- | Phantom type for @MPSeekCommandEvent@.
data MPSeekCommandEvent

instance IsObjCObject (Id MPSeekCommandEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSeekCommandEvent"

class IsMPRemoteCommandEvent a => IsMPSeekCommandEvent a where
  toMPSeekCommandEvent :: a -> Id MPSeekCommandEvent

instance IsMPSeekCommandEvent (Id MPSeekCommandEvent) where
  toMPSeekCommandEvent = unsafeCastId

instance IsMPRemoteCommandEvent (Id MPSeekCommandEvent) where
  toMPRemoteCommandEvent = unsafeCastId

instance IsNSObject (Id MPSeekCommandEvent) where
  toNSObject = unsafeCastId

-- ---------- MPSkipIntervalCommandEvent ----------

-- | Phantom type for @MPSkipIntervalCommandEvent@.
data MPSkipIntervalCommandEvent

instance IsObjCObject (Id MPSkipIntervalCommandEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSkipIntervalCommandEvent"

class IsMPRemoteCommandEvent a => IsMPSkipIntervalCommandEvent a where
  toMPSkipIntervalCommandEvent :: a -> Id MPSkipIntervalCommandEvent

instance IsMPSkipIntervalCommandEvent (Id MPSkipIntervalCommandEvent) where
  toMPSkipIntervalCommandEvent = unsafeCastId

instance IsMPRemoteCommandEvent (Id MPSkipIntervalCommandEvent) where
  toMPRemoteCommandEvent = unsafeCastId

instance IsNSObject (Id MPSkipIntervalCommandEvent) where
  toNSObject = unsafeCastId

-- ---------- MPMediaPlaylist ----------

-- | Phantom type for @MPMediaPlaylist@.
data MPMediaPlaylist

instance IsObjCObject (Id MPMediaPlaylist) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPMediaPlaylist"

class IsMPMediaItemCollection a => IsMPMediaPlaylist a where
  toMPMediaPlaylist :: a -> Id MPMediaPlaylist

instance IsMPMediaPlaylist (Id MPMediaPlaylist) where
  toMPMediaPlaylist = unsafeCastId

instance IsMPMediaEntity (Id MPMediaPlaylist) where
  toMPMediaEntity = unsafeCastId

instance IsMPMediaItemCollection (Id MPMediaPlaylist) where
  toMPMediaItemCollection = unsafeCastId

instance IsNSObject (Id MPMediaPlaylist) where
  toNSObject = unsafeCastId
