{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPContentItem represents high-level metadata for a particular media item for representation outside the client application. Examples of media items that a developer might want to represent include song files, streaming audio URLs, or radio stations.
--
-- Generated bindings for @MPContentItem@.
module ObjC.MediaPlayer.MPContentItem
  ( MPContentItem
  , IsMPContentItem(..)
  , initWithIdentifier
  , identifier
  , title
  , setTitle
  , subtitle
  , setSubtitle
  , artwork
  , setArtwork
  , playbackProgress
  , setPlaybackProgress
  , streamingContent
  , setStreamingContent
  , explicitContent
  , setExplicitContent
  , container
  , setContainer
  , playable
  , setPlayable
  , artworkSelector
  , containerSelector
  , explicitContentSelector
  , identifierSelector
  , initWithIdentifierSelector
  , playableSelector
  , playbackProgressSelector
  , setArtworkSelector
  , setContainerSelector
  , setExplicitContentSelector
  , setPlayableSelector
  , setPlaybackProgressSelector
  , setStreamingContentSelector
  , setSubtitleSelector
  , setTitleSelector
  , streamingContentSelector
  , subtitleSelector
  , titleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Designated initializer. A unique identifier is required to identify the item for later use.
--
-- ObjC selector: @- initWithIdentifier:@
initWithIdentifier :: (IsMPContentItem mpContentItem, IsNSString identifier) => mpContentItem -> identifier -> IO (Id MPContentItem)
initWithIdentifier mpContentItem identifier =
  sendOwnedMessage mpContentItem initWithIdentifierSelector (toNSString identifier)

-- | A unique identifier for this content item. (Required)
--
-- ObjC selector: @- identifier@
identifier :: IsMPContentItem mpContentItem => mpContentItem -> IO (Id NSString)
identifier mpContentItem =
  sendMessage mpContentItem identifierSelector

-- | A title for this item. Usually this would be the track name, if representing a song, the episode name of a podcast, etc.
--
-- ObjC selector: @- title@
title :: IsMPContentItem mpContentItem => mpContentItem -> IO (Id NSString)
title mpContentItem =
  sendMessage mpContentItem titleSelector

-- | A title for this item. Usually this would be the track name, if representing a song, the episode name of a podcast, etc.
--
-- ObjC selector: @- setTitle:@
setTitle :: (IsMPContentItem mpContentItem, IsNSString value) => mpContentItem -> value -> IO ()
setTitle mpContentItem value =
  sendMessage mpContentItem setTitleSelector (toNSString value)

-- | A subtitle for this item. If this were representing a song, this would usually be the artist or composer.
--
-- ObjC selector: @- subtitle@
subtitle :: IsMPContentItem mpContentItem => mpContentItem -> IO (Id NSString)
subtitle mpContentItem =
  sendMessage mpContentItem subtitleSelector

-- | A subtitle for this item. If this were representing a song, this would usually be the artist or composer.
--
-- ObjC selector: @- setSubtitle:@
setSubtitle :: (IsMPContentItem mpContentItem, IsNSString value) => mpContentItem -> value -> IO ()
setSubtitle mpContentItem value =
  sendMessage mpContentItem setSubtitleSelector (toNSString value)

-- | Artwork for this item. Examples of artwork for a content item are the album cover for a song, or a movie poster for a movie.
--
-- ObjC selector: @- artwork@
artwork :: IsMPContentItem mpContentItem => mpContentItem -> IO (Id MPMediaItemArtwork)
artwork mpContentItem =
  sendMessage mpContentItem artworkSelector

-- | Artwork for this item. Examples of artwork for a content item are the album cover for a song, or a movie poster for a movie.
--
-- ObjC selector: @- setArtwork:@
setArtwork :: (IsMPContentItem mpContentItem, IsMPMediaItemArtwork value) => mpContentItem -> value -> IO ()
setArtwork mpContentItem value =
  sendMessage mpContentItem setArtworkSelector (toMPMediaItemArtwork value)

-- | Represents the current playback progress of the item. 0.0 = not watched/listened/viewed, 1.0 = fully watched/listened/viewed Default is -1.0 (no progress indicator shown)
--
-- ObjC selector: @- playbackProgress@
playbackProgress :: IsMPContentItem mpContentItem => mpContentItem -> IO CFloat
playbackProgress mpContentItem =
  sendMessage mpContentItem playbackProgressSelector

-- | Represents the current playback progress of the item. 0.0 = not watched/listened/viewed, 1.0 = fully watched/listened/viewed Default is -1.0 (no progress indicator shown)
--
-- ObjC selector: @- setPlaybackProgress:@
setPlaybackProgress :: IsMPContentItem mpContentItem => mpContentItem -> CFloat -> IO ()
setPlaybackProgress mpContentItem value =
  sendMessage mpContentItem setPlaybackProgressSelector value

-- | Represents whether this content item is streaming content, i.e. from the cloud where the content is not stored locally.
--
-- ObjC selector: @- streamingContent@
streamingContent :: IsMPContentItem mpContentItem => mpContentItem -> IO Bool
streamingContent mpContentItem =
  sendMessage mpContentItem streamingContentSelector

-- | Represents whether this content item is streaming content, i.e. from the cloud where the content is not stored locally.
--
-- ObjC selector: @- setStreamingContent:@
setStreamingContent :: IsMPContentItem mpContentItem => mpContentItem -> Bool -> IO ()
setStreamingContent mpContentItem value =
  sendMessage mpContentItem setStreamingContentSelector value

-- | Represents whether this content item is explicit content
--
-- ObjC selector: @- explicitContent@
explicitContent :: IsMPContentItem mpContentItem => mpContentItem -> IO Bool
explicitContent mpContentItem =
  sendMessage mpContentItem explicitContentSelector

-- | Represents whether this content item is explicit content
--
-- ObjC selector: @- setExplicitContent:@
setExplicitContent :: IsMPContentItem mpContentItem => mpContentItem -> Bool -> IO ()
setExplicitContent mpContentItem value =
  sendMessage mpContentItem setExplicitContentSelector value

-- | Represents whether the content item is a container that may contain other content items, e.g. an album or a playlist.
--
-- ObjC selector: @- container@
container :: IsMPContentItem mpContentItem => mpContentItem -> IO Bool
container mpContentItem =
  sendMessage mpContentItem containerSelector

-- | Represents whether the content item is a container that may contain other content items, e.g. an album or a playlist.
--
-- ObjC selector: @- setContainer:@
setContainer :: IsMPContentItem mpContentItem => mpContentItem -> Bool -> IO ()
setContainer mpContentItem value =
  sendMessage mpContentItem setContainerSelector value

-- | Represents whether the content item is actionable from a playback perspective. Albums are playable, for example, because selecting an album for playback means the app should play each song in the album in order. An example of a content item that may not be playable is a genre, since an app experience typically doesn't involve selecting an entire genre for playback.
--
-- ObjC selector: @- playable@
playable :: IsMPContentItem mpContentItem => mpContentItem -> IO Bool
playable mpContentItem =
  sendMessage mpContentItem playableSelector

-- | Represents whether the content item is actionable from a playback perspective. Albums are playable, for example, because selecting an album for playback means the app should play each song in the album in order. An example of a content item that may not be playable is a genre, since an app experience typically doesn't involve selecting an entire genre for playback.
--
-- ObjC selector: @- setPlayable:@
setPlayable :: IsMPContentItem mpContentItem => mpContentItem -> Bool -> IO ()
setPlayable mpContentItem value =
  sendMessage mpContentItem setPlayableSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithIdentifier:@
initWithIdentifierSelector :: Selector '[Id NSString] (Id MPContentItem)
initWithIdentifierSelector = mkSelector "initWithIdentifier:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @subtitle@
subtitleSelector :: Selector '[] (Id NSString)
subtitleSelector = mkSelector "subtitle"

-- | @Selector@ for @setSubtitle:@
setSubtitleSelector :: Selector '[Id NSString] ()
setSubtitleSelector = mkSelector "setSubtitle:"

-- | @Selector@ for @artwork@
artworkSelector :: Selector '[] (Id MPMediaItemArtwork)
artworkSelector = mkSelector "artwork"

-- | @Selector@ for @setArtwork:@
setArtworkSelector :: Selector '[Id MPMediaItemArtwork] ()
setArtworkSelector = mkSelector "setArtwork:"

-- | @Selector@ for @playbackProgress@
playbackProgressSelector :: Selector '[] CFloat
playbackProgressSelector = mkSelector "playbackProgress"

-- | @Selector@ for @setPlaybackProgress:@
setPlaybackProgressSelector :: Selector '[CFloat] ()
setPlaybackProgressSelector = mkSelector "setPlaybackProgress:"

-- | @Selector@ for @streamingContent@
streamingContentSelector :: Selector '[] Bool
streamingContentSelector = mkSelector "streamingContent"

-- | @Selector@ for @setStreamingContent:@
setStreamingContentSelector :: Selector '[Bool] ()
setStreamingContentSelector = mkSelector "setStreamingContent:"

-- | @Selector@ for @explicitContent@
explicitContentSelector :: Selector '[] Bool
explicitContentSelector = mkSelector "explicitContent"

-- | @Selector@ for @setExplicitContent:@
setExplicitContentSelector :: Selector '[Bool] ()
setExplicitContentSelector = mkSelector "setExplicitContent:"

-- | @Selector@ for @container@
containerSelector :: Selector '[] Bool
containerSelector = mkSelector "container"

-- | @Selector@ for @setContainer:@
setContainerSelector :: Selector '[Bool] ()
setContainerSelector = mkSelector "setContainer:"

-- | @Selector@ for @playable@
playableSelector :: Selector '[] Bool
playableSelector = mkSelector "playable"

-- | @Selector@ for @setPlayable:@
setPlayableSelector :: Selector '[Bool] ()
setPlayableSelector = mkSelector "setPlayable:"

