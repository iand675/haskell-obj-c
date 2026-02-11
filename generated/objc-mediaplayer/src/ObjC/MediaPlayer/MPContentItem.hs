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
  , initWithIdentifierSelector
  , identifierSelector
  , titleSelector
  , setTitleSelector
  , subtitleSelector
  , setSubtitleSelector
  , artworkSelector
  , setArtworkSelector
  , playbackProgressSelector
  , setPlaybackProgressSelector
  , streamingContentSelector
  , setStreamingContentSelector
  , explicitContentSelector
  , setExplicitContentSelector
  , containerSelector
  , setContainerSelector
  , playableSelector
  , setPlayableSelector


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

import ObjC.MediaPlayer.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Designated initializer. A unique identifier is required to identify the item for later use.
--
-- ObjC selector: @- initWithIdentifier:@
initWithIdentifier :: (IsMPContentItem mpContentItem, IsNSString identifier) => mpContentItem -> identifier -> IO (Id MPContentItem)
initWithIdentifier mpContentItem  identifier =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg mpContentItem (mkSelector "initWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

-- | A unique identifier for this content item. (Required)
--
-- ObjC selector: @- identifier@
identifier :: IsMPContentItem mpContentItem => mpContentItem -> IO (Id NSString)
identifier mpContentItem  =
  sendMsg mpContentItem (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A title for this item. Usually this would be the track name, if representing a song, the episode name of a podcast, etc.
--
-- ObjC selector: @- title@
title :: IsMPContentItem mpContentItem => mpContentItem -> IO (Id NSString)
title mpContentItem  =
  sendMsg mpContentItem (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A title for this item. Usually this would be the track name, if representing a song, the episode name of a podcast, etc.
--
-- ObjC selector: @- setTitle:@
setTitle :: (IsMPContentItem mpContentItem, IsNSString value) => mpContentItem -> value -> IO ()
setTitle mpContentItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg mpContentItem (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A subtitle for this item. If this were representing a song, this would usually be the artist or composer.
--
-- ObjC selector: @- subtitle@
subtitle :: IsMPContentItem mpContentItem => mpContentItem -> IO (Id NSString)
subtitle mpContentItem  =
  sendMsg mpContentItem (mkSelector "subtitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A subtitle for this item. If this were representing a song, this would usually be the artist or composer.
--
-- ObjC selector: @- setSubtitle:@
setSubtitle :: (IsMPContentItem mpContentItem, IsNSString value) => mpContentItem -> value -> IO ()
setSubtitle mpContentItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg mpContentItem (mkSelector "setSubtitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Artwork for this item. Examples of artwork for a content item are the album cover for a song, or a movie poster for a movie.
--
-- ObjC selector: @- artwork@
artwork :: IsMPContentItem mpContentItem => mpContentItem -> IO (Id MPMediaItemArtwork)
artwork mpContentItem  =
  sendMsg mpContentItem (mkSelector "artwork") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Artwork for this item. Examples of artwork for a content item are the album cover for a song, or a movie poster for a movie.
--
-- ObjC selector: @- setArtwork:@
setArtwork :: (IsMPContentItem mpContentItem, IsMPMediaItemArtwork value) => mpContentItem -> value -> IO ()
setArtwork mpContentItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg mpContentItem (mkSelector "setArtwork:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Represents the current playback progress of the item. 0.0 = not watched/listened/viewed, 1.0 = fully watched/listened/viewed Default is -1.0 (no progress indicator shown)
--
-- ObjC selector: @- playbackProgress@
playbackProgress :: IsMPContentItem mpContentItem => mpContentItem -> IO CFloat
playbackProgress mpContentItem  =
  sendMsg mpContentItem (mkSelector "playbackProgress") retCFloat []

-- | Represents the current playback progress of the item. 0.0 = not watched/listened/viewed, 1.0 = fully watched/listened/viewed Default is -1.0 (no progress indicator shown)
--
-- ObjC selector: @- setPlaybackProgress:@
setPlaybackProgress :: IsMPContentItem mpContentItem => mpContentItem -> CFloat -> IO ()
setPlaybackProgress mpContentItem  value =
  sendMsg mpContentItem (mkSelector "setPlaybackProgress:") retVoid [argCFloat (fromIntegral value)]

-- | Represents whether this content item is streaming content, i.e. from the cloud where the content is not stored locally.
--
-- ObjC selector: @- streamingContent@
streamingContent :: IsMPContentItem mpContentItem => mpContentItem -> IO Bool
streamingContent mpContentItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpContentItem (mkSelector "streamingContent") retCULong []

-- | Represents whether this content item is streaming content, i.e. from the cloud where the content is not stored locally.
--
-- ObjC selector: @- setStreamingContent:@
setStreamingContent :: IsMPContentItem mpContentItem => mpContentItem -> Bool -> IO ()
setStreamingContent mpContentItem  value =
  sendMsg mpContentItem (mkSelector "setStreamingContent:") retVoid [argCULong (if value then 1 else 0)]

-- | Represents whether this content item is explicit content
--
-- ObjC selector: @- explicitContent@
explicitContent :: IsMPContentItem mpContentItem => mpContentItem -> IO Bool
explicitContent mpContentItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpContentItem (mkSelector "explicitContent") retCULong []

-- | Represents whether this content item is explicit content
--
-- ObjC selector: @- setExplicitContent:@
setExplicitContent :: IsMPContentItem mpContentItem => mpContentItem -> Bool -> IO ()
setExplicitContent mpContentItem  value =
  sendMsg mpContentItem (mkSelector "setExplicitContent:") retVoid [argCULong (if value then 1 else 0)]

-- | Represents whether the content item is a container that may contain other content items, e.g. an album or a playlist.
--
-- ObjC selector: @- container@
container :: IsMPContentItem mpContentItem => mpContentItem -> IO Bool
container mpContentItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpContentItem (mkSelector "container") retCULong []

-- | Represents whether the content item is a container that may contain other content items, e.g. an album or a playlist.
--
-- ObjC selector: @- setContainer:@
setContainer :: IsMPContentItem mpContentItem => mpContentItem -> Bool -> IO ()
setContainer mpContentItem  value =
  sendMsg mpContentItem (mkSelector "setContainer:") retVoid [argCULong (if value then 1 else 0)]

-- | Represents whether the content item is actionable from a playback perspective. Albums are playable, for example, because selecting an album for playback means the app should play each song in the album in order. An example of a content item that may not be playable is a genre, since an app experience typically doesn't involve selecting an entire genre for playback.
--
-- ObjC selector: @- playable@
playable :: IsMPContentItem mpContentItem => mpContentItem -> IO Bool
playable mpContentItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpContentItem (mkSelector "playable") retCULong []

-- | Represents whether the content item is actionable from a playback perspective. Albums are playable, for example, because selecting an album for playback means the app should play each song in the album in order. An example of a content item that may not be playable is a genre, since an app experience typically doesn't involve selecting an entire genre for playback.
--
-- ObjC selector: @- setPlayable:@
setPlayable :: IsMPContentItem mpContentItem => mpContentItem -> Bool -> IO ()
setPlayable mpContentItem  value =
  sendMsg mpContentItem (mkSelector "setPlayable:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithIdentifier:@
initWithIdentifierSelector :: Selector
initWithIdentifierSelector = mkSelector "initWithIdentifier:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @subtitle@
subtitleSelector :: Selector
subtitleSelector = mkSelector "subtitle"

-- | @Selector@ for @setSubtitle:@
setSubtitleSelector :: Selector
setSubtitleSelector = mkSelector "setSubtitle:"

-- | @Selector@ for @artwork@
artworkSelector :: Selector
artworkSelector = mkSelector "artwork"

-- | @Selector@ for @setArtwork:@
setArtworkSelector :: Selector
setArtworkSelector = mkSelector "setArtwork:"

-- | @Selector@ for @playbackProgress@
playbackProgressSelector :: Selector
playbackProgressSelector = mkSelector "playbackProgress"

-- | @Selector@ for @setPlaybackProgress:@
setPlaybackProgressSelector :: Selector
setPlaybackProgressSelector = mkSelector "setPlaybackProgress:"

-- | @Selector@ for @streamingContent@
streamingContentSelector :: Selector
streamingContentSelector = mkSelector "streamingContent"

-- | @Selector@ for @setStreamingContent:@
setStreamingContentSelector :: Selector
setStreamingContentSelector = mkSelector "setStreamingContent:"

-- | @Selector@ for @explicitContent@
explicitContentSelector :: Selector
explicitContentSelector = mkSelector "explicitContent"

-- | @Selector@ for @setExplicitContent:@
setExplicitContentSelector :: Selector
setExplicitContentSelector = mkSelector "setExplicitContent:"

-- | @Selector@ for @container@
containerSelector :: Selector
containerSelector = mkSelector "container"

-- | @Selector@ for @setContainer:@
setContainerSelector :: Selector
setContainerSelector = mkSelector "setContainer:"

-- | @Selector@ for @playable@
playableSelector :: Selector
playableSelector = mkSelector "playable"

-- | @Selector@ for @setPlayable:@
setPlayableSelector :: Selector
setPlayableSelector = mkSelector "setPlayable:"

