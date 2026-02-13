{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INMediaItem@.
module ObjC.Intents.INMediaItem
  ( INMediaItem
  , IsINMediaItem(..)
  , init_
  , initWithIdentifier_title_type_artwork_artist
  , initWithIdentifier_title_type_artwork
  , identifier
  , title
  , type_
  , artwork
  , artist
  , artistSelector
  , artworkSelector
  , identifierSelector
  , initSelector
  , initWithIdentifier_title_type_artworkSelector
  , initWithIdentifier_title_type_artwork_artistSelector
  , titleSelector
  , typeSelector

  -- * Enum types
  , INMediaItemType(INMediaItemType)
  , pattern INMediaItemTypeUnknown
  , pattern INMediaItemTypeSong
  , pattern INMediaItemTypeAlbum
  , pattern INMediaItemTypeArtist
  , pattern INMediaItemTypeGenre
  , pattern INMediaItemTypePlaylist
  , pattern INMediaItemTypePodcastShow
  , pattern INMediaItemTypePodcastEpisode
  , pattern INMediaItemTypePodcastPlaylist
  , pattern INMediaItemTypeMusicStation
  , pattern INMediaItemTypeAudioBook
  , pattern INMediaItemTypeMovie
  , pattern INMediaItemTypeTVShow
  , pattern INMediaItemTypeTVShowEpisode
  , pattern INMediaItemTypeMusicVideo
  , pattern INMediaItemTypePodcastStation
  , pattern INMediaItemTypeRadioStation
  , pattern INMediaItemTypeStation
  , pattern INMediaItemTypeMusic
  , pattern INMediaItemTypeAlgorithmicRadioStation
  , pattern INMediaItemTypeNews

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINMediaItem inMediaItem => inMediaItem -> IO (Id INMediaItem)
init_ inMediaItem =
  sendOwnedMessage inMediaItem initSelector

-- | @- initWithIdentifier:title:type:artwork:artist:@
initWithIdentifier_title_type_artwork_artist :: (IsINMediaItem inMediaItem, IsNSString identifier, IsNSString title, IsINImage artwork, IsNSString artist) => inMediaItem -> identifier -> title -> INMediaItemType -> artwork -> artist -> IO (Id INMediaItem)
initWithIdentifier_title_type_artwork_artist inMediaItem identifier title type_ artwork artist =
  sendOwnedMessage inMediaItem initWithIdentifier_title_type_artwork_artistSelector (toNSString identifier) (toNSString title) type_ (toINImage artwork) (toNSString artist)

-- | @- initWithIdentifier:title:type:artwork:@
initWithIdentifier_title_type_artwork :: (IsINMediaItem inMediaItem, IsNSString identifier, IsNSString title, IsINImage artwork) => inMediaItem -> identifier -> title -> INMediaItemType -> artwork -> IO (Id INMediaItem)
initWithIdentifier_title_type_artwork inMediaItem identifier title type_ artwork =
  sendOwnedMessage inMediaItem initWithIdentifier_title_type_artworkSelector (toNSString identifier) (toNSString title) type_ (toINImage artwork)

-- | @- identifier@
identifier :: IsINMediaItem inMediaItem => inMediaItem -> IO (Id NSString)
identifier inMediaItem =
  sendMessage inMediaItem identifierSelector

-- | @- title@
title :: IsINMediaItem inMediaItem => inMediaItem -> IO (Id NSString)
title inMediaItem =
  sendMessage inMediaItem titleSelector

-- | @- type@
type_ :: IsINMediaItem inMediaItem => inMediaItem -> IO INMediaItemType
type_ inMediaItem =
  sendMessage inMediaItem typeSelector

-- | @- artwork@
artwork :: IsINMediaItem inMediaItem => inMediaItem -> IO (Id INImage)
artwork inMediaItem =
  sendMessage inMediaItem artworkSelector

-- | @- artist@
artist :: IsINMediaItem inMediaItem => inMediaItem -> IO (Id NSString)
artist inMediaItem =
  sendMessage inMediaItem artistSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INMediaItem)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithIdentifier:title:type:artwork:artist:@
initWithIdentifier_title_type_artwork_artistSelector :: Selector '[Id NSString, Id NSString, INMediaItemType, Id INImage, Id NSString] (Id INMediaItem)
initWithIdentifier_title_type_artwork_artistSelector = mkSelector "initWithIdentifier:title:type:artwork:artist:"

-- | @Selector@ for @initWithIdentifier:title:type:artwork:@
initWithIdentifier_title_type_artworkSelector :: Selector '[Id NSString, Id NSString, INMediaItemType, Id INImage] (Id INMediaItem)
initWithIdentifier_title_type_artworkSelector = mkSelector "initWithIdentifier:title:type:artwork:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @type@
typeSelector :: Selector '[] INMediaItemType
typeSelector = mkSelector "type"

-- | @Selector@ for @artwork@
artworkSelector :: Selector '[] (Id INImage)
artworkSelector = mkSelector "artwork"

-- | @Selector@ for @artist@
artistSelector :: Selector '[] (Id NSString)
artistSelector = mkSelector "artist"

