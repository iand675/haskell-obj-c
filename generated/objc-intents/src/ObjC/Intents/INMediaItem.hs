{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithIdentifier_title_type_artwork_artistSelector
  , initWithIdentifier_title_type_artworkSelector
  , identifierSelector
  , titleSelector
  , typeSelector
  , artworkSelector
  , artistSelector

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

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINMediaItem inMediaItem => inMediaItem -> IO (Id INMediaItem)
init_ inMediaItem  =
  sendMsg inMediaItem (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithIdentifier:title:type:artwork:artist:@
initWithIdentifier_title_type_artwork_artist :: (IsINMediaItem inMediaItem, IsNSString identifier, IsNSString title, IsINImage artwork, IsNSString artist) => inMediaItem -> identifier -> title -> INMediaItemType -> artwork -> artist -> IO (Id INMediaItem)
initWithIdentifier_title_type_artwork_artist inMediaItem  identifier title type_ artwork artist =
withObjCPtr identifier $ \raw_identifier ->
  withObjCPtr title $ \raw_title ->
    withObjCPtr artwork $ \raw_artwork ->
      withObjCPtr artist $ \raw_artist ->
          sendMsg inMediaItem (mkSelector "initWithIdentifier:title:type:artwork:artist:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_title :: Ptr ()), argCLong (coerce type_), argPtr (castPtr raw_artwork :: Ptr ()), argPtr (castPtr raw_artist :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithIdentifier:title:type:artwork:@
initWithIdentifier_title_type_artwork :: (IsINMediaItem inMediaItem, IsNSString identifier, IsNSString title, IsINImage artwork) => inMediaItem -> identifier -> title -> INMediaItemType -> artwork -> IO (Id INMediaItem)
initWithIdentifier_title_type_artwork inMediaItem  identifier title type_ artwork =
withObjCPtr identifier $ \raw_identifier ->
  withObjCPtr title $ \raw_title ->
    withObjCPtr artwork $ \raw_artwork ->
        sendMsg inMediaItem (mkSelector "initWithIdentifier:title:type:artwork:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_title :: Ptr ()), argCLong (coerce type_), argPtr (castPtr raw_artwork :: Ptr ())] >>= ownedObject . castPtr

-- | @- identifier@
identifier :: IsINMediaItem inMediaItem => inMediaItem -> IO (Id NSString)
identifier inMediaItem  =
  sendMsg inMediaItem (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- title@
title :: IsINMediaItem inMediaItem => inMediaItem -> IO (Id NSString)
title inMediaItem  =
  sendMsg inMediaItem (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- type@
type_ :: IsINMediaItem inMediaItem => inMediaItem -> IO INMediaItemType
type_ inMediaItem  =
  fmap (coerce :: CLong -> INMediaItemType) $ sendMsg inMediaItem (mkSelector "type") retCLong []

-- | @- artwork@
artwork :: IsINMediaItem inMediaItem => inMediaItem -> IO (Id INImage)
artwork inMediaItem  =
  sendMsg inMediaItem (mkSelector "artwork") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- artist@
artist :: IsINMediaItem inMediaItem => inMediaItem -> IO (Id NSString)
artist inMediaItem  =
  sendMsg inMediaItem (mkSelector "artist") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithIdentifier:title:type:artwork:artist:@
initWithIdentifier_title_type_artwork_artistSelector :: Selector
initWithIdentifier_title_type_artwork_artistSelector = mkSelector "initWithIdentifier:title:type:artwork:artist:"

-- | @Selector@ for @initWithIdentifier:title:type:artwork:@
initWithIdentifier_title_type_artworkSelector :: Selector
initWithIdentifier_title_type_artworkSelector = mkSelector "initWithIdentifier:title:type:artwork:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @artwork@
artworkSelector :: Selector
artworkSelector = mkSelector "artwork"

-- | @Selector@ for @artist@
artistSelector :: Selector
artistSelector = mkSelector "artist"

