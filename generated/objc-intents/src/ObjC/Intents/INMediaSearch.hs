{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INMediaSearch@.
module ObjC.Intents.INMediaSearch
  ( INMediaSearch
  , IsINMediaSearch(..)
  , init_
  , initWithMediaType_sortOrder_mediaName_artistName_albumName_genreNames_moodNames_releaseDate_reference_mediaIdentifier
  , mediaType
  , sortOrder
  , mediaName
  , artistName
  , albumName
  , genreNames
  , moodNames
  , releaseDate
  , reference
  , mediaIdentifier
  , activityNames
  , initSelector
  , initWithMediaType_sortOrder_mediaName_artistName_albumName_genreNames_moodNames_releaseDate_reference_mediaIdentifierSelector
  , mediaTypeSelector
  , sortOrderSelector
  , mediaNameSelector
  , artistNameSelector
  , albumNameSelector
  , genreNamesSelector
  , moodNamesSelector
  , releaseDateSelector
  , referenceSelector
  , mediaIdentifierSelector
  , activityNamesSelector

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
  , INMediaReference(INMediaReference)
  , pattern INMediaReferenceUnknown
  , pattern INMediaReferenceCurrentlyPlaying
  , pattern INMediaReferenceMy
  , INMediaSortOrder(INMediaSortOrder)
  , pattern INMediaSortOrderUnknown
  , pattern INMediaSortOrderNewest
  , pattern INMediaSortOrderOldest
  , pattern INMediaSortOrderBest
  , pattern INMediaSortOrderWorst
  , pattern INMediaSortOrderPopular
  , pattern INMediaSortOrderUnpopular
  , pattern INMediaSortOrderTrending
  , pattern INMediaSortOrderRecommended

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
init_ :: IsINMediaSearch inMediaSearch => inMediaSearch -> IO (Id INMediaSearch)
init_ inMediaSearch  =
  sendMsg inMediaSearch (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithMediaType:sortOrder:mediaName:artistName:albumName:genreNames:moodNames:releaseDate:reference:mediaIdentifier:@
initWithMediaType_sortOrder_mediaName_artistName_albumName_genreNames_moodNames_releaseDate_reference_mediaIdentifier :: (IsINMediaSearch inMediaSearch, IsNSString mediaName, IsNSString artistName, IsNSString albumName, IsNSArray genreNames, IsNSArray moodNames, IsINDateComponentsRange releaseDate, IsNSString mediaIdentifier) => inMediaSearch -> INMediaItemType -> INMediaSortOrder -> mediaName -> artistName -> albumName -> genreNames -> moodNames -> releaseDate -> INMediaReference -> mediaIdentifier -> IO (Id INMediaSearch)
initWithMediaType_sortOrder_mediaName_artistName_albumName_genreNames_moodNames_releaseDate_reference_mediaIdentifier inMediaSearch  mediaType sortOrder mediaName artistName albumName genreNames moodNames releaseDate reference mediaIdentifier =
withObjCPtr mediaName $ \raw_mediaName ->
  withObjCPtr artistName $ \raw_artistName ->
    withObjCPtr albumName $ \raw_albumName ->
      withObjCPtr genreNames $ \raw_genreNames ->
        withObjCPtr moodNames $ \raw_moodNames ->
          withObjCPtr releaseDate $ \raw_releaseDate ->
            withObjCPtr mediaIdentifier $ \raw_mediaIdentifier ->
                sendMsg inMediaSearch (mkSelector "initWithMediaType:sortOrder:mediaName:artistName:albumName:genreNames:moodNames:releaseDate:reference:mediaIdentifier:") (retPtr retVoid) [argCLong (coerce mediaType), argCLong (coerce sortOrder), argPtr (castPtr raw_mediaName :: Ptr ()), argPtr (castPtr raw_artistName :: Ptr ()), argPtr (castPtr raw_albumName :: Ptr ()), argPtr (castPtr raw_genreNames :: Ptr ()), argPtr (castPtr raw_moodNames :: Ptr ()), argPtr (castPtr raw_releaseDate :: Ptr ()), argCLong (coerce reference), argPtr (castPtr raw_mediaIdentifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- mediaType@
mediaType :: IsINMediaSearch inMediaSearch => inMediaSearch -> IO INMediaItemType
mediaType inMediaSearch  =
  fmap (coerce :: CLong -> INMediaItemType) $ sendMsg inMediaSearch (mkSelector "mediaType") retCLong []

-- | @- sortOrder@
sortOrder :: IsINMediaSearch inMediaSearch => inMediaSearch -> IO INMediaSortOrder
sortOrder inMediaSearch  =
  fmap (coerce :: CLong -> INMediaSortOrder) $ sendMsg inMediaSearch (mkSelector "sortOrder") retCLong []

-- | @- mediaName@
mediaName :: IsINMediaSearch inMediaSearch => inMediaSearch -> IO (Id NSString)
mediaName inMediaSearch  =
  sendMsg inMediaSearch (mkSelector "mediaName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- artistName@
artistName :: IsINMediaSearch inMediaSearch => inMediaSearch -> IO (Id NSString)
artistName inMediaSearch  =
  sendMsg inMediaSearch (mkSelector "artistName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- albumName@
albumName :: IsINMediaSearch inMediaSearch => inMediaSearch -> IO (Id NSString)
albumName inMediaSearch  =
  sendMsg inMediaSearch (mkSelector "albumName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- genreNames@
genreNames :: IsINMediaSearch inMediaSearch => inMediaSearch -> IO (Id NSArray)
genreNames inMediaSearch  =
  sendMsg inMediaSearch (mkSelector "genreNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- moodNames@
moodNames :: IsINMediaSearch inMediaSearch => inMediaSearch -> IO (Id NSArray)
moodNames inMediaSearch  =
  sendMsg inMediaSearch (mkSelector "moodNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- releaseDate@
releaseDate :: IsINMediaSearch inMediaSearch => inMediaSearch -> IO (Id INDateComponentsRange)
releaseDate inMediaSearch  =
  sendMsg inMediaSearch (mkSelector "releaseDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- reference@
reference :: IsINMediaSearch inMediaSearch => inMediaSearch -> IO INMediaReference
reference inMediaSearch  =
  fmap (coerce :: CLong -> INMediaReference) $ sendMsg inMediaSearch (mkSelector "reference") retCLong []

-- | @- mediaIdentifier@
mediaIdentifier :: IsINMediaSearch inMediaSearch => inMediaSearch -> IO (Id NSString)
mediaIdentifier inMediaSearch  =
  sendMsg inMediaSearch (mkSelector "mediaIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- activityNames@
activityNames :: IsINMediaSearch inMediaSearch => inMediaSearch -> IO (Id NSArray)
activityNames inMediaSearch  =
  sendMsg inMediaSearch (mkSelector "activityNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithMediaType:sortOrder:mediaName:artistName:albumName:genreNames:moodNames:releaseDate:reference:mediaIdentifier:@
initWithMediaType_sortOrder_mediaName_artistName_albumName_genreNames_moodNames_releaseDate_reference_mediaIdentifierSelector :: Selector
initWithMediaType_sortOrder_mediaName_artistName_albumName_genreNames_moodNames_releaseDate_reference_mediaIdentifierSelector = mkSelector "initWithMediaType:sortOrder:mediaName:artistName:albumName:genreNames:moodNames:releaseDate:reference:mediaIdentifier:"

-- | @Selector@ for @mediaType@
mediaTypeSelector :: Selector
mediaTypeSelector = mkSelector "mediaType"

-- | @Selector@ for @sortOrder@
sortOrderSelector :: Selector
sortOrderSelector = mkSelector "sortOrder"

-- | @Selector@ for @mediaName@
mediaNameSelector :: Selector
mediaNameSelector = mkSelector "mediaName"

-- | @Selector@ for @artistName@
artistNameSelector :: Selector
artistNameSelector = mkSelector "artistName"

-- | @Selector@ for @albumName@
albumNameSelector :: Selector
albumNameSelector = mkSelector "albumName"

-- | @Selector@ for @genreNames@
genreNamesSelector :: Selector
genreNamesSelector = mkSelector "genreNames"

-- | @Selector@ for @moodNames@
moodNamesSelector :: Selector
moodNamesSelector = mkSelector "moodNames"

-- | @Selector@ for @releaseDate@
releaseDateSelector :: Selector
releaseDateSelector = mkSelector "releaseDate"

-- | @Selector@ for @reference@
referenceSelector :: Selector
referenceSelector = mkSelector "reference"

-- | @Selector@ for @mediaIdentifier@
mediaIdentifierSelector :: Selector
mediaIdentifierSelector = mkSelector "mediaIdentifier"

-- | @Selector@ for @activityNames@
activityNamesSelector :: Selector
activityNamesSelector = mkSelector "activityNames"

