{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , activityNamesSelector
  , albumNameSelector
  , artistNameSelector
  , genreNamesSelector
  , initSelector
  , initWithMediaType_sortOrder_mediaName_artistName_albumName_genreNames_moodNames_releaseDate_reference_mediaIdentifierSelector
  , mediaIdentifierSelector
  , mediaNameSelector
  , mediaTypeSelector
  , moodNamesSelector
  , referenceSelector
  , releaseDateSelector
  , sortOrderSelector

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
init_ :: IsINMediaSearch inMediaSearch => inMediaSearch -> IO (Id INMediaSearch)
init_ inMediaSearch =
  sendOwnedMessage inMediaSearch initSelector

-- | @- initWithMediaType:sortOrder:mediaName:artistName:albumName:genreNames:moodNames:releaseDate:reference:mediaIdentifier:@
initWithMediaType_sortOrder_mediaName_artistName_albumName_genreNames_moodNames_releaseDate_reference_mediaIdentifier :: (IsINMediaSearch inMediaSearch, IsNSString mediaName, IsNSString artistName, IsNSString albumName, IsNSArray genreNames, IsNSArray moodNames, IsINDateComponentsRange releaseDate, IsNSString mediaIdentifier) => inMediaSearch -> INMediaItemType -> INMediaSortOrder -> mediaName -> artistName -> albumName -> genreNames -> moodNames -> releaseDate -> INMediaReference -> mediaIdentifier -> IO (Id INMediaSearch)
initWithMediaType_sortOrder_mediaName_artistName_albumName_genreNames_moodNames_releaseDate_reference_mediaIdentifier inMediaSearch mediaType sortOrder mediaName artistName albumName genreNames moodNames releaseDate reference mediaIdentifier =
  sendOwnedMessage inMediaSearch initWithMediaType_sortOrder_mediaName_artistName_albumName_genreNames_moodNames_releaseDate_reference_mediaIdentifierSelector mediaType sortOrder (toNSString mediaName) (toNSString artistName) (toNSString albumName) (toNSArray genreNames) (toNSArray moodNames) (toINDateComponentsRange releaseDate) reference (toNSString mediaIdentifier)

-- | @- mediaType@
mediaType :: IsINMediaSearch inMediaSearch => inMediaSearch -> IO INMediaItemType
mediaType inMediaSearch =
  sendMessage inMediaSearch mediaTypeSelector

-- | @- sortOrder@
sortOrder :: IsINMediaSearch inMediaSearch => inMediaSearch -> IO INMediaSortOrder
sortOrder inMediaSearch =
  sendMessage inMediaSearch sortOrderSelector

-- | @- mediaName@
mediaName :: IsINMediaSearch inMediaSearch => inMediaSearch -> IO (Id NSString)
mediaName inMediaSearch =
  sendMessage inMediaSearch mediaNameSelector

-- | @- artistName@
artistName :: IsINMediaSearch inMediaSearch => inMediaSearch -> IO (Id NSString)
artistName inMediaSearch =
  sendMessage inMediaSearch artistNameSelector

-- | @- albumName@
albumName :: IsINMediaSearch inMediaSearch => inMediaSearch -> IO (Id NSString)
albumName inMediaSearch =
  sendMessage inMediaSearch albumNameSelector

-- | @- genreNames@
genreNames :: IsINMediaSearch inMediaSearch => inMediaSearch -> IO (Id NSArray)
genreNames inMediaSearch =
  sendMessage inMediaSearch genreNamesSelector

-- | @- moodNames@
moodNames :: IsINMediaSearch inMediaSearch => inMediaSearch -> IO (Id NSArray)
moodNames inMediaSearch =
  sendMessage inMediaSearch moodNamesSelector

-- | @- releaseDate@
releaseDate :: IsINMediaSearch inMediaSearch => inMediaSearch -> IO (Id INDateComponentsRange)
releaseDate inMediaSearch =
  sendMessage inMediaSearch releaseDateSelector

-- | @- reference@
reference :: IsINMediaSearch inMediaSearch => inMediaSearch -> IO INMediaReference
reference inMediaSearch =
  sendMessage inMediaSearch referenceSelector

-- | @- mediaIdentifier@
mediaIdentifier :: IsINMediaSearch inMediaSearch => inMediaSearch -> IO (Id NSString)
mediaIdentifier inMediaSearch =
  sendMessage inMediaSearch mediaIdentifierSelector

-- | @- activityNames@
activityNames :: IsINMediaSearch inMediaSearch => inMediaSearch -> IO (Id NSArray)
activityNames inMediaSearch =
  sendMessage inMediaSearch activityNamesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INMediaSearch)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithMediaType:sortOrder:mediaName:artistName:albumName:genreNames:moodNames:releaseDate:reference:mediaIdentifier:@
initWithMediaType_sortOrder_mediaName_artistName_albumName_genreNames_moodNames_releaseDate_reference_mediaIdentifierSelector :: Selector '[INMediaItemType, INMediaSortOrder, Id NSString, Id NSString, Id NSString, Id NSArray, Id NSArray, Id INDateComponentsRange, INMediaReference, Id NSString] (Id INMediaSearch)
initWithMediaType_sortOrder_mediaName_artistName_albumName_genreNames_moodNames_releaseDate_reference_mediaIdentifierSelector = mkSelector "initWithMediaType:sortOrder:mediaName:artistName:albumName:genreNames:moodNames:releaseDate:reference:mediaIdentifier:"

-- | @Selector@ for @mediaType@
mediaTypeSelector :: Selector '[] INMediaItemType
mediaTypeSelector = mkSelector "mediaType"

-- | @Selector@ for @sortOrder@
sortOrderSelector :: Selector '[] INMediaSortOrder
sortOrderSelector = mkSelector "sortOrder"

-- | @Selector@ for @mediaName@
mediaNameSelector :: Selector '[] (Id NSString)
mediaNameSelector = mkSelector "mediaName"

-- | @Selector@ for @artistName@
artistNameSelector :: Selector '[] (Id NSString)
artistNameSelector = mkSelector "artistName"

-- | @Selector@ for @albumName@
albumNameSelector :: Selector '[] (Id NSString)
albumNameSelector = mkSelector "albumName"

-- | @Selector@ for @genreNames@
genreNamesSelector :: Selector '[] (Id NSArray)
genreNamesSelector = mkSelector "genreNames"

-- | @Selector@ for @moodNames@
moodNamesSelector :: Selector '[] (Id NSArray)
moodNamesSelector = mkSelector "moodNames"

-- | @Selector@ for @releaseDate@
releaseDateSelector :: Selector '[] (Id INDateComponentsRange)
releaseDateSelector = mkSelector "releaseDate"

-- | @Selector@ for @reference@
referenceSelector :: Selector '[] INMediaReference
referenceSelector = mkSelector "reference"

-- | @Selector@ for @mediaIdentifier@
mediaIdentifierSelector :: Selector '[] (Id NSString)
mediaIdentifierSelector = mkSelector "mediaIdentifier"

-- | @Selector@ for @activityNames@
activityNamesSelector :: Selector '[] (Id NSArray)
activityNamesSelector = mkSelector "activityNames"

