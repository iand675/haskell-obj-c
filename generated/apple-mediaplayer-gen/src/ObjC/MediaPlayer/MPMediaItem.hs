{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPMediaItem@.
module ObjC.MediaPlayer.MPMediaItem
  ( MPMediaItem
  , IsMPMediaItem(..)
  , persistentIDPropertyForGroupingType
  , titlePropertyForGroupingType
  , persistentID
  , mediaType
  , title
  , albumTitle
  , albumPersistentID
  , artist
  , artistPersistentID
  , albumArtist
  , albumArtistPersistentID
  , genre
  , genrePersistentID
  , composer
  , composerPersistentID
  , playbackDuration
  , albumTrackNumber
  , albumTrackCount
  , discNumber
  , discCount
  , artwork
  , explicitItem
  , lyrics
  , compilation
  , releaseDate
  , beatsPerMinute
  , comments
  , assetURL
  , cloudItem
  , protectedAsset
  , podcastTitle
  , podcastPersistentID
  , playCount
  , skipCount
  , rating
  , lastPlayedDate
  , userGrouping
  , bookmarkTime
  , dateAdded
  , playbackStoreID
  , preorder
  , albumArtistPersistentIDSelector
  , albumArtistSelector
  , albumPersistentIDSelector
  , albumTitleSelector
  , albumTrackCountSelector
  , albumTrackNumberSelector
  , artistPersistentIDSelector
  , artistSelector
  , artworkSelector
  , assetURLSelector
  , beatsPerMinuteSelector
  , bookmarkTimeSelector
  , cloudItemSelector
  , commentsSelector
  , compilationSelector
  , composerPersistentIDSelector
  , composerSelector
  , dateAddedSelector
  , discCountSelector
  , discNumberSelector
  , explicitItemSelector
  , genrePersistentIDSelector
  , genreSelector
  , lastPlayedDateSelector
  , lyricsSelector
  , mediaTypeSelector
  , persistentIDPropertyForGroupingTypeSelector
  , persistentIDSelector
  , playCountSelector
  , playbackDurationSelector
  , playbackStoreIDSelector
  , podcastPersistentIDSelector
  , podcastTitleSelector
  , preorderSelector
  , protectedAssetSelector
  , ratingSelector
  , releaseDateSelector
  , skipCountSelector
  , titlePropertyForGroupingTypeSelector
  , titleSelector
  , userGroupingSelector

  -- * Enum types
  , MPMediaGrouping(MPMediaGrouping)
  , pattern MPMediaGroupingTitle
  , pattern MPMediaGroupingAlbum
  , pattern MPMediaGroupingArtist
  , pattern MPMediaGroupingAlbumArtist
  , pattern MPMediaGroupingComposer
  , pattern MPMediaGroupingGenre
  , pattern MPMediaGroupingPlaylist
  , pattern MPMediaGroupingPodcastTitle
  , MPMediaType(MPMediaType)
  , pattern MPMediaTypeMusic
  , pattern MPMediaTypePodcast
  , pattern MPMediaTypeAudioBook
  , pattern MPMediaTypeAudioITunesU
  , pattern MPMediaTypeAnyAudio
  , pattern MPMediaTypeMovie
  , pattern MPMediaTypeTVShow
  , pattern MPMediaTypeVideoPodcast
  , pattern MPMediaTypeMusicVideo
  , pattern MPMediaTypeVideoITunesU
  , pattern MPMediaTypeHomeVideo
  , pattern MPMediaTypeAnyVideo
  , pattern MPMediaTypeAny

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.MediaPlayer.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ persistentIDPropertyForGroupingType:@
persistentIDPropertyForGroupingType :: MPMediaGrouping -> IO (Id NSString)
persistentIDPropertyForGroupingType groupingType =
  do
    cls' <- getRequiredClass "MPMediaItem"
    sendClassMessage cls' persistentIDPropertyForGroupingTypeSelector groupingType

-- | @+ titlePropertyForGroupingType:@
titlePropertyForGroupingType :: MPMediaGrouping -> IO (Id NSString)
titlePropertyForGroupingType groupingType =
  do
    cls' <- getRequiredClass "MPMediaItem"
    sendClassMessage cls' titlePropertyForGroupingTypeSelector groupingType

-- | @- persistentID@
persistentID :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO CULong
persistentID mpMediaItem =
  sendMessage mpMediaItem persistentIDSelector

-- | @- mediaType@
mediaType :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO MPMediaType
mediaType mpMediaItem =
  sendMessage mpMediaItem mediaTypeSelector

-- | @- title@
title :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO (Id NSString)
title mpMediaItem =
  sendMessage mpMediaItem titleSelector

-- | @- albumTitle@
albumTitle :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO (Id NSString)
albumTitle mpMediaItem =
  sendMessage mpMediaItem albumTitleSelector

-- | @- albumPersistentID@
albumPersistentID :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO CULong
albumPersistentID mpMediaItem =
  sendMessage mpMediaItem albumPersistentIDSelector

-- | @- artist@
artist :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO (Id NSString)
artist mpMediaItem =
  sendMessage mpMediaItem artistSelector

-- | @- artistPersistentID@
artistPersistentID :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO CULong
artistPersistentID mpMediaItem =
  sendMessage mpMediaItem artistPersistentIDSelector

-- | @- albumArtist@
albumArtist :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO (Id NSString)
albumArtist mpMediaItem =
  sendMessage mpMediaItem albumArtistSelector

-- | @- albumArtistPersistentID@
albumArtistPersistentID :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO CULong
albumArtistPersistentID mpMediaItem =
  sendMessage mpMediaItem albumArtistPersistentIDSelector

-- | @- genre@
genre :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO (Id NSString)
genre mpMediaItem =
  sendMessage mpMediaItem genreSelector

-- | @- genrePersistentID@
genrePersistentID :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO CULong
genrePersistentID mpMediaItem =
  sendMessage mpMediaItem genrePersistentIDSelector

-- | @- composer@
composer :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO (Id NSString)
composer mpMediaItem =
  sendMessage mpMediaItem composerSelector

-- | @- composerPersistentID@
composerPersistentID :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO CULong
composerPersistentID mpMediaItem =
  sendMessage mpMediaItem composerPersistentIDSelector

-- | @- playbackDuration@
playbackDuration :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO CDouble
playbackDuration mpMediaItem =
  sendMessage mpMediaItem playbackDurationSelector

-- | @- albumTrackNumber@
albumTrackNumber :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO CULong
albumTrackNumber mpMediaItem =
  sendMessage mpMediaItem albumTrackNumberSelector

-- | @- albumTrackCount@
albumTrackCount :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO CULong
albumTrackCount mpMediaItem =
  sendMessage mpMediaItem albumTrackCountSelector

-- | @- discNumber@
discNumber :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO CULong
discNumber mpMediaItem =
  sendMessage mpMediaItem discNumberSelector

-- | @- discCount@
discCount :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO CULong
discCount mpMediaItem =
  sendMessage mpMediaItem discCountSelector

-- | @- artwork@
artwork :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO (Id MPMediaItemArtwork)
artwork mpMediaItem =
  sendMessage mpMediaItem artworkSelector

-- | @- explicitItem@
explicitItem :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO Bool
explicitItem mpMediaItem =
  sendMessage mpMediaItem explicitItemSelector

-- | @- lyrics@
lyrics :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO (Id NSString)
lyrics mpMediaItem =
  sendMessage mpMediaItem lyricsSelector

-- | @- compilation@
compilation :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO Bool
compilation mpMediaItem =
  sendMessage mpMediaItem compilationSelector

-- | @- releaseDate@
releaseDate :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO (Id NSDate)
releaseDate mpMediaItem =
  sendMessage mpMediaItem releaseDateSelector

-- | @- beatsPerMinute@
beatsPerMinute :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO CULong
beatsPerMinute mpMediaItem =
  sendMessage mpMediaItem beatsPerMinuteSelector

-- | @- comments@
comments :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO (Id NSString)
comments mpMediaItem =
  sendMessage mpMediaItem commentsSelector

-- | @- assetURL@
assetURL :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO (Id NSURL)
assetURL mpMediaItem =
  sendMessage mpMediaItem assetURLSelector

-- | @- cloudItem@
cloudItem :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO Bool
cloudItem mpMediaItem =
  sendMessage mpMediaItem cloudItemSelector

-- | @- protectedAsset@
protectedAsset :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO Bool
protectedAsset mpMediaItem =
  sendMessage mpMediaItem protectedAssetSelector

-- | @- podcastTitle@
podcastTitle :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO (Id NSString)
podcastTitle mpMediaItem =
  sendMessage mpMediaItem podcastTitleSelector

-- | @- podcastPersistentID@
podcastPersistentID :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO CULong
podcastPersistentID mpMediaItem =
  sendMessage mpMediaItem podcastPersistentIDSelector

-- | @- playCount@
playCount :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO CULong
playCount mpMediaItem =
  sendMessage mpMediaItem playCountSelector

-- | @- skipCount@
skipCount :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO CULong
skipCount mpMediaItem =
  sendMessage mpMediaItem skipCountSelector

-- | @- rating@
rating :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO CULong
rating mpMediaItem =
  sendMessage mpMediaItem ratingSelector

-- | @- lastPlayedDate@
lastPlayedDate :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO (Id NSDate)
lastPlayedDate mpMediaItem =
  sendMessage mpMediaItem lastPlayedDateSelector

-- | @- userGrouping@
userGrouping :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO (Id NSString)
userGrouping mpMediaItem =
  sendMessage mpMediaItem userGroupingSelector

-- | @- bookmarkTime@
bookmarkTime :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO CDouble
bookmarkTime mpMediaItem =
  sendMessage mpMediaItem bookmarkTimeSelector

-- | @- dateAdded@
dateAdded :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO (Id NSDate)
dateAdded mpMediaItem =
  sendMessage mpMediaItem dateAddedSelector

-- | @- playbackStoreID@
playbackStoreID :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO (Id NSString)
playbackStoreID mpMediaItem =
  sendMessage mpMediaItem playbackStoreIDSelector

-- | @- preorder@
preorder :: IsMPMediaItem mpMediaItem => mpMediaItem -> IO Bool
preorder mpMediaItem =
  sendMessage mpMediaItem preorderSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @persistentIDPropertyForGroupingType:@
persistentIDPropertyForGroupingTypeSelector :: Selector '[MPMediaGrouping] (Id NSString)
persistentIDPropertyForGroupingTypeSelector = mkSelector "persistentIDPropertyForGroupingType:"

-- | @Selector@ for @titlePropertyForGroupingType:@
titlePropertyForGroupingTypeSelector :: Selector '[MPMediaGrouping] (Id NSString)
titlePropertyForGroupingTypeSelector = mkSelector "titlePropertyForGroupingType:"

-- | @Selector@ for @persistentID@
persistentIDSelector :: Selector '[] CULong
persistentIDSelector = mkSelector "persistentID"

-- | @Selector@ for @mediaType@
mediaTypeSelector :: Selector '[] MPMediaType
mediaTypeSelector = mkSelector "mediaType"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @albumTitle@
albumTitleSelector :: Selector '[] (Id NSString)
albumTitleSelector = mkSelector "albumTitle"

-- | @Selector@ for @albumPersistentID@
albumPersistentIDSelector :: Selector '[] CULong
albumPersistentIDSelector = mkSelector "albumPersistentID"

-- | @Selector@ for @artist@
artistSelector :: Selector '[] (Id NSString)
artistSelector = mkSelector "artist"

-- | @Selector@ for @artistPersistentID@
artistPersistentIDSelector :: Selector '[] CULong
artistPersistentIDSelector = mkSelector "artistPersistentID"

-- | @Selector@ for @albumArtist@
albumArtistSelector :: Selector '[] (Id NSString)
albumArtistSelector = mkSelector "albumArtist"

-- | @Selector@ for @albumArtistPersistentID@
albumArtistPersistentIDSelector :: Selector '[] CULong
albumArtistPersistentIDSelector = mkSelector "albumArtistPersistentID"

-- | @Selector@ for @genre@
genreSelector :: Selector '[] (Id NSString)
genreSelector = mkSelector "genre"

-- | @Selector@ for @genrePersistentID@
genrePersistentIDSelector :: Selector '[] CULong
genrePersistentIDSelector = mkSelector "genrePersistentID"

-- | @Selector@ for @composer@
composerSelector :: Selector '[] (Id NSString)
composerSelector = mkSelector "composer"

-- | @Selector@ for @composerPersistentID@
composerPersistentIDSelector :: Selector '[] CULong
composerPersistentIDSelector = mkSelector "composerPersistentID"

-- | @Selector@ for @playbackDuration@
playbackDurationSelector :: Selector '[] CDouble
playbackDurationSelector = mkSelector "playbackDuration"

-- | @Selector@ for @albumTrackNumber@
albumTrackNumberSelector :: Selector '[] CULong
albumTrackNumberSelector = mkSelector "albumTrackNumber"

-- | @Selector@ for @albumTrackCount@
albumTrackCountSelector :: Selector '[] CULong
albumTrackCountSelector = mkSelector "albumTrackCount"

-- | @Selector@ for @discNumber@
discNumberSelector :: Selector '[] CULong
discNumberSelector = mkSelector "discNumber"

-- | @Selector@ for @discCount@
discCountSelector :: Selector '[] CULong
discCountSelector = mkSelector "discCount"

-- | @Selector@ for @artwork@
artworkSelector :: Selector '[] (Id MPMediaItemArtwork)
artworkSelector = mkSelector "artwork"

-- | @Selector@ for @explicitItem@
explicitItemSelector :: Selector '[] Bool
explicitItemSelector = mkSelector "explicitItem"

-- | @Selector@ for @lyrics@
lyricsSelector :: Selector '[] (Id NSString)
lyricsSelector = mkSelector "lyrics"

-- | @Selector@ for @compilation@
compilationSelector :: Selector '[] Bool
compilationSelector = mkSelector "compilation"

-- | @Selector@ for @releaseDate@
releaseDateSelector :: Selector '[] (Id NSDate)
releaseDateSelector = mkSelector "releaseDate"

-- | @Selector@ for @beatsPerMinute@
beatsPerMinuteSelector :: Selector '[] CULong
beatsPerMinuteSelector = mkSelector "beatsPerMinute"

-- | @Selector@ for @comments@
commentsSelector :: Selector '[] (Id NSString)
commentsSelector = mkSelector "comments"

-- | @Selector@ for @assetURL@
assetURLSelector :: Selector '[] (Id NSURL)
assetURLSelector = mkSelector "assetURL"

-- | @Selector@ for @cloudItem@
cloudItemSelector :: Selector '[] Bool
cloudItemSelector = mkSelector "cloudItem"

-- | @Selector@ for @protectedAsset@
protectedAssetSelector :: Selector '[] Bool
protectedAssetSelector = mkSelector "protectedAsset"

-- | @Selector@ for @podcastTitle@
podcastTitleSelector :: Selector '[] (Id NSString)
podcastTitleSelector = mkSelector "podcastTitle"

-- | @Selector@ for @podcastPersistentID@
podcastPersistentIDSelector :: Selector '[] CULong
podcastPersistentIDSelector = mkSelector "podcastPersistentID"

-- | @Selector@ for @playCount@
playCountSelector :: Selector '[] CULong
playCountSelector = mkSelector "playCount"

-- | @Selector@ for @skipCount@
skipCountSelector :: Selector '[] CULong
skipCountSelector = mkSelector "skipCount"

-- | @Selector@ for @rating@
ratingSelector :: Selector '[] CULong
ratingSelector = mkSelector "rating"

-- | @Selector@ for @lastPlayedDate@
lastPlayedDateSelector :: Selector '[] (Id NSDate)
lastPlayedDateSelector = mkSelector "lastPlayedDate"

-- | @Selector@ for @userGrouping@
userGroupingSelector :: Selector '[] (Id NSString)
userGroupingSelector = mkSelector "userGrouping"

-- | @Selector@ for @bookmarkTime@
bookmarkTimeSelector :: Selector '[] CDouble
bookmarkTimeSelector = mkSelector "bookmarkTime"

-- | @Selector@ for @dateAdded@
dateAddedSelector :: Selector '[] (Id NSDate)
dateAddedSelector = mkSelector "dateAdded"

-- | @Selector@ for @playbackStoreID@
playbackStoreIDSelector :: Selector '[] (Id NSString)
playbackStoreIDSelector = mkSelector "playbackStoreID"

-- | @Selector@ for @preorder@
preorderSelector :: Selector '[] Bool
preorderSelector = mkSelector "preorder"

