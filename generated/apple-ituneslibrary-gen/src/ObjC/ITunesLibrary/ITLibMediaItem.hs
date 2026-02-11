{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A media item represents a single piece of media (such as a song, a video, a podcast, etc) in the iTunes library. 			  A media item has an overall unique identifier, accessed using the persistentID property. The media item			  metadata may be accessed through its individual properties or via the ITLibMediaEntity general property accessor			  methods.
--
-- Generated bindings for @ITLibMediaItem@.
module ObjC.ITunesLibrary.ITLibMediaItem
  ( ITLibMediaItem
  , IsITLibMediaItem(..)
  , title
  , sortTitle
  , artist
  , composer
  , sortComposer
  , rating
  , ratingComputed
  , startTime
  , stopTime
  , album
  , genre
  , kind
  , mediaKind
  , fileSize
  , size
  , totalTime
  , trackNumber
  , category
  , description
  , lyricsContentRating
  , contentRating
  , modifiedDate
  , addedDate
  , bitrate
  , sampleRate
  , beatsPerMinute
  , playCount
  , lastPlayedDate
  , playStatus
  , location
  , artworkAvailable
  , artwork
  , comments
  , purchased
  , cloud
  , drmProtected
  , video
  , videoInfo
  , releaseDate
  , year
  , fileType
  , skipCount
  , skipDate
  , voiceOverLanguage
  , volumeAdjustment
  , volumeNormalizationEnergy
  , userDisabled
  , grouping
  , locationType
  , titleSelector
  , sortTitleSelector
  , artistSelector
  , composerSelector
  , sortComposerSelector
  , ratingSelector
  , ratingComputedSelector
  , startTimeSelector
  , stopTimeSelector
  , albumSelector
  , genreSelector
  , kindSelector
  , mediaKindSelector
  , fileSizeSelector
  , sizeSelector
  , totalTimeSelector
  , trackNumberSelector
  , categorySelector
  , descriptionSelector
  , lyricsContentRatingSelector
  , contentRatingSelector
  , modifiedDateSelector
  , addedDateSelector
  , bitrateSelector
  , sampleRateSelector
  , beatsPerMinuteSelector
  , playCountSelector
  , lastPlayedDateSelector
  , playStatusSelector
  , locationSelector
  , artworkAvailableSelector
  , artworkSelector
  , commentsSelector
  , purchasedSelector
  , cloudSelector
  , drmProtectedSelector
  , videoSelector
  , videoInfoSelector
  , releaseDateSelector
  , yearSelector
  , fileTypeSelector
  , skipCountSelector
  , skipDateSelector
  , voiceOverLanguageSelector
  , volumeAdjustmentSelector
  , volumeNormalizationEnergySelector
  , userDisabledSelector
  , groupingSelector
  , locationTypeSelector

  -- * Enum types
  , ITLibMediaItemLocationType(ITLibMediaItemLocationType)
  , pattern ITLibMediaItemLocationTypeUnknown
  , pattern ITLibMediaItemLocationTypeFile
  , pattern ITLibMediaItemLocationTypeURL
  , pattern ITLibMediaItemLocationTypeRemote
  , ITLibMediaItemLyricsContentRating(ITLibMediaItemLyricsContentRating)
  , pattern ITLibMediaItemLyricsContentRatingNone
  , pattern ITLibMediaItemLyricsContentRatingExplicit
  , pattern ITLibMediaItemLyricsContentRatingClean
  , ITLibMediaItemMediaKind(ITLibMediaItemMediaKind)
  , pattern ITLibMediaItemMediaKindUnknown
  , pattern ITLibMediaItemMediaKindSong
  , pattern ITLibMediaItemMediaKindMovie
  , pattern ITLibMediaItemMediaKindPodcast
  , pattern ITLibMediaItemMediaKindAudiobook
  , pattern ITLibMediaItemMediaKindPDFBooklet
  , pattern ITLibMediaItemMediaKindMusicVideo
  , pattern ITLibMediaItemMediaKindTVShow
  , pattern ITLibMediaItemMediaKindInteractiveBooklet
  , pattern ITLibMediaItemMediaKindHomeVideo
  , pattern ITLibMediaItemMediaKindRingtone
  , pattern ITLibMediaItemMediaKindDigitalBooklet
  , pattern ITLibMediaItemMediaKindIOSApplication
  , pattern ITLibMediaItemMediaKindVoiceMemo
  , pattern ITLibMediaItemMediaKindiTunesU
  , pattern ITLibMediaItemMediaKindBook
  , pattern ITLibMediaItemMediaKindPDFBook
  , pattern ITLibMediaItemMediaKindAlertTone
  , ITLibMediaItemPlayStatus(ITLibMediaItemPlayStatus)
  , pattern ITLibMediaItemPlayStatusNone
  , pattern ITLibMediaItemPlayStatusPartiallyPlayed
  , pattern ITLibMediaItemPlayStatusUnplayed

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

import ObjC.ITunesLibrary.Internal.Classes
import ObjC.ITunesLibrary.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | The title of this media item. May be empty.
--
-- ObjC selector: @- title@
title :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id NSString)
title itLibMediaItem  =
    sendMsg itLibMediaItem (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The title of this media item that should be used for sorting purposes.  If nil, use the title field.
--
-- ObjC selector: @- sortTitle@
sortTitle :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id NSString)
sortTitle itLibMediaItem  =
    sendMsg itLibMediaItem (mkSelector "sortTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The artist associated with this media item.
--
-- ObjC selector: @- artist@
artist :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id ITLibArtist)
artist itLibMediaItem  =
    sendMsg itLibMediaItem (mkSelector "artist") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The name of the composer associated with this media item.  May be empty.
--
-- ObjC selector: @- composer@
composer :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id NSString)
composer itLibMediaItem  =
    sendMsg itLibMediaItem (mkSelector "composer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The name of the composer associated with this media item that should be used for sorting purposes. If nil, use the composer field.
--
-- ObjC selector: @- sortComposer@
sortComposer :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id NSString)
sortComposer itLibMediaItem  =
    sendMsg itLibMediaItem (mkSelector "sortComposer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The rating of this media item.
--
-- ObjC selector: @- rating@
rating :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO CLong
rating itLibMediaItem  =
    sendMsg itLibMediaItem (mkSelector "rating") retCLong []

-- | Whether this media item's rating is computed.
--
-- ObjC selector: @- ratingComputed@
ratingComputed :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO Bool
ratingComputed itLibMediaItem  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg itLibMediaItem (mkSelector "ratingComputed") retCULong []

-- | If non-zero, the actual time playback for this media item will start instead of 0:00 (in milliseconds).
--
-- ObjC selector: @- startTime@
startTime :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO CULong
startTime itLibMediaItem  =
    sendMsg itLibMediaItem (mkSelector "startTime") retCULong []

-- | If non-zero, the actual time playback for this media item will stop vs. the total time (in milliseconds).
--
-- ObjC selector: @- stopTime@
stopTime :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO CULong
stopTime itLibMediaItem  =
    sendMsg itLibMediaItem (mkSelector "stopTime") retCULong []

-- | The album where this media item belongs.
--
-- ObjC selector: @- album@
album :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id ITLibAlbum)
album itLibMediaItem  =
    sendMsg itLibMediaItem (mkSelector "album") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The genre associated with this media item. May be empty.
--
-- ObjC selector: @- genre@
genre :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id NSString)
genre itLibMediaItem  =
    sendMsg itLibMediaItem (mkSelector "genre") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | This media item's file kind (ex. MPEG audio file).
--
-- ObjC selector: @- kind@
kind :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id NSString)
kind itLibMediaItem  =
    sendMsg itLibMediaItem (mkSelector "kind") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | This media item's media kind.
--
-- ObjC selector: @- mediaKind@
mediaKind :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO ITLibMediaItemMediaKind
mediaKind itLibMediaItem  =
    fmap (coerce :: CULong -> ITLibMediaItemMediaKind) $ sendMsg itLibMediaItem (mkSelector "mediaKind") retCULong []

-- | The size in bytes of this media item on disk.
--
-- ObjC selector: @- fileSize@
fileSize :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO CULong
fileSize itLibMediaItem  =
    sendMsg itLibMediaItem (mkSelector "fileSize") retCULong []

-- | The size in bytes of this media item on disk. (deprecated: use fileSize instead)
--
-- ObjC selector: @- size@
size :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO CULong
size itLibMediaItem  =
    sendMsg itLibMediaItem (mkSelector "size") retCULong []

-- | The length of this media item in milliseconds.
--
-- ObjC selector: @- totalTime@
totalTime :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO CULong
totalTime itLibMediaItem  =
    sendMsg itLibMediaItem (mkSelector "totalTime") retCULong []

-- | The position of this media item within its album.
--
-- ObjC selector: @- trackNumber@
trackNumber :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO CULong
trackNumber itLibMediaItem  =
    sendMsg itLibMediaItem (mkSelector "trackNumber") retCULong []

-- | The podcast category of this media item (implies this media item is a podcast).
--
-- ObjC selector: @- category@
category :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id NSString)
category itLibMediaItem  =
    sendMsg itLibMediaItem (mkSelector "category") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Any podcast description of with this media item (implies this media item is a podcast).
--
-- ObjC selector: @- description@
description :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id NSString)
description itLibMediaItem  =
    sendMsg itLibMediaItem (mkSelector "description") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The content rating of this media item's lyrics.
--
-- ObjC selector: @- lyricsContentRating@
lyricsContentRating :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO ITLibMediaItemLyricsContentRating
lyricsContentRating itLibMediaItem  =
    fmap (coerce :: CULong -> ITLibMediaItemLyricsContentRating) $ sendMsg itLibMediaItem (mkSelector "lyricsContentRating") retCULong []

-- | The extended content rating of this media item.
--
-- ObjC selector: @- contentRating@
contentRating :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id NSString)
contentRating itLibMediaItem  =
    sendMsg itLibMediaItem (mkSelector "contentRating") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The date and time this media item was last modified.
--
-- ObjC selector: @- modifiedDate@
modifiedDate :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id NSDate)
modifiedDate itLibMediaItem  =
    sendMsg itLibMediaItem (mkSelector "modifiedDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The date and media item this media item was added to the iTunes database.
--
-- ObjC selector: @- addedDate@
addedDate :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id NSDate)
addedDate itLibMediaItem  =
    sendMsg itLibMediaItem (mkSelector "addedDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The bitrate of this media item in kbps.
--
-- ObjC selector: @- bitrate@
bitrate :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO CULong
bitrate itLibMediaItem  =
    sendMsg itLibMediaItem (mkSelector "bitrate") retCULong []

-- | The sample rate of this media item in samples per second.
--
-- ObjC selector: @- sampleRate@
sampleRate :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO CULong
sampleRate itLibMediaItem  =
    sendMsg itLibMediaItem (mkSelector "sampleRate") retCULong []

-- | The BPM (beats per minute) of this media item.
--
-- ObjC selector: @- beatsPerMinute@
beatsPerMinute :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO CULong
beatsPerMinute itLibMediaItem  =
    sendMsg itLibMediaItem (mkSelector "beatsPerMinute") retCULong []

-- | The number of times this media item has been played in iTunes.
--
-- ObjC selector: @- playCount@
playCount :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO CULong
playCount itLibMediaItem  =
    sendMsg itLibMediaItem (mkSelector "playCount") retCULong []

-- | The date and time this media item was last played in iTunes, or nil if this media item has not been played.
--
-- ObjC selector: @- lastPlayedDate@
lastPlayedDate :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id NSDate)
lastPlayedDate itLibMediaItem  =
    sendMsg itLibMediaItem (mkSelector "lastPlayedDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The play status for this media.  Represents partially played and unplayed states for videos and podcasts. Other media kinds always return "none".
--
-- ObjC selector: @- playStatus@
playStatus :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO ITLibMediaItemPlayStatus
playStatus itLibMediaItem  =
    fmap (coerce :: CULong -> ITLibMediaItemPlayStatus) $ sendMsg itLibMediaItem (mkSelector "playStatus") retCULong []

-- | The location of this media item on disk.
--
-- ObjC selector: @- location@
location :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id NSURL)
location itLibMediaItem  =
    sendMsg itLibMediaItem (mkSelector "location") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Whether this media item has artwork.
--
-- ObjC selector: @- artworkAvailable@
artworkAvailable :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO Bool
artworkAvailable itLibMediaItem  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg itLibMediaItem (mkSelector "artworkAvailable") retCULong []

-- | Whether this media item has artwork.
--
-- ObjC selector: @- artwork@
artwork :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id ITLibArtwork)
artwork itLibMediaItem  =
    sendMsg itLibMediaItem (mkSelector "artwork") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Any comments associated with this media item.
--
-- ObjC selector: @- comments@
comments :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id NSString)
comments itLibMediaItem  =
    sendMsg itLibMediaItem (mkSelector "comments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Whether this media item was purchased.
--
-- ObjC selector: @- purchased@
purchased :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO Bool
purchased itLibMediaItem  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg itLibMediaItem (mkSelector "purchased") retCULong []

-- | Whether this media item is iTunes Match or iTunes in the Cloud.
--
-- ObjC selector: @- cloud@
cloud :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO Bool
cloud itLibMediaItem  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg itLibMediaItem (mkSelector "cloud") retCULong []

-- | Whether this media item is DRM protected.
--
-- ObjC selector: @- drmProtected@
drmProtected :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO Bool
drmProtected itLibMediaItem  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg itLibMediaItem (mkSelector "drmProtected") retCULong []

-- | Whether this media item is a video media item (video podcast, movie, etc).
--
-- ObjC selector: @- video@
video :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO Bool
video itLibMediaItem  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg itLibMediaItem (mkSelector "video") retCULong []

-- | The video information of this media item (implies this media item is a video media item).
--
-- ObjC selector: @- videoInfo@
videoInfo :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id ITLibMediaItemVideoInfo)
videoInfo itLibMediaItem  =
    sendMsg itLibMediaItem (mkSelector "videoInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The date this media item was released.
--
-- ObjC selector: @- releaseDate@
releaseDate :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id NSDate)
releaseDate itLibMediaItem  =
    sendMsg itLibMediaItem (mkSelector "releaseDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The year when this media item was released.
--
-- ObjC selector: @- year@
year :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO CULong
year itLibMediaItem  =
    sendMsg itLibMediaItem (mkSelector "year") retCULong []

-- | The type of the file this media item refers to.
--
-- ObjC selector: @- fileType@
fileType :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO CULong
fileType itLibMediaItem  =
    sendMsg itLibMediaItem (mkSelector "fileType") retCULong []

-- | The number of times this media item has been skiped.
--
-- ObjC selector: @- skipCount@
skipCount :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO CULong
skipCount itLibMediaItem  =
    sendMsg itLibMediaItem (mkSelector "skipCount") retCULong []

-- | The date and time when this media item was last skipped.
--
-- ObjC selector: @- skipDate@
skipDate :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id NSDate)
skipDate itLibMediaItem  =
    sendMsg itLibMediaItem (mkSelector "skipDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The voice-over language of this media item
--
-- ObjC selector: @- voiceOverLanguage@
voiceOverLanguage :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO RawId
voiceOverLanguage itLibMediaItem  =
    fmap (RawId . castPtr) $ sendMsg itLibMediaItem (mkSelector "voiceOverLanguage") (retPtr retVoid) []

-- | The volume adjustment used for this media item if any.
--
-- ObjC selector: @- volumeAdjustment@
volumeAdjustment :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO CLong
volumeAdjustment itLibMediaItem  =
    sendMsg itLibMediaItem (mkSelector "volumeAdjustment") retCLong []

-- | The volume normalization energy applied to this media item.
--
-- ObjC selector: @- volumeNormalizationEnergy@
volumeNormalizationEnergy :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO CULong
volumeNormalizationEnergy itLibMediaItem  =
    sendMsg itLibMediaItem (mkSelector "volumeNormalizationEnergy") retCULong []

-- | Whether the user has disabled this media item.
--
-- ObjC selector: @- userDisabled@
userDisabled :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO Bool
userDisabled itLibMediaItem  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg itLibMediaItem (mkSelector "userDisabled") retCULong []

-- | The grouping of this media item.
--
-- ObjC selector: @- grouping@
grouping :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO (Id NSString)
grouping itLibMediaItem  =
    sendMsg itLibMediaItem (mkSelector "grouping") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The type of this media item with respect to its location.
--
-- ObjC selector: @- locationType@
locationType :: IsITLibMediaItem itLibMediaItem => itLibMediaItem -> IO ITLibMediaItemLocationType
locationType itLibMediaItem  =
    fmap (coerce :: CULong -> ITLibMediaItemLocationType) $ sendMsg itLibMediaItem (mkSelector "locationType") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @sortTitle@
sortTitleSelector :: Selector
sortTitleSelector = mkSelector "sortTitle"

-- | @Selector@ for @artist@
artistSelector :: Selector
artistSelector = mkSelector "artist"

-- | @Selector@ for @composer@
composerSelector :: Selector
composerSelector = mkSelector "composer"

-- | @Selector@ for @sortComposer@
sortComposerSelector :: Selector
sortComposerSelector = mkSelector "sortComposer"

-- | @Selector@ for @rating@
ratingSelector :: Selector
ratingSelector = mkSelector "rating"

-- | @Selector@ for @ratingComputed@
ratingComputedSelector :: Selector
ratingComputedSelector = mkSelector "ratingComputed"

-- | @Selector@ for @startTime@
startTimeSelector :: Selector
startTimeSelector = mkSelector "startTime"

-- | @Selector@ for @stopTime@
stopTimeSelector :: Selector
stopTimeSelector = mkSelector "stopTime"

-- | @Selector@ for @album@
albumSelector :: Selector
albumSelector = mkSelector "album"

-- | @Selector@ for @genre@
genreSelector :: Selector
genreSelector = mkSelector "genre"

-- | @Selector@ for @kind@
kindSelector :: Selector
kindSelector = mkSelector "kind"

-- | @Selector@ for @mediaKind@
mediaKindSelector :: Selector
mediaKindSelector = mkSelector "mediaKind"

-- | @Selector@ for @fileSize@
fileSizeSelector :: Selector
fileSizeSelector = mkSelector "fileSize"

-- | @Selector@ for @size@
sizeSelector :: Selector
sizeSelector = mkSelector "size"

-- | @Selector@ for @totalTime@
totalTimeSelector :: Selector
totalTimeSelector = mkSelector "totalTime"

-- | @Selector@ for @trackNumber@
trackNumberSelector :: Selector
trackNumberSelector = mkSelector "trackNumber"

-- | @Selector@ for @category@
categorySelector :: Selector
categorySelector = mkSelector "category"

-- | @Selector@ for @description@
descriptionSelector :: Selector
descriptionSelector = mkSelector "description"

-- | @Selector@ for @lyricsContentRating@
lyricsContentRatingSelector :: Selector
lyricsContentRatingSelector = mkSelector "lyricsContentRating"

-- | @Selector@ for @contentRating@
contentRatingSelector :: Selector
contentRatingSelector = mkSelector "contentRating"

-- | @Selector@ for @modifiedDate@
modifiedDateSelector :: Selector
modifiedDateSelector = mkSelector "modifiedDate"

-- | @Selector@ for @addedDate@
addedDateSelector :: Selector
addedDateSelector = mkSelector "addedDate"

-- | @Selector@ for @bitrate@
bitrateSelector :: Selector
bitrateSelector = mkSelector "bitrate"

-- | @Selector@ for @sampleRate@
sampleRateSelector :: Selector
sampleRateSelector = mkSelector "sampleRate"

-- | @Selector@ for @beatsPerMinute@
beatsPerMinuteSelector :: Selector
beatsPerMinuteSelector = mkSelector "beatsPerMinute"

-- | @Selector@ for @playCount@
playCountSelector :: Selector
playCountSelector = mkSelector "playCount"

-- | @Selector@ for @lastPlayedDate@
lastPlayedDateSelector :: Selector
lastPlayedDateSelector = mkSelector "lastPlayedDate"

-- | @Selector@ for @playStatus@
playStatusSelector :: Selector
playStatusSelector = mkSelector "playStatus"

-- | @Selector@ for @location@
locationSelector :: Selector
locationSelector = mkSelector "location"

-- | @Selector@ for @artworkAvailable@
artworkAvailableSelector :: Selector
artworkAvailableSelector = mkSelector "artworkAvailable"

-- | @Selector@ for @artwork@
artworkSelector :: Selector
artworkSelector = mkSelector "artwork"

-- | @Selector@ for @comments@
commentsSelector :: Selector
commentsSelector = mkSelector "comments"

-- | @Selector@ for @purchased@
purchasedSelector :: Selector
purchasedSelector = mkSelector "purchased"

-- | @Selector@ for @cloud@
cloudSelector :: Selector
cloudSelector = mkSelector "cloud"

-- | @Selector@ for @drmProtected@
drmProtectedSelector :: Selector
drmProtectedSelector = mkSelector "drmProtected"

-- | @Selector@ for @video@
videoSelector :: Selector
videoSelector = mkSelector "video"

-- | @Selector@ for @videoInfo@
videoInfoSelector :: Selector
videoInfoSelector = mkSelector "videoInfo"

-- | @Selector@ for @releaseDate@
releaseDateSelector :: Selector
releaseDateSelector = mkSelector "releaseDate"

-- | @Selector@ for @year@
yearSelector :: Selector
yearSelector = mkSelector "year"

-- | @Selector@ for @fileType@
fileTypeSelector :: Selector
fileTypeSelector = mkSelector "fileType"

-- | @Selector@ for @skipCount@
skipCountSelector :: Selector
skipCountSelector = mkSelector "skipCount"

-- | @Selector@ for @skipDate@
skipDateSelector :: Selector
skipDateSelector = mkSelector "skipDate"

-- | @Selector@ for @voiceOverLanguage@
voiceOverLanguageSelector :: Selector
voiceOverLanguageSelector = mkSelector "voiceOverLanguage"

-- | @Selector@ for @volumeAdjustment@
volumeAdjustmentSelector :: Selector
volumeAdjustmentSelector = mkSelector "volumeAdjustment"

-- | @Selector@ for @volumeNormalizationEnergy@
volumeNormalizationEnergySelector :: Selector
volumeNormalizationEnergySelector = mkSelector "volumeNormalizationEnergy"

-- | @Selector@ for @userDisabled@
userDisabledSelector :: Selector
userDisabledSelector = mkSelector "userDisabled"

-- | @Selector@ for @grouping@
groupingSelector :: Selector
groupingSelector = mkSelector "grouping"

-- | @Selector@ for @locationType@
locationTypeSelector :: Selector
locationTypeSelector = mkSelector "locationType"

