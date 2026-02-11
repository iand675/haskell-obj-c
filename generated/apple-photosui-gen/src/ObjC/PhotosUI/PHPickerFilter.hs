{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A filter that restricts which types of assets @PHPickerViewController@ can show.
--
-- Generated bindings for @PHPickerFilter@.
module ObjC.PhotosUI.PHPickerFilter
  ( PHPickerFilter
  , IsPHPickerFilter(..)
  , playbackStyleFilter
  , anyFilterMatchingSubfilters
  , allFilterMatchingSubfilters
  , notFilterOfSubfilter
  , new
  , init_
  , imagesFilter
  , videosFilter
  , livePhotosFilter
  , depthEffectPhotosFilter
  , burstsFilter
  , panoramasFilter
  , screenshotsFilter
  , screenRecordingsFilter
  , cinematicVideosFilter
  , slomoVideosFilter
  , timelapseVideosFilter
  , spatialMediaFilter
  , playbackStyleFilterSelector
  , anyFilterMatchingSubfiltersSelector
  , allFilterMatchingSubfiltersSelector
  , notFilterOfSubfilterSelector
  , newSelector
  , initSelector
  , imagesFilterSelector
  , videosFilterSelector
  , livePhotosFilterSelector
  , depthEffectPhotosFilterSelector
  , burstsFilterSelector
  , panoramasFilterSelector
  , screenshotsFilterSelector
  , screenRecordingsFilterSelector
  , cinematicVideosFilterSelector
  , slomoVideosFilterSelector
  , timelapseVideosFilterSelector
  , spatialMediaFilterSelector

  -- * Enum types
  , PHAssetPlaybackStyle(PHAssetPlaybackStyle)
  , pattern PHAssetPlaybackStyleUnsupported
  , pattern PHAssetPlaybackStyleImage
  , pattern PHAssetPlaybackStyleImageAnimated
  , pattern PHAssetPlaybackStyleLivePhoto
  , pattern PHAssetPlaybackStyleVideo
  , pattern PHAssetPlaybackStyleVideoLooping

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

import ObjC.PhotosUI.Internal.Classes
import ObjC.Photos.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Returns a new filter based on the asset playback style.
--
-- ObjC selector: @+ playbackStyleFilter:@
playbackStyleFilter :: PHAssetPlaybackStyle -> IO (Id PHPickerFilter)
playbackStyleFilter playbackStyle =
  do
    cls' <- getRequiredClass "PHPickerFilter"
    sendClassMsg cls' (mkSelector "playbackStyleFilter:") (retPtr retVoid) [argCLong (coerce playbackStyle)] >>= retainedObject . castPtr

-- | Returns a new filter formed by OR-ing the filters in a given array.
--
-- ObjC selector: @+ anyFilterMatchingSubfilters:@
anyFilterMatchingSubfilters :: IsNSArray subfilters => subfilters -> IO (Id PHPickerFilter)
anyFilterMatchingSubfilters subfilters =
  do
    cls' <- getRequiredClass "PHPickerFilter"
    withObjCPtr subfilters $ \raw_subfilters ->
      sendClassMsg cls' (mkSelector "anyFilterMatchingSubfilters:") (retPtr retVoid) [argPtr (castPtr raw_subfilters :: Ptr ())] >>= retainedObject . castPtr

-- | Returns a new filter formed by AND-ing the filters in a given array.
--
-- ObjC selector: @+ allFilterMatchingSubfilters:@
allFilterMatchingSubfilters :: IsNSArray subfilters => subfilters -> IO (Id PHPickerFilter)
allFilterMatchingSubfilters subfilters =
  do
    cls' <- getRequiredClass "PHPickerFilter"
    withObjCPtr subfilters $ \raw_subfilters ->
      sendClassMsg cls' (mkSelector "allFilterMatchingSubfilters:") (retPtr retVoid) [argPtr (castPtr raw_subfilters :: Ptr ())] >>= retainedObject . castPtr

-- | Returns a new filter formed by negating the given filter.
--
-- ObjC selector: @+ notFilterOfSubfilter:@
notFilterOfSubfilter :: IsPHPickerFilter subfilter => subfilter -> IO (Id PHPickerFilter)
notFilterOfSubfilter subfilter =
  do
    cls' <- getRequiredClass "PHPickerFilter"
    withObjCPtr subfilter $ \raw_subfilter ->
      sendClassMsg cls' (mkSelector "notFilterOfSubfilter:") (retPtr retVoid) [argPtr (castPtr raw_subfilter :: Ptr ())] >>= retainedObject . castPtr

-- | @+ new@
new :: IO (Id PHPickerFilter)
new  =
  do
    cls' <- getRequiredClass "PHPickerFilter"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsPHPickerFilter phPickerFilter => phPickerFilter -> IO (Id PHPickerFilter)
init_ phPickerFilter  =
    sendMsg phPickerFilter (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The filter for images.
--
-- ObjC selector: @+ imagesFilter@
imagesFilter :: IO (Id PHPickerFilter)
imagesFilter  =
  do
    cls' <- getRequiredClass "PHPickerFilter"
    sendClassMsg cls' (mkSelector "imagesFilter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The filter for videos.
--
-- ObjC selector: @+ videosFilter@
videosFilter :: IO (Id PHPickerFilter)
videosFilter  =
  do
    cls' <- getRequiredClass "PHPickerFilter"
    sendClassMsg cls' (mkSelector "videosFilter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The filter for live photos.
--
-- ObjC selector: @+ livePhotosFilter@
livePhotosFilter :: IO (Id PHPickerFilter)
livePhotosFilter  =
  do
    cls' <- getRequiredClass "PHPickerFilter"
    sendClassMsg cls' (mkSelector "livePhotosFilter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The filter for Depth Effect photos.
--
-- ObjC selector: @+ depthEffectPhotosFilter@
depthEffectPhotosFilter :: IO (Id PHPickerFilter)
depthEffectPhotosFilter  =
  do
    cls' <- getRequiredClass "PHPickerFilter"
    sendClassMsg cls' (mkSelector "depthEffectPhotosFilter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The filter for bursts.
--
-- ObjC selector: @+ burstsFilter@
burstsFilter :: IO (Id PHPickerFilter)
burstsFilter  =
  do
    cls' <- getRequiredClass "PHPickerFilter"
    sendClassMsg cls' (mkSelector "burstsFilter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The filter for panorama photos.
--
-- ObjC selector: @+ panoramasFilter@
panoramasFilter :: IO (Id PHPickerFilter)
panoramasFilter  =
  do
    cls' <- getRequiredClass "PHPickerFilter"
    sendClassMsg cls' (mkSelector "panoramasFilter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The filter for screenshots.
--
-- ObjC selector: @+ screenshotsFilter@
screenshotsFilter :: IO (Id PHPickerFilter)
screenshotsFilter  =
  do
    cls' <- getRequiredClass "PHPickerFilter"
    sendClassMsg cls' (mkSelector "screenshotsFilter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The filter for screen recordings.
--
-- ObjC selector: @+ screenRecordingsFilter@
screenRecordingsFilter :: IO (Id PHPickerFilter)
screenRecordingsFilter  =
  do
    cls' <- getRequiredClass "PHPickerFilter"
    sendClassMsg cls' (mkSelector "screenRecordingsFilter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The filter for Cinematic videos.
--
-- ObjC selector: @+ cinematicVideosFilter@
cinematicVideosFilter :: IO (Id PHPickerFilter)
cinematicVideosFilter  =
  do
    cls' <- getRequiredClass "PHPickerFilter"
    sendClassMsg cls' (mkSelector "cinematicVideosFilter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The filter for Slow-Mo videos.
--
-- ObjC selector: @+ slomoVideosFilter@
slomoVideosFilter :: IO (Id PHPickerFilter)
slomoVideosFilter  =
  do
    cls' <- getRequiredClass "PHPickerFilter"
    sendClassMsg cls' (mkSelector "slomoVideosFilter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The filter for time-lapse videos.
--
-- ObjC selector: @+ timelapseVideosFilter@
timelapseVideosFilter :: IO (Id PHPickerFilter)
timelapseVideosFilter  =
  do
    cls' <- getRequiredClass "PHPickerFilter"
    sendClassMsg cls' (mkSelector "timelapseVideosFilter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The filter for spatial media.
--
-- ObjC selector: @+ spatialMediaFilter@
spatialMediaFilter :: IO (Id PHPickerFilter)
spatialMediaFilter  =
  do
    cls' <- getRequiredClass "PHPickerFilter"
    sendClassMsg cls' (mkSelector "spatialMediaFilter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @playbackStyleFilter:@
playbackStyleFilterSelector :: Selector
playbackStyleFilterSelector = mkSelector "playbackStyleFilter:"

-- | @Selector@ for @anyFilterMatchingSubfilters:@
anyFilterMatchingSubfiltersSelector :: Selector
anyFilterMatchingSubfiltersSelector = mkSelector "anyFilterMatchingSubfilters:"

-- | @Selector@ for @allFilterMatchingSubfilters:@
allFilterMatchingSubfiltersSelector :: Selector
allFilterMatchingSubfiltersSelector = mkSelector "allFilterMatchingSubfilters:"

-- | @Selector@ for @notFilterOfSubfilter:@
notFilterOfSubfilterSelector :: Selector
notFilterOfSubfilterSelector = mkSelector "notFilterOfSubfilter:"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @imagesFilter@
imagesFilterSelector :: Selector
imagesFilterSelector = mkSelector "imagesFilter"

-- | @Selector@ for @videosFilter@
videosFilterSelector :: Selector
videosFilterSelector = mkSelector "videosFilter"

-- | @Selector@ for @livePhotosFilter@
livePhotosFilterSelector :: Selector
livePhotosFilterSelector = mkSelector "livePhotosFilter"

-- | @Selector@ for @depthEffectPhotosFilter@
depthEffectPhotosFilterSelector :: Selector
depthEffectPhotosFilterSelector = mkSelector "depthEffectPhotosFilter"

-- | @Selector@ for @burstsFilter@
burstsFilterSelector :: Selector
burstsFilterSelector = mkSelector "burstsFilter"

-- | @Selector@ for @panoramasFilter@
panoramasFilterSelector :: Selector
panoramasFilterSelector = mkSelector "panoramasFilter"

-- | @Selector@ for @screenshotsFilter@
screenshotsFilterSelector :: Selector
screenshotsFilterSelector = mkSelector "screenshotsFilter"

-- | @Selector@ for @screenRecordingsFilter@
screenRecordingsFilterSelector :: Selector
screenRecordingsFilterSelector = mkSelector "screenRecordingsFilter"

-- | @Selector@ for @cinematicVideosFilter@
cinematicVideosFilterSelector :: Selector
cinematicVideosFilterSelector = mkSelector "cinematicVideosFilter"

-- | @Selector@ for @slomoVideosFilter@
slomoVideosFilterSelector :: Selector
slomoVideosFilterSelector = mkSelector "slomoVideosFilter"

-- | @Selector@ for @timelapseVideosFilter@
timelapseVideosFilterSelector :: Selector
timelapseVideosFilterSelector = mkSelector "timelapseVideosFilter"

-- | @Selector@ for @spatialMediaFilter@
spatialMediaFilterSelector :: Selector
spatialMediaFilterSelector = mkSelector "spatialMediaFilter"

