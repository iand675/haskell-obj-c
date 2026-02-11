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
  , videosFilter
  , livePhotosFilter
  , spatialMediaFilter
  , playbackStyleFilterSelector
  , anyFilterMatchingSubfiltersSelector
  , allFilterMatchingSubfiltersSelector
  , notFilterOfSubfilterSelector
  , newSelector
  , initSelector
  , videosFilterSelector
  , livePhotosFilterSelector
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

-- | @Selector@ for @videosFilter@
videosFilterSelector :: Selector
videosFilterSelector = mkSelector "videosFilter"

-- | @Selector@ for @livePhotosFilter@
livePhotosFilterSelector :: Selector
livePhotosFilterSelector = mkSelector "livePhotosFilter"

-- | @Selector@ for @spatialMediaFilter@
spatialMediaFilterSelector :: Selector
spatialMediaFilterSelector = mkSelector "spatialMediaFilter"

