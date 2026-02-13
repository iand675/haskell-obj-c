{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The ITLibMediaItemVideoInfo class encapsulates the video information of a video media item.
--
-- Generated bindings for @ITLibMediaItemVideoInfo@.
module ObjC.ITunesLibrary.ITLibMediaItemVideoInfo
  ( ITLibMediaItemVideoInfo
  , IsITLibMediaItemVideoInfo(..)
  , series
  , sortSeries
  , season
  , episode
  , episodeOrder
  , hd
  , videoWidth
  , videoHeight
  , episodeOrderSelector
  , episodeSelector
  , hdSelector
  , seasonSelector
  , seriesSelector
  , sortSeriesSelector
  , videoHeightSelector
  , videoWidthSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ITunesLibrary.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The name of TV series the video is associated with (implies track is a TV show).
--
-- ObjC selector: @- series@
series :: IsITLibMediaItemVideoInfo itLibMediaItemVideoInfo => itLibMediaItemVideoInfo -> IO (Id NSString)
series itLibMediaItemVideoInfo =
  sendMessage itLibMediaItemVideoInfo seriesSelector

-- | The name of the TV series the video is associated with that should be used for when sorting (implies the track is a TV show).
--
-- ObjC selector: @- sortSeries@
sortSeries :: IsITLibMediaItemVideoInfo itLibMediaItemVideoInfo => itLibMediaItemVideoInfo -> IO (Id NSString)
sortSeries itLibMediaItemVideoInfo =
  sendMessage itLibMediaItemVideoInfo sortSeriesSelector

-- | The name of TV season the video is associated with (implies the track is a TV show).
--
-- ObjC selector: @- season@
season :: IsITLibMediaItemVideoInfo itLibMediaItemVideoInfo => itLibMediaItemVideoInfo -> IO CULong
season itLibMediaItemVideoInfo =
  sendMessage itLibMediaItemVideoInfo seasonSelector

-- | The TV episode the video is associated with (implies the track is a TV show).
--
-- ObjC selector: @- episode@
episode :: IsITLibMediaItemVideoInfo itLibMediaItemVideoInfo => itLibMediaItemVideoInfo -> IO (Id NSString)
episode itLibMediaItemVideoInfo =
  sendMessage itLibMediaItemVideoInfo episodeSelector

-- | The TV episode order the video is associated with (implies the track is a TV show).
--
-- ObjC selector: @- episodeOrder@
episodeOrder :: IsITLibMediaItemVideoInfo itLibMediaItemVideoInfo => itLibMediaItemVideoInfo -> IO CLong
episodeOrder itLibMediaItemVideoInfo =
  sendMessage itLibMediaItemVideoInfo episodeOrderSelector

-- | Whether the video is high definition.
--
-- ObjC selector: @- hd@
hd :: IsITLibMediaItemVideoInfo itLibMediaItemVideoInfo => itLibMediaItemVideoInfo -> IO Bool
hd itLibMediaItemVideoInfo =
  sendMessage itLibMediaItemVideoInfo hdSelector

-- | The width of the video.
--
-- ObjC selector: @- videoWidth@
videoWidth :: IsITLibMediaItemVideoInfo itLibMediaItemVideoInfo => itLibMediaItemVideoInfo -> IO CULong
videoWidth itLibMediaItemVideoInfo =
  sendMessage itLibMediaItemVideoInfo videoWidthSelector

-- | The height of the video.
--
-- ObjC selector: @- videoHeight@
videoHeight :: IsITLibMediaItemVideoInfo itLibMediaItemVideoInfo => itLibMediaItemVideoInfo -> IO CULong
videoHeight itLibMediaItemVideoInfo =
  sendMessage itLibMediaItemVideoInfo videoHeightSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @series@
seriesSelector :: Selector '[] (Id NSString)
seriesSelector = mkSelector "series"

-- | @Selector@ for @sortSeries@
sortSeriesSelector :: Selector '[] (Id NSString)
sortSeriesSelector = mkSelector "sortSeries"

-- | @Selector@ for @season@
seasonSelector :: Selector '[] CULong
seasonSelector = mkSelector "season"

-- | @Selector@ for @episode@
episodeSelector :: Selector '[] (Id NSString)
episodeSelector = mkSelector "episode"

-- | @Selector@ for @episodeOrder@
episodeOrderSelector :: Selector '[] CLong
episodeOrderSelector = mkSelector "episodeOrder"

-- | @Selector@ for @hd@
hdSelector :: Selector '[] Bool
hdSelector = mkSelector "hd"

-- | @Selector@ for @videoWidth@
videoWidthSelector :: Selector '[] CULong
videoWidthSelector = mkSelector "videoWidth"

-- | @Selector@ for @videoHeight@
videoHeightSelector :: Selector '[] CULong
videoHeightSelector = mkSelector "videoHeight"

