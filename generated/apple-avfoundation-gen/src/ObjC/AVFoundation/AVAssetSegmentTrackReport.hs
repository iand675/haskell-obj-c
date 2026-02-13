{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAssetSegmentTrackReport
--
-- This class is vended by AVAssetSegmentReport. It will provide information on a track in a segment data.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVAssetSegmentTrackReport@.
module ObjC.AVFoundation.AVAssetSegmentTrackReport
  ( AVAssetSegmentTrackReport
  , IsAVAssetSegmentTrackReport(..)
  , init_
  , new
  , trackID
  , mediaType
  , firstVideoSampleInformation
  , firstVideoSampleInformationSelector
  , initSelector
  , mediaTypeSelector
  , newSelector
  , trackIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAssetSegmentTrackReport avAssetSegmentTrackReport => avAssetSegmentTrackReport -> IO (Id AVAssetSegmentTrackReport)
init_ avAssetSegmentTrackReport =
  sendOwnedMessage avAssetSegmentTrackReport initSelector

-- | @+ new@
new :: IO (Id AVAssetSegmentTrackReport)
new  =
  do
    cls' <- getRequiredClass "AVAssetSegmentTrackReport"
    sendOwnedClassMessage cls' newSelector

-- | trackID
--
-- Indicates the persistent unique identifier for this track.
--
-- ObjC selector: @- trackID@
trackID :: IsAVAssetSegmentTrackReport avAssetSegmentTrackReport => avAssetSegmentTrackReport -> IO CInt
trackID avAssetSegmentTrackReport =
  sendMessage avAssetSegmentTrackReport trackIDSelector

-- | mediaType
--
-- Indicates the media type for this track. Media types are declared in AVMediaFormat.h.
--
-- ObjC selector: @- mediaType@
mediaType :: IsAVAssetSegmentTrackReport avAssetSegmentTrackReport => avAssetSegmentTrackReport -> IO (Id NSString)
mediaType avAssetSegmentTrackReport =
  sendMessage avAssetSegmentTrackReport mediaTypeSelector

-- | firstVideoSampleInformation
--
-- Provides information on the first video sample in this track. The value is nil if this track is not video track or no information available.
--
-- ObjC selector: @- firstVideoSampleInformation@
firstVideoSampleInformation :: IsAVAssetSegmentTrackReport avAssetSegmentTrackReport => avAssetSegmentTrackReport -> IO (Id AVAssetSegmentReportSampleInformation)
firstVideoSampleInformation avAssetSegmentTrackReport =
  sendMessage avAssetSegmentTrackReport firstVideoSampleInformationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAssetSegmentTrackReport)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVAssetSegmentTrackReport)
newSelector = mkSelector "new"

-- | @Selector@ for @trackID@
trackIDSelector :: Selector '[] CInt
trackIDSelector = mkSelector "trackID"

-- | @Selector@ for @mediaType@
mediaTypeSelector :: Selector '[] (Id NSString)
mediaTypeSelector = mkSelector "mediaType"

-- | @Selector@ for @firstVideoSampleInformation@
firstVideoSampleInformationSelector :: Selector '[] (Id AVAssetSegmentReportSampleInformation)
firstVideoSampleInformationSelector = mkSelector "firstVideoSampleInformation"

