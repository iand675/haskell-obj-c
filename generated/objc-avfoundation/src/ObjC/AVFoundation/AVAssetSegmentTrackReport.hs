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
  , initSelector
  , newSelector
  , trackIDSelector
  , mediaTypeSelector
  , firstVideoSampleInformationSelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAssetSegmentTrackReport avAssetSegmentTrackReport => avAssetSegmentTrackReport -> IO (Id AVAssetSegmentTrackReport)
init_ avAssetSegmentTrackReport  =
  sendMsg avAssetSegmentTrackReport (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVAssetSegmentTrackReport)
new  =
  do
    cls' <- getRequiredClass "AVAssetSegmentTrackReport"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | trackID
--
-- Indicates the persistent unique identifier for this track.
--
-- ObjC selector: @- trackID@
trackID :: IsAVAssetSegmentTrackReport avAssetSegmentTrackReport => avAssetSegmentTrackReport -> IO CInt
trackID avAssetSegmentTrackReport  =
  sendMsg avAssetSegmentTrackReport (mkSelector "trackID") retCInt []

-- | mediaType
--
-- Indicates the media type for this track. Media types are declared in AVMediaFormat.h.
--
-- ObjC selector: @- mediaType@
mediaType :: IsAVAssetSegmentTrackReport avAssetSegmentTrackReport => avAssetSegmentTrackReport -> IO (Id NSString)
mediaType avAssetSegmentTrackReport  =
  sendMsg avAssetSegmentTrackReport (mkSelector "mediaType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | firstVideoSampleInformation
--
-- Provides information on the first video sample in this track. The value is nil if this track is not video track or no information available.
--
-- ObjC selector: @- firstVideoSampleInformation@
firstVideoSampleInformation :: IsAVAssetSegmentTrackReport avAssetSegmentTrackReport => avAssetSegmentTrackReport -> IO (Id AVAssetSegmentReportSampleInformation)
firstVideoSampleInformation avAssetSegmentTrackReport  =
  sendMsg avAssetSegmentTrackReport (mkSelector "firstVideoSampleInformation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @trackID@
trackIDSelector :: Selector
trackIDSelector = mkSelector "trackID"

-- | @Selector@ for @mediaType@
mediaTypeSelector :: Selector
mediaTypeSelector = mkSelector "mediaType"

-- | @Selector@ for @firstVideoSampleInformation@
firstVideoSampleInformationSelector :: Selector
firstVideoSampleInformationSelector = mkSelector "firstVideoSampleInformation"

