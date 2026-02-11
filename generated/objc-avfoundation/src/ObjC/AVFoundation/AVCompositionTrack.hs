{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVCompositionTrack@.
module ObjC.AVFoundation.AVCompositionTrack
  ( AVCompositionTrack
  , IsAVCompositionTrack(..)
  , hasMediaCharacteristic
  , metadataForFormat
  , associatedTracksOfType
  , segments
  , formatDescriptionReplacements
  , hasMediaCharacteristicSelector
  , metadataForFormatSelector
  , associatedTracksOfTypeSelector
  , segmentsSelector
  , formatDescriptionReplacementsSelector


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

-- | @- hasMediaCharacteristic:@
hasMediaCharacteristic :: (IsAVCompositionTrack avCompositionTrack, IsNSString mediaCharacteristic) => avCompositionTrack -> mediaCharacteristic -> IO Bool
hasMediaCharacteristic avCompositionTrack  mediaCharacteristic =
withObjCPtr mediaCharacteristic $ \raw_mediaCharacteristic ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCompositionTrack (mkSelector "hasMediaCharacteristic:") retCULong [argPtr (castPtr raw_mediaCharacteristic :: Ptr ())]

-- | @- metadataForFormat:@
metadataForFormat :: (IsAVCompositionTrack avCompositionTrack, IsNSString format) => avCompositionTrack -> format -> IO (Id NSArray)
metadataForFormat avCompositionTrack  format =
withObjCPtr format $ \raw_format ->
    sendMsg avCompositionTrack (mkSelector "metadataForFormat:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ())] >>= retainedObject . castPtr

-- | @- associatedTracksOfType:@
associatedTracksOfType :: (IsAVCompositionTrack avCompositionTrack, IsNSString trackAssociationType) => avCompositionTrack -> trackAssociationType -> IO (Id NSArray)
associatedTracksOfType avCompositionTrack  trackAssociationType =
withObjCPtr trackAssociationType $ \raw_trackAssociationType ->
    sendMsg avCompositionTrack (mkSelector "associatedTracksOfType:") (retPtr retVoid) [argPtr (castPtr raw_trackAssociationType :: Ptr ())] >>= retainedObject . castPtr

-- | segments
--
-- Provides read-only access to the array of track segments, each an instance of AVCompositionTrackSegment.
--
-- Note that timeMapping.target.start of the first AVCompositionTrackSegment must be kCMTimeZero, and the timeMapping.target.start of each subsequent AVCompositionTrackSegment must equal CMTimeRangeGetEnd(the previous AVCompositionTrackSegment's timeMapping.target).      Use -validateTrackSegments:error: to perform a test to ensure that an array of AVCompositionTrackSegments conforms to this rule.
--
-- ObjC selector: @- segments@
segments :: IsAVCompositionTrack avCompositionTrack => avCompositionTrack -> IO (Id NSArray)
segments avCompositionTrack  =
  sendMsg avCompositionTrack (mkSelector "segments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | formatDescriptionReplacements
--
-- An array of AVCompositionTrackFormatDescriptionReplacement objects indicating original format descriptions and their replacements.
--
-- The value of this property is an array of AVCompositionTrackFormatDescriptionReplacement objects, each of which specifies an original format description together with its replacement format description (as specified by a previous call to -replaceFormatDescription:withFormatDescription:). Only format descriptions that are to be replaced will occur as the originalFormatDescription elements in the AVCompositionTrackFormatDescriptionReplacement objects in this array.
--
-- ObjC selector: @- formatDescriptionReplacements@
formatDescriptionReplacements :: IsAVCompositionTrack avCompositionTrack => avCompositionTrack -> IO (Id NSArray)
formatDescriptionReplacements avCompositionTrack  =
  sendMsg avCompositionTrack (mkSelector "formatDescriptionReplacements") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @hasMediaCharacteristic:@
hasMediaCharacteristicSelector :: Selector
hasMediaCharacteristicSelector = mkSelector "hasMediaCharacteristic:"

-- | @Selector@ for @metadataForFormat:@
metadataForFormatSelector :: Selector
metadataForFormatSelector = mkSelector "metadataForFormat:"

-- | @Selector@ for @associatedTracksOfType:@
associatedTracksOfTypeSelector :: Selector
associatedTracksOfTypeSelector = mkSelector "associatedTracksOfType:"

-- | @Selector@ for @segments@
segmentsSelector :: Selector
segmentsSelector = mkSelector "segments"

-- | @Selector@ for @formatDescriptionReplacements@
formatDescriptionReplacementsSelector :: Selector
formatDescriptionReplacementsSelector = mkSelector "formatDescriptionReplacements"

