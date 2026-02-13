{-# LANGUAGE DataKinds #-}
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
  , associatedTracksOfTypeSelector
  , formatDescriptionReplacementsSelector
  , hasMediaCharacteristicSelector
  , metadataForFormatSelector
  , segmentsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- hasMediaCharacteristic:@
hasMediaCharacteristic :: (IsAVCompositionTrack avCompositionTrack, IsNSString mediaCharacteristic) => avCompositionTrack -> mediaCharacteristic -> IO Bool
hasMediaCharacteristic avCompositionTrack mediaCharacteristic =
  sendMessage avCompositionTrack hasMediaCharacteristicSelector (toNSString mediaCharacteristic)

-- | @- metadataForFormat:@
metadataForFormat :: (IsAVCompositionTrack avCompositionTrack, IsNSString format) => avCompositionTrack -> format -> IO (Id NSArray)
metadataForFormat avCompositionTrack format =
  sendMessage avCompositionTrack metadataForFormatSelector (toNSString format)

-- | @- associatedTracksOfType:@
associatedTracksOfType :: (IsAVCompositionTrack avCompositionTrack, IsNSString trackAssociationType) => avCompositionTrack -> trackAssociationType -> IO (Id NSArray)
associatedTracksOfType avCompositionTrack trackAssociationType =
  sendMessage avCompositionTrack associatedTracksOfTypeSelector (toNSString trackAssociationType)

-- | segments
--
-- Provides read-only access to the array of track segments, each an instance of AVCompositionTrackSegment.
--
-- Note that timeMapping.target.start of the first AVCompositionTrackSegment must be kCMTimeZero, and the timeMapping.target.start of each subsequent AVCompositionTrackSegment must equal CMTimeRangeGetEnd(the previous AVCompositionTrackSegment's timeMapping.target).      Use -validateTrackSegments:error: to perform a test to ensure that an array of AVCompositionTrackSegments conforms to this rule.
--
-- ObjC selector: @- segments@
segments :: IsAVCompositionTrack avCompositionTrack => avCompositionTrack -> IO (Id NSArray)
segments avCompositionTrack =
  sendMessage avCompositionTrack segmentsSelector

-- | formatDescriptionReplacements
--
-- An array of AVCompositionTrackFormatDescriptionReplacement objects indicating original format descriptions and their replacements.
--
-- The value of this property is an array of AVCompositionTrackFormatDescriptionReplacement objects, each of which specifies an original format description together with its replacement format description (as specified by a previous call to -replaceFormatDescription:withFormatDescription:). Only format descriptions that are to be replaced will occur as the originalFormatDescription elements in the AVCompositionTrackFormatDescriptionReplacement objects in this array.
--
-- ObjC selector: @- formatDescriptionReplacements@
formatDescriptionReplacements :: IsAVCompositionTrack avCompositionTrack => avCompositionTrack -> IO (Id NSArray)
formatDescriptionReplacements avCompositionTrack =
  sendMessage avCompositionTrack formatDescriptionReplacementsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @hasMediaCharacteristic:@
hasMediaCharacteristicSelector :: Selector '[Id NSString] Bool
hasMediaCharacteristicSelector = mkSelector "hasMediaCharacteristic:"

-- | @Selector@ for @metadataForFormat:@
metadataForFormatSelector :: Selector '[Id NSString] (Id NSArray)
metadataForFormatSelector = mkSelector "metadataForFormat:"

-- | @Selector@ for @associatedTracksOfType:@
associatedTracksOfTypeSelector :: Selector '[Id NSString] (Id NSArray)
associatedTracksOfTypeSelector = mkSelector "associatedTracksOfType:"

-- | @Selector@ for @segments@
segmentsSelector :: Selector '[] (Id NSArray)
segmentsSelector = mkSelector "segments"

-- | @Selector@ for @formatDescriptionReplacements@
formatDescriptionReplacementsSelector :: Selector '[] (Id NSArray)
formatDescriptionReplacementsSelector = mkSelector "formatDescriptionReplacements"

