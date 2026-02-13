{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAssetSegmentReportSampleInformation
--
-- This class is vended by AVAssetSegmentTrackReport. It will provide information on a sample in a track.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVAssetSegmentReportSampleInformation@.
module ObjC.AVFoundation.AVAssetSegmentReportSampleInformation
  ( AVAssetSegmentReportSampleInformation
  , IsAVAssetSegmentReportSampleInformation(..)
  , init_
  , new
  , offset
  , length_
  , isSyncSample
  , initSelector
  , isSyncSampleSelector
  , lengthSelector
  , newSelector
  , offsetSelector


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
init_ :: IsAVAssetSegmentReportSampleInformation avAssetSegmentReportSampleInformation => avAssetSegmentReportSampleInformation -> IO (Id AVAssetSegmentReportSampleInformation)
init_ avAssetSegmentReportSampleInformation =
  sendOwnedMessage avAssetSegmentReportSampleInformation initSelector

-- | @+ new@
new :: IO (Id AVAssetSegmentReportSampleInformation)
new  =
  do
    cls' <- getRequiredClass "AVAssetSegmentReportSampleInformation"
    sendOwnedClassMessage cls' newSelector

-- | offset
--
-- The offset of the sample in the segment.
--
-- ObjC selector: @- offset@
offset :: IsAVAssetSegmentReportSampleInformation avAssetSegmentReportSampleInformation => avAssetSegmentReportSampleInformation -> IO CLong
offset avAssetSegmentReportSampleInformation =
  sendMessage avAssetSegmentReportSampleInformation offsetSelector

-- | length
--
-- The length of the sample.
--
-- ObjC selector: @- length@
length_ :: IsAVAssetSegmentReportSampleInformation avAssetSegmentReportSampleInformation => avAssetSegmentReportSampleInformation -> IO CLong
length_ avAssetSegmentReportSampleInformation =
  sendMessage avAssetSegmentReportSampleInformation lengthSelector

-- | isSyncSample
--
-- Indicates whether the sample is a sync sample.
--
-- ObjC selector: @- isSyncSample@
isSyncSample :: IsAVAssetSegmentReportSampleInformation avAssetSegmentReportSampleInformation => avAssetSegmentReportSampleInformation -> IO Bool
isSyncSample avAssetSegmentReportSampleInformation =
  sendMessage avAssetSegmentReportSampleInformation isSyncSampleSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAssetSegmentReportSampleInformation)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVAssetSegmentReportSampleInformation)
newSelector = mkSelector "new"

-- | @Selector@ for @offset@
offsetSelector :: Selector '[] CLong
offsetSelector = mkSelector "offset"

-- | @Selector@ for @length@
lengthSelector :: Selector '[] CLong
lengthSelector = mkSelector "length"

-- | @Selector@ for @isSyncSample@
isSyncSampleSelector :: Selector '[] Bool
isSyncSampleSelector = mkSelector "isSyncSample"

