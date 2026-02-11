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
  , newSelector
  , offsetSelector
  , lengthSelector
  , isSyncSampleSelector


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
init_ :: IsAVAssetSegmentReportSampleInformation avAssetSegmentReportSampleInformation => avAssetSegmentReportSampleInformation -> IO (Id AVAssetSegmentReportSampleInformation)
init_ avAssetSegmentReportSampleInformation  =
  sendMsg avAssetSegmentReportSampleInformation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVAssetSegmentReportSampleInformation)
new  =
  do
    cls' <- getRequiredClass "AVAssetSegmentReportSampleInformation"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | offset
--
-- The offset of the sample in the segment.
--
-- ObjC selector: @- offset@
offset :: IsAVAssetSegmentReportSampleInformation avAssetSegmentReportSampleInformation => avAssetSegmentReportSampleInformation -> IO CLong
offset avAssetSegmentReportSampleInformation  =
  sendMsg avAssetSegmentReportSampleInformation (mkSelector "offset") retCLong []

-- | length
--
-- The length of the sample.
--
-- ObjC selector: @- length@
length_ :: IsAVAssetSegmentReportSampleInformation avAssetSegmentReportSampleInformation => avAssetSegmentReportSampleInformation -> IO CLong
length_ avAssetSegmentReportSampleInformation  =
  sendMsg avAssetSegmentReportSampleInformation (mkSelector "length") retCLong []

-- | isSyncSample
--
-- Indicates whether the sample is a sync sample.
--
-- ObjC selector: @- isSyncSample@
isSyncSample :: IsAVAssetSegmentReportSampleInformation avAssetSegmentReportSampleInformation => avAssetSegmentReportSampleInformation -> IO Bool
isSyncSample avAssetSegmentReportSampleInformation  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAssetSegmentReportSampleInformation (mkSelector "isSyncSample") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @offset@
offsetSelector :: Selector
offsetSelector = mkSelector "offset"

-- | @Selector@ for @length@
lengthSelector :: Selector
lengthSelector = mkSelector "length"

-- | @Selector@ for @isSyncSample@
isSyncSampleSelector :: Selector
isSyncSampleSelector = mkSelector "isSyncSample"

