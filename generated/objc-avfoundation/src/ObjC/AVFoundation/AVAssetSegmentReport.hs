{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAssetSegmentReport
--
-- This class provides information on a segment data.
--
-- Clients may get an instance of AVAssetSegmentReport through the -assetWriter:didOutputSegmentData:segmentType:segmentReport: delegate method, which is defined in AVAssetWriter.h. Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVAssetSegmentReport@.
module ObjC.AVFoundation.AVAssetSegmentReport
  ( AVAssetSegmentReport
  , IsAVAssetSegmentReport(..)
  , init_
  , new
  , segmentType
  , trackReports
  , initSelector
  , newSelector
  , segmentTypeSelector
  , trackReportsSelector

  -- * Enum types
  , AVAssetSegmentType(AVAssetSegmentType)
  , pattern AVAssetSegmentTypeInitialization
  , pattern AVAssetSegmentTypeSeparable

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
import ObjC.AVFoundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAssetSegmentReport avAssetSegmentReport => avAssetSegmentReport -> IO (Id AVAssetSegmentReport)
init_ avAssetSegmentReport  =
  sendMsg avAssetSegmentReport (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVAssetSegmentReport)
new  =
  do
    cls' <- getRequiredClass "AVAssetSegmentReport"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | segmentType
--
-- A segment type of the segment data.
--
-- ObjC selector: @- segmentType@
segmentType :: IsAVAssetSegmentReport avAssetSegmentReport => avAssetSegmentReport -> IO AVAssetSegmentType
segmentType avAssetSegmentReport  =
  fmap (coerce :: CLong -> AVAssetSegmentType) $ sendMsg avAssetSegmentReport (mkSelector "segmentType") retCLong []

-- | trackReports
--
-- Provides an array of AVAssetSegmentTrackReport in the segment data.
--
-- ObjC selector: @- trackReports@
trackReports :: IsAVAssetSegmentReport avAssetSegmentReport => avAssetSegmentReport -> IO (Id NSArray)
trackReports avAssetSegmentReport  =
  sendMsg avAssetSegmentReport (mkSelector "trackReports") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @segmentType@
segmentTypeSelector :: Selector
segmentTypeSelector = mkSelector "segmentType"

-- | @Selector@ for @trackReports@
trackReportsSelector :: Selector
trackReportsSelector = mkSelector "trackReports"

