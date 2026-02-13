{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.AVFoundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAssetSegmentReport avAssetSegmentReport => avAssetSegmentReport -> IO (Id AVAssetSegmentReport)
init_ avAssetSegmentReport =
  sendOwnedMessage avAssetSegmentReport initSelector

-- | @+ new@
new :: IO (Id AVAssetSegmentReport)
new  =
  do
    cls' <- getRequiredClass "AVAssetSegmentReport"
    sendOwnedClassMessage cls' newSelector

-- | segmentType
--
-- A segment type of the segment data.
--
-- ObjC selector: @- segmentType@
segmentType :: IsAVAssetSegmentReport avAssetSegmentReport => avAssetSegmentReport -> IO AVAssetSegmentType
segmentType avAssetSegmentReport =
  sendMessage avAssetSegmentReport segmentTypeSelector

-- | trackReports
--
-- Provides an array of AVAssetSegmentTrackReport in the segment data.
--
-- ObjC selector: @- trackReports@
trackReports :: IsAVAssetSegmentReport avAssetSegmentReport => avAssetSegmentReport -> IO (Id NSArray)
trackReports avAssetSegmentReport =
  sendMessage avAssetSegmentReport trackReportsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAssetSegmentReport)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVAssetSegmentReport)
newSelector = mkSelector "new"

-- | @Selector@ for @segmentType@
segmentTypeSelector :: Selector '[] AVAssetSegmentType
segmentTypeSelector = mkSelector "segmentType"

-- | @Selector@ for @trackReports@
trackReportsSelector :: Selector '[] (Id NSArray)
trackReportsSelector = mkSelector "trackReports"

