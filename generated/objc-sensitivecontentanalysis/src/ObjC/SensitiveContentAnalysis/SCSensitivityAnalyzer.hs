{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Main class for content sensitivity analysis
--
-- Generated bindings for @SCSensitivityAnalyzer@.
module ObjC.SensitiveContentAnalysis.SCSensitivityAnalyzer
  ( SCSensitivityAnalyzer
  , IsSCSensitivityAnalyzer(..)
  , init_
  , analyzeImageFile_completionHandler
  , analyzeCGImage_completionHandler
  , analyzeVideoFile_completionHandler
  , analysisPolicy
  , initSelector
  , analyzeImageFile_completionHandlerSelector
  , analyzeCGImage_completionHandlerSelector
  , analyzeVideoFile_completionHandlerSelector
  , analysisPolicySelector

  -- * Enum types
  , SCSensitivityAnalysisPolicy(SCSensitivityAnalysisPolicy)
  , pattern SCSensitivityAnalysisPolicyDisabled
  , pattern SCSensitivityAnalysisPolicySimpleInterventions
  , pattern SCSensitivityAnalysisPolicyDescriptiveInterventions

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

import ObjC.SensitiveContentAnalysis.Internal.Classes
import ObjC.SensitiveContentAnalysis.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSCSensitivityAnalyzer scSensitivityAnalyzer => scSensitivityAnalyzer -> IO (Id SCSensitivityAnalyzer)
init_ scSensitivityAnalyzer  =
  sendMsg scSensitivityAnalyzer (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Analyze sensitivity of Image File on disk (only local fileURL)
--
-- @fileURL@ — Image file location on disk
--
-- @completionHandler@ — Block to be called on completion (callback is called on unspecified queue)
--
-- ObjC selector: @- analyzeImageFile:completionHandler:@
analyzeImageFile_completionHandler :: (IsSCSensitivityAnalyzer scSensitivityAnalyzer, IsNSURL fileURL) => scSensitivityAnalyzer -> fileURL -> Ptr () -> IO ()
analyzeImageFile_completionHandler scSensitivityAnalyzer  fileURL completionHandler =
withObjCPtr fileURL $ \raw_fileURL ->
    sendMsg scSensitivityAnalyzer (mkSelector "analyzeImageFile:completionHandler:") retVoid [argPtr (castPtr raw_fileURL :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Analyze sensitivity of CGImage in memory
--
-- @image@ — CGImage reference
--
-- @completionHandler@ — Block to be called on completion (callback is called on unspecified queue)
--
-- ObjC selector: @- analyzeCGImage:completionHandler:@
analyzeCGImage_completionHandler :: IsSCSensitivityAnalyzer scSensitivityAnalyzer => scSensitivityAnalyzer -> Ptr () -> Ptr () -> IO ()
analyzeCGImage_completionHandler scSensitivityAnalyzer  image completionHandler =
  sendMsg scSensitivityAnalyzer (mkSelector "analyzeCGImage:completionHandler:") retVoid [argPtr image, argPtr (castPtr completionHandler :: Ptr ())]

-- | Analyze sensitivity of Video File on disk.
--
-- @fileURL@ — Video file location on disk
--
-- @completionHandler@ — Block to be called on completion (callback is called on unspecified queue)
--
-- Returns: An NSProgress instance for tracking video file analysis progress
--
-- ObjC selector: @- analyzeVideoFile:completionHandler:@
analyzeVideoFile_completionHandler :: (IsSCSensitivityAnalyzer scSensitivityAnalyzer, IsNSURL fileURL) => scSensitivityAnalyzer -> fileURL -> Ptr () -> IO (Id NSProgress)
analyzeVideoFile_completionHandler scSensitivityAnalyzer  fileURL completionHandler =
withObjCPtr fileURL $ \raw_fileURL ->
    sendMsg scSensitivityAnalyzer (mkSelector "analyzeVideoFile:completionHandler:") (retPtr retVoid) [argPtr (castPtr raw_fileURL :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= retainedObject . castPtr

-- | Current SCSensitivityAnalysisPolicy set on device. Can be used to determine whether analysis is available or not
--
-- ObjC selector: @- analysisPolicy@
analysisPolicy :: IsSCSensitivityAnalyzer scSensitivityAnalyzer => scSensitivityAnalyzer -> IO SCSensitivityAnalysisPolicy
analysisPolicy scSensitivityAnalyzer  =
  fmap (coerce :: CLong -> SCSensitivityAnalysisPolicy) $ sendMsg scSensitivityAnalyzer (mkSelector "analysisPolicy") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @analyzeImageFile:completionHandler:@
analyzeImageFile_completionHandlerSelector :: Selector
analyzeImageFile_completionHandlerSelector = mkSelector "analyzeImageFile:completionHandler:"

-- | @Selector@ for @analyzeCGImage:completionHandler:@
analyzeCGImage_completionHandlerSelector :: Selector
analyzeCGImage_completionHandlerSelector = mkSelector "analyzeCGImage:completionHandler:"

-- | @Selector@ for @analyzeVideoFile:completionHandler:@
analyzeVideoFile_completionHandlerSelector :: Selector
analyzeVideoFile_completionHandlerSelector = mkSelector "analyzeVideoFile:completionHandler:"

-- | @Selector@ for @analysisPolicy@
analysisPolicySelector :: Selector
analysisPolicySelector = mkSelector "analysisPolicy"

