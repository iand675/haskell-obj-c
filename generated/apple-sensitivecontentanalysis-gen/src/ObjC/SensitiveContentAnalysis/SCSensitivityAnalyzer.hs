{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , analysisPolicySelector
  , analyzeCGImage_completionHandlerSelector
  , analyzeImageFile_completionHandlerSelector
  , analyzeVideoFile_completionHandlerSelector
  , initSelector

  -- * Enum types
  , SCSensitivityAnalysisPolicy(SCSensitivityAnalysisPolicy)
  , pattern SCSensitivityAnalysisPolicyDisabled
  , pattern SCSensitivityAnalysisPolicySimpleInterventions
  , pattern SCSensitivityAnalysisPolicyDescriptiveInterventions

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SensitiveContentAnalysis.Internal.Classes
import ObjC.SensitiveContentAnalysis.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSCSensitivityAnalyzer scSensitivityAnalyzer => scSensitivityAnalyzer -> IO (Id SCSensitivityAnalyzer)
init_ scSensitivityAnalyzer =
  sendOwnedMessage scSensitivityAnalyzer initSelector

-- | Analyze sensitivity of Image File on disk (only local fileURL)
--
-- @fileURL@ — Image file location on disk
--
-- @completionHandler@ — Block to be called on completion (callback is called on unspecified queue)
--
-- ObjC selector: @- analyzeImageFile:completionHandler:@
analyzeImageFile_completionHandler :: (IsSCSensitivityAnalyzer scSensitivityAnalyzer, IsNSURL fileURL) => scSensitivityAnalyzer -> fileURL -> Ptr () -> IO ()
analyzeImageFile_completionHandler scSensitivityAnalyzer fileURL completionHandler =
  sendMessage scSensitivityAnalyzer analyzeImageFile_completionHandlerSelector (toNSURL fileURL) completionHandler

-- | Analyze sensitivity of CGImage in memory
--
-- @image@ — CGImage reference
--
-- @completionHandler@ — Block to be called on completion (callback is called on unspecified queue)
--
-- ObjC selector: @- analyzeCGImage:completionHandler:@
analyzeCGImage_completionHandler :: IsSCSensitivityAnalyzer scSensitivityAnalyzer => scSensitivityAnalyzer -> Ptr () -> Ptr () -> IO ()
analyzeCGImage_completionHandler scSensitivityAnalyzer image completionHandler =
  sendMessage scSensitivityAnalyzer analyzeCGImage_completionHandlerSelector image completionHandler

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
analyzeVideoFile_completionHandler scSensitivityAnalyzer fileURL completionHandler =
  sendMessage scSensitivityAnalyzer analyzeVideoFile_completionHandlerSelector (toNSURL fileURL) completionHandler

-- | Current SCSensitivityAnalysisPolicy set on device. Can be used to determine whether analysis is available or not
--
-- ObjC selector: @- analysisPolicy@
analysisPolicy :: IsSCSensitivityAnalyzer scSensitivityAnalyzer => scSensitivityAnalyzer -> IO SCSensitivityAnalysisPolicy
analysisPolicy scSensitivityAnalyzer =
  sendMessage scSensitivityAnalyzer analysisPolicySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SCSensitivityAnalyzer)
initSelector = mkSelector "init"

-- | @Selector@ for @analyzeImageFile:completionHandler:@
analyzeImageFile_completionHandlerSelector :: Selector '[Id NSURL, Ptr ()] ()
analyzeImageFile_completionHandlerSelector = mkSelector "analyzeImageFile:completionHandler:"

-- | @Selector@ for @analyzeCGImage:completionHandler:@
analyzeCGImage_completionHandlerSelector :: Selector '[Ptr (), Ptr ()] ()
analyzeCGImage_completionHandlerSelector = mkSelector "analyzeCGImage:completionHandler:"

-- | @Selector@ for @analyzeVideoFile:completionHandler:@
analyzeVideoFile_completionHandlerSelector :: Selector '[Id NSURL, Ptr ()] (Id NSProgress)
analyzeVideoFile_completionHandlerSelector = mkSelector "analyzeVideoFile:completionHandler:"

-- | @Selector@ for @analysisPolicy@
analysisPolicySelector :: Selector '[] SCSensitivityAnalysisPolicy
analysisPolicySelector = mkSelector "analysisPolicy"

