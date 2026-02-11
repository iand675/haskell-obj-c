{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Analyzes a stream of audio data and provides analysis results to the client
--
-- SNAudioStreamAnalyzer should be used to analyze a stream of audio, represented by a sequence of audio buffers over time.
--
-- Generated bindings for @SNAudioStreamAnalyzer@.
module ObjC.SoundAnalysis.SNAudioStreamAnalyzer
  ( SNAudioStreamAnalyzer
  , IsSNAudioStreamAnalyzer(..)
  , initWithFormat
  , init_
  , addRequest_withObserver_error
  , removeRequest
  , removeAllRequests
  , analyzeAudioBuffer_atAudioFramePosition
  , completeAnalysis
  , initWithFormatSelector
  , initSelector
  , addRequest_withObserver_errorSelector
  , removeRequestSelector
  , removeAllRequestsSelector
  , analyzeAudioBuffer_atAudioFramePositionSelector
  , completeAnalysisSelector


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

import ObjC.SoundAnalysis.Internal.Classes
import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a new analyzer
--
-- - Parameter format: The format of the audio stream to be analyzed. Only PCM formats are supported.
--
-- ObjC selector: @- initWithFormat:@
initWithFormat :: (IsSNAudioStreamAnalyzer snAudioStreamAnalyzer, IsAVAudioFormat format) => snAudioStreamAnalyzer -> format -> IO (Id SNAudioStreamAnalyzer)
initWithFormat snAudioStreamAnalyzer  format =
withObjCPtr format $ \raw_format ->
    sendMsg snAudioStreamAnalyzer (mkSelector "initWithFormat:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsSNAudioStreamAnalyzer snAudioStreamAnalyzer => snAudioStreamAnalyzer -> IO (Id SNAudioStreamAnalyzer)
init_ snAudioStreamAnalyzer  =
  sendMsg snAudioStreamAnalyzer (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Adds a new analysis request to the analyzer
--
-- - Parameters:
--
-- - request: An audio analysis request to be performed on the audio stream
--
-- - observer: The object that will receive the analysis results for the supplied request. The observer is weakly retained by the analyzer.
--
-- - error: On input, a pointer to an error object. If an error occurs, this pointer is set to an actual error object containing the error information. You may specify nil for this parameter if you do not want the error information.
--
-- - Returns: YES if the request was successfully added, and NO otherwise.
--
-- Requests can be added while analysis is in progress. If the analyzer cannot perform the requested analysis, an error will be returned. For example, an error could be returned if the request requires a stream format that doesn't match the analyzer's stream format.
--
-- ObjC selector: @- addRequest:withObserver:error:@
addRequest_withObserver_error :: (IsSNAudioStreamAnalyzer snAudioStreamAnalyzer, IsNSError error_) => snAudioStreamAnalyzer -> RawId -> RawId -> error_ -> IO Bool
addRequest_withObserver_error snAudioStreamAnalyzer  request observer error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg snAudioStreamAnalyzer (mkSelector "addRequest:withObserver:error:") retCULong [argPtr (castPtr (unRawId request) :: Ptr ()), argPtr (castPtr (unRawId observer) :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Removes an existing analysis request from the analyzer - Parameter request: An audio analysis request to be removed Requests can be removed while analysis is in progress. Once the removeRequest method returns, the previously registered observer will not receive any more callbacks.
--
-- ObjC selector: @- removeRequest:@
removeRequest :: IsSNAudioStreamAnalyzer snAudioStreamAnalyzer => snAudioStreamAnalyzer -> RawId -> IO ()
removeRequest snAudioStreamAnalyzer  request =
  sendMsg snAudioStreamAnalyzer (mkSelector "removeRequest:") retVoid [argPtr (castPtr (unRawId request) :: Ptr ())]

-- | Removes all requests from the analyzer
--
-- ObjC selector: @- removeAllRequests@
removeAllRequests :: IsSNAudioStreamAnalyzer snAudioStreamAnalyzer => snAudioStreamAnalyzer -> IO ()
removeAllRequests snAudioStreamAnalyzer  =
  sendMsg snAudioStreamAnalyzer (mkSelector "removeAllRequests") retVoid []

-- | Provides the next buffer for analysis
--
-- - Parameters:
--
-- - audioBuffer: The buffer containing the audio to be processed
--
-- - audioFramePosition: The frame position of the data in the buffer
--
-- The framePosition should be a monotonically increasing sample timestamp. If the sample timeline is detected to be non-continuous, the analyzer's internal state may reset to account for the jump. Some types of audio analysis are performed at a fixed block size, which may differ from the buffer sizes provided for analysis. For this reason, an invocation of analyzeAudioBuffer may cause an analysis request observer to be called zero times, one time, or many times, depending on the relationship between the input buffer size, current analyzer state, and native analysis block size. Any errors produced during analysis will be provided through the request observers. This method may block as a means of indicating backpressure to the caller. These methods are not safe to call from a realtime audio context but may be called from lower priority threads (i.e. AVAudioEngine tap callback or AudioQueue callback).
--
-- ObjC selector: @- analyzeAudioBuffer:atAudioFramePosition:@
analyzeAudioBuffer_atAudioFramePosition :: (IsSNAudioStreamAnalyzer snAudioStreamAnalyzer, IsAVAudioBuffer audioBuffer) => snAudioStreamAnalyzer -> audioBuffer -> CLong -> IO ()
analyzeAudioBuffer_atAudioFramePosition snAudioStreamAnalyzer  audioBuffer audioFramePosition =
withObjCPtr audioBuffer $ \raw_audioBuffer ->
    sendMsg snAudioStreamAnalyzer (mkSelector "analyzeAudioBuffer:atAudioFramePosition:") retVoid [argPtr (castPtr raw_audioBuffer :: Ptr ()), argCLong (fromIntegral audioFramePosition)]

-- | Indicates that the audio stream has ended, and no more audio buffers will be analyzed
--
-- After this method has been called, it is invalid to provide any more audio data for analysis, and any provided buffers will be ignored. This method is useful for types of analysis that may have final results to provide upon the completion of the stream.
--
-- ObjC selector: @- completeAnalysis@
completeAnalysis :: IsSNAudioStreamAnalyzer snAudioStreamAnalyzer => snAudioStreamAnalyzer -> IO ()
completeAnalysis snAudioStreamAnalyzer  =
  sendMsg snAudioStreamAnalyzer (mkSelector "completeAnalysis") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFormat:@
initWithFormatSelector :: Selector
initWithFormatSelector = mkSelector "initWithFormat:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @addRequest:withObserver:error:@
addRequest_withObserver_errorSelector :: Selector
addRequest_withObserver_errorSelector = mkSelector "addRequest:withObserver:error:"

-- | @Selector@ for @removeRequest:@
removeRequestSelector :: Selector
removeRequestSelector = mkSelector "removeRequest:"

-- | @Selector@ for @removeAllRequests@
removeAllRequestsSelector :: Selector
removeAllRequestsSelector = mkSelector "removeAllRequests"

-- | @Selector@ for @analyzeAudioBuffer:atAudioFramePosition:@
analyzeAudioBuffer_atAudioFramePositionSelector :: Selector
analyzeAudioBuffer_atAudioFramePositionSelector = mkSelector "analyzeAudioBuffer:atAudioFramePosition:"

-- | @Selector@ for @completeAnalysis@
completeAnalysisSelector :: Selector
completeAnalysisSelector = mkSelector "completeAnalysis"

