{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Analyzes an audio file and provides analysis results to the client
--
-- Generated bindings for @SNAudioFileAnalyzer@.
module ObjC.SoundAnalysis.SNAudioFileAnalyzer
  ( SNAudioFileAnalyzer
  , IsSNAudioFileAnalyzer(..)
  , initWithURL_error
  , init_
  , addRequest_withObserver_error
  , removeRequest
  , removeAllRequests
  , analyze
  , analyzeWithCompletionHandler
  , cancelAnalysis
  , addRequest_withObserver_errorSelector
  , analyzeSelector
  , analyzeWithCompletionHandlerSelector
  , cancelAnalysisSelector
  , initSelector
  , initWithURL_errorSelector
  , removeAllRequestsSelector
  , removeRequestSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SoundAnalysis.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a new analyzer
--
-- - Parameters:
--
-- - url: The url for the audio file to be analyzed
--
-- - error: On input, a pointer to an error object. If an error occurs, this pointer is set to an actual error object containing the error information. You may specify nil for this parameter if you do not want the error information.
--
-- ObjC selector: @- initWithURL:error:@
initWithURL_error :: (IsSNAudioFileAnalyzer snAudioFileAnalyzer, IsNSURL url, IsNSError error_) => snAudioFileAnalyzer -> url -> error_ -> IO (Id SNAudioFileAnalyzer)
initWithURL_error snAudioFileAnalyzer url error_ =
  sendOwnedMessage snAudioFileAnalyzer initWithURL_errorSelector (toNSURL url) (toNSError error_)

-- | @- init@
init_ :: IsSNAudioFileAnalyzer snAudioFileAnalyzer => snAudioFileAnalyzer -> IO (Id SNAudioFileAnalyzer)
init_ snAudioFileAnalyzer =
  sendOwnedMessage snAudioFileAnalyzer initSelector

-- | Adds a new analysis request to the analyzer
--
-- - Parameters:
--
-- - request: An audio analysis request to be performed on the audio stream
--
-- - observer: The object that will receive the analysis results for the supplied request. The observer is weakly retained by the analyzer.
--
-- - error On input, a pointer to an error object. If an error occurs, this pointer is set to an actual error object containing the error information. You may specify nil for this parameter if you do not want the error information.
--
-- - Returns: YES if the request was successfully added, and NO otherwise.
--
-- If addRequest is called while the file is being processed, an error will be returned.
--
-- ObjC selector: @- addRequest:withObserver:error:@
addRequest_withObserver_error :: (IsSNAudioFileAnalyzer snAudioFileAnalyzer, IsNSError error_) => snAudioFileAnalyzer -> RawId -> RawId -> error_ -> IO Bool
addRequest_withObserver_error snAudioFileAnalyzer request observer error_ =
  sendMessage snAudioFileAnalyzer addRequest_withObserver_errorSelector request observer (toNSError error_)

-- | Removes an existing analysis request from the analyzer
--
-- - Parameter request: An audio analysis request to be removed
--
-- Requests can be removed while analysis is in progress. Once the removeRequest method returns, the previously registered observer will not receive any more callbacks.
--
-- ObjC selector: @- removeRequest:@
removeRequest :: IsSNAudioFileAnalyzer snAudioFileAnalyzer => snAudioFileAnalyzer -> RawId -> IO ()
removeRequest snAudioFileAnalyzer request =
  sendMessage snAudioFileAnalyzer removeRequestSelector request

-- | Removes all requests from the analyzer
--
-- ObjC selector: @- removeAllRequests@
removeAllRequests :: IsSNAudioFileAnalyzer snAudioFileAnalyzer => snAudioFileAnalyzer -> IO ()
removeAllRequests snAudioFileAnalyzer =
  sendMessage snAudioFileAnalyzer removeAllRequestsSelector

-- | Analyzes the audio file synchronously
--
-- This function executes synchronously. Any errors produced during analysis will flow downstream to the request observers. This method may block for a long period of time, so be careful to ensure this call does not block UI or other important tasks.
--
-- ObjC selector: @- analyze@
analyze :: IsSNAudioFileAnalyzer snAudioFileAnalyzer => snAudioFileAnalyzer -> IO ()
analyze snAudioFileAnalyzer =
  sendMessage snAudioFileAnalyzer analyzeSelector

-- | Analyzes the audio file asynchronously
--
-- This function executes asynchronously, calling the completion after the entire file has completed analysis. Any errors produced during analysis will flow downstream to the request observers. If the cancelAnalysis method is called, the completionHandler will still be called, but with didReachEndOfFile set to NO.
--
-- ObjC selector: @- analyzeWithCompletionHandler:@
analyzeWithCompletionHandler :: IsSNAudioFileAnalyzer snAudioFileAnalyzer => snAudioFileAnalyzer -> Ptr () -> IO ()
analyzeWithCompletionHandler snAudioFileAnalyzer completionHandler =
  sendMessage snAudioFileAnalyzer analyzeWithCompletionHandlerSelector completionHandler

-- | Cancels any in-progress analysis of the audio file
--
-- This function executes asynchronously, and will trigger the completion handler provided in the analyzeWithCompletionHandler method after the cancellation is complete.
--
-- ObjC selector: @- cancelAnalysis@
cancelAnalysis :: IsSNAudioFileAnalyzer snAudioFileAnalyzer => snAudioFileAnalyzer -> IO ()
cancelAnalysis snAudioFileAnalyzer =
  sendMessage snAudioFileAnalyzer cancelAnalysisSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithURL:error:@
initWithURL_errorSelector :: Selector '[Id NSURL, Id NSError] (Id SNAudioFileAnalyzer)
initWithURL_errorSelector = mkSelector "initWithURL:error:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SNAudioFileAnalyzer)
initSelector = mkSelector "init"

-- | @Selector@ for @addRequest:withObserver:error:@
addRequest_withObserver_errorSelector :: Selector '[RawId, RawId, Id NSError] Bool
addRequest_withObserver_errorSelector = mkSelector "addRequest:withObserver:error:"

-- | @Selector@ for @removeRequest:@
removeRequestSelector :: Selector '[RawId] ()
removeRequestSelector = mkSelector "removeRequest:"

-- | @Selector@ for @removeAllRequests@
removeAllRequestsSelector :: Selector '[] ()
removeAllRequestsSelector = mkSelector "removeAllRequests"

-- | @Selector@ for @analyze@
analyzeSelector :: Selector '[] ()
analyzeSelector = mkSelector "analyze"

-- | @Selector@ for @analyzeWithCompletionHandler:@
analyzeWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
analyzeWithCompletionHandlerSelector = mkSelector "analyzeWithCompletionHandler:"

-- | @Selector@ for @cancelAnalysis@
cancelAnalysisSelector :: Selector '[] ()
cancelAnalysisSelector = mkSelector "cancelAnalysis"

