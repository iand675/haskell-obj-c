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
  , initWithURL_errorSelector
  , initSelector
  , addRequest_withObserver_errorSelector
  , removeRequestSelector
  , removeAllRequestsSelector
  , analyzeSelector
  , analyzeWithCompletionHandlerSelector
  , cancelAnalysisSelector


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
initWithURL_error snAudioFileAnalyzer  url error_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg snAudioFileAnalyzer (mkSelector "initWithURL:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsSNAudioFileAnalyzer snAudioFileAnalyzer => snAudioFileAnalyzer -> IO (Id SNAudioFileAnalyzer)
init_ snAudioFileAnalyzer  =
  sendMsg snAudioFileAnalyzer (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
addRequest_withObserver_error snAudioFileAnalyzer  request observer error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg snAudioFileAnalyzer (mkSelector "addRequest:withObserver:error:") retCULong [argPtr (castPtr (unRawId request) :: Ptr ()), argPtr (castPtr (unRawId observer) :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Removes an existing analysis request from the analyzer
--
-- - Parameter request: An audio analysis request to be removed
--
-- Requests can be removed while analysis is in progress. Once the removeRequest method returns, the previously registered observer will not receive any more callbacks.
--
-- ObjC selector: @- removeRequest:@
removeRequest :: IsSNAudioFileAnalyzer snAudioFileAnalyzer => snAudioFileAnalyzer -> RawId -> IO ()
removeRequest snAudioFileAnalyzer  request =
  sendMsg snAudioFileAnalyzer (mkSelector "removeRequest:") retVoid [argPtr (castPtr (unRawId request) :: Ptr ())]

-- | Removes all requests from the analyzer
--
-- ObjC selector: @- removeAllRequests@
removeAllRequests :: IsSNAudioFileAnalyzer snAudioFileAnalyzer => snAudioFileAnalyzer -> IO ()
removeAllRequests snAudioFileAnalyzer  =
  sendMsg snAudioFileAnalyzer (mkSelector "removeAllRequests") retVoid []

-- | Analyzes the audio file synchronously
--
-- This function executes synchronously. Any errors produced during analysis will flow downstream to the request observers. This method may block for a long period of time, so be careful to ensure this call does not block UI or other important tasks.
--
-- ObjC selector: @- analyze@
analyze :: IsSNAudioFileAnalyzer snAudioFileAnalyzer => snAudioFileAnalyzer -> IO ()
analyze snAudioFileAnalyzer  =
  sendMsg snAudioFileAnalyzer (mkSelector "analyze") retVoid []

-- | Analyzes the audio file asynchronously
--
-- This function executes asynchronously, calling the completion after the entire file has completed analysis. Any errors produced during analysis will flow downstream to the request observers. If the cancelAnalysis method is called, the completionHandler will still be called, but with didReachEndOfFile set to NO.
--
-- ObjC selector: @- analyzeWithCompletionHandler:@
analyzeWithCompletionHandler :: IsSNAudioFileAnalyzer snAudioFileAnalyzer => snAudioFileAnalyzer -> Ptr () -> IO ()
analyzeWithCompletionHandler snAudioFileAnalyzer  completionHandler =
  sendMsg snAudioFileAnalyzer (mkSelector "analyzeWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | Cancels any in-progress analysis of the audio file
--
-- This function executes asynchronously, and will trigger the completion handler provided in the analyzeWithCompletionHandler method after the cancellation is complete.
--
-- ObjC selector: @- cancelAnalysis@
cancelAnalysis :: IsSNAudioFileAnalyzer snAudioFileAnalyzer => snAudioFileAnalyzer -> IO ()
cancelAnalysis snAudioFileAnalyzer  =
  sendMsg snAudioFileAnalyzer (mkSelector "cancelAnalysis") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithURL:error:@
initWithURL_errorSelector :: Selector
initWithURL_errorSelector = mkSelector "initWithURL:error:"

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

-- | @Selector@ for @analyze@
analyzeSelector :: Selector
analyzeSelector = mkSelector "analyze"

-- | @Selector@ for @analyzeWithCompletionHandler:@
analyzeWithCompletionHandlerSelector :: Selector
analyzeWithCompletionHandlerSelector = mkSelector "analyzeWithCompletionHandler:"

-- | @Selector@ for @cancelAnalysis@
cancelAnalysisSelector :: Selector
cancelAnalysisSelector = mkSelector "cancelAnalysis"

