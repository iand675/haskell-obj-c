{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | RPBroadcastSampleHandler
--
-- Subclass this class to handle CMSampleBuffer objects as they are captured by ReplayKit. To enable this mode of handling, set the RPBroadcastProcessMode in the extension's info.plist to RPBroadcastProcessModeSampleBuffer.
--
-- Generated bindings for @RPBroadcastSampleHandler@.
module ObjC.ReplayKit.RPBroadcastSampleHandler
  ( RPBroadcastSampleHandler
  , IsRPBroadcastSampleHandler(..)
  , broadcastStartedWithSetupInfo
  , broadcastPaused
  , broadcastResumed
  , broadcastFinished
  , broadcastAnnotatedWithApplicationInfo
  , processSampleBuffer_withType
  , finishBroadcastWithError
  , broadcastStartedWithSetupInfoSelector
  , broadcastPausedSelector
  , broadcastResumedSelector
  , broadcastFinishedSelector
  , broadcastAnnotatedWithApplicationInfoSelector
  , processSampleBuffer_withTypeSelector
  , finishBroadcastWithErrorSelector

  -- * Enum types
  , RPSampleBufferType(RPSampleBufferType)
  , pattern RPSampleBufferTypeVideo
  , pattern RPSampleBufferTypeAudioApp
  , pattern RPSampleBufferTypeAudioMic

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

import ObjC.ReplayKit.Internal.Classes
import ObjC.ReplayKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Method is called when the RPBroadcastController startBroadcast method is called from the broadcasting application.
--
-- @setupInfo@ — Dictionary that can be supplied by the UI extension to the sample handler.
--
-- ObjC selector: @- broadcastStartedWithSetupInfo:@
broadcastStartedWithSetupInfo :: (IsRPBroadcastSampleHandler rpBroadcastSampleHandler, IsNSDictionary setupInfo) => rpBroadcastSampleHandler -> setupInfo -> IO ()
broadcastStartedWithSetupInfo rpBroadcastSampleHandler  setupInfo =
withObjCPtr setupInfo $ \raw_setupInfo ->
    sendMsg rpBroadcastSampleHandler (mkSelector "broadcastStartedWithSetupInfo:") retVoid [argPtr (castPtr raw_setupInfo :: Ptr ())]

-- | Method is called when the RPBroadcastController pauseBroadcast method is called from the broadcasting application.
--
-- ObjC selector: @- broadcastPaused@
broadcastPaused :: IsRPBroadcastSampleHandler rpBroadcastSampleHandler => rpBroadcastSampleHandler -> IO ()
broadcastPaused rpBroadcastSampleHandler  =
  sendMsg rpBroadcastSampleHandler (mkSelector "broadcastPaused") retVoid []

-- | Method is called when the RPBroadcastController resumeBroadcast method is called from the broadcasting application.
--
-- ObjC selector: @- broadcastResumed@
broadcastResumed :: IsRPBroadcastSampleHandler rpBroadcastSampleHandler => rpBroadcastSampleHandler -> IO ()
broadcastResumed rpBroadcastSampleHandler  =
  sendMsg rpBroadcastSampleHandler (mkSelector "broadcastResumed") retVoid []

-- | Method is called when the RPBroadcastController finishBroadcast method is called from the broadcasting application.
--
-- ObjC selector: @- broadcastFinished@
broadcastFinished :: IsRPBroadcastSampleHandler rpBroadcastSampleHandler => rpBroadcastSampleHandler -> IO ()
broadcastFinished rpBroadcastSampleHandler  =
  sendMsg rpBroadcastSampleHandler (mkSelector "broadcastFinished") retVoid []

-- | Method is called when broadcast is started from Control Center and provides extension information about the first application opened or used during the broadcast.
--
-- @applicationInfo@ — Dictionary that contains information about the first application opened or used buring the broadcast.
--
-- ObjC selector: @- broadcastAnnotatedWithApplicationInfo:@
broadcastAnnotatedWithApplicationInfo :: (IsRPBroadcastSampleHandler rpBroadcastSampleHandler, IsNSDictionary applicationInfo) => rpBroadcastSampleHandler -> applicationInfo -> IO ()
broadcastAnnotatedWithApplicationInfo rpBroadcastSampleHandler  applicationInfo =
withObjCPtr applicationInfo $ \raw_applicationInfo ->
    sendMsg rpBroadcastSampleHandler (mkSelector "broadcastAnnotatedWithApplicationInfo:") retVoid [argPtr (castPtr raw_applicationInfo :: Ptr ())]

-- | Method is called as video and audio data become available during a broadcast session and is delivered as CMSampleBuffer objects.
--
-- @sampleBuffer@ — CMSampleBuffer object which contains either video or audio data.
--
-- @sampleBufferType@ — Determine's the type of the sample buffer defined by the RPSampleBufferType enum.
--
-- ObjC selector: @- processSampleBuffer:withType:@
processSampleBuffer_withType :: IsRPBroadcastSampleHandler rpBroadcastSampleHandler => rpBroadcastSampleHandler -> Ptr () -> RPSampleBufferType -> IO ()
processSampleBuffer_withType rpBroadcastSampleHandler  sampleBuffer sampleBufferType =
  sendMsg rpBroadcastSampleHandler (mkSelector "processSampleBuffer:withType:") retVoid [argPtr sampleBuffer, argCLong (coerce sampleBufferType)]

-- | Method that should be called when broadcasting can not proceed due to an error. Calling this method will stop the broadcast and deliver the error back to the broadcasting app through RPBroadcastController's delegate.
--
-- @error@ — NSError object that will be passed back to the broadcasting app through RPBroadcastControllerDelegate's broadcastController:didFinishWithError: method.
--
-- ObjC selector: @- finishBroadcastWithError:@
finishBroadcastWithError :: (IsRPBroadcastSampleHandler rpBroadcastSampleHandler, IsNSError error_) => rpBroadcastSampleHandler -> error_ -> IO ()
finishBroadcastWithError rpBroadcastSampleHandler  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg rpBroadcastSampleHandler (mkSelector "finishBroadcastWithError:") retVoid [argPtr (castPtr raw_error_ :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @broadcastStartedWithSetupInfo:@
broadcastStartedWithSetupInfoSelector :: Selector
broadcastStartedWithSetupInfoSelector = mkSelector "broadcastStartedWithSetupInfo:"

-- | @Selector@ for @broadcastPaused@
broadcastPausedSelector :: Selector
broadcastPausedSelector = mkSelector "broadcastPaused"

-- | @Selector@ for @broadcastResumed@
broadcastResumedSelector :: Selector
broadcastResumedSelector = mkSelector "broadcastResumed"

-- | @Selector@ for @broadcastFinished@
broadcastFinishedSelector :: Selector
broadcastFinishedSelector = mkSelector "broadcastFinished"

-- | @Selector@ for @broadcastAnnotatedWithApplicationInfo:@
broadcastAnnotatedWithApplicationInfoSelector :: Selector
broadcastAnnotatedWithApplicationInfoSelector = mkSelector "broadcastAnnotatedWithApplicationInfo:"

-- | @Selector@ for @processSampleBuffer:withType:@
processSampleBuffer_withTypeSelector :: Selector
processSampleBuffer_withTypeSelector = mkSelector "processSampleBuffer:withType:"

-- | @Selector@ for @finishBroadcastWithError:@
finishBroadcastWithErrorSelector :: Selector
finishBroadcastWithErrorSelector = mkSelector "finishBroadcastWithError:"

