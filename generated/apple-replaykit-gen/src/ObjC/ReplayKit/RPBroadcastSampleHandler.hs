{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , broadcastAnnotatedWithApplicationInfoSelector
  , broadcastFinishedSelector
  , broadcastPausedSelector
  , broadcastResumedSelector
  , broadcastStartedWithSetupInfoSelector
  , finishBroadcastWithErrorSelector
  , processSampleBuffer_withTypeSelector

  -- * Enum types
  , RPSampleBufferType(RPSampleBufferType)
  , pattern RPSampleBufferTypeVideo
  , pattern RPSampleBufferTypeAudioApp
  , pattern RPSampleBufferTypeAudioMic

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
broadcastStartedWithSetupInfo rpBroadcastSampleHandler setupInfo =
  sendMessage rpBroadcastSampleHandler broadcastStartedWithSetupInfoSelector (toNSDictionary setupInfo)

-- | Method is called when the RPBroadcastController pauseBroadcast method is called from the broadcasting application.
--
-- ObjC selector: @- broadcastPaused@
broadcastPaused :: IsRPBroadcastSampleHandler rpBroadcastSampleHandler => rpBroadcastSampleHandler -> IO ()
broadcastPaused rpBroadcastSampleHandler =
  sendMessage rpBroadcastSampleHandler broadcastPausedSelector

-- | Method is called when the RPBroadcastController resumeBroadcast method is called from the broadcasting application.
--
-- ObjC selector: @- broadcastResumed@
broadcastResumed :: IsRPBroadcastSampleHandler rpBroadcastSampleHandler => rpBroadcastSampleHandler -> IO ()
broadcastResumed rpBroadcastSampleHandler =
  sendMessage rpBroadcastSampleHandler broadcastResumedSelector

-- | Method is called when the RPBroadcastController finishBroadcast method is called from the broadcasting application.
--
-- ObjC selector: @- broadcastFinished@
broadcastFinished :: IsRPBroadcastSampleHandler rpBroadcastSampleHandler => rpBroadcastSampleHandler -> IO ()
broadcastFinished rpBroadcastSampleHandler =
  sendMessage rpBroadcastSampleHandler broadcastFinishedSelector

-- | Method is called when broadcast is started from Control Center and provides extension information about the first application opened or used during the broadcast.
--
-- @applicationInfo@ — Dictionary that contains information about the first application opened or used buring the broadcast.
--
-- ObjC selector: @- broadcastAnnotatedWithApplicationInfo:@
broadcastAnnotatedWithApplicationInfo :: (IsRPBroadcastSampleHandler rpBroadcastSampleHandler, IsNSDictionary applicationInfo) => rpBroadcastSampleHandler -> applicationInfo -> IO ()
broadcastAnnotatedWithApplicationInfo rpBroadcastSampleHandler applicationInfo =
  sendMessage rpBroadcastSampleHandler broadcastAnnotatedWithApplicationInfoSelector (toNSDictionary applicationInfo)

-- | Method is called as video and audio data become available during a broadcast session and is delivered as CMSampleBuffer objects.
--
-- @sampleBuffer@ — CMSampleBuffer object which contains either video or audio data.
--
-- @sampleBufferType@ — Determine's the type of the sample buffer defined by the RPSampleBufferType enum.
--
-- ObjC selector: @- processSampleBuffer:withType:@
processSampleBuffer_withType :: IsRPBroadcastSampleHandler rpBroadcastSampleHandler => rpBroadcastSampleHandler -> Ptr () -> RPSampleBufferType -> IO ()
processSampleBuffer_withType rpBroadcastSampleHandler sampleBuffer sampleBufferType =
  sendMessage rpBroadcastSampleHandler processSampleBuffer_withTypeSelector sampleBuffer sampleBufferType

-- | Method that should be called when broadcasting can not proceed due to an error. Calling this method will stop the broadcast and deliver the error back to the broadcasting app through RPBroadcastController's delegate.
--
-- @error@ — NSError object that will be passed back to the broadcasting app through RPBroadcastControllerDelegate's broadcastController:didFinishWithError: method.
--
-- ObjC selector: @- finishBroadcastWithError:@
finishBroadcastWithError :: (IsRPBroadcastSampleHandler rpBroadcastSampleHandler, IsNSError error_) => rpBroadcastSampleHandler -> error_ -> IO ()
finishBroadcastWithError rpBroadcastSampleHandler error_ =
  sendMessage rpBroadcastSampleHandler finishBroadcastWithErrorSelector (toNSError error_)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @broadcastStartedWithSetupInfo:@
broadcastStartedWithSetupInfoSelector :: Selector '[Id NSDictionary] ()
broadcastStartedWithSetupInfoSelector = mkSelector "broadcastStartedWithSetupInfo:"

-- | @Selector@ for @broadcastPaused@
broadcastPausedSelector :: Selector '[] ()
broadcastPausedSelector = mkSelector "broadcastPaused"

-- | @Selector@ for @broadcastResumed@
broadcastResumedSelector :: Selector '[] ()
broadcastResumedSelector = mkSelector "broadcastResumed"

-- | @Selector@ for @broadcastFinished@
broadcastFinishedSelector :: Selector '[] ()
broadcastFinishedSelector = mkSelector "broadcastFinished"

-- | @Selector@ for @broadcastAnnotatedWithApplicationInfo:@
broadcastAnnotatedWithApplicationInfoSelector :: Selector '[Id NSDictionary] ()
broadcastAnnotatedWithApplicationInfoSelector = mkSelector "broadcastAnnotatedWithApplicationInfo:"

-- | @Selector@ for @processSampleBuffer:withType:@
processSampleBuffer_withTypeSelector :: Selector '[Ptr (), RPSampleBufferType] ()
processSampleBuffer_withTypeSelector = mkSelector "processSampleBuffer:withType:"

-- | @Selector@ for @finishBroadcastWithError:@
finishBroadcastWithErrorSelector :: Selector '[Id NSError] ()
finishBroadcastWithErrorSelector = mkSelector "finishBroadcastWithError:"

