{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAudioRoutingArbiter
--
-- The interface to participate in audio routing arbitration.
--
-- Generated bindings for @AVAudioRoutingArbiter@.
module ObjC.AVFAudio.AVAudioRoutingArbiter
  ( AVAudioRoutingArbiter
  , IsAVAudioRoutingArbiter(..)
  , init_
  , new
  , beginArbitrationWithCategory_completionHandler
  , leaveArbitration
  , sharedRoutingArbiter
  , beginArbitrationWithCategory_completionHandlerSelector
  , initSelector
  , leaveArbitrationSelector
  , newSelector
  , sharedRoutingArbiterSelector

  -- * Enum types
  , AVAudioRoutingArbitrationCategory(AVAudioRoutingArbitrationCategory)
  , pattern AVAudioRoutingArbitrationCategoryPlayback
  , pattern AVAudioRoutingArbitrationCategoryPlayAndRecord
  , pattern AVAudioRoutingArbitrationCategoryPlayAndRecordVoice

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.AVFAudio.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAudioRoutingArbiter avAudioRoutingArbiter => avAudioRoutingArbiter -> IO (Id AVAudioRoutingArbiter)
init_ avAudioRoutingArbiter =
  sendOwnedMessage avAudioRoutingArbiter initSelector

-- | @+ new@
new :: IO (Id AVAudioRoutingArbiter)
new  =
  do
    cls' <- getRequiredClass "AVAudioRoutingArbiter"
    sendOwnedClassMessage cls' newSelector

-- | beginArbitrationWithCategory:completionHandler:
--
-- Begin routing arbitration to take ownership of nearby Bluetooth audio routes.
--
-- When an app wants to participate in automatic audio arbitration for the wireless headphones route, it has to begin arbitration       specifying its arbitration session category. It provides the operating system time to arbitrate with other nearby Apple       devices to obtain ownership of supported Bluetooth audio devices. Then upon completion of arbitration, the operating system       will automatically determine  whether to route audio to the nearby Bluetooth device. Once arbitration completes, the application       is free to start running audio I/O. I/O will be started upon the app request even if the -beginArbitrationWithCategory:completionHandler: fails.       This method should also be used whenever restarting audio I/O in order to allow the system to arbitrate for ownership of a Bluetooth       device that may have been taken by another nearby Apple device during the time that I/O was stopped.
--
-- @category@ — The category describes the general type of audio that the app plans to use.
--
-- @handler@ — A client-supplied block called asynchronously when audio routing arbitration is completed.        This completion handler takes the following parameters:        defaultDeviceChanged            Indicating that the system default audio device has been changed as a result of the arbitration operation.        error            An error object that indicates why the request failed, or nil if the request was successful.
--
-- ObjC selector: @- beginArbitrationWithCategory:completionHandler:@
beginArbitrationWithCategory_completionHandler :: IsAVAudioRoutingArbiter avAudioRoutingArbiter => avAudioRoutingArbiter -> AVAudioRoutingArbitrationCategory -> Ptr () -> IO ()
beginArbitrationWithCategory_completionHandler avAudioRoutingArbiter category handler =
  sendMessage avAudioRoutingArbiter beginArbitrationWithCategory_completionHandlerSelector category handler

-- | leaveArbitration
--
-- Stop participating in audio routing arbitration.
--
-- When an application has stopped using audio for the foreseeable future, it should notify the system. For example,        in Voice over IP (VoIP)  use cases, the application should call -leaveArbitration when the VoIP call has ended.        This allows the system to make a better decision when other participating Apple devices would like to take ownership        of a nearby Bluetooth device. Applications should not call this API in cases where audio is only momentarily paused.
--
-- ObjC selector: @- leaveArbitration@
leaveArbitration :: IsAVAudioRoutingArbiter avAudioRoutingArbiter => avAudioRoutingArbiter -> IO ()
leaveArbitration avAudioRoutingArbiter =
  sendMessage avAudioRoutingArbiter leaveArbitrationSelector

-- | sharedRoutingArbiter
--
-- Returns the singleton AVAudioRoutingArbiter instance.
--
-- ObjC selector: @+ sharedRoutingArbiter@
sharedRoutingArbiter :: IO (Id AVAudioRoutingArbiter)
sharedRoutingArbiter  =
  do
    cls' <- getRequiredClass "AVAudioRoutingArbiter"
    sendClassMessage cls' sharedRoutingArbiterSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAudioRoutingArbiter)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVAudioRoutingArbiter)
newSelector = mkSelector "new"

-- | @Selector@ for @beginArbitrationWithCategory:completionHandler:@
beginArbitrationWithCategory_completionHandlerSelector :: Selector '[AVAudioRoutingArbitrationCategory, Ptr ()] ()
beginArbitrationWithCategory_completionHandlerSelector = mkSelector "beginArbitrationWithCategory:completionHandler:"

-- | @Selector@ for @leaveArbitration@
leaveArbitrationSelector :: Selector '[] ()
leaveArbitrationSelector = mkSelector "leaveArbitration"

-- | @Selector@ for @sharedRoutingArbiter@
sharedRoutingArbiterSelector :: Selector '[] (Id AVAudioRoutingArbiter)
sharedRoutingArbiterSelector = mkSelector "sharedRoutingArbiter"

