{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that represents a media playback environment
--
-- Generated bindings for @BEMediaEnvironment@.
module ObjC.BrowserEngineKit.BEMediaEnvironment
  ( BEMediaEnvironment
  , IsBEMediaEnvironment(..)
  , init_
  , new
  , initWithWebPageURL
  , activateWithError
  , suspendWithError
  , makeCaptureSessionWithError
  , activateWithErrorSelector
  , initSelector
  , initWithWebPageURLSelector
  , makeCaptureSessionWithErrorSelector
  , newSelector
  , suspendWithErrorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.BrowserEngineKit.Internal.Classes
import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsBEMediaEnvironment beMediaEnvironment => beMediaEnvironment -> IO (Id BEMediaEnvironment)
init_ beMediaEnvironment =
  sendOwnedMessage beMediaEnvironment initSelector

-- | @+ new@
new :: IO (Id BEMediaEnvironment)
new  =
  do
    cls' <- getRequiredClass "BEMediaEnvironment"
    sendOwnedClassMessage cls' newSelector

-- | Creates a new media playback environment identified by the web page URL
--
-- - Parameters:   - url: The URL identifying the media playback environment
--
-- ObjC selector: @- initWithWebPageURL:@
initWithWebPageURL :: (IsBEMediaEnvironment beMediaEnvironment, IsNSURL url) => beMediaEnvironment -> url -> IO (Id BEMediaEnvironment)
initWithWebPageURL beMediaEnvironment url =
  sendOwnedMessage beMediaEnvironment initWithWebPageURLSelector (toNSURL url)

-- | Activates the media environment.
--
-- ObjC selector: @- activateWithError:@
activateWithError :: (IsBEMediaEnvironment beMediaEnvironment, IsNSError error_) => beMediaEnvironment -> error_ -> IO Bool
activateWithError beMediaEnvironment error_ =
  sendMessage beMediaEnvironment activateWithErrorSelector (toNSError error_)

-- | Suspends the media environment.
--
-- ObjC selector: @- suspendWithError:@
suspendWithError :: (IsBEMediaEnvironment beMediaEnvironment, IsNSError error_) => beMediaEnvironment -> error_ -> IO Bool
suspendWithError beMediaEnvironment error_ =
  sendMessage beMediaEnvironment suspendWithErrorSelector (toNSError error_)

-- | Creates a new capture session in this media environment.
--
-- The media environment must be activated before the capture session can be started.
--
-- ObjC selector: @- makeCaptureSessionWithError:@
makeCaptureSessionWithError :: (IsBEMediaEnvironment beMediaEnvironment, IsNSError error_) => beMediaEnvironment -> error_ -> IO (Id AVCaptureSession)
makeCaptureSessionWithError beMediaEnvironment error_ =
  sendMessage beMediaEnvironment makeCaptureSessionWithErrorSelector (toNSError error_)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id BEMediaEnvironment)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id BEMediaEnvironment)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithWebPageURL:@
initWithWebPageURLSelector :: Selector '[Id NSURL] (Id BEMediaEnvironment)
initWithWebPageURLSelector = mkSelector "initWithWebPageURL:"

-- | @Selector@ for @activateWithError:@
activateWithErrorSelector :: Selector '[Id NSError] Bool
activateWithErrorSelector = mkSelector "activateWithError:"

-- | @Selector@ for @suspendWithError:@
suspendWithErrorSelector :: Selector '[Id NSError] Bool
suspendWithErrorSelector = mkSelector "suspendWithError:"

-- | @Selector@ for @makeCaptureSessionWithError:@
makeCaptureSessionWithErrorSelector :: Selector '[Id NSError] (Id AVCaptureSession)
makeCaptureSessionWithErrorSelector = mkSelector "makeCaptureSessionWithError:"

