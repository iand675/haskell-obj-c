{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureInput
--
-- AVCaptureInput is an abstract class that provides an interface for connecting capture input sources to an AVCaptureSession.
--
-- Concrete instances of AVCaptureInput representing input sources such as cameras can be added to instances of AVCaptureSession using the -[AVCaptureSession addInput:] method. An AVCaptureInput vends one or more streams of media data. For example, input devices can provide both audio and video data. Each media stream provided by an input is represented by an AVCaptureInputPort object. Within a capture session, connections are made between AVCaptureInput instances and AVCaptureOutput instances via AVCaptureConnection objects that define the mapping between a set of AVCaptureInputPort objects and a single AVCaptureOutput.
--
-- Generated bindings for @AVCaptureInput@.
module ObjC.AVFoundation.AVCaptureInput
  ( AVCaptureInput
  , IsAVCaptureInput(..)
  , init_
  , new
  , ports
  , initSelector
  , newSelector
  , portsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVCaptureInput avCaptureInput => avCaptureInput -> IO (Id AVCaptureInput)
init_ avCaptureInput =
  sendOwnedMessage avCaptureInput initSelector

-- | @+ new@
new :: IO (Id AVCaptureInput)
new  =
  do
    cls' <- getRequiredClass "AVCaptureInput"
    sendOwnedClassMessage cls' newSelector

-- | ports
--
-- The ports owned by the receiver.
--
-- The value of this property is an array of AVCaptureInputPort objects, each exposing an interface to a single stream of media data provided by an input.
--
-- ObjC selector: @- ports@
ports :: IsAVCaptureInput avCaptureInput => avCaptureInput -> IO (Id NSArray)
ports avCaptureInput =
  sendMessage avCaptureInput portsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVCaptureInput)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVCaptureInput)
newSelector = mkSelector "new"

-- | @Selector@ for @ports@
portsSelector :: Selector '[] (Id NSArray)
portsSelector = mkSelector "ports"

