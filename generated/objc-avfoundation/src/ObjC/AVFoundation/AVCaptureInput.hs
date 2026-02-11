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
  , initSelector
  , newSelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVCaptureInput avCaptureInput => avCaptureInput -> IO (Id AVCaptureInput)
init_ avCaptureInput  =
  sendMsg avCaptureInput (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVCaptureInput)
new  =
  do
    cls' <- getRequiredClass "AVCaptureInput"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

