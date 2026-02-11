{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureScreenInput
--
-- AVCaptureScreenInput is a concrete subclass of AVCaptureInput that provides an interface for capturing media from a screen or portion thereof.
--
-- Instances of AVCaptureScreenInput are input sources for AVCaptureSession that provide media data from one of the screens connected to the system, represented by CGDirectDisplayIDs.
--
-- Generated bindings for @AVCaptureScreenInput@.
module ObjC.AVFoundation.AVCaptureScreenInput
  ( AVCaptureScreenInput
  , IsAVCaptureScreenInput(..)
  , init_
  , new
  , initWithDisplayID
  , scaleFactor
  , setScaleFactor
  , capturesMouseClicks
  , setCapturesMouseClicks
  , capturesCursor
  , setCapturesCursor
  , removesDuplicateFrames
  , setRemovesDuplicateFrames
  , initSelector
  , newSelector
  , initWithDisplayIDSelector
  , scaleFactorSelector
  , setScaleFactorSelector
  , capturesMouseClicksSelector
  , setCapturesMouseClicksSelector
  , capturesCursorSelector
  , setCapturesCursorSelector
  , removesDuplicateFramesSelector
  , setRemovesDuplicateFramesSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | init
--
-- Creates an AVCaptureScreenInput instance that provides media data from the main display.
--
-- This method creates an instance of AVCaptureScreenInput using the main display whose id is returned from CGMainDisplayID().
--
-- ObjC selector: @- init@
init_ :: IsAVCaptureScreenInput avCaptureScreenInput => avCaptureScreenInput -> IO (Id AVCaptureScreenInput)
init_ avCaptureScreenInput  =
  sendMsg avCaptureScreenInput (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVCaptureScreenInput)
new  =
  do
    cls' <- getRequiredClass "AVCaptureScreenInput"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | initWithDisplayID:
--
-- Creates an AVCaptureScreenInput instance that provides media data from the given display.
--
-- @displayID@ — The id of the display from which to capture video. CGDirectDisplayID is defined in <CoreGraphics/CGDirectDisplay.h>
--
-- Returns: An AVCaptureScreenInput instance that provides data from the given screen, or nil, if the screen could not be used for capture.
--
-- This method creates an instance of AVCaptureScreenInput that can be used to capture data from a display in an AVCaptureSession. This method validates the displayID. If the display cannot be used because it is not available on the system, for example, this method returns nil.
--
-- ObjC selector: @- initWithDisplayID:@
initWithDisplayID :: IsAVCaptureScreenInput avCaptureScreenInput => avCaptureScreenInput -> CUInt -> IO (Id AVCaptureScreenInput)
initWithDisplayID avCaptureScreenInput  displayID =
  sendMsg avCaptureScreenInput (mkSelector "initWithDisplayID:") (retPtr retVoid) [argCUInt (fromIntegral displayID)] >>= ownedObject . castPtr

-- | scaleFactor
--
-- A property indicating the factor by which video buffers captured from the screen are to be scaled.
--
-- By default, AVCaptureScreenInput captures the video buffers from the display at a scale factor of 1.0 (no scaling). Set this property to scale the buffers by a given factor. For instance, a 320x240 capture area with a scaleFactor of 2.0f produces video buffers at 640x480.
--
-- ObjC selector: @- scaleFactor@
scaleFactor :: IsAVCaptureScreenInput avCaptureScreenInput => avCaptureScreenInput -> IO CDouble
scaleFactor avCaptureScreenInput  =
  sendMsg avCaptureScreenInput (mkSelector "scaleFactor") retCDouble []

-- | scaleFactor
--
-- A property indicating the factor by which video buffers captured from the screen are to be scaled.
--
-- By default, AVCaptureScreenInput captures the video buffers from the display at a scale factor of 1.0 (no scaling). Set this property to scale the buffers by a given factor. For instance, a 320x240 capture area with a scaleFactor of 2.0f produces video buffers at 640x480.
--
-- ObjC selector: @- setScaleFactor:@
setScaleFactor :: IsAVCaptureScreenInput avCaptureScreenInput => avCaptureScreenInput -> CDouble -> IO ()
setScaleFactor avCaptureScreenInput  value =
  sendMsg avCaptureScreenInput (mkSelector "setScaleFactor:") retVoid [argCDouble (fromIntegral value)]

-- | capturesMouseClicks
--
-- A property indicating whether mouse clicks should be highlighted in the captured output.
--
-- By default, AVCaptureScreenInput does not highlight mouse clicks in its captured output. If this property is set to YES, mouse clicks are highlighted (a circle is drawn around the mouse for the duration of the click) in the captured output.
--
-- ObjC selector: @- capturesMouseClicks@
capturesMouseClicks :: IsAVCaptureScreenInput avCaptureScreenInput => avCaptureScreenInput -> IO Bool
capturesMouseClicks avCaptureScreenInput  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureScreenInput (mkSelector "capturesMouseClicks") retCULong []

-- | capturesMouseClicks
--
-- A property indicating whether mouse clicks should be highlighted in the captured output.
--
-- By default, AVCaptureScreenInput does not highlight mouse clicks in its captured output. If this property is set to YES, mouse clicks are highlighted (a circle is drawn around the mouse for the duration of the click) in the captured output.
--
-- ObjC selector: @- setCapturesMouseClicks:@
setCapturesMouseClicks :: IsAVCaptureScreenInput avCaptureScreenInput => avCaptureScreenInput -> Bool -> IO ()
setCapturesMouseClicks avCaptureScreenInput  value =
  sendMsg avCaptureScreenInput (mkSelector "setCapturesMouseClicks:") retVoid [argCULong (if value then 1 else 0)]

-- | capturesCursor
--
-- A property indicating whether the cursor should be rendered to the captured output.
--
-- By default, AVCaptureScreenInput draws the cursor in its captured output. If this property is set to NO, the captured output contains only the windows on the screen. Cursor is omitted. Note that cursor position and mouse button state at the time of capture is preserved in CMSampleBuffers emitted from AVCaptureScreenInput. See the inline documentation for kCMIOSampleBufferAttachmentKey_MouseAndKeyboardModifiers in <CoreMediaIO/CMIOSampleBuffer.h>
--
-- ObjC selector: @- capturesCursor@
capturesCursor :: IsAVCaptureScreenInput avCaptureScreenInput => avCaptureScreenInput -> IO Bool
capturesCursor avCaptureScreenInput  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureScreenInput (mkSelector "capturesCursor") retCULong []

-- | capturesCursor
--
-- A property indicating whether the cursor should be rendered to the captured output.
--
-- By default, AVCaptureScreenInput draws the cursor in its captured output. If this property is set to NO, the captured output contains only the windows on the screen. Cursor is omitted. Note that cursor position and mouse button state at the time of capture is preserved in CMSampleBuffers emitted from AVCaptureScreenInput. See the inline documentation for kCMIOSampleBufferAttachmentKey_MouseAndKeyboardModifiers in <CoreMediaIO/CMIOSampleBuffer.h>
--
-- ObjC selector: @- setCapturesCursor:@
setCapturesCursor :: IsAVCaptureScreenInput avCaptureScreenInput => avCaptureScreenInput -> Bool -> IO ()
setCapturesCursor avCaptureScreenInput  value =
  sendMsg avCaptureScreenInput (mkSelector "setCapturesCursor:") retVoid [argCULong (if value then 1 else 0)]

-- | removesDuplicateFrames
--
-- A property indicating whether duplicate frames should be removed by the input.
--
-- If this property is set to YES, AVCaptureScreenInput performs frame differencing and when it detects duplicate frames, it drops them. If set to NO, the captured output receives all frames from the input. Prior to 10.9 this value defaulted to YES. In 10.9 and later, it defaults to NO, as modern platforms support frame differencing in hardware-based encoders.
--
-- As of 10.10, this property has been deprecated and is ignored. Clients wishing to re-create this functionality can use an AVCaptureVideoDataOutput and compare frame contents in their own code. If they wish to write a movie file, they can then pass the unique frames to an AVAssetWriterInput.
--
-- ObjC selector: @- removesDuplicateFrames@
removesDuplicateFrames :: IsAVCaptureScreenInput avCaptureScreenInput => avCaptureScreenInput -> IO Bool
removesDuplicateFrames avCaptureScreenInput  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptureScreenInput (mkSelector "removesDuplicateFrames") retCULong []

-- | removesDuplicateFrames
--
-- A property indicating whether duplicate frames should be removed by the input.
--
-- If this property is set to YES, AVCaptureScreenInput performs frame differencing and when it detects duplicate frames, it drops them. If set to NO, the captured output receives all frames from the input. Prior to 10.9 this value defaulted to YES. In 10.9 and later, it defaults to NO, as modern platforms support frame differencing in hardware-based encoders.
--
-- As of 10.10, this property has been deprecated and is ignored. Clients wishing to re-create this functionality can use an AVCaptureVideoDataOutput and compare frame contents in their own code. If they wish to write a movie file, they can then pass the unique frames to an AVAssetWriterInput.
--
-- ObjC selector: @- setRemovesDuplicateFrames:@
setRemovesDuplicateFrames :: IsAVCaptureScreenInput avCaptureScreenInput => avCaptureScreenInput -> Bool -> IO ()
setRemovesDuplicateFrames avCaptureScreenInput  value =
  sendMsg avCaptureScreenInput (mkSelector "setRemovesDuplicateFrames:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDisplayID:@
initWithDisplayIDSelector :: Selector
initWithDisplayIDSelector = mkSelector "initWithDisplayID:"

-- | @Selector@ for @scaleFactor@
scaleFactorSelector :: Selector
scaleFactorSelector = mkSelector "scaleFactor"

-- | @Selector@ for @setScaleFactor:@
setScaleFactorSelector :: Selector
setScaleFactorSelector = mkSelector "setScaleFactor:"

-- | @Selector@ for @capturesMouseClicks@
capturesMouseClicksSelector :: Selector
capturesMouseClicksSelector = mkSelector "capturesMouseClicks"

-- | @Selector@ for @setCapturesMouseClicks:@
setCapturesMouseClicksSelector :: Selector
setCapturesMouseClicksSelector = mkSelector "setCapturesMouseClicks:"

-- | @Selector@ for @capturesCursor@
capturesCursorSelector :: Selector
capturesCursorSelector = mkSelector "capturesCursor"

-- | @Selector@ for @setCapturesCursor:@
setCapturesCursorSelector :: Selector
setCapturesCursorSelector = mkSelector "setCapturesCursor:"

-- | @Selector@ for @removesDuplicateFrames@
removesDuplicateFramesSelector :: Selector
removesDuplicateFramesSelector = mkSelector "removesDuplicateFrames"

-- | @Selector@ for @setRemovesDuplicateFrames:@
setRemovesDuplicateFramesSelector :: Selector
setRemovesDuplicateFramesSelector = mkSelector "setRemovesDuplicateFrames:"

