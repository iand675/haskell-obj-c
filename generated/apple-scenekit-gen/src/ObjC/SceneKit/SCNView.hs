{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNView
--
-- A SCNView is a subclass of NSView that can display a SCNScene
--
-- Generated bindings for @SCNView@.
module ObjC.SceneKit.SCNView
  ( SCNView
  , IsSCNView(..)
  , initWithFrame_options
  , snapshot
  , play
  , pause
  , stop
  , scene
  , setScene
  , rendersContinuously
  , setRendersContinuously
  , backgroundColor
  , setBackgroundColor
  , allowsCameraControl
  , setAllowsCameraControl
  , cameraControlConfiguration
  , defaultCameraController
  , preferredFramesPerSecond
  , setPreferredFramesPerSecond
  , drawableResizesAsynchronously
  , setDrawableResizesAsynchronously
  , openGLContext
  , setOpenGLContext
  , antialiasingMode
  , setAntialiasingMode
  , pixelFormat
  , setPixelFormat
  , initWithFrame_optionsSelector
  , snapshotSelector
  , playSelector
  , pauseSelector
  , stopSelector
  , sceneSelector
  , setSceneSelector
  , rendersContinuouslySelector
  , setRendersContinuouslySelector
  , backgroundColorSelector
  , setBackgroundColorSelector
  , allowsCameraControlSelector
  , setAllowsCameraControlSelector
  , cameraControlConfigurationSelector
  , defaultCameraControllerSelector
  , preferredFramesPerSecondSelector
  , setPreferredFramesPerSecondSelector
  , drawableResizesAsynchronouslySelector
  , setDrawableResizesAsynchronouslySelector
  , openGLContextSelector
  , setOpenGLContextSelector
  , antialiasingModeSelector
  , setAntialiasingModeSelector
  , pixelFormatSelector
  , setPixelFormatSelector

  -- * Enum types
  , SCNAntialiasingMode(SCNAntialiasingMode)
  , pattern SCNAntialiasingModeNone
  , pattern SCNAntialiasingModeMultisampling2X
  , pattern SCNAntialiasingModeMultisampling4X
  , pattern SCNAntialiasingModeMultisampling8X
  , pattern SCNAntialiasingModeMultisampling16X

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

import ObjC.SceneKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.SceneKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithFrame:options:
--
-- Initializes and returns a newly allocated SCNView object with a specified frame rectangle.
--
-- @frame@ — The frame rectangle for the created view object.
--
-- @options@ — An optional dictionary. See "View initialization options" above.
--
-- ObjC selector: @- initWithFrame:options:@
initWithFrame_options :: (IsSCNView scnView, IsNSDictionary options) => scnView -> NSRect -> options -> IO (Id SCNView)
initWithFrame_options scnView  frame options =
  withObjCPtr options $ \raw_options ->
      sendMsg scnView (mkSelector "initWithFrame:options:") (retPtr retVoid) [argNSRect frame, argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

-- | snapshot
--
-- Draws the contents of the view and returns them as a new image object
--
-- This method is thread-safe and may be called at any time.
--
-- ObjC selector: @- snapshot@
snapshot :: IsSCNView scnView => scnView -> IO (Id NSImage)
snapshot scnView  =
    sendMsg scnView (mkSelector "snapshot") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | play:
--
-- This action method begins playing the scene at its current location.
--
-- @sender@ — The object (such as a button or menu item) sending the message to play the scene.
--
-- This method does not do anything if the scene is already playing.
--
-- ObjC selector: @- play:@
play :: IsSCNView scnView => scnView -> RawId -> IO ()
play scnView  sender =
    sendMsg scnView (mkSelector "play:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | pause:
--
-- This action method pauses the scene playback.
--
-- @sender@ — The object (such as a button or menu item) sending the message to pause the scene.
--
-- This method does not do anything if the scene is already paused.
--
-- ObjC selector: @- pause:@
pause :: IsSCNView scnView => scnView -> RawId -> IO ()
pause scnView  sender =
    sendMsg scnView (mkSelector "pause:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | stop:
--
-- This action method stops the scene playback and resets the current time to the start time of the scene.
--
-- @sender@ — The object (such as a button or menu item) sending the message to stop playing the scene.
--
-- ObjC selector: @- stop:@
stop :: IsSCNView scnView => scnView -> RawId -> IO ()
stop scnView  sender =
    sendMsg scnView (mkSelector "stop:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | scene
--
-- Specifies the scene of the receiver
--
-- ObjC selector: @- scene@
scene :: IsSCNView scnView => scnView -> IO (Id SCNScene)
scene scnView  =
    sendMsg scnView (mkSelector "scene") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | scene
--
-- Specifies the scene of the receiver
--
-- ObjC selector: @- setScene:@
setScene :: (IsSCNView scnView, IsSCNScene value) => scnView -> value -> IO ()
setScene scnView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg scnView (mkSelector "setScene:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | rendersContinuously
--
-- When set to YES, the view continously redraw at the display link frame rate. When set to NO the view will only redraw when something change or animates in the receiver's scene. Defaults to NO.
--
-- ObjC selector: @- rendersContinuously@
rendersContinuously :: IsSCNView scnView => scnView -> IO Bool
rendersContinuously scnView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnView (mkSelector "rendersContinuously") retCULong []

-- | rendersContinuously
--
-- When set to YES, the view continously redraw at the display link frame rate. When set to NO the view will only redraw when something change or animates in the receiver's scene. Defaults to NO.
--
-- ObjC selector: @- setRendersContinuously:@
setRendersContinuously :: IsSCNView scnView => scnView -> Bool -> IO ()
setRendersContinuously scnView  value =
    sendMsg scnView (mkSelector "setRendersContinuously:") retVoid [argCULong (if value then 1 else 0)]

-- | backgroundColor
--
-- Specifies the background color of the receiver. Defaults to opaque white.
--
-- ObjC selector: @- backgroundColor@
backgroundColor :: IsSCNView scnView => scnView -> IO (Id NSColor)
backgroundColor scnView  =
    sendMsg scnView (mkSelector "backgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | backgroundColor
--
-- Specifies the background color of the receiver. Defaults to opaque white.
--
-- ObjC selector: @- setBackgroundColor:@
setBackgroundColor :: (IsSCNView scnView, IsNSColor value) => scnView -> value -> IO ()
setBackgroundColor scnView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg scnView (mkSelector "setBackgroundColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | allowsCameraControl
--
-- A Boolean value that determines whether the user can manipulate the point of view used to render the scene.
--
-- When set to YES, the user can manipulate the current point of view with the mouse or the trackpad. The scene graph and existing cameras won't be modified by this action. The default value of this property is NO.     Note that the primary purpose of this property is to aid in debugging your application. You may want to implement your own camera controller suitable for your application.     The built-in camera controller let you:       - drag the mouse to rotate the camera around the scene       - drag+cmd to rotate the camera in local space       - drag+shift to rotate using sticky axis       - use the scroll wheel or alt+drag the mouse to translate the camera on its local X,Y plan       - alt+scroll wheel to move the camera forward/backward       - rotate gesture (trackpad only) to roll the camera (rotation around the Z axis)       - pinch gesture (trackpad only) move the camera forward/backward       - alt + pinch gesture (trackpad only) to zoom-in / zoom-out (change the field of view of the camera)
--
-- ObjC selector: @- allowsCameraControl@
allowsCameraControl :: IsSCNView scnView => scnView -> IO Bool
allowsCameraControl scnView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnView (mkSelector "allowsCameraControl") retCULong []

-- | allowsCameraControl
--
-- A Boolean value that determines whether the user can manipulate the point of view used to render the scene.
--
-- When set to YES, the user can manipulate the current point of view with the mouse or the trackpad. The scene graph and existing cameras won't be modified by this action. The default value of this property is NO.     Note that the primary purpose of this property is to aid in debugging your application. You may want to implement your own camera controller suitable for your application.     The built-in camera controller let you:       - drag the mouse to rotate the camera around the scene       - drag+cmd to rotate the camera in local space       - drag+shift to rotate using sticky axis       - use the scroll wheel or alt+drag the mouse to translate the camera on its local X,Y plan       - alt+scroll wheel to move the camera forward/backward       - rotate gesture (trackpad only) to roll the camera (rotation around the Z axis)       - pinch gesture (trackpad only) move the camera forward/backward       - alt + pinch gesture (trackpad only) to zoom-in / zoom-out (change the field of view of the camera)
--
-- ObjC selector: @- setAllowsCameraControl:@
setAllowsCameraControl :: IsSCNView scnView => scnView -> Bool -> IO ()
setAllowsCameraControl scnView  value =
    sendMsg scnView (mkSelector "setAllowsCameraControl:") retVoid [argCULong (if value then 1 else 0)]

-- | cameraControlConfiguration
--
-- An object describing the current configuration of the event handler which pilot the default camera controller.
--
-- This object will be used to configure the event handler when allowCameraControl is set to YES.
--
-- ObjC selector: @- cameraControlConfiguration@
cameraControlConfiguration :: IsSCNView scnView => scnView -> IO RawId
cameraControlConfiguration scnView  =
    fmap (RawId . castPtr) $ sendMsg scnView (mkSelector "cameraControlConfiguration") (retPtr retVoid) []

-- | defaultCameraController
--
-- Returns the default SCNCameraController used to drive the current point of view when allowCameraController is set to YES.
--
-- ObjC selector: @- defaultCameraController@
defaultCameraController :: IsSCNView scnView => scnView -> IO (Id SCNCameraController)
defaultCameraController scnView  =
    sendMsg scnView (mkSelector "defaultCameraController") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | preferredFramesPerSecond
--
-- The rate you want the view to redraw its contents.
--
-- When your application sets its preferred frame rate, the view chooses a frame rate as close to that as possible based on the capabilities of the screen the view is displayed on. The actual frame rate chosen is usually a factor of the maximum refresh rate of the screen to provide a consistent frame rate. For example, if the maximum refresh rate of the screen is 60 frames per second, that is also the highest frame rate the view sets as the actual frame rate. However, if you ask for a lower frame rate, it might choose 30, 20, 15 or some other factor to be the actual frame rate. Your application should choose a frame rate that it can consistently maintain.     The default value is 0 which means the display link will fire at the native cadence of the display hardware.
--
-- ObjC selector: @- preferredFramesPerSecond@
preferredFramesPerSecond :: IsSCNView scnView => scnView -> IO CLong
preferredFramesPerSecond scnView  =
    sendMsg scnView (mkSelector "preferredFramesPerSecond") retCLong []

-- | preferredFramesPerSecond
--
-- The rate you want the view to redraw its contents.
--
-- When your application sets its preferred frame rate, the view chooses a frame rate as close to that as possible based on the capabilities of the screen the view is displayed on. The actual frame rate chosen is usually a factor of the maximum refresh rate of the screen to provide a consistent frame rate. For example, if the maximum refresh rate of the screen is 60 frames per second, that is also the highest frame rate the view sets as the actual frame rate. However, if you ask for a lower frame rate, it might choose 30, 20, 15 or some other factor to be the actual frame rate. Your application should choose a frame rate that it can consistently maintain.     The default value is 0 which means the display link will fire at the native cadence of the display hardware.
--
-- ObjC selector: @- setPreferredFramesPerSecond:@
setPreferredFramesPerSecond :: IsSCNView scnView => scnView -> CLong -> IO ()
setPreferredFramesPerSecond scnView  value =
    sendMsg scnView (mkSelector "setPreferredFramesPerSecond:") retVoid [argCLong value]

-- | drawableResizesAsynchronously
--
-- Specifies whether the drawable is resized asynchonously during a live resize operation. Defaults to YES.
--
-- If set to YES, the actual viewport size during a live resize can be retrieved using currentViewport (see SCNSceneRenderer.h)
--
-- ObjC selector: @- drawableResizesAsynchronously@
drawableResizesAsynchronously :: IsSCNView scnView => scnView -> IO Bool
drawableResizesAsynchronously scnView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnView (mkSelector "drawableResizesAsynchronously") retCULong []

-- | drawableResizesAsynchronously
--
-- Specifies whether the drawable is resized asynchonously during a live resize operation. Defaults to YES.
--
-- If set to YES, the actual viewport size during a live resize can be retrieved using currentViewport (see SCNSceneRenderer.h)
--
-- ObjC selector: @- setDrawableResizesAsynchronously:@
setDrawableResizesAsynchronously :: IsSCNView scnView => scnView -> Bool -> IO ()
setDrawableResizesAsynchronously scnView  value =
    sendMsg scnView (mkSelector "setDrawableResizesAsynchronously:") retVoid [argCULong (if value then 1 else 0)]

-- | openGLContext
--
-- Specifies the OpenGL context associated with the receiver.
--
-- This property returns nil and has no effect if the current API is Metal.
--
-- ObjC selector: @- openGLContext@
openGLContext :: IsSCNView scnView => scnView -> IO RawId
openGLContext scnView  =
    fmap (RawId . castPtr) $ sendMsg scnView (mkSelector "openGLContext") (retPtr retVoid) []

-- | openGLContext
--
-- Specifies the OpenGL context associated with the receiver.
--
-- This property returns nil and has no effect if the current API is Metal.
--
-- ObjC selector: @- setOpenGLContext:@
setOpenGLContext :: IsSCNView scnView => scnView -> RawId -> IO ()
setOpenGLContext scnView  value =
    sendMsg scnView (mkSelector "setOpenGLContext:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | antialiasingMode
--
-- Defaults to SCNAntialiasingModeMultisampling4X on macOS and SCNAntialiasingModeNone on iOS.
--
-- ObjC selector: @- antialiasingMode@
antialiasingMode :: IsSCNView scnView => scnView -> IO SCNAntialiasingMode
antialiasingMode scnView  =
    fmap (coerce :: CULong -> SCNAntialiasingMode) $ sendMsg scnView (mkSelector "antialiasingMode") retCULong []

-- | antialiasingMode
--
-- Defaults to SCNAntialiasingModeMultisampling4X on macOS and SCNAntialiasingModeNone on iOS.
--
-- ObjC selector: @- setAntialiasingMode:@
setAntialiasingMode :: IsSCNView scnView => scnView -> SCNAntialiasingMode -> IO ()
setAntialiasingMode scnView  value =
    sendMsg scnView (mkSelector "setAntialiasingMode:") retVoid [argCULong (coerce value)]

-- | pixelFormat
--
-- Specifies the pixel format of the receiver.
--
-- This property returns nil and has no effect if the current API is Metal.
--
-- ObjC selector: @- pixelFormat@
pixelFormat :: IsSCNView scnView => scnView -> IO RawId
pixelFormat scnView  =
    fmap (RawId . castPtr) $ sendMsg scnView (mkSelector "pixelFormat") (retPtr retVoid) []

-- | pixelFormat
--
-- Specifies the pixel format of the receiver.
--
-- This property returns nil and has no effect if the current API is Metal.
--
-- ObjC selector: @- setPixelFormat:@
setPixelFormat :: IsSCNView scnView => scnView -> RawId -> IO ()
setPixelFormat scnView  value =
    sendMsg scnView (mkSelector "setPixelFormat:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFrame:options:@
initWithFrame_optionsSelector :: Selector
initWithFrame_optionsSelector = mkSelector "initWithFrame:options:"

-- | @Selector@ for @snapshot@
snapshotSelector :: Selector
snapshotSelector = mkSelector "snapshot"

-- | @Selector@ for @play:@
playSelector :: Selector
playSelector = mkSelector "play:"

-- | @Selector@ for @pause:@
pauseSelector :: Selector
pauseSelector = mkSelector "pause:"

-- | @Selector@ for @stop:@
stopSelector :: Selector
stopSelector = mkSelector "stop:"

-- | @Selector@ for @scene@
sceneSelector :: Selector
sceneSelector = mkSelector "scene"

-- | @Selector@ for @setScene:@
setSceneSelector :: Selector
setSceneSelector = mkSelector "setScene:"

-- | @Selector@ for @rendersContinuously@
rendersContinuouslySelector :: Selector
rendersContinuouslySelector = mkSelector "rendersContinuously"

-- | @Selector@ for @setRendersContinuously:@
setRendersContinuouslySelector :: Selector
setRendersContinuouslySelector = mkSelector "setRendersContinuously:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @allowsCameraControl@
allowsCameraControlSelector :: Selector
allowsCameraControlSelector = mkSelector "allowsCameraControl"

-- | @Selector@ for @setAllowsCameraControl:@
setAllowsCameraControlSelector :: Selector
setAllowsCameraControlSelector = mkSelector "setAllowsCameraControl:"

-- | @Selector@ for @cameraControlConfiguration@
cameraControlConfigurationSelector :: Selector
cameraControlConfigurationSelector = mkSelector "cameraControlConfiguration"

-- | @Selector@ for @defaultCameraController@
defaultCameraControllerSelector :: Selector
defaultCameraControllerSelector = mkSelector "defaultCameraController"

-- | @Selector@ for @preferredFramesPerSecond@
preferredFramesPerSecondSelector :: Selector
preferredFramesPerSecondSelector = mkSelector "preferredFramesPerSecond"

-- | @Selector@ for @setPreferredFramesPerSecond:@
setPreferredFramesPerSecondSelector :: Selector
setPreferredFramesPerSecondSelector = mkSelector "setPreferredFramesPerSecond:"

-- | @Selector@ for @drawableResizesAsynchronously@
drawableResizesAsynchronouslySelector :: Selector
drawableResizesAsynchronouslySelector = mkSelector "drawableResizesAsynchronously"

-- | @Selector@ for @setDrawableResizesAsynchronously:@
setDrawableResizesAsynchronouslySelector :: Selector
setDrawableResizesAsynchronouslySelector = mkSelector "setDrawableResizesAsynchronously:"

-- | @Selector@ for @openGLContext@
openGLContextSelector :: Selector
openGLContextSelector = mkSelector "openGLContext"

-- | @Selector@ for @setOpenGLContext:@
setOpenGLContextSelector :: Selector
setOpenGLContextSelector = mkSelector "setOpenGLContext:"

-- | @Selector@ for @antialiasingMode@
antialiasingModeSelector :: Selector
antialiasingModeSelector = mkSelector "antialiasingMode"

-- | @Selector@ for @setAntialiasingMode:@
setAntialiasingModeSelector :: Selector
setAntialiasingModeSelector = mkSelector "setAntialiasingMode:"

-- | @Selector@ for @pixelFormat@
pixelFormatSelector :: Selector
pixelFormatSelector = mkSelector "pixelFormat"

-- | @Selector@ for @setPixelFormat:@
setPixelFormatSelector :: Selector
setPixelFormatSelector = mkSelector "setPixelFormat:"

