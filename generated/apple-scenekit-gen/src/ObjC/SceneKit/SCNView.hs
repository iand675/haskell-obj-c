{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , allowsCameraControlSelector
  , antialiasingModeSelector
  , backgroundColorSelector
  , cameraControlConfigurationSelector
  , defaultCameraControllerSelector
  , drawableResizesAsynchronouslySelector
  , initWithFrame_optionsSelector
  , openGLContextSelector
  , pauseSelector
  , pixelFormatSelector
  , playSelector
  , preferredFramesPerSecondSelector
  , rendersContinuouslySelector
  , sceneSelector
  , setAllowsCameraControlSelector
  , setAntialiasingModeSelector
  , setBackgroundColorSelector
  , setDrawableResizesAsynchronouslySelector
  , setOpenGLContextSelector
  , setPixelFormatSelector
  , setPreferredFramesPerSecondSelector
  , setRendersContinuouslySelector
  , setSceneSelector
  , snapshotSelector
  , stopSelector

  -- * Enum types
  , SCNAntialiasingMode(SCNAntialiasingMode)
  , pattern SCNAntialiasingModeNone
  , pattern SCNAntialiasingModeMultisampling2X
  , pattern SCNAntialiasingModeMultisampling4X
  , pattern SCNAntialiasingModeMultisampling8X
  , pattern SCNAntialiasingModeMultisampling16X

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithFrame_options scnView frame options =
  sendOwnedMessage scnView initWithFrame_optionsSelector frame (toNSDictionary options)

-- | snapshot
--
-- Draws the contents of the view and returns them as a new image object
--
-- This method is thread-safe and may be called at any time.
--
-- ObjC selector: @- snapshot@
snapshot :: IsSCNView scnView => scnView -> IO (Id NSImage)
snapshot scnView =
  sendMessage scnView snapshotSelector

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
play scnView sender =
  sendMessage scnView playSelector sender

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
pause scnView sender =
  sendMessage scnView pauseSelector sender

-- | stop:
--
-- This action method stops the scene playback and resets the current time to the start time of the scene.
--
-- @sender@ — The object (such as a button or menu item) sending the message to stop playing the scene.
--
-- ObjC selector: @- stop:@
stop :: IsSCNView scnView => scnView -> RawId -> IO ()
stop scnView sender =
  sendMessage scnView stopSelector sender

-- | scene
--
-- Specifies the scene of the receiver
--
-- ObjC selector: @- scene@
scene :: IsSCNView scnView => scnView -> IO (Id SCNScene)
scene scnView =
  sendMessage scnView sceneSelector

-- | scene
--
-- Specifies the scene of the receiver
--
-- ObjC selector: @- setScene:@
setScene :: (IsSCNView scnView, IsSCNScene value) => scnView -> value -> IO ()
setScene scnView value =
  sendMessage scnView setSceneSelector (toSCNScene value)

-- | rendersContinuously
--
-- When set to YES, the view continously redraw at the display link frame rate. When set to NO the view will only redraw when something change or animates in the receiver's scene. Defaults to NO.
--
-- ObjC selector: @- rendersContinuously@
rendersContinuously :: IsSCNView scnView => scnView -> IO Bool
rendersContinuously scnView =
  sendMessage scnView rendersContinuouslySelector

-- | rendersContinuously
--
-- When set to YES, the view continously redraw at the display link frame rate. When set to NO the view will only redraw when something change or animates in the receiver's scene. Defaults to NO.
--
-- ObjC selector: @- setRendersContinuously:@
setRendersContinuously :: IsSCNView scnView => scnView -> Bool -> IO ()
setRendersContinuously scnView value =
  sendMessage scnView setRendersContinuouslySelector value

-- | backgroundColor
--
-- Specifies the background color of the receiver. Defaults to opaque white.
--
-- ObjC selector: @- backgroundColor@
backgroundColor :: IsSCNView scnView => scnView -> IO (Id NSColor)
backgroundColor scnView =
  sendMessage scnView backgroundColorSelector

-- | backgroundColor
--
-- Specifies the background color of the receiver. Defaults to opaque white.
--
-- ObjC selector: @- setBackgroundColor:@
setBackgroundColor :: (IsSCNView scnView, IsNSColor value) => scnView -> value -> IO ()
setBackgroundColor scnView value =
  sendMessage scnView setBackgroundColorSelector (toNSColor value)

-- | allowsCameraControl
--
-- A Boolean value that determines whether the user can manipulate the point of view used to render the scene.
--
-- When set to YES, the user can manipulate the current point of view with the mouse or the trackpad. The scene graph and existing cameras won't be modified by this action. The default value of this property is NO.     Note that the primary purpose of this property is to aid in debugging your application. You may want to implement your own camera controller suitable for your application.     The built-in camera controller let you:       - drag the mouse to rotate the camera around the scene       - drag+cmd to rotate the camera in local space       - drag+shift to rotate using sticky axis       - use the scroll wheel or alt+drag the mouse to translate the camera on its local X,Y plan       - alt+scroll wheel to move the camera forward/backward       - rotate gesture (trackpad only) to roll the camera (rotation around the Z axis)       - pinch gesture (trackpad only) move the camera forward/backward       - alt + pinch gesture (trackpad only) to zoom-in / zoom-out (change the field of view of the camera)
--
-- ObjC selector: @- allowsCameraControl@
allowsCameraControl :: IsSCNView scnView => scnView -> IO Bool
allowsCameraControl scnView =
  sendMessage scnView allowsCameraControlSelector

-- | allowsCameraControl
--
-- A Boolean value that determines whether the user can manipulate the point of view used to render the scene.
--
-- When set to YES, the user can manipulate the current point of view with the mouse or the trackpad. The scene graph and existing cameras won't be modified by this action. The default value of this property is NO.     Note that the primary purpose of this property is to aid in debugging your application. You may want to implement your own camera controller suitable for your application.     The built-in camera controller let you:       - drag the mouse to rotate the camera around the scene       - drag+cmd to rotate the camera in local space       - drag+shift to rotate using sticky axis       - use the scroll wheel or alt+drag the mouse to translate the camera on its local X,Y plan       - alt+scroll wheel to move the camera forward/backward       - rotate gesture (trackpad only) to roll the camera (rotation around the Z axis)       - pinch gesture (trackpad only) move the camera forward/backward       - alt + pinch gesture (trackpad only) to zoom-in / zoom-out (change the field of view of the camera)
--
-- ObjC selector: @- setAllowsCameraControl:@
setAllowsCameraControl :: IsSCNView scnView => scnView -> Bool -> IO ()
setAllowsCameraControl scnView value =
  sendMessage scnView setAllowsCameraControlSelector value

-- | cameraControlConfiguration
--
-- An object describing the current configuration of the event handler which pilot the default camera controller.
--
-- This object will be used to configure the event handler when allowCameraControl is set to YES.
--
-- ObjC selector: @- cameraControlConfiguration@
cameraControlConfiguration :: IsSCNView scnView => scnView -> IO RawId
cameraControlConfiguration scnView =
  sendMessage scnView cameraControlConfigurationSelector

-- | defaultCameraController
--
-- Returns the default SCNCameraController used to drive the current point of view when allowCameraController is set to YES.
--
-- ObjC selector: @- defaultCameraController@
defaultCameraController :: IsSCNView scnView => scnView -> IO (Id SCNCameraController)
defaultCameraController scnView =
  sendMessage scnView defaultCameraControllerSelector

-- | preferredFramesPerSecond
--
-- The rate you want the view to redraw its contents.
--
-- When your application sets its preferred frame rate, the view chooses a frame rate as close to that as possible based on the capabilities of the screen the view is displayed on. The actual frame rate chosen is usually a factor of the maximum refresh rate of the screen to provide a consistent frame rate. For example, if the maximum refresh rate of the screen is 60 frames per second, that is also the highest frame rate the view sets as the actual frame rate. However, if you ask for a lower frame rate, it might choose 30, 20, 15 or some other factor to be the actual frame rate. Your application should choose a frame rate that it can consistently maintain.     The default value is 0 which means the display link will fire at the native cadence of the display hardware.
--
-- ObjC selector: @- preferredFramesPerSecond@
preferredFramesPerSecond :: IsSCNView scnView => scnView -> IO CLong
preferredFramesPerSecond scnView =
  sendMessage scnView preferredFramesPerSecondSelector

-- | preferredFramesPerSecond
--
-- The rate you want the view to redraw its contents.
--
-- When your application sets its preferred frame rate, the view chooses a frame rate as close to that as possible based on the capabilities of the screen the view is displayed on. The actual frame rate chosen is usually a factor of the maximum refresh rate of the screen to provide a consistent frame rate. For example, if the maximum refresh rate of the screen is 60 frames per second, that is also the highest frame rate the view sets as the actual frame rate. However, if you ask for a lower frame rate, it might choose 30, 20, 15 or some other factor to be the actual frame rate. Your application should choose a frame rate that it can consistently maintain.     The default value is 0 which means the display link will fire at the native cadence of the display hardware.
--
-- ObjC selector: @- setPreferredFramesPerSecond:@
setPreferredFramesPerSecond :: IsSCNView scnView => scnView -> CLong -> IO ()
setPreferredFramesPerSecond scnView value =
  sendMessage scnView setPreferredFramesPerSecondSelector value

-- | drawableResizesAsynchronously
--
-- Specifies whether the drawable is resized asynchonously during a live resize operation. Defaults to YES.
--
-- If set to YES, the actual viewport size during a live resize can be retrieved using currentViewport (see SCNSceneRenderer.h)
--
-- ObjC selector: @- drawableResizesAsynchronously@
drawableResizesAsynchronously :: IsSCNView scnView => scnView -> IO Bool
drawableResizesAsynchronously scnView =
  sendMessage scnView drawableResizesAsynchronouslySelector

-- | drawableResizesAsynchronously
--
-- Specifies whether the drawable is resized asynchonously during a live resize operation. Defaults to YES.
--
-- If set to YES, the actual viewport size during a live resize can be retrieved using currentViewport (see SCNSceneRenderer.h)
--
-- ObjC selector: @- setDrawableResizesAsynchronously:@
setDrawableResizesAsynchronously :: IsSCNView scnView => scnView -> Bool -> IO ()
setDrawableResizesAsynchronously scnView value =
  sendMessage scnView setDrawableResizesAsynchronouslySelector value

-- | openGLContext
--
-- Specifies the OpenGL context associated with the receiver.
--
-- This property returns nil and has no effect if the current API is Metal.
--
-- ObjC selector: @- openGLContext@
openGLContext :: IsSCNView scnView => scnView -> IO RawId
openGLContext scnView =
  sendMessage scnView openGLContextSelector

-- | openGLContext
--
-- Specifies the OpenGL context associated with the receiver.
--
-- This property returns nil and has no effect if the current API is Metal.
--
-- ObjC selector: @- setOpenGLContext:@
setOpenGLContext :: IsSCNView scnView => scnView -> RawId -> IO ()
setOpenGLContext scnView value =
  sendMessage scnView setOpenGLContextSelector value

-- | antialiasingMode
--
-- Defaults to SCNAntialiasingModeMultisampling4X on macOS and SCNAntialiasingModeNone on iOS.
--
-- ObjC selector: @- antialiasingMode@
antialiasingMode :: IsSCNView scnView => scnView -> IO SCNAntialiasingMode
antialiasingMode scnView =
  sendMessage scnView antialiasingModeSelector

-- | antialiasingMode
--
-- Defaults to SCNAntialiasingModeMultisampling4X on macOS and SCNAntialiasingModeNone on iOS.
--
-- ObjC selector: @- setAntialiasingMode:@
setAntialiasingMode :: IsSCNView scnView => scnView -> SCNAntialiasingMode -> IO ()
setAntialiasingMode scnView value =
  sendMessage scnView setAntialiasingModeSelector value

-- | pixelFormat
--
-- Specifies the pixel format of the receiver.
--
-- This property returns nil and has no effect if the current API is Metal.
--
-- ObjC selector: @- pixelFormat@
pixelFormat :: IsSCNView scnView => scnView -> IO RawId
pixelFormat scnView =
  sendMessage scnView pixelFormatSelector

-- | pixelFormat
--
-- Specifies the pixel format of the receiver.
--
-- This property returns nil and has no effect if the current API is Metal.
--
-- ObjC selector: @- setPixelFormat:@
setPixelFormat :: IsSCNView scnView => scnView -> RawId -> IO ()
setPixelFormat scnView value =
  sendMessage scnView setPixelFormatSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFrame:options:@
initWithFrame_optionsSelector :: Selector '[NSRect, Id NSDictionary] (Id SCNView)
initWithFrame_optionsSelector = mkSelector "initWithFrame:options:"

-- | @Selector@ for @snapshot@
snapshotSelector :: Selector '[] (Id NSImage)
snapshotSelector = mkSelector "snapshot"

-- | @Selector@ for @play:@
playSelector :: Selector '[RawId] ()
playSelector = mkSelector "play:"

-- | @Selector@ for @pause:@
pauseSelector :: Selector '[RawId] ()
pauseSelector = mkSelector "pause:"

-- | @Selector@ for @stop:@
stopSelector :: Selector '[RawId] ()
stopSelector = mkSelector "stop:"

-- | @Selector@ for @scene@
sceneSelector :: Selector '[] (Id SCNScene)
sceneSelector = mkSelector "scene"

-- | @Selector@ for @setScene:@
setSceneSelector :: Selector '[Id SCNScene] ()
setSceneSelector = mkSelector "setScene:"

-- | @Selector@ for @rendersContinuously@
rendersContinuouslySelector :: Selector '[] Bool
rendersContinuouslySelector = mkSelector "rendersContinuously"

-- | @Selector@ for @setRendersContinuously:@
setRendersContinuouslySelector :: Selector '[Bool] ()
setRendersContinuouslySelector = mkSelector "setRendersContinuously:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector '[] (Id NSColor)
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector '[Id NSColor] ()
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @allowsCameraControl@
allowsCameraControlSelector :: Selector '[] Bool
allowsCameraControlSelector = mkSelector "allowsCameraControl"

-- | @Selector@ for @setAllowsCameraControl:@
setAllowsCameraControlSelector :: Selector '[Bool] ()
setAllowsCameraControlSelector = mkSelector "setAllowsCameraControl:"

-- | @Selector@ for @cameraControlConfiguration@
cameraControlConfigurationSelector :: Selector '[] RawId
cameraControlConfigurationSelector = mkSelector "cameraControlConfiguration"

-- | @Selector@ for @defaultCameraController@
defaultCameraControllerSelector :: Selector '[] (Id SCNCameraController)
defaultCameraControllerSelector = mkSelector "defaultCameraController"

-- | @Selector@ for @preferredFramesPerSecond@
preferredFramesPerSecondSelector :: Selector '[] CLong
preferredFramesPerSecondSelector = mkSelector "preferredFramesPerSecond"

-- | @Selector@ for @setPreferredFramesPerSecond:@
setPreferredFramesPerSecondSelector :: Selector '[CLong] ()
setPreferredFramesPerSecondSelector = mkSelector "setPreferredFramesPerSecond:"

-- | @Selector@ for @drawableResizesAsynchronously@
drawableResizesAsynchronouslySelector :: Selector '[] Bool
drawableResizesAsynchronouslySelector = mkSelector "drawableResizesAsynchronously"

-- | @Selector@ for @setDrawableResizesAsynchronously:@
setDrawableResizesAsynchronouslySelector :: Selector '[Bool] ()
setDrawableResizesAsynchronouslySelector = mkSelector "setDrawableResizesAsynchronously:"

-- | @Selector@ for @openGLContext@
openGLContextSelector :: Selector '[] RawId
openGLContextSelector = mkSelector "openGLContext"

-- | @Selector@ for @setOpenGLContext:@
setOpenGLContextSelector :: Selector '[RawId] ()
setOpenGLContextSelector = mkSelector "setOpenGLContext:"

-- | @Selector@ for @antialiasingMode@
antialiasingModeSelector :: Selector '[] SCNAntialiasingMode
antialiasingModeSelector = mkSelector "antialiasingMode"

-- | @Selector@ for @setAntialiasingMode:@
setAntialiasingModeSelector :: Selector '[SCNAntialiasingMode] ()
setAntialiasingModeSelector = mkSelector "setAntialiasingMode:"

-- | @Selector@ for @pixelFormat@
pixelFormatSelector :: Selector '[] RawId
pixelFormatSelector = mkSelector "pixelFormat"

-- | @Selector@ for @setPixelFormat:@
setPixelFormatSelector :: Selector '[RawId] ()
setPixelFormatSelector = mkSelector "setPixelFormat:"

