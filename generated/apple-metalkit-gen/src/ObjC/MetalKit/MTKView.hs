{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTKView
--
-- View for rendering metal content
--
-- Generated bindings for @MTKView@.
module ObjC.MetalKit.MTKView
  ( MTKView
  , IsMTKView(..)
  , initWithCoder
  , releaseDrawables
  , draw
  , delegate
  , setDelegate
  , device
  , setDevice
  , currentDrawable
  , framebufferOnly
  , setFramebufferOnly
  , depthStencilAttachmentTextureUsage
  , setDepthStencilAttachmentTextureUsage
  , multisampleColorAttachmentTextureUsage
  , setMultisampleColorAttachmentTextureUsage
  , presentsWithTransaction
  , setPresentsWithTransaction
  , colorPixelFormat
  , setColorPixelFormat
  , depthStencilPixelFormat
  , setDepthStencilPixelFormat
  , depthStencilStorageMode
  , setDepthStencilStorageMode
  , sampleCount
  , setSampleCount
  , clearDepth
  , setClearDepth
  , clearStencil
  , setClearStencil
  , depthStencilTexture
  , multisampleColorTexture
  , preferredFramesPerSecond
  , setPreferredFramesPerSecond
  , enableSetNeedsDisplay
  , setEnableSetNeedsDisplay
  , autoResizeDrawable
  , setAutoResizeDrawable
  , preferredDevice
  , paused
  , setPaused
  , colorspace
  , setColorspace
  , autoResizeDrawableSelector
  , clearDepthSelector
  , clearStencilSelector
  , colorPixelFormatSelector
  , colorspaceSelector
  , currentDrawableSelector
  , delegateSelector
  , depthStencilAttachmentTextureUsageSelector
  , depthStencilPixelFormatSelector
  , depthStencilStorageModeSelector
  , depthStencilTextureSelector
  , deviceSelector
  , drawSelector
  , enableSetNeedsDisplaySelector
  , framebufferOnlySelector
  , initWithCoderSelector
  , multisampleColorAttachmentTextureUsageSelector
  , multisampleColorTextureSelector
  , pausedSelector
  , preferredDeviceSelector
  , preferredFramesPerSecondSelector
  , presentsWithTransactionSelector
  , releaseDrawablesSelector
  , sampleCountSelector
  , setAutoResizeDrawableSelector
  , setClearDepthSelector
  , setClearStencilSelector
  , setColorPixelFormatSelector
  , setColorspaceSelector
  , setDelegateSelector
  , setDepthStencilAttachmentTextureUsageSelector
  , setDepthStencilPixelFormatSelector
  , setDepthStencilStorageModeSelector
  , setDeviceSelector
  , setEnableSetNeedsDisplaySelector
  , setFramebufferOnlySelector
  , setMultisampleColorAttachmentTextureUsageSelector
  , setPausedSelector
  , setPreferredFramesPerSecondSelector
  , setPresentsWithTransactionSelector
  , setSampleCountSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithCoder:
--
-- Returns a view initalized from data in a given unarchiver
--
-- @coder@ — An unarchiver object
--
-- ObjC selector: @- initWithCoder:@
initWithCoder :: (IsMTKView mtkView, IsNSCoder coder) => mtkView -> coder -> IO (Id MTKView)
initWithCoder mtkView coder =
  sendOwnedMessage mtkView initWithCoderSelector (toNSCoder coder)

-- | releaseDrawables
--
-- Release the depthStencilTexture and multisampleColorTexture
--
-- Can be called by the app to release the textures in order to conserve memory when it goes into the background.   The view will recreate multisampleColorTexture or depthStencilTexture upon the next access of the respective properties.  Both multisampleColorTexture and depthStencilTexture will be recreated in the access to currentRenderPassDescriptor.
--
-- ObjC selector: @- releaseDrawables@
releaseDrawables :: IsMTKView mtkView => mtkView -> IO ()
releaseDrawables mtkView =
  sendMessage mtkView releaseDrawablesSelector

-- | draw
--
-- Manually ask the view to draw new contents. This causes the view to call either the drawInMTKView (delegate) or drawRect (subclass) method.
--
-- Manually ask the view to draw new contents. This causes the view to call either the drawInMTKView (delegate) or drawRect (subclass) method. This should be used when the view's paused proprety is set to true and enableSetNeedsDisplay is set to false.
--
-- ObjC selector: @- draw@
draw :: IsMTKView mtkView => mtkView -> IO ()
draw mtkView =
  sendMessage mtkView drawSelector

-- | delegate
--
-- The delegate handling common view operations
--
-- ObjC selector: @- delegate@
delegate :: IsMTKView mtkView => mtkView -> IO RawId
delegate mtkView =
  sendMessage mtkView delegateSelector

-- | delegate
--
-- The delegate handling common view operations
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsMTKView mtkView => mtkView -> RawId -> IO ()
setDelegate mtkView value =
  sendMessage mtkView setDelegateSelector value

-- | device
--
-- The MTLDevice used to create Metal objects
--
-- This must be explicitly set by the application unless it was passed into the initializer. Defaults to nil
--
-- ObjC selector: @- device@
device :: IsMTKView mtkView => mtkView -> IO RawId
device mtkView =
  sendMessage mtkView deviceSelector

-- | device
--
-- The MTLDevice used to create Metal objects
--
-- This must be explicitly set by the application unless it was passed into the initializer. Defaults to nil
--
-- ObjC selector: @- setDevice:@
setDevice :: IsMTKView mtkView => mtkView -> RawId -> IO ()
setDevice mtkView value =
  sendMessage mtkView setDeviceSelector value

-- | currentDrawable
--
-- The drawable to be used for the current frame.
--
-- currentDrawable is updated at the end -draw (i.e. after the delegate's drawInMTKView method is called)
--
-- ObjC selector: @- currentDrawable@
currentDrawable :: IsMTKView mtkView => mtkView -> IO RawId
currentDrawable mtkView =
  sendMessage mtkView currentDrawableSelector

-- | framebufferOnly
--
-- If the currentDrawable can be used for sampling or texture read operations
--
-- This defaults to YES. This property controls whether or not the returned drawables' MTLTextures may only be used for framebuffer attachments (YES) or whether they may also be used for texture sampling and pixel read/write operations (NO). A value of YES allows the CAMetalLayer to allocate the MTLTexture objects in ways that are optimized for display purposes that makes them unsuitable for sampling. The recommended value for most applications is YES.
--
-- ObjC selector: @- framebufferOnly@
framebufferOnly :: IsMTKView mtkView => mtkView -> IO Bool
framebufferOnly mtkView =
  sendMessage mtkView framebufferOnlySelector

-- | framebufferOnly
--
-- If the currentDrawable can be used for sampling or texture read operations
--
-- This defaults to YES. This property controls whether or not the returned drawables' MTLTextures may only be used for framebuffer attachments (YES) or whether they may also be used for texture sampling and pixel read/write operations (NO). A value of YES allows the CAMetalLayer to allocate the MTLTexture objects in ways that are optimized for display purposes that makes them unsuitable for sampling. The recommended value for most applications is YES.
--
-- ObjC selector: @- setFramebufferOnly:@
setFramebufferOnly :: IsMTKView mtkView => mtkView -> Bool -> IO ()
setFramebufferOnly mtkView value =
  sendMessage mtkView setFramebufferOnlySelector value

-- | depthStencilAttachmentTextureUsage
--
-- The usage flags set on the depth attachment.
--
-- This property controls the texture usage flags set on the MTKView's depth-stencil attachment on creation.  This value defaults to MTLTextureUsageRenderTarget. The recommended value for most applications is MTLTextureUsageRenderTarget. Changing this value re-creates the depth attachment, but any data currently in the depth attachment will be lost.
--
-- ObjC selector: @- depthStencilAttachmentTextureUsage@
depthStencilAttachmentTextureUsage :: IsMTKView mtkView => mtkView -> IO CInt
depthStencilAttachmentTextureUsage mtkView =
  sendMessage mtkView depthStencilAttachmentTextureUsageSelector

-- | depthStencilAttachmentTextureUsage
--
-- The usage flags set on the depth attachment.
--
-- This property controls the texture usage flags set on the MTKView's depth-stencil attachment on creation.  This value defaults to MTLTextureUsageRenderTarget. The recommended value for most applications is MTLTextureUsageRenderTarget. Changing this value re-creates the depth attachment, but any data currently in the depth attachment will be lost.
--
-- ObjC selector: @- setDepthStencilAttachmentTextureUsage:@
setDepthStencilAttachmentTextureUsage :: IsMTKView mtkView => mtkView -> CInt -> IO ()
setDepthStencilAttachmentTextureUsage mtkView value =
  sendMessage mtkView setDepthStencilAttachmentTextureUsageSelector value

-- | multisampleColorAttachmentTextureUsage
--
-- The texture usage flags for the multisample color attachment.
--
-- This property controls the texture usage flags set on the the multisample color attachment attachment.  This value defaults to MTLTextureUsageRenderTarget. The recommended value for most applications is MTLTextureUsageRenderTarget. Changing this value re-creates the multisample color attachment, but any data currently in the multisample color attachment will be lost.
--
-- ObjC selector: @- multisampleColorAttachmentTextureUsage@
multisampleColorAttachmentTextureUsage :: IsMTKView mtkView => mtkView -> IO CInt
multisampleColorAttachmentTextureUsage mtkView =
  sendMessage mtkView multisampleColorAttachmentTextureUsageSelector

-- | multisampleColorAttachmentTextureUsage
--
-- The texture usage flags for the multisample color attachment.
--
-- This property controls the texture usage flags set on the the multisample color attachment attachment.  This value defaults to MTLTextureUsageRenderTarget. The recommended value for most applications is MTLTextureUsageRenderTarget. Changing this value re-creates the multisample color attachment, but any data currently in the multisample color attachment will be lost.
--
-- ObjC selector: @- setMultisampleColorAttachmentTextureUsage:@
setMultisampleColorAttachmentTextureUsage :: IsMTKView mtkView => mtkView -> CInt -> IO ()
setMultisampleColorAttachmentTextureUsage mtkView value =
  sendMessage mtkView setMultisampleColorAttachmentTextureUsageSelector value

-- | presentsWithTransaction
--
-- If the layer should be presented synchronously
--
-- Defaults to NO. When NO, changes to the layer's render buffer appear on-screen asynchronously to normal layer updates. When YES, changes to the MTL content are sent to the screen via the standard CATransaction mechanisms.
--
-- ObjC selector: @- presentsWithTransaction@
presentsWithTransaction :: IsMTKView mtkView => mtkView -> IO Bool
presentsWithTransaction mtkView =
  sendMessage mtkView presentsWithTransactionSelector

-- | presentsWithTransaction
--
-- If the layer should be presented synchronously
--
-- Defaults to NO. When NO, changes to the layer's render buffer appear on-screen asynchronously to normal layer updates. When YES, changes to the MTL content are sent to the screen via the standard CATransaction mechanisms.
--
-- ObjC selector: @- setPresentsWithTransaction:@
setPresentsWithTransaction :: IsMTKView mtkView => mtkView -> Bool -> IO ()
setPresentsWithTransaction mtkView value =
  sendMessage mtkView setPresentsWithTransactionSelector value

-- | colorPixelFormat
--
-- The pixelFormat for the drawable's texture.
--
-- ObjC selector: @- colorPixelFormat@
colorPixelFormat :: IsMTKView mtkView => mtkView -> IO CInt
colorPixelFormat mtkView =
  sendMessage mtkView colorPixelFormatSelector

-- | colorPixelFormat
--
-- The pixelFormat for the drawable's texture.
--
-- ObjC selector: @- setColorPixelFormat:@
setColorPixelFormat :: IsMTKView mtkView => mtkView -> CInt -> IO ()
setColorPixelFormat mtkView value =
  sendMessage mtkView setColorPixelFormatSelector value

-- | depthStencilPixelFormat
--
-- The pixelFormat used to create depthStencilTexture
--
-- ObjC selector: @- depthStencilPixelFormat@
depthStencilPixelFormat :: IsMTKView mtkView => mtkView -> IO CInt
depthStencilPixelFormat mtkView =
  sendMessage mtkView depthStencilPixelFormatSelector

-- | depthStencilPixelFormat
--
-- The pixelFormat used to create depthStencilTexture
--
-- ObjC selector: @- setDepthStencilPixelFormat:@
setDepthStencilPixelFormat :: IsMTKView mtkView => mtkView -> CInt -> IO ()
setDepthStencilPixelFormat mtkView value =
  sendMessage mtkView setDepthStencilPixelFormatSelector value

-- | depthStencilStorageMode
--
-- The storage mode for the depthStencilTexture. Defaults to MTLStorageModePrivate.
--
-- ObjC selector: @- depthStencilStorageMode@
depthStencilStorageMode :: IsMTKView mtkView => mtkView -> IO CInt
depthStencilStorageMode mtkView =
  sendMessage mtkView depthStencilStorageModeSelector

-- | depthStencilStorageMode
--
-- The storage mode for the depthStencilTexture. Defaults to MTLStorageModePrivate.
--
-- ObjC selector: @- setDepthStencilStorageMode:@
setDepthStencilStorageMode :: IsMTKView mtkView => mtkView -> CInt -> IO ()
setDepthStencilStorageMode mtkView value =
  sendMessage mtkView setDepthStencilStorageModeSelector value

-- | sampleCount
--
-- The sample count used to to create multisampleColorTexture
--
-- This defaults to 1.  If sampleCount is greater than 1 a multisampled color texture will be created and the currentDrawable's texture will be set as the resolve texture in the currentRenderPassDescriptor and the store action will be set to MTLStoreActionMultisampleResolve
--
-- ObjC selector: @- sampleCount@
sampleCount :: IsMTKView mtkView => mtkView -> IO CULong
sampleCount mtkView =
  sendMessage mtkView sampleCountSelector

-- | sampleCount
--
-- The sample count used to to create multisampleColorTexture
--
-- This defaults to 1.  If sampleCount is greater than 1 a multisampled color texture will be created and the currentDrawable's texture will be set as the resolve texture in the currentRenderPassDescriptor and the store action will be set to MTLStoreActionMultisampleResolve
--
-- ObjC selector: @- setSampleCount:@
setSampleCount :: IsMTKView mtkView => mtkView -> CULong -> IO ()
setSampleCount mtkView value =
  sendMessage mtkView setSampleCountSelector value

-- | clearDepth
--
-- The clear depth value used to generate the currentRenderPassDescriptor
--
-- This defaults to 1.0
--
-- ObjC selector: @- clearDepth@
clearDepth :: IsMTKView mtkView => mtkView -> IO CDouble
clearDepth mtkView =
  sendMessage mtkView clearDepthSelector

-- | clearDepth
--
-- The clear depth value used to generate the currentRenderPassDescriptor
--
-- This defaults to 1.0
--
-- ObjC selector: @- setClearDepth:@
setClearDepth :: IsMTKView mtkView => mtkView -> CDouble -> IO ()
setClearDepth mtkView value =
  sendMessage mtkView setClearDepthSelector value

-- | clearStencil
--
-- The clear stencil value used to generate currentRenderPassDescriptor
--
-- This defaults to 0
--
-- ObjC selector: @- clearStencil@
clearStencil :: IsMTKView mtkView => mtkView -> IO CUInt
clearStencil mtkView =
  sendMessage mtkView clearStencilSelector

-- | clearStencil
--
-- The clear stencil value used to generate currentRenderPassDescriptor
--
-- This defaults to 0
--
-- ObjC selector: @- setClearStencil:@
setClearStencil :: IsMTKView mtkView => mtkView -> CUInt -> IO ()
setClearStencil mtkView value =
  sendMessage mtkView setClearStencilSelector value

-- | depthStencilTexture
--
-- A packed depth and stencil texture to be attached to a MTLRenderPassDescriptor
--
-- The view will generate the depth buffer using the specified depthPixelFormat.  This will be nil if depthStencilPixelFormat is MTLPixelFormatInvalid.
--
-- ObjC selector: @- depthStencilTexture@
depthStencilTexture :: IsMTKView mtkView => mtkView -> IO RawId
depthStencilTexture mtkView =
  sendMessage mtkView depthStencilTextureSelector

-- | multisampleColorTexture
--
-- A multisample color texture that will be resolved into the currentDrawable's texture
--
-- The view will generate the multisample color buffer using the specified colorPixelFormat.  This will be nil if sampleCount is less than or equal to 1.
--
-- ObjC selector: @- multisampleColorTexture@
multisampleColorTexture :: IsMTKView mtkView => mtkView -> IO RawId
multisampleColorTexture mtkView =
  sendMessage mtkView multisampleColorTextureSelector

-- | preferredFramesPerSecond
--
-- The rate you want the view to redraw its contents.
--
-- When your application sets its preferred frame rate, the view chooses a frame rate as close to that as possible based on the capabilities of the screen the view is displayed on. The actual frame rate chosen is usually a factor of the maximum refresh rate of the screen to provide a consistent frame rate. For example, if the maximum refresh rate of the screen is 60 frames per second, that is also the highest frame rate the view sets as the actual frame rate. However, if you ask for a lower frame rate, it might choose 30, 20, 15 or some other factor to be the actual frame rate. Your application should choose a frame rate that it can consistently maintain. The default value is 60 frames per second.
--
-- ObjC selector: @- preferredFramesPerSecond@
preferredFramesPerSecond :: IsMTKView mtkView => mtkView -> IO CLong
preferredFramesPerSecond mtkView =
  sendMessage mtkView preferredFramesPerSecondSelector

-- | preferredFramesPerSecond
--
-- The rate you want the view to redraw its contents.
--
-- When your application sets its preferred frame rate, the view chooses a frame rate as close to that as possible based on the capabilities of the screen the view is displayed on. The actual frame rate chosen is usually a factor of the maximum refresh rate of the screen to provide a consistent frame rate. For example, if the maximum refresh rate of the screen is 60 frames per second, that is also the highest frame rate the view sets as the actual frame rate. However, if you ask for a lower frame rate, it might choose 30, 20, 15 or some other factor to be the actual frame rate. Your application should choose a frame rate that it can consistently maintain. The default value is 60 frames per second.
--
-- ObjC selector: @- setPreferredFramesPerSecond:@
setPreferredFramesPerSecond :: IsMTKView mtkView => mtkView -> CLong -> IO ()
setPreferredFramesPerSecond mtkView value =
  sendMessage mtkView setPreferredFramesPerSecondSelector value

-- | enableSetNeedsDisplay
--
-- Controls whether the view responds to setNeedsDisplay.
--
-- If true, then the view behaves similarily to a UIView or NSView, responding to calls to setNeedsDisplay. When the view has been marked for display, the view is automatically redisplayed on each pass through the application’s event loop. Setting enableSetNeedsDisplay to true will also pause the MTKView's internal render loop and updates will instead be event driven. The default value is false.
--
-- ObjC selector: @- enableSetNeedsDisplay@
enableSetNeedsDisplay :: IsMTKView mtkView => mtkView -> IO Bool
enableSetNeedsDisplay mtkView =
  sendMessage mtkView enableSetNeedsDisplaySelector

-- | enableSetNeedsDisplay
--
-- Controls whether the view responds to setNeedsDisplay.
--
-- If true, then the view behaves similarily to a UIView or NSView, responding to calls to setNeedsDisplay. When the view has been marked for display, the view is automatically redisplayed on each pass through the application’s event loop. Setting enableSetNeedsDisplay to true will also pause the MTKView's internal render loop and updates will instead be event driven. The default value is false.
--
-- ObjC selector: @- setEnableSetNeedsDisplay:@
setEnableSetNeedsDisplay :: IsMTKView mtkView => mtkView -> Bool -> IO ()
setEnableSetNeedsDisplay mtkView value =
  sendMessage mtkView setEnableSetNeedsDisplaySelector value

-- | autoResizeDrawable
--
-- Controls whether to resize the drawable as the view changes size.
--
-- If true, the size of the currentDrawable's texture, depthStencilTexture, and multisampleColorTexture will automatically resize as the view resizes.  If false, these textures will take on the size of drawableSize and drawableSize will not change. The default value is true.
--
-- ObjC selector: @- autoResizeDrawable@
autoResizeDrawable :: IsMTKView mtkView => mtkView -> IO Bool
autoResizeDrawable mtkView =
  sendMessage mtkView autoResizeDrawableSelector

-- | autoResizeDrawable
--
-- Controls whether to resize the drawable as the view changes size.
--
-- If true, the size of the currentDrawable's texture, depthStencilTexture, and multisampleColorTexture will automatically resize as the view resizes.  If false, these textures will take on the size of drawableSize and drawableSize will not change. The default value is true.
--
-- ObjC selector: @- setAutoResizeDrawable:@
setAutoResizeDrawable :: IsMTKView mtkView => mtkView -> Bool -> IO ()
setAutoResizeDrawable mtkView value =
  sendMessage mtkView setAutoResizeDrawableSelector value

-- | preferredDevice
--
-- The preferred device is updated per-frame by the system in order to identify the most efficient GPU for presentation (e.g. the one being used for compositing).
--
-- This value is determined by the underlying CAMetalLayer and this property is a convenience accessor for it.
--
-- ObjC selector: @- preferredDevice@
preferredDevice :: IsMTKView mtkView => mtkView -> IO RawId
preferredDevice mtkView =
  sendMessage mtkView preferredDeviceSelector

-- | paused
--
-- Controls whether the draw methods should countinue at preferredFramesPerSecond
--
-- If true, the delegate will receive drawInMTKView: messages or the subclass will receive drawRect: messages at a rate of preferredFramesPerSecond based on an internal timer. The default value is false.
--
-- ObjC selector: @- paused@
paused :: IsMTKView mtkView => mtkView -> IO Bool
paused mtkView =
  sendMessage mtkView pausedSelector

-- | paused
--
-- Controls whether the draw methods should countinue at preferredFramesPerSecond
--
-- If true, the delegate will receive drawInMTKView: messages or the subclass will receive drawRect: messages at a rate of preferredFramesPerSecond based on an internal timer. The default value is false.
--
-- ObjC selector: @- setPaused:@
setPaused :: IsMTKView mtkView => mtkView -> Bool -> IO ()
setPaused mtkView value =
  sendMessage mtkView setPausedSelector value

-- | colorspace
--
-- The colorspace of the rendered frames. '
--
-- If nil, no colormatching occurs.  If non-nil, the rendered content will be colormatched to the colorspace of the context containing this layer (typically the display's colorspace).  This property aliases the olorspace property or the view's CAMetalLayer
--
-- ObjC selector: @- colorspace@
colorspace :: IsMTKView mtkView => mtkView -> IO (Ptr ())
colorspace mtkView =
  sendMessage mtkView colorspaceSelector

-- | colorspace
--
-- The colorspace of the rendered frames. '
--
-- If nil, no colormatching occurs.  If non-nil, the rendered content will be colormatched to the colorspace of the context containing this layer (typically the display's colorspace).  This property aliases the olorspace property or the view's CAMetalLayer
--
-- ObjC selector: @- setColorspace:@
setColorspace :: IsMTKView mtkView => mtkView -> Ptr () -> IO ()
setColorspace mtkView value =
  sendMessage mtkView setColorspaceSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id MTKView)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @releaseDrawables@
releaseDrawablesSelector :: Selector '[] ()
releaseDrawablesSelector = mkSelector "releaseDrawables"

-- | @Selector@ for @draw@
drawSelector :: Selector '[] ()
drawSelector = mkSelector "draw"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @device@
deviceSelector :: Selector '[] RawId
deviceSelector = mkSelector "device"

-- | @Selector@ for @setDevice:@
setDeviceSelector :: Selector '[RawId] ()
setDeviceSelector = mkSelector "setDevice:"

-- | @Selector@ for @currentDrawable@
currentDrawableSelector :: Selector '[] RawId
currentDrawableSelector = mkSelector "currentDrawable"

-- | @Selector@ for @framebufferOnly@
framebufferOnlySelector :: Selector '[] Bool
framebufferOnlySelector = mkSelector "framebufferOnly"

-- | @Selector@ for @setFramebufferOnly:@
setFramebufferOnlySelector :: Selector '[Bool] ()
setFramebufferOnlySelector = mkSelector "setFramebufferOnly:"

-- | @Selector@ for @depthStencilAttachmentTextureUsage@
depthStencilAttachmentTextureUsageSelector :: Selector '[] CInt
depthStencilAttachmentTextureUsageSelector = mkSelector "depthStencilAttachmentTextureUsage"

-- | @Selector@ for @setDepthStencilAttachmentTextureUsage:@
setDepthStencilAttachmentTextureUsageSelector :: Selector '[CInt] ()
setDepthStencilAttachmentTextureUsageSelector = mkSelector "setDepthStencilAttachmentTextureUsage:"

-- | @Selector@ for @multisampleColorAttachmentTextureUsage@
multisampleColorAttachmentTextureUsageSelector :: Selector '[] CInt
multisampleColorAttachmentTextureUsageSelector = mkSelector "multisampleColorAttachmentTextureUsage"

-- | @Selector@ for @setMultisampleColorAttachmentTextureUsage:@
setMultisampleColorAttachmentTextureUsageSelector :: Selector '[CInt] ()
setMultisampleColorAttachmentTextureUsageSelector = mkSelector "setMultisampleColorAttachmentTextureUsage:"

-- | @Selector@ for @presentsWithTransaction@
presentsWithTransactionSelector :: Selector '[] Bool
presentsWithTransactionSelector = mkSelector "presentsWithTransaction"

-- | @Selector@ for @setPresentsWithTransaction:@
setPresentsWithTransactionSelector :: Selector '[Bool] ()
setPresentsWithTransactionSelector = mkSelector "setPresentsWithTransaction:"

-- | @Selector@ for @colorPixelFormat@
colorPixelFormatSelector :: Selector '[] CInt
colorPixelFormatSelector = mkSelector "colorPixelFormat"

-- | @Selector@ for @setColorPixelFormat:@
setColorPixelFormatSelector :: Selector '[CInt] ()
setColorPixelFormatSelector = mkSelector "setColorPixelFormat:"

-- | @Selector@ for @depthStencilPixelFormat@
depthStencilPixelFormatSelector :: Selector '[] CInt
depthStencilPixelFormatSelector = mkSelector "depthStencilPixelFormat"

-- | @Selector@ for @setDepthStencilPixelFormat:@
setDepthStencilPixelFormatSelector :: Selector '[CInt] ()
setDepthStencilPixelFormatSelector = mkSelector "setDepthStencilPixelFormat:"

-- | @Selector@ for @depthStencilStorageMode@
depthStencilStorageModeSelector :: Selector '[] CInt
depthStencilStorageModeSelector = mkSelector "depthStencilStorageMode"

-- | @Selector@ for @setDepthStencilStorageMode:@
setDepthStencilStorageModeSelector :: Selector '[CInt] ()
setDepthStencilStorageModeSelector = mkSelector "setDepthStencilStorageMode:"

-- | @Selector@ for @sampleCount@
sampleCountSelector :: Selector '[] CULong
sampleCountSelector = mkSelector "sampleCount"

-- | @Selector@ for @setSampleCount:@
setSampleCountSelector :: Selector '[CULong] ()
setSampleCountSelector = mkSelector "setSampleCount:"

-- | @Selector@ for @clearDepth@
clearDepthSelector :: Selector '[] CDouble
clearDepthSelector = mkSelector "clearDepth"

-- | @Selector@ for @setClearDepth:@
setClearDepthSelector :: Selector '[CDouble] ()
setClearDepthSelector = mkSelector "setClearDepth:"

-- | @Selector@ for @clearStencil@
clearStencilSelector :: Selector '[] CUInt
clearStencilSelector = mkSelector "clearStencil"

-- | @Selector@ for @setClearStencil:@
setClearStencilSelector :: Selector '[CUInt] ()
setClearStencilSelector = mkSelector "setClearStencil:"

-- | @Selector@ for @depthStencilTexture@
depthStencilTextureSelector :: Selector '[] RawId
depthStencilTextureSelector = mkSelector "depthStencilTexture"

-- | @Selector@ for @multisampleColorTexture@
multisampleColorTextureSelector :: Selector '[] RawId
multisampleColorTextureSelector = mkSelector "multisampleColorTexture"

-- | @Selector@ for @preferredFramesPerSecond@
preferredFramesPerSecondSelector :: Selector '[] CLong
preferredFramesPerSecondSelector = mkSelector "preferredFramesPerSecond"

-- | @Selector@ for @setPreferredFramesPerSecond:@
setPreferredFramesPerSecondSelector :: Selector '[CLong] ()
setPreferredFramesPerSecondSelector = mkSelector "setPreferredFramesPerSecond:"

-- | @Selector@ for @enableSetNeedsDisplay@
enableSetNeedsDisplaySelector :: Selector '[] Bool
enableSetNeedsDisplaySelector = mkSelector "enableSetNeedsDisplay"

-- | @Selector@ for @setEnableSetNeedsDisplay:@
setEnableSetNeedsDisplaySelector :: Selector '[Bool] ()
setEnableSetNeedsDisplaySelector = mkSelector "setEnableSetNeedsDisplay:"

-- | @Selector@ for @autoResizeDrawable@
autoResizeDrawableSelector :: Selector '[] Bool
autoResizeDrawableSelector = mkSelector "autoResizeDrawable"

-- | @Selector@ for @setAutoResizeDrawable:@
setAutoResizeDrawableSelector :: Selector '[Bool] ()
setAutoResizeDrawableSelector = mkSelector "setAutoResizeDrawable:"

-- | @Selector@ for @preferredDevice@
preferredDeviceSelector :: Selector '[] RawId
preferredDeviceSelector = mkSelector "preferredDevice"

-- | @Selector@ for @paused@
pausedSelector :: Selector '[] Bool
pausedSelector = mkSelector "paused"

-- | @Selector@ for @setPaused:@
setPausedSelector :: Selector '[Bool] ()
setPausedSelector = mkSelector "setPaused:"

-- | @Selector@ for @colorspace@
colorspaceSelector :: Selector '[] (Ptr ())
colorspaceSelector = mkSelector "colorspace"

-- | @Selector@ for @setColorspace:@
setColorspaceSelector :: Selector '[Ptr ()] ()
setColorspaceSelector = mkSelector "setColorspace:"

