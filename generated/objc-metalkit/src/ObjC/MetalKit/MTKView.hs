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
  , preferredFramesPerSecond
  , setPreferredFramesPerSecond
  , enableSetNeedsDisplay
  , setEnableSetNeedsDisplay
  , autoResizeDrawable
  , setAutoResizeDrawable
  , paused
  , setPaused
  , colorspace
  , setColorspace
  , initWithCoderSelector
  , releaseDrawablesSelector
  , drawSelector
  , framebufferOnlySelector
  , setFramebufferOnlySelector
  , depthStencilAttachmentTextureUsageSelector
  , setDepthStencilAttachmentTextureUsageSelector
  , multisampleColorAttachmentTextureUsageSelector
  , setMultisampleColorAttachmentTextureUsageSelector
  , presentsWithTransactionSelector
  , setPresentsWithTransactionSelector
  , colorPixelFormatSelector
  , setColorPixelFormatSelector
  , depthStencilPixelFormatSelector
  , setDepthStencilPixelFormatSelector
  , depthStencilStorageModeSelector
  , setDepthStencilStorageModeSelector
  , sampleCountSelector
  , setSampleCountSelector
  , clearDepthSelector
  , setClearDepthSelector
  , clearStencilSelector
  , setClearStencilSelector
  , preferredFramesPerSecondSelector
  , setPreferredFramesPerSecondSelector
  , enableSetNeedsDisplaySelector
  , setEnableSetNeedsDisplaySelector
  , autoResizeDrawableSelector
  , setAutoResizeDrawableSelector
  , pausedSelector
  , setPausedSelector
  , colorspaceSelector
  , setColorspaceSelector


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
initWithCoder mtkView  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg mtkView (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | releaseDrawables
--
-- Release the depthStencilTexture and multisampleColorTexture
--
-- Can be called by the app to release the textures in order to conserve memory when it goes into the background.   The view will recreate multisampleColorTexture or depthStencilTexture upon the next access of the respective properties.  Both multisampleColorTexture and depthStencilTexture will be recreated in the access to currentRenderPassDescriptor.
--
-- ObjC selector: @- releaseDrawables@
releaseDrawables :: IsMTKView mtkView => mtkView -> IO ()
releaseDrawables mtkView  =
  sendMsg mtkView (mkSelector "releaseDrawables") retVoid []

-- | draw
--
-- Manually ask the view to draw new contents. This causes the view to call either the drawInMTKView (delegate) or drawRect (subclass) method.
--
-- Manually ask the view to draw new contents. This causes the view to call either the drawInMTKView (delegate) or drawRect (subclass) method. This should be used when the view's paused proprety is set to true and enableSetNeedsDisplay is set to false.
--
-- ObjC selector: @- draw@
draw :: IsMTKView mtkView => mtkView -> IO ()
draw mtkView  =
  sendMsg mtkView (mkSelector "draw") retVoid []

-- | framebufferOnly
--
-- If the currentDrawable can be used for sampling or texture read operations
--
-- This defaults to YES. This property controls whether or not the returned drawables' MTLTextures may only be used for framebuffer attachments (YES) or whether they may also be used for texture sampling and pixel read/write operations (NO). A value of YES allows the CAMetalLayer to allocate the MTLTexture objects in ways that are optimized for display purposes that makes them unsuitable for sampling. The recommended value for most applications is YES.
--
-- ObjC selector: @- framebufferOnly@
framebufferOnly :: IsMTKView mtkView => mtkView -> IO Bool
framebufferOnly mtkView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtkView (mkSelector "framebufferOnly") retCULong []

-- | framebufferOnly
--
-- If the currentDrawable can be used for sampling or texture read operations
--
-- This defaults to YES. This property controls whether or not the returned drawables' MTLTextures may only be used for framebuffer attachments (YES) or whether they may also be used for texture sampling and pixel read/write operations (NO). A value of YES allows the CAMetalLayer to allocate the MTLTexture objects in ways that are optimized for display purposes that makes them unsuitable for sampling. The recommended value for most applications is YES.
--
-- ObjC selector: @- setFramebufferOnly:@
setFramebufferOnly :: IsMTKView mtkView => mtkView -> Bool -> IO ()
setFramebufferOnly mtkView  value =
  sendMsg mtkView (mkSelector "setFramebufferOnly:") retVoid [argCULong (if value then 1 else 0)]

-- | depthStencilAttachmentTextureUsage
--
-- The usage flags set on the depth attachment.
--
-- This property controls the texture usage flags set on the MTKView's depth-stencil attachment on creation.  This value defaults to MTLTextureUsageRenderTarget. The recommended value for most applications is MTLTextureUsageRenderTarget. Changing this value re-creates the depth attachment, but any data currently in the depth attachment will be lost.
--
-- ObjC selector: @- depthStencilAttachmentTextureUsage@
depthStencilAttachmentTextureUsage :: IsMTKView mtkView => mtkView -> IO MTLTextureUsage
depthStencilAttachmentTextureUsage mtkView  =
  fmap (coerce :: CULong -> MTLTextureUsage) $ sendMsg mtkView (mkSelector "depthStencilAttachmentTextureUsage") retCULong []

-- | depthStencilAttachmentTextureUsage
--
-- The usage flags set on the depth attachment.
--
-- This property controls the texture usage flags set on the MTKView's depth-stencil attachment on creation.  This value defaults to MTLTextureUsageRenderTarget. The recommended value for most applications is MTLTextureUsageRenderTarget. Changing this value re-creates the depth attachment, but any data currently in the depth attachment will be lost.
--
-- ObjC selector: @- setDepthStencilAttachmentTextureUsage:@
setDepthStencilAttachmentTextureUsage :: IsMTKView mtkView => mtkView -> MTLTextureUsage -> IO ()
setDepthStencilAttachmentTextureUsage mtkView  value =
  sendMsg mtkView (mkSelector "setDepthStencilAttachmentTextureUsage:") retVoid [argCULong (coerce value)]

-- | multisampleColorAttachmentTextureUsage
--
-- The texture usage flags for the multisample color attachment.
--
-- This property controls the texture usage flags set on the the multisample color attachment attachment.  This value defaults to MTLTextureUsageRenderTarget. The recommended value for most applications is MTLTextureUsageRenderTarget. Changing this value re-creates the multisample color attachment, but any data currently in the multisample color attachment will be lost.
--
-- ObjC selector: @- multisampleColorAttachmentTextureUsage@
multisampleColorAttachmentTextureUsage :: IsMTKView mtkView => mtkView -> IO MTLTextureUsage
multisampleColorAttachmentTextureUsage mtkView  =
  fmap (coerce :: CULong -> MTLTextureUsage) $ sendMsg mtkView (mkSelector "multisampleColorAttachmentTextureUsage") retCULong []

-- | multisampleColorAttachmentTextureUsage
--
-- The texture usage flags for the multisample color attachment.
--
-- This property controls the texture usage flags set on the the multisample color attachment attachment.  This value defaults to MTLTextureUsageRenderTarget. The recommended value for most applications is MTLTextureUsageRenderTarget. Changing this value re-creates the multisample color attachment, but any data currently in the multisample color attachment will be lost.
--
-- ObjC selector: @- setMultisampleColorAttachmentTextureUsage:@
setMultisampleColorAttachmentTextureUsage :: IsMTKView mtkView => mtkView -> MTLTextureUsage -> IO ()
setMultisampleColorAttachmentTextureUsage mtkView  value =
  sendMsg mtkView (mkSelector "setMultisampleColorAttachmentTextureUsage:") retVoid [argCULong (coerce value)]

-- | presentsWithTransaction
--
-- If the layer should be presented synchronously
--
-- Defaults to NO. When NO, changes to the layer's render buffer appear on-screen asynchronously to normal layer updates. When YES, changes to the MTL content are sent to the screen via the standard CATransaction mechanisms.
--
-- ObjC selector: @- presentsWithTransaction@
presentsWithTransaction :: IsMTKView mtkView => mtkView -> IO Bool
presentsWithTransaction mtkView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtkView (mkSelector "presentsWithTransaction") retCULong []

-- | presentsWithTransaction
--
-- If the layer should be presented synchronously
--
-- Defaults to NO. When NO, changes to the layer's render buffer appear on-screen asynchronously to normal layer updates. When YES, changes to the MTL content are sent to the screen via the standard CATransaction mechanisms.
--
-- ObjC selector: @- setPresentsWithTransaction:@
setPresentsWithTransaction :: IsMTKView mtkView => mtkView -> Bool -> IO ()
setPresentsWithTransaction mtkView  value =
  sendMsg mtkView (mkSelector "setPresentsWithTransaction:") retVoid [argCULong (if value then 1 else 0)]

-- | colorPixelFormat
--
-- The pixelFormat for the drawable's texture.
--
-- ObjC selector: @- colorPixelFormat@
colorPixelFormat :: IsMTKView mtkView => mtkView -> IO MTLPixelFormat
colorPixelFormat mtkView  =
  fmap (coerce :: CULong -> MTLPixelFormat) $ sendMsg mtkView (mkSelector "colorPixelFormat") retCULong []

-- | colorPixelFormat
--
-- The pixelFormat for the drawable's texture.
--
-- ObjC selector: @- setColorPixelFormat:@
setColorPixelFormat :: IsMTKView mtkView => mtkView -> MTLPixelFormat -> IO ()
setColorPixelFormat mtkView  value =
  sendMsg mtkView (mkSelector "setColorPixelFormat:") retVoid [argCULong (coerce value)]

-- | depthStencilPixelFormat
--
-- The pixelFormat used to create depthStencilTexture
--
-- ObjC selector: @- depthStencilPixelFormat@
depthStencilPixelFormat :: IsMTKView mtkView => mtkView -> IO MTLPixelFormat
depthStencilPixelFormat mtkView  =
  fmap (coerce :: CULong -> MTLPixelFormat) $ sendMsg mtkView (mkSelector "depthStencilPixelFormat") retCULong []

-- | depthStencilPixelFormat
--
-- The pixelFormat used to create depthStencilTexture
--
-- ObjC selector: @- setDepthStencilPixelFormat:@
setDepthStencilPixelFormat :: IsMTKView mtkView => mtkView -> MTLPixelFormat -> IO ()
setDepthStencilPixelFormat mtkView  value =
  sendMsg mtkView (mkSelector "setDepthStencilPixelFormat:") retVoid [argCULong (coerce value)]

-- | depthStencilStorageMode
--
-- The storage mode for the depthStencilTexture. Defaults to MTLStorageModePrivate.
--
-- ObjC selector: @- depthStencilStorageMode@
depthStencilStorageMode :: IsMTKView mtkView => mtkView -> IO MTLStorageMode
depthStencilStorageMode mtkView  =
  fmap (coerce :: CULong -> MTLStorageMode) $ sendMsg mtkView (mkSelector "depthStencilStorageMode") retCULong []

-- | depthStencilStorageMode
--
-- The storage mode for the depthStencilTexture. Defaults to MTLStorageModePrivate.
--
-- ObjC selector: @- setDepthStencilStorageMode:@
setDepthStencilStorageMode :: IsMTKView mtkView => mtkView -> MTLStorageMode -> IO ()
setDepthStencilStorageMode mtkView  value =
  sendMsg mtkView (mkSelector "setDepthStencilStorageMode:") retVoid [argCULong (coerce value)]

-- | sampleCount
--
-- The sample count used to to create multisampleColorTexture
--
-- This defaults to 1.  If sampleCount is greater than 1 a multisampled color texture will be created and the currentDrawable's texture will be set as the resolve texture in the currentRenderPassDescriptor and the store action will be set to MTLStoreActionMultisampleResolve
--
-- ObjC selector: @- sampleCount@
sampleCount :: IsMTKView mtkView => mtkView -> IO CULong
sampleCount mtkView  =
  sendMsg mtkView (mkSelector "sampleCount") retCULong []

-- | sampleCount
--
-- The sample count used to to create multisampleColorTexture
--
-- This defaults to 1.  If sampleCount is greater than 1 a multisampled color texture will be created and the currentDrawable's texture will be set as the resolve texture in the currentRenderPassDescriptor and the store action will be set to MTLStoreActionMultisampleResolve
--
-- ObjC selector: @- setSampleCount:@
setSampleCount :: IsMTKView mtkView => mtkView -> CULong -> IO ()
setSampleCount mtkView  value =
  sendMsg mtkView (mkSelector "setSampleCount:") retVoid [argCULong (fromIntegral value)]

-- | clearDepth
--
-- The clear depth value used to generate the currentRenderPassDescriptor
--
-- This defaults to 1.0
--
-- ObjC selector: @- clearDepth@
clearDepth :: IsMTKView mtkView => mtkView -> IO CDouble
clearDepth mtkView  =
  sendMsg mtkView (mkSelector "clearDepth") retCDouble []

-- | clearDepth
--
-- The clear depth value used to generate the currentRenderPassDescriptor
--
-- This defaults to 1.0
--
-- ObjC selector: @- setClearDepth:@
setClearDepth :: IsMTKView mtkView => mtkView -> CDouble -> IO ()
setClearDepth mtkView  value =
  sendMsg mtkView (mkSelector "setClearDepth:") retVoid [argCDouble (fromIntegral value)]

-- | clearStencil
--
-- The clear stencil value used to generate currentRenderPassDescriptor
--
-- This defaults to 0
--
-- ObjC selector: @- clearStencil@
clearStencil :: IsMTKView mtkView => mtkView -> IO CUInt
clearStencil mtkView  =
  sendMsg mtkView (mkSelector "clearStencil") retCUInt []

-- | clearStencil
--
-- The clear stencil value used to generate currentRenderPassDescriptor
--
-- This defaults to 0
--
-- ObjC selector: @- setClearStencil:@
setClearStencil :: IsMTKView mtkView => mtkView -> CUInt -> IO ()
setClearStencil mtkView  value =
  sendMsg mtkView (mkSelector "setClearStencil:") retVoid [argCUInt (fromIntegral value)]

-- | preferredFramesPerSecond
--
-- The rate you want the view to redraw its contents.
--
-- When your application sets its preferred frame rate, the view chooses a frame rate as close to that as possible based on the capabilities of the screen the view is displayed on. The actual frame rate chosen is usually a factor of the maximum refresh rate of the screen to provide a consistent frame rate. For example, if the maximum refresh rate of the screen is 60 frames per second, that is also the highest frame rate the view sets as the actual frame rate. However, if you ask for a lower frame rate, it might choose 30, 20, 15 or some other factor to be the actual frame rate. Your application should choose a frame rate that it can consistently maintain. The default value is 60 frames per second.
--
-- ObjC selector: @- preferredFramesPerSecond@
preferredFramesPerSecond :: IsMTKView mtkView => mtkView -> IO CLong
preferredFramesPerSecond mtkView  =
  sendMsg mtkView (mkSelector "preferredFramesPerSecond") retCLong []

-- | preferredFramesPerSecond
--
-- The rate you want the view to redraw its contents.
--
-- When your application sets its preferred frame rate, the view chooses a frame rate as close to that as possible based on the capabilities of the screen the view is displayed on. The actual frame rate chosen is usually a factor of the maximum refresh rate of the screen to provide a consistent frame rate. For example, if the maximum refresh rate of the screen is 60 frames per second, that is also the highest frame rate the view sets as the actual frame rate. However, if you ask for a lower frame rate, it might choose 30, 20, 15 or some other factor to be the actual frame rate. Your application should choose a frame rate that it can consistently maintain. The default value is 60 frames per second.
--
-- ObjC selector: @- setPreferredFramesPerSecond:@
setPreferredFramesPerSecond :: IsMTKView mtkView => mtkView -> CLong -> IO ()
setPreferredFramesPerSecond mtkView  value =
  sendMsg mtkView (mkSelector "setPreferredFramesPerSecond:") retVoid [argCLong (fromIntegral value)]

-- | enableSetNeedsDisplay
--
-- Controls whether the view responds to setNeedsDisplay.
--
-- If true, then the view behaves similarily to a UIView or NSView, responding to calls to setNeedsDisplay. When the view has been marked for display, the view is automatically redisplayed on each pass through the application’s event loop. Setting enableSetNeedsDisplay to true will also pause the MTKView's internal render loop and updates will instead be event driven. The default value is false.
--
-- ObjC selector: @- enableSetNeedsDisplay@
enableSetNeedsDisplay :: IsMTKView mtkView => mtkView -> IO Bool
enableSetNeedsDisplay mtkView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtkView (mkSelector "enableSetNeedsDisplay") retCULong []

-- | enableSetNeedsDisplay
--
-- Controls whether the view responds to setNeedsDisplay.
--
-- If true, then the view behaves similarily to a UIView or NSView, responding to calls to setNeedsDisplay. When the view has been marked for display, the view is automatically redisplayed on each pass through the application’s event loop. Setting enableSetNeedsDisplay to true will also pause the MTKView's internal render loop and updates will instead be event driven. The default value is false.
--
-- ObjC selector: @- setEnableSetNeedsDisplay:@
setEnableSetNeedsDisplay :: IsMTKView mtkView => mtkView -> Bool -> IO ()
setEnableSetNeedsDisplay mtkView  value =
  sendMsg mtkView (mkSelector "setEnableSetNeedsDisplay:") retVoid [argCULong (if value then 1 else 0)]

-- | autoResizeDrawable
--
-- Controls whether to resize the drawable as the view changes size.
--
-- If true, the size of the currentDrawable's texture, depthStencilTexture, and multisampleColorTexture will automatically resize as the view resizes.  If false, these textures will take on the size of drawableSize and drawableSize will not change. The default value is true.
--
-- ObjC selector: @- autoResizeDrawable@
autoResizeDrawable :: IsMTKView mtkView => mtkView -> IO Bool
autoResizeDrawable mtkView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtkView (mkSelector "autoResizeDrawable") retCULong []

-- | autoResizeDrawable
--
-- Controls whether to resize the drawable as the view changes size.
--
-- If true, the size of the currentDrawable's texture, depthStencilTexture, and multisampleColorTexture will automatically resize as the view resizes.  If false, these textures will take on the size of drawableSize and drawableSize will not change. The default value is true.
--
-- ObjC selector: @- setAutoResizeDrawable:@
setAutoResizeDrawable :: IsMTKView mtkView => mtkView -> Bool -> IO ()
setAutoResizeDrawable mtkView  value =
  sendMsg mtkView (mkSelector "setAutoResizeDrawable:") retVoid [argCULong (if value then 1 else 0)]

-- | paused
--
-- Controls whether the draw methods should countinue at preferredFramesPerSecond
--
-- If true, the delegate will receive drawInMTKView: messages or the subclass will receive drawRect: messages at a rate of preferredFramesPerSecond based on an internal timer. The default value is false.
--
-- ObjC selector: @- paused@
paused :: IsMTKView mtkView => mtkView -> IO Bool
paused mtkView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtkView (mkSelector "paused") retCULong []

-- | paused
--
-- Controls whether the draw methods should countinue at preferredFramesPerSecond
--
-- If true, the delegate will receive drawInMTKView: messages or the subclass will receive drawRect: messages at a rate of preferredFramesPerSecond based on an internal timer. The default value is false.
--
-- ObjC selector: @- setPaused:@
setPaused :: IsMTKView mtkView => mtkView -> Bool -> IO ()
setPaused mtkView  value =
  sendMsg mtkView (mkSelector "setPaused:") retVoid [argCULong (if value then 1 else 0)]

-- | colorspace
--
-- The colorspace of the rendered frames. '
--
-- If nil, no colormatching occurs.  If non-nil, the rendered content will be colormatched to the colorspace of the context containing this layer (typically the display's colorspace).  This property aliases the olorspace property or the view's CAMetalLayer
--
-- ObjC selector: @- colorspace@
colorspace :: IsMTKView mtkView => mtkView -> IO (Ptr ())
colorspace mtkView  =
  fmap castPtr $ sendMsg mtkView (mkSelector "colorspace") (retPtr retVoid) []

-- | colorspace
--
-- The colorspace of the rendered frames. '
--
-- If nil, no colormatching occurs.  If non-nil, the rendered content will be colormatched to the colorspace of the context containing this layer (typically the display's colorspace).  This property aliases the olorspace property or the view's CAMetalLayer
--
-- ObjC selector: @- setColorspace:@
setColorspace :: IsMTKView mtkView => mtkView -> Ptr () -> IO ()
setColorspace mtkView  value =
  sendMsg mtkView (mkSelector "setColorspace:") retVoid [argPtr value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @releaseDrawables@
releaseDrawablesSelector :: Selector
releaseDrawablesSelector = mkSelector "releaseDrawables"

-- | @Selector@ for @draw@
drawSelector :: Selector
drawSelector = mkSelector "draw"

-- | @Selector@ for @framebufferOnly@
framebufferOnlySelector :: Selector
framebufferOnlySelector = mkSelector "framebufferOnly"

-- | @Selector@ for @setFramebufferOnly:@
setFramebufferOnlySelector :: Selector
setFramebufferOnlySelector = mkSelector "setFramebufferOnly:"

-- | @Selector@ for @depthStencilAttachmentTextureUsage@
depthStencilAttachmentTextureUsageSelector :: Selector
depthStencilAttachmentTextureUsageSelector = mkSelector "depthStencilAttachmentTextureUsage"

-- | @Selector@ for @setDepthStencilAttachmentTextureUsage:@
setDepthStencilAttachmentTextureUsageSelector :: Selector
setDepthStencilAttachmentTextureUsageSelector = mkSelector "setDepthStencilAttachmentTextureUsage:"

-- | @Selector@ for @multisampleColorAttachmentTextureUsage@
multisampleColorAttachmentTextureUsageSelector :: Selector
multisampleColorAttachmentTextureUsageSelector = mkSelector "multisampleColorAttachmentTextureUsage"

-- | @Selector@ for @setMultisampleColorAttachmentTextureUsage:@
setMultisampleColorAttachmentTextureUsageSelector :: Selector
setMultisampleColorAttachmentTextureUsageSelector = mkSelector "setMultisampleColorAttachmentTextureUsage:"

-- | @Selector@ for @presentsWithTransaction@
presentsWithTransactionSelector :: Selector
presentsWithTransactionSelector = mkSelector "presentsWithTransaction"

-- | @Selector@ for @setPresentsWithTransaction:@
setPresentsWithTransactionSelector :: Selector
setPresentsWithTransactionSelector = mkSelector "setPresentsWithTransaction:"

-- | @Selector@ for @colorPixelFormat@
colorPixelFormatSelector :: Selector
colorPixelFormatSelector = mkSelector "colorPixelFormat"

-- | @Selector@ for @setColorPixelFormat:@
setColorPixelFormatSelector :: Selector
setColorPixelFormatSelector = mkSelector "setColorPixelFormat:"

-- | @Selector@ for @depthStencilPixelFormat@
depthStencilPixelFormatSelector :: Selector
depthStencilPixelFormatSelector = mkSelector "depthStencilPixelFormat"

-- | @Selector@ for @setDepthStencilPixelFormat:@
setDepthStencilPixelFormatSelector :: Selector
setDepthStencilPixelFormatSelector = mkSelector "setDepthStencilPixelFormat:"

-- | @Selector@ for @depthStencilStorageMode@
depthStencilStorageModeSelector :: Selector
depthStencilStorageModeSelector = mkSelector "depthStencilStorageMode"

-- | @Selector@ for @setDepthStencilStorageMode:@
setDepthStencilStorageModeSelector :: Selector
setDepthStencilStorageModeSelector = mkSelector "setDepthStencilStorageMode:"

-- | @Selector@ for @sampleCount@
sampleCountSelector :: Selector
sampleCountSelector = mkSelector "sampleCount"

-- | @Selector@ for @setSampleCount:@
setSampleCountSelector :: Selector
setSampleCountSelector = mkSelector "setSampleCount:"

-- | @Selector@ for @clearDepth@
clearDepthSelector :: Selector
clearDepthSelector = mkSelector "clearDepth"

-- | @Selector@ for @setClearDepth:@
setClearDepthSelector :: Selector
setClearDepthSelector = mkSelector "setClearDepth:"

-- | @Selector@ for @clearStencil@
clearStencilSelector :: Selector
clearStencilSelector = mkSelector "clearStencil"

-- | @Selector@ for @setClearStencil:@
setClearStencilSelector :: Selector
setClearStencilSelector = mkSelector "setClearStencil:"

-- | @Selector@ for @preferredFramesPerSecond@
preferredFramesPerSecondSelector :: Selector
preferredFramesPerSecondSelector = mkSelector "preferredFramesPerSecond"

-- | @Selector@ for @setPreferredFramesPerSecond:@
setPreferredFramesPerSecondSelector :: Selector
setPreferredFramesPerSecondSelector = mkSelector "setPreferredFramesPerSecond:"

-- | @Selector@ for @enableSetNeedsDisplay@
enableSetNeedsDisplaySelector :: Selector
enableSetNeedsDisplaySelector = mkSelector "enableSetNeedsDisplay"

-- | @Selector@ for @setEnableSetNeedsDisplay:@
setEnableSetNeedsDisplaySelector :: Selector
setEnableSetNeedsDisplaySelector = mkSelector "setEnableSetNeedsDisplay:"

-- | @Selector@ for @autoResizeDrawable@
autoResizeDrawableSelector :: Selector
autoResizeDrawableSelector = mkSelector "autoResizeDrawable"

-- | @Selector@ for @setAutoResizeDrawable:@
setAutoResizeDrawableSelector :: Selector
setAutoResizeDrawableSelector = mkSelector "setAutoResizeDrawable:"

-- | @Selector@ for @paused@
pausedSelector :: Selector
pausedSelector = mkSelector "paused"

-- | @Selector@ for @setPaused:@
setPausedSelector :: Selector
setPausedSelector = mkSelector "setPaused:"

-- | @Selector@ for @colorspace@
colorspaceSelector :: Selector
colorspaceSelector = mkSelector "colorspace"

-- | @Selector@ for @setColorspace:@
setColorspaceSelector :: Selector
setColorspaceSelector = mkSelector "setColorspace:"

