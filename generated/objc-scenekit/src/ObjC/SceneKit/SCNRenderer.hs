{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNRenderer
--
-- SCNRenderer lets you use the SceneKit renderer in an OpenGL context or Metal render pass descriptor of your own.
--
-- Generated bindings for @SCNRenderer@.
module ObjC.SceneKit.SCNRenderer
  ( SCNRenderer
  , IsSCNRenderer(..)
  , rendererWithContext_options
  , rendererWithDevice_options
  , renderAtTime
  , updateAtTime
  , updateProbes_atTime
  , render
  , scene
  , setScene
  , nextFrameTime
  , rendererWithContext_optionsSelector
  , rendererWithDevice_optionsSelector
  , renderAtTimeSelector
  , updateAtTimeSelector
  , updateProbes_atTimeSelector
  , renderSelector
  , sceneSelector
  , setSceneSelector
  , nextFrameTimeSelector

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
import ObjC.SceneKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.Metal.Internal.Classes

-- | rendererWithContext:options:
--
-- Creates a new renderer object.
--
-- @context@ — The context to render into.
--
-- @options@ — An optional dictionary for future extensions.
--
-- ObjC selector: @+ rendererWithContext:options:@
rendererWithContext_options :: IsNSDictionary options => Ptr () -> options -> IO (Id SCNRenderer)
rendererWithContext_options context options =
  do
    cls' <- getRequiredClass "SCNRenderer"
    withObjCPtr options $ \raw_options ->
      sendClassMsg cls' (mkSelector "rendererWithContext:options:") (retPtr retVoid) [argPtr context, argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | rendererWithDevice:options:
--
-- Creates a new renderer object that renders using Metal.
--
-- @device@ — The metal device to use. Pass nil to let SceneKit choose a default device.
--
-- @options@ — An optional dictionary for future extensions.
--
-- ObjC selector: @+ rendererWithDevice:options:@
rendererWithDevice_options :: IsNSDictionary options => RawId -> options -> IO (Id SCNRenderer)
rendererWithDevice_options device options =
  do
    cls' <- getRequiredClass "SCNRenderer"
    withObjCPtr options $ \raw_options ->
      sendClassMsg cls' (mkSelector "rendererWithDevice:options:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | renderAtTime:
--
-- updates and renders the receiver's scene at the specified time (system time).
--
-- This method only work if the receiver was allocated with an OpenGL context. Use renderAtTime:withEncoder:pass:commandQueue: to render with Metal.
--
-- ObjC selector: @- renderAtTime:@
renderAtTime :: IsSCNRenderer scnRenderer => scnRenderer -> CDouble -> IO ()
renderAtTime scnRenderer  time =
  sendMsg scnRenderer (mkSelector "renderAtTime:") retVoid [argCDouble (fromIntegral time)]

-- | updateAtTime:
--
-- updates the receiver's scene at the specified time (system time).
--
-- ObjC selector: @- updateAtTime:@
updateAtTime :: IsSCNRenderer scnRenderer => scnRenderer -> CDouble -> IO ()
updateAtTime scnRenderer  time =
  sendMsg scnRenderer (mkSelector "updateAtTime:") retVoid [argCDouble (fromIntegral time)]

-- | updateProbes:atTime:
--
-- Update the specified probes by computing their incoming irradiance in the receiver's scene at the specified time.
--
-- @lightProbes@ — An array of nodes that must have a light probe attached.
--
-- @time@ — The time used to render the scene when computing the light probes irradiance.
--
-- Light probes are only supported with Metal. This method is observable using NSProgress.
--
-- ObjC selector: @- updateProbes:atTime:@
updateProbes_atTime :: (IsSCNRenderer scnRenderer, IsNSArray lightProbes) => scnRenderer -> lightProbes -> CDouble -> IO ()
updateProbes_atTime scnRenderer  lightProbes time =
withObjCPtr lightProbes $ \raw_lightProbes ->
    sendMsg scnRenderer (mkSelector "updateProbes:atTime:") retVoid [argPtr (castPtr raw_lightProbes :: Ptr ()), argCDouble (fromIntegral time)]

-- | render
--
-- renders the receiver's scene at the current system time.
--
-- This method only work if the receiver was allocated with an OpenGL context and it is deprecated (use renderAtTime: instead). Use renderAtTime:withEncoder:pass:commandQueue: to render with Metal.
--
-- ObjC selector: @- render@
render :: IsSCNRenderer scnRenderer => scnRenderer -> IO ()
render scnRenderer  =
  sendMsg scnRenderer (mkSelector "render") retVoid []

-- | scene
--
-- Specifies the scene of the receiver
--
-- ObjC selector: @- scene@
scene :: IsSCNRenderer scnRenderer => scnRenderer -> IO (Id SCNScene)
scene scnRenderer  =
  sendMsg scnRenderer (mkSelector "scene") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | scene
--
-- Specifies the scene of the receiver
--
-- ObjC selector: @- setScene:@
setScene :: (IsSCNRenderer scnRenderer, IsSCNScene value) => scnRenderer -> value -> IO ()
setScene scnRenderer  value =
withObjCPtr value $ \raw_value ->
    sendMsg scnRenderer (mkSelector "setScene:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | nextFrameTime
--
-- Returns the time at which the next update should happen. If infinite no update needs to be scheduled yet. If the current frame time, a continuous animation is running and an update should be scheduled after a "natural" delay.
--
-- ObjC selector: @- nextFrameTime@
nextFrameTime :: IsSCNRenderer scnRenderer => scnRenderer -> IO CDouble
nextFrameTime scnRenderer  =
  sendMsg scnRenderer (mkSelector "nextFrameTime") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rendererWithContext:options:@
rendererWithContext_optionsSelector :: Selector
rendererWithContext_optionsSelector = mkSelector "rendererWithContext:options:"

-- | @Selector@ for @rendererWithDevice:options:@
rendererWithDevice_optionsSelector :: Selector
rendererWithDevice_optionsSelector = mkSelector "rendererWithDevice:options:"

-- | @Selector@ for @renderAtTime:@
renderAtTimeSelector :: Selector
renderAtTimeSelector = mkSelector "renderAtTime:"

-- | @Selector@ for @updateAtTime:@
updateAtTimeSelector :: Selector
updateAtTimeSelector = mkSelector "updateAtTime:"

-- | @Selector@ for @updateProbes:atTime:@
updateProbes_atTimeSelector :: Selector
updateProbes_atTimeSelector = mkSelector "updateProbes:atTime:"

-- | @Selector@ for @render@
renderSelector :: Selector
renderSelector = mkSelector "render"

-- | @Selector@ for @scene@
sceneSelector :: Selector
sceneSelector = mkSelector "scene"

-- | @Selector@ for @setScene:@
setSceneSelector :: Selector
setSceneSelector = mkSelector "setScene:"

-- | @Selector@ for @nextFrameTime@
nextFrameTimeSelector :: Selector
nextFrameTimeSelector = mkSelector "nextFrameTime"

