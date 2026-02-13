{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , nextFrameTimeSelector
  , renderAtTimeSelector
  , renderSelector
  , rendererWithContext_optionsSelector
  , rendererWithDevice_optionsSelector
  , sceneSelector
  , setSceneSelector
  , updateAtTimeSelector
  , updateProbes_atTimeSelector

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
    sendClassMessage cls' rendererWithContext_optionsSelector context (toNSDictionary options)

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
    sendClassMessage cls' rendererWithDevice_optionsSelector device (toNSDictionary options)

-- | renderAtTime:
--
-- updates and renders the receiver's scene at the specified time (system time).
--
-- This method only work if the receiver was allocated with an OpenGL context. Use renderAtTime:withEncoder:pass:commandQueue: to render with Metal.
--
-- ObjC selector: @- renderAtTime:@
renderAtTime :: IsSCNRenderer scnRenderer => scnRenderer -> CDouble -> IO ()
renderAtTime scnRenderer time =
  sendMessage scnRenderer renderAtTimeSelector time

-- | updateAtTime:
--
-- updates the receiver's scene at the specified time (system time).
--
-- ObjC selector: @- updateAtTime:@
updateAtTime :: IsSCNRenderer scnRenderer => scnRenderer -> CDouble -> IO ()
updateAtTime scnRenderer time =
  sendMessage scnRenderer updateAtTimeSelector time

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
updateProbes_atTime scnRenderer lightProbes time =
  sendMessage scnRenderer updateProbes_atTimeSelector (toNSArray lightProbes) time

-- | render
--
-- renders the receiver's scene at the current system time.
--
-- This method only work if the receiver was allocated with an OpenGL context and it is deprecated (use renderAtTime: instead). Use renderAtTime:withEncoder:pass:commandQueue: to render with Metal.
--
-- ObjC selector: @- render@
render :: IsSCNRenderer scnRenderer => scnRenderer -> IO ()
render scnRenderer =
  sendMessage scnRenderer renderSelector

-- | scene
--
-- Specifies the scene of the receiver
--
-- ObjC selector: @- scene@
scene :: IsSCNRenderer scnRenderer => scnRenderer -> IO (Id SCNScene)
scene scnRenderer =
  sendMessage scnRenderer sceneSelector

-- | scene
--
-- Specifies the scene of the receiver
--
-- ObjC selector: @- setScene:@
setScene :: (IsSCNRenderer scnRenderer, IsSCNScene value) => scnRenderer -> value -> IO ()
setScene scnRenderer value =
  sendMessage scnRenderer setSceneSelector (toSCNScene value)

-- | nextFrameTime
--
-- Returns the time at which the next update should happen. If infinite no update needs to be scheduled yet. If the current frame time, a continuous animation is running and an update should be scheduled after a "natural" delay.
--
-- ObjC selector: @- nextFrameTime@
nextFrameTime :: IsSCNRenderer scnRenderer => scnRenderer -> IO CDouble
nextFrameTime scnRenderer =
  sendMessage scnRenderer nextFrameTimeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rendererWithContext:options:@
rendererWithContext_optionsSelector :: Selector '[Ptr (), Id NSDictionary] (Id SCNRenderer)
rendererWithContext_optionsSelector = mkSelector "rendererWithContext:options:"

-- | @Selector@ for @rendererWithDevice:options:@
rendererWithDevice_optionsSelector :: Selector '[RawId, Id NSDictionary] (Id SCNRenderer)
rendererWithDevice_optionsSelector = mkSelector "rendererWithDevice:options:"

-- | @Selector@ for @renderAtTime:@
renderAtTimeSelector :: Selector '[CDouble] ()
renderAtTimeSelector = mkSelector "renderAtTime:"

-- | @Selector@ for @updateAtTime:@
updateAtTimeSelector :: Selector '[CDouble] ()
updateAtTimeSelector = mkSelector "updateAtTime:"

-- | @Selector@ for @updateProbes:atTime:@
updateProbes_atTimeSelector :: Selector '[Id NSArray, CDouble] ()
updateProbes_atTimeSelector = mkSelector "updateProbes:atTime:"

-- | @Selector@ for @render@
renderSelector :: Selector '[] ()
renderSelector = mkSelector "render"

-- | @Selector@ for @scene@
sceneSelector :: Selector '[] (Id SCNScene)
sceneSelector = mkSelector "scene"

-- | @Selector@ for @setScene:@
setSceneSelector :: Selector '[Id SCNScene] ()
setSceneSelector = mkSelector "setScene:"

-- | @Selector@ for @nextFrameTime@
nextFrameTimeSelector :: Selector '[] CDouble
nextFrameTimeSelector = mkSelector "nextFrameTime"

