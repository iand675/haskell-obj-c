{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CARenderer@.
module ObjC.QuartzCore.CARenderer
  ( CARenderer
  , IsCARenderer(..)
  , rendererWithCGLContext_options
  , rendererWithMTLTexture_options
  , beginFrameAtTime_timeStamp
  , render
  , nextFrameTime
  , endFrame
  , setDestination
  , layer
  , setLayer
  , beginFrameAtTime_timeStampSelector
  , endFrameSelector
  , layerSelector
  , nextFrameTimeSelector
  , renderSelector
  , rendererWithCGLContext_optionsSelector
  , rendererWithMTLTexture_optionsSelector
  , setDestinationSelector
  , setLayerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ rendererWithCGLContext:options:@
rendererWithCGLContext_options :: IsNSDictionary dict => Ptr () -> dict -> IO (Id CARenderer)
rendererWithCGLContext_options ctx dict =
  do
    cls' <- getRequiredClass "CARenderer"
    sendClassMessage cls' rendererWithCGLContext_optionsSelector ctx (toNSDictionary dict)

-- | @+ rendererWithMTLTexture:options:@
rendererWithMTLTexture_options :: IsNSDictionary dict => RawId -> dict -> IO (Id CARenderer)
rendererWithMTLTexture_options tex dict =
  do
    cls' <- getRequiredClass "CARenderer"
    sendClassMessage cls' rendererWithMTLTexture_optionsSelector tex (toNSDictionary dict)

-- | @- beginFrameAtTime:timeStamp:@
beginFrameAtTime_timeStamp :: IsCARenderer caRenderer => caRenderer -> CDouble -> RawId -> IO ()
beginFrameAtTime_timeStamp caRenderer t ts =
  sendMessage caRenderer beginFrameAtTime_timeStampSelector t ts

-- | @- render@
render :: IsCARenderer caRenderer => caRenderer -> IO ()
render caRenderer =
  sendMessage caRenderer renderSelector

-- | @- nextFrameTime@
nextFrameTime :: IsCARenderer caRenderer => caRenderer -> IO CDouble
nextFrameTime caRenderer =
  sendMessage caRenderer nextFrameTimeSelector

-- | @- endFrame@
endFrame :: IsCARenderer caRenderer => caRenderer -> IO ()
endFrame caRenderer =
  sendMessage caRenderer endFrameSelector

-- | @- setDestination:@
setDestination :: IsCARenderer caRenderer => caRenderer -> RawId -> IO ()
setDestination caRenderer tex =
  sendMessage caRenderer setDestinationSelector tex

-- | @- layer@
layer :: IsCARenderer caRenderer => caRenderer -> IO (Id CALayer)
layer caRenderer =
  sendMessage caRenderer layerSelector

-- | @- setLayer:@
setLayer :: (IsCARenderer caRenderer, IsCALayer value) => caRenderer -> value -> IO ()
setLayer caRenderer value =
  sendMessage caRenderer setLayerSelector (toCALayer value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rendererWithCGLContext:options:@
rendererWithCGLContext_optionsSelector :: Selector '[Ptr (), Id NSDictionary] (Id CARenderer)
rendererWithCGLContext_optionsSelector = mkSelector "rendererWithCGLContext:options:"

-- | @Selector@ for @rendererWithMTLTexture:options:@
rendererWithMTLTexture_optionsSelector :: Selector '[RawId, Id NSDictionary] (Id CARenderer)
rendererWithMTLTexture_optionsSelector = mkSelector "rendererWithMTLTexture:options:"

-- | @Selector@ for @beginFrameAtTime:timeStamp:@
beginFrameAtTime_timeStampSelector :: Selector '[CDouble, RawId] ()
beginFrameAtTime_timeStampSelector = mkSelector "beginFrameAtTime:timeStamp:"

-- | @Selector@ for @render@
renderSelector :: Selector '[] ()
renderSelector = mkSelector "render"

-- | @Selector@ for @nextFrameTime@
nextFrameTimeSelector :: Selector '[] CDouble
nextFrameTimeSelector = mkSelector "nextFrameTime"

-- | @Selector@ for @endFrame@
endFrameSelector :: Selector '[] ()
endFrameSelector = mkSelector "endFrame"

-- | @Selector@ for @setDestination:@
setDestinationSelector :: Selector '[RawId] ()
setDestinationSelector = mkSelector "setDestination:"

-- | @Selector@ for @layer@
layerSelector :: Selector '[] (Id CALayer)
layerSelector = mkSelector "layer"

-- | @Selector@ for @setLayer:@
setLayerSelector :: Selector '[Id CALayer] ()
setLayerSelector = mkSelector "setLayer:"

