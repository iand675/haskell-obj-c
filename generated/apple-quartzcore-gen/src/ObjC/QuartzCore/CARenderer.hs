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
  , rendererWithCGLContext_optionsSelector
  , rendererWithMTLTexture_optionsSelector
  , beginFrameAtTime_timeStampSelector
  , renderSelector
  , nextFrameTimeSelector
  , endFrameSelector
  , setDestinationSelector
  , layerSelector
  , setLayerSelector


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

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ rendererWithCGLContext:options:@
rendererWithCGLContext_options :: IsNSDictionary dict => Ptr () -> dict -> IO (Id CARenderer)
rendererWithCGLContext_options ctx dict =
  do
    cls' <- getRequiredClass "CARenderer"
    withObjCPtr dict $ \raw_dict ->
      sendClassMsg cls' (mkSelector "rendererWithCGLContext:options:") (retPtr retVoid) [argPtr ctx, argPtr (castPtr raw_dict :: Ptr ())] >>= retainedObject . castPtr

-- | @+ rendererWithMTLTexture:options:@
rendererWithMTLTexture_options :: IsNSDictionary dict => RawId -> dict -> IO (Id CARenderer)
rendererWithMTLTexture_options tex dict =
  do
    cls' <- getRequiredClass "CARenderer"
    withObjCPtr dict $ \raw_dict ->
      sendClassMsg cls' (mkSelector "rendererWithMTLTexture:options:") (retPtr retVoid) [argPtr (castPtr (unRawId tex) :: Ptr ()), argPtr (castPtr raw_dict :: Ptr ())] >>= retainedObject . castPtr

-- | @- beginFrameAtTime:timeStamp:@
beginFrameAtTime_timeStamp :: IsCARenderer caRenderer => caRenderer -> CDouble -> RawId -> IO ()
beginFrameAtTime_timeStamp caRenderer  t ts =
    sendMsg caRenderer (mkSelector "beginFrameAtTime:timeStamp:") retVoid [argCDouble t, argPtr (castPtr (unRawId ts) :: Ptr ())]

-- | @- render@
render :: IsCARenderer caRenderer => caRenderer -> IO ()
render caRenderer  =
    sendMsg caRenderer (mkSelector "render") retVoid []

-- | @- nextFrameTime@
nextFrameTime :: IsCARenderer caRenderer => caRenderer -> IO CDouble
nextFrameTime caRenderer  =
    sendMsg caRenderer (mkSelector "nextFrameTime") retCDouble []

-- | @- endFrame@
endFrame :: IsCARenderer caRenderer => caRenderer -> IO ()
endFrame caRenderer  =
    sendMsg caRenderer (mkSelector "endFrame") retVoid []

-- | @- setDestination:@
setDestination :: IsCARenderer caRenderer => caRenderer -> RawId -> IO ()
setDestination caRenderer  tex =
    sendMsg caRenderer (mkSelector "setDestination:") retVoid [argPtr (castPtr (unRawId tex) :: Ptr ())]

-- | @- layer@
layer :: IsCARenderer caRenderer => caRenderer -> IO (Id CALayer)
layer caRenderer  =
    sendMsg caRenderer (mkSelector "layer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLayer:@
setLayer :: (IsCARenderer caRenderer, IsCALayer value) => caRenderer -> value -> IO ()
setLayer caRenderer  value =
  withObjCPtr value $ \raw_value ->
      sendMsg caRenderer (mkSelector "setLayer:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rendererWithCGLContext:options:@
rendererWithCGLContext_optionsSelector :: Selector
rendererWithCGLContext_optionsSelector = mkSelector "rendererWithCGLContext:options:"

-- | @Selector@ for @rendererWithMTLTexture:options:@
rendererWithMTLTexture_optionsSelector :: Selector
rendererWithMTLTexture_optionsSelector = mkSelector "rendererWithMTLTexture:options:"

-- | @Selector@ for @beginFrameAtTime:timeStamp:@
beginFrameAtTime_timeStampSelector :: Selector
beginFrameAtTime_timeStampSelector = mkSelector "beginFrameAtTime:timeStamp:"

-- | @Selector@ for @render@
renderSelector :: Selector
renderSelector = mkSelector "render"

-- | @Selector@ for @nextFrameTime@
nextFrameTimeSelector :: Selector
nextFrameTimeSelector = mkSelector "nextFrameTime"

-- | @Selector@ for @endFrame@
endFrameSelector :: Selector
endFrameSelector = mkSelector "endFrame"

-- | @Selector@ for @setDestination:@
setDestinationSelector :: Selector
setDestinationSelector = mkSelector "setDestination:"

-- | @Selector@ for @layer@
layerSelector :: Selector
layerSelector = mkSelector "layer"

-- | @Selector@ for @setLayer:@
setLayerSelector :: Selector
setLayerSelector = mkSelector "setLayer:"

