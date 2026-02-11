{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CAEmitterLayer@.
module ObjC.QuartzCore.CAEmitterLayer
  ( CAEmitterLayer
  , IsCAEmitterLayer(..)
  , emitterCells
  , setEmitterCells
  , birthRate
  , setBirthRate
  , lifetime
  , setLifetime
  , emitterZPosition
  , setEmitterZPosition
  , emitterDepth
  , setEmitterDepth
  , emitterShape
  , setEmitterShape
  , emitterMode
  , setEmitterMode
  , renderMode
  , setRenderMode
  , preservesDepth
  , setPreservesDepth
  , velocity
  , setVelocity
  , scale
  , setScale
  , spin
  , setSpin
  , seed
  , setSeed
  , emitterCellsSelector
  , setEmitterCellsSelector
  , birthRateSelector
  , setBirthRateSelector
  , lifetimeSelector
  , setLifetimeSelector
  , emitterZPositionSelector
  , setEmitterZPositionSelector
  , emitterDepthSelector
  , setEmitterDepthSelector
  , emitterShapeSelector
  , setEmitterShapeSelector
  , emitterModeSelector
  , setEmitterModeSelector
  , renderModeSelector
  , setRenderModeSelector
  , preservesDepthSelector
  , setPreservesDepthSelector
  , velocitySelector
  , setVelocitySelector
  , scaleSelector
  , setScaleSelector
  , spinSelector
  , setSpinSelector
  , seedSelector
  , setSeedSelector


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

-- | @- emitterCells@
emitterCells :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> IO (Id NSArray)
emitterCells caEmitterLayer  =
  sendMsg caEmitterLayer (mkSelector "emitterCells") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEmitterCells:@
setEmitterCells :: (IsCAEmitterLayer caEmitterLayer, IsNSArray value) => caEmitterLayer -> value -> IO ()
setEmitterCells caEmitterLayer  value =
withObjCPtr value $ \raw_value ->
    sendMsg caEmitterLayer (mkSelector "setEmitterCells:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- birthRate@
birthRate :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> IO CFloat
birthRate caEmitterLayer  =
  sendMsg caEmitterLayer (mkSelector "birthRate") retCFloat []

-- | @- setBirthRate:@
setBirthRate :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> CFloat -> IO ()
setBirthRate caEmitterLayer  value =
  sendMsg caEmitterLayer (mkSelector "setBirthRate:") retVoid [argCFloat (fromIntegral value)]

-- | @- lifetime@
lifetime :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> IO CFloat
lifetime caEmitterLayer  =
  sendMsg caEmitterLayer (mkSelector "lifetime") retCFloat []

-- | @- setLifetime:@
setLifetime :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> CFloat -> IO ()
setLifetime caEmitterLayer  value =
  sendMsg caEmitterLayer (mkSelector "setLifetime:") retVoid [argCFloat (fromIntegral value)]

-- | @- emitterZPosition@
emitterZPosition :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> IO CDouble
emitterZPosition caEmitterLayer  =
  sendMsg caEmitterLayer (mkSelector "emitterZPosition") retCDouble []

-- | @- setEmitterZPosition:@
setEmitterZPosition :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> CDouble -> IO ()
setEmitterZPosition caEmitterLayer  value =
  sendMsg caEmitterLayer (mkSelector "setEmitterZPosition:") retVoid [argCDouble (fromIntegral value)]

-- | @- emitterDepth@
emitterDepth :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> IO CDouble
emitterDepth caEmitterLayer  =
  sendMsg caEmitterLayer (mkSelector "emitterDepth") retCDouble []

-- | @- setEmitterDepth:@
setEmitterDepth :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> CDouble -> IO ()
setEmitterDepth caEmitterLayer  value =
  sendMsg caEmitterLayer (mkSelector "setEmitterDepth:") retVoid [argCDouble (fromIntegral value)]

-- | @- emitterShape@
emitterShape :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> IO (Id NSString)
emitterShape caEmitterLayer  =
  sendMsg caEmitterLayer (mkSelector "emitterShape") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEmitterShape:@
setEmitterShape :: (IsCAEmitterLayer caEmitterLayer, IsNSString value) => caEmitterLayer -> value -> IO ()
setEmitterShape caEmitterLayer  value =
withObjCPtr value $ \raw_value ->
    sendMsg caEmitterLayer (mkSelector "setEmitterShape:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- emitterMode@
emitterMode :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> IO (Id NSString)
emitterMode caEmitterLayer  =
  sendMsg caEmitterLayer (mkSelector "emitterMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEmitterMode:@
setEmitterMode :: (IsCAEmitterLayer caEmitterLayer, IsNSString value) => caEmitterLayer -> value -> IO ()
setEmitterMode caEmitterLayer  value =
withObjCPtr value $ \raw_value ->
    sendMsg caEmitterLayer (mkSelector "setEmitterMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- renderMode@
renderMode :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> IO (Id NSString)
renderMode caEmitterLayer  =
  sendMsg caEmitterLayer (mkSelector "renderMode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRenderMode:@
setRenderMode :: (IsCAEmitterLayer caEmitterLayer, IsNSString value) => caEmitterLayer -> value -> IO ()
setRenderMode caEmitterLayer  value =
withObjCPtr value $ \raw_value ->
    sendMsg caEmitterLayer (mkSelector "setRenderMode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- preservesDepth@
preservesDepth :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> IO Bool
preservesDepth caEmitterLayer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg caEmitterLayer (mkSelector "preservesDepth") retCULong []

-- | @- setPreservesDepth:@
setPreservesDepth :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> Bool -> IO ()
setPreservesDepth caEmitterLayer  value =
  sendMsg caEmitterLayer (mkSelector "setPreservesDepth:") retVoid [argCULong (if value then 1 else 0)]

-- | @- velocity@
velocity :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> IO CFloat
velocity caEmitterLayer  =
  sendMsg caEmitterLayer (mkSelector "velocity") retCFloat []

-- | @- setVelocity:@
setVelocity :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> CFloat -> IO ()
setVelocity caEmitterLayer  value =
  sendMsg caEmitterLayer (mkSelector "setVelocity:") retVoid [argCFloat (fromIntegral value)]

-- | @- scale@
scale :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> IO CFloat
scale caEmitterLayer  =
  sendMsg caEmitterLayer (mkSelector "scale") retCFloat []

-- | @- setScale:@
setScale :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> CFloat -> IO ()
setScale caEmitterLayer  value =
  sendMsg caEmitterLayer (mkSelector "setScale:") retVoid [argCFloat (fromIntegral value)]

-- | @- spin@
spin :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> IO CFloat
spin caEmitterLayer  =
  sendMsg caEmitterLayer (mkSelector "spin") retCFloat []

-- | @- setSpin:@
setSpin :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> CFloat -> IO ()
setSpin caEmitterLayer  value =
  sendMsg caEmitterLayer (mkSelector "setSpin:") retVoid [argCFloat (fromIntegral value)]

-- | @- seed@
seed :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> IO CUInt
seed caEmitterLayer  =
  sendMsg caEmitterLayer (mkSelector "seed") retCUInt []

-- | @- setSeed:@
setSeed :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> CUInt -> IO ()
setSeed caEmitterLayer  value =
  sendMsg caEmitterLayer (mkSelector "setSeed:") retVoid [argCUInt (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @emitterCells@
emitterCellsSelector :: Selector
emitterCellsSelector = mkSelector "emitterCells"

-- | @Selector@ for @setEmitterCells:@
setEmitterCellsSelector :: Selector
setEmitterCellsSelector = mkSelector "setEmitterCells:"

-- | @Selector@ for @birthRate@
birthRateSelector :: Selector
birthRateSelector = mkSelector "birthRate"

-- | @Selector@ for @setBirthRate:@
setBirthRateSelector :: Selector
setBirthRateSelector = mkSelector "setBirthRate:"

-- | @Selector@ for @lifetime@
lifetimeSelector :: Selector
lifetimeSelector = mkSelector "lifetime"

-- | @Selector@ for @setLifetime:@
setLifetimeSelector :: Selector
setLifetimeSelector = mkSelector "setLifetime:"

-- | @Selector@ for @emitterZPosition@
emitterZPositionSelector :: Selector
emitterZPositionSelector = mkSelector "emitterZPosition"

-- | @Selector@ for @setEmitterZPosition:@
setEmitterZPositionSelector :: Selector
setEmitterZPositionSelector = mkSelector "setEmitterZPosition:"

-- | @Selector@ for @emitterDepth@
emitterDepthSelector :: Selector
emitterDepthSelector = mkSelector "emitterDepth"

-- | @Selector@ for @setEmitterDepth:@
setEmitterDepthSelector :: Selector
setEmitterDepthSelector = mkSelector "setEmitterDepth:"

-- | @Selector@ for @emitterShape@
emitterShapeSelector :: Selector
emitterShapeSelector = mkSelector "emitterShape"

-- | @Selector@ for @setEmitterShape:@
setEmitterShapeSelector :: Selector
setEmitterShapeSelector = mkSelector "setEmitterShape:"

-- | @Selector@ for @emitterMode@
emitterModeSelector :: Selector
emitterModeSelector = mkSelector "emitterMode"

-- | @Selector@ for @setEmitterMode:@
setEmitterModeSelector :: Selector
setEmitterModeSelector = mkSelector "setEmitterMode:"

-- | @Selector@ for @renderMode@
renderModeSelector :: Selector
renderModeSelector = mkSelector "renderMode"

-- | @Selector@ for @setRenderMode:@
setRenderModeSelector :: Selector
setRenderModeSelector = mkSelector "setRenderMode:"

-- | @Selector@ for @preservesDepth@
preservesDepthSelector :: Selector
preservesDepthSelector = mkSelector "preservesDepth"

-- | @Selector@ for @setPreservesDepth:@
setPreservesDepthSelector :: Selector
setPreservesDepthSelector = mkSelector "setPreservesDepth:"

-- | @Selector@ for @velocity@
velocitySelector :: Selector
velocitySelector = mkSelector "velocity"

-- | @Selector@ for @setVelocity:@
setVelocitySelector :: Selector
setVelocitySelector = mkSelector "setVelocity:"

-- | @Selector@ for @scale@
scaleSelector :: Selector
scaleSelector = mkSelector "scale"

-- | @Selector@ for @setScale:@
setScaleSelector :: Selector
setScaleSelector = mkSelector "setScale:"

-- | @Selector@ for @spin@
spinSelector :: Selector
spinSelector = mkSelector "spin"

-- | @Selector@ for @setSpin:@
setSpinSelector :: Selector
setSpinSelector = mkSelector "setSpin:"

-- | @Selector@ for @seed@
seedSelector :: Selector
seedSelector = mkSelector "seed"

-- | @Selector@ for @setSeed:@
setSeedSelector :: Selector
setSeedSelector = mkSelector "setSeed:"

