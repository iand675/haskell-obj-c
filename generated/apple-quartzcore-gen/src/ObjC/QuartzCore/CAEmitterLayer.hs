{-# LANGUAGE DataKinds #-}
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
  , birthRateSelector
  , emitterCellsSelector
  , emitterDepthSelector
  , emitterModeSelector
  , emitterShapeSelector
  , emitterZPositionSelector
  , lifetimeSelector
  , preservesDepthSelector
  , renderModeSelector
  , scaleSelector
  , seedSelector
  , setBirthRateSelector
  , setEmitterCellsSelector
  , setEmitterDepthSelector
  , setEmitterModeSelector
  , setEmitterShapeSelector
  , setEmitterZPositionSelector
  , setLifetimeSelector
  , setPreservesDepthSelector
  , setRenderModeSelector
  , setScaleSelector
  , setSeedSelector
  , setSpinSelector
  , setVelocitySelector
  , spinSelector
  , velocitySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuartzCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- emitterCells@
emitterCells :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> IO (Id NSArray)
emitterCells caEmitterLayer =
  sendMessage caEmitterLayer emitterCellsSelector

-- | @- setEmitterCells:@
setEmitterCells :: (IsCAEmitterLayer caEmitterLayer, IsNSArray value) => caEmitterLayer -> value -> IO ()
setEmitterCells caEmitterLayer value =
  sendMessage caEmitterLayer setEmitterCellsSelector (toNSArray value)

-- | @- birthRate@
birthRate :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> IO CFloat
birthRate caEmitterLayer =
  sendMessage caEmitterLayer birthRateSelector

-- | @- setBirthRate:@
setBirthRate :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> CFloat -> IO ()
setBirthRate caEmitterLayer value =
  sendMessage caEmitterLayer setBirthRateSelector value

-- | @- lifetime@
lifetime :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> IO CFloat
lifetime caEmitterLayer =
  sendMessage caEmitterLayer lifetimeSelector

-- | @- setLifetime:@
setLifetime :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> CFloat -> IO ()
setLifetime caEmitterLayer value =
  sendMessage caEmitterLayer setLifetimeSelector value

-- | @- emitterZPosition@
emitterZPosition :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> IO CDouble
emitterZPosition caEmitterLayer =
  sendMessage caEmitterLayer emitterZPositionSelector

-- | @- setEmitterZPosition:@
setEmitterZPosition :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> CDouble -> IO ()
setEmitterZPosition caEmitterLayer value =
  sendMessage caEmitterLayer setEmitterZPositionSelector value

-- | @- emitterDepth@
emitterDepth :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> IO CDouble
emitterDepth caEmitterLayer =
  sendMessage caEmitterLayer emitterDepthSelector

-- | @- setEmitterDepth:@
setEmitterDepth :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> CDouble -> IO ()
setEmitterDepth caEmitterLayer value =
  sendMessage caEmitterLayer setEmitterDepthSelector value

-- | @- emitterShape@
emitterShape :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> IO (Id NSString)
emitterShape caEmitterLayer =
  sendMessage caEmitterLayer emitterShapeSelector

-- | @- setEmitterShape:@
setEmitterShape :: (IsCAEmitterLayer caEmitterLayer, IsNSString value) => caEmitterLayer -> value -> IO ()
setEmitterShape caEmitterLayer value =
  sendMessage caEmitterLayer setEmitterShapeSelector (toNSString value)

-- | @- emitterMode@
emitterMode :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> IO (Id NSString)
emitterMode caEmitterLayer =
  sendMessage caEmitterLayer emitterModeSelector

-- | @- setEmitterMode:@
setEmitterMode :: (IsCAEmitterLayer caEmitterLayer, IsNSString value) => caEmitterLayer -> value -> IO ()
setEmitterMode caEmitterLayer value =
  sendMessage caEmitterLayer setEmitterModeSelector (toNSString value)

-- | @- renderMode@
renderMode :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> IO (Id NSString)
renderMode caEmitterLayer =
  sendMessage caEmitterLayer renderModeSelector

-- | @- setRenderMode:@
setRenderMode :: (IsCAEmitterLayer caEmitterLayer, IsNSString value) => caEmitterLayer -> value -> IO ()
setRenderMode caEmitterLayer value =
  sendMessage caEmitterLayer setRenderModeSelector (toNSString value)

-- | @- preservesDepth@
preservesDepth :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> IO Bool
preservesDepth caEmitterLayer =
  sendMessage caEmitterLayer preservesDepthSelector

-- | @- setPreservesDepth:@
setPreservesDepth :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> Bool -> IO ()
setPreservesDepth caEmitterLayer value =
  sendMessage caEmitterLayer setPreservesDepthSelector value

-- | @- velocity@
velocity :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> IO CFloat
velocity caEmitterLayer =
  sendMessage caEmitterLayer velocitySelector

-- | @- setVelocity:@
setVelocity :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> CFloat -> IO ()
setVelocity caEmitterLayer value =
  sendMessage caEmitterLayer setVelocitySelector value

-- | @- scale@
scale :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> IO CFloat
scale caEmitterLayer =
  sendMessage caEmitterLayer scaleSelector

-- | @- setScale:@
setScale :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> CFloat -> IO ()
setScale caEmitterLayer value =
  sendMessage caEmitterLayer setScaleSelector value

-- | @- spin@
spin :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> IO CFloat
spin caEmitterLayer =
  sendMessage caEmitterLayer spinSelector

-- | @- setSpin:@
setSpin :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> CFloat -> IO ()
setSpin caEmitterLayer value =
  sendMessage caEmitterLayer setSpinSelector value

-- | @- seed@
seed :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> IO CUInt
seed caEmitterLayer =
  sendMessage caEmitterLayer seedSelector

-- | @- setSeed:@
setSeed :: IsCAEmitterLayer caEmitterLayer => caEmitterLayer -> CUInt -> IO ()
setSeed caEmitterLayer value =
  sendMessage caEmitterLayer setSeedSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @emitterCells@
emitterCellsSelector :: Selector '[] (Id NSArray)
emitterCellsSelector = mkSelector "emitterCells"

-- | @Selector@ for @setEmitterCells:@
setEmitterCellsSelector :: Selector '[Id NSArray] ()
setEmitterCellsSelector = mkSelector "setEmitterCells:"

-- | @Selector@ for @birthRate@
birthRateSelector :: Selector '[] CFloat
birthRateSelector = mkSelector "birthRate"

-- | @Selector@ for @setBirthRate:@
setBirthRateSelector :: Selector '[CFloat] ()
setBirthRateSelector = mkSelector "setBirthRate:"

-- | @Selector@ for @lifetime@
lifetimeSelector :: Selector '[] CFloat
lifetimeSelector = mkSelector "lifetime"

-- | @Selector@ for @setLifetime:@
setLifetimeSelector :: Selector '[CFloat] ()
setLifetimeSelector = mkSelector "setLifetime:"

-- | @Selector@ for @emitterZPosition@
emitterZPositionSelector :: Selector '[] CDouble
emitterZPositionSelector = mkSelector "emitterZPosition"

-- | @Selector@ for @setEmitterZPosition:@
setEmitterZPositionSelector :: Selector '[CDouble] ()
setEmitterZPositionSelector = mkSelector "setEmitterZPosition:"

-- | @Selector@ for @emitterDepth@
emitterDepthSelector :: Selector '[] CDouble
emitterDepthSelector = mkSelector "emitterDepth"

-- | @Selector@ for @setEmitterDepth:@
setEmitterDepthSelector :: Selector '[CDouble] ()
setEmitterDepthSelector = mkSelector "setEmitterDepth:"

-- | @Selector@ for @emitterShape@
emitterShapeSelector :: Selector '[] (Id NSString)
emitterShapeSelector = mkSelector "emitterShape"

-- | @Selector@ for @setEmitterShape:@
setEmitterShapeSelector :: Selector '[Id NSString] ()
setEmitterShapeSelector = mkSelector "setEmitterShape:"

-- | @Selector@ for @emitterMode@
emitterModeSelector :: Selector '[] (Id NSString)
emitterModeSelector = mkSelector "emitterMode"

-- | @Selector@ for @setEmitterMode:@
setEmitterModeSelector :: Selector '[Id NSString] ()
setEmitterModeSelector = mkSelector "setEmitterMode:"

-- | @Selector@ for @renderMode@
renderModeSelector :: Selector '[] (Id NSString)
renderModeSelector = mkSelector "renderMode"

-- | @Selector@ for @setRenderMode:@
setRenderModeSelector :: Selector '[Id NSString] ()
setRenderModeSelector = mkSelector "setRenderMode:"

-- | @Selector@ for @preservesDepth@
preservesDepthSelector :: Selector '[] Bool
preservesDepthSelector = mkSelector "preservesDepth"

-- | @Selector@ for @setPreservesDepth:@
setPreservesDepthSelector :: Selector '[Bool] ()
setPreservesDepthSelector = mkSelector "setPreservesDepth:"

-- | @Selector@ for @velocity@
velocitySelector :: Selector '[] CFloat
velocitySelector = mkSelector "velocity"

-- | @Selector@ for @setVelocity:@
setVelocitySelector :: Selector '[CFloat] ()
setVelocitySelector = mkSelector "setVelocity:"

-- | @Selector@ for @scale@
scaleSelector :: Selector '[] CFloat
scaleSelector = mkSelector "scale"

-- | @Selector@ for @setScale:@
setScaleSelector :: Selector '[CFloat] ()
setScaleSelector = mkSelector "setScale:"

-- | @Selector@ for @spin@
spinSelector :: Selector '[] CFloat
spinSelector = mkSelector "spin"

-- | @Selector@ for @setSpin:@
setSpinSelector :: Selector '[CFloat] ()
setSpinSelector = mkSelector "setSpin:"

-- | @Selector@ for @seed@
seedSelector :: Selector '[] CUInt
seedSelector = mkSelector "seed"

-- | @Selector@ for @setSeed:@
setSeedSelector :: Selector '[CUInt] ()
setSeedSelector = mkSelector "setSeed:"

