{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An emitter of particle sprites.
--
-- Generated bindings for @SKEmitterNode@.
module ObjC.SpriteKit.SKEmitterNode
  ( SKEmitterNode
  , IsSKEmitterNode(..)
  , advanceSimulationTime
  , resetSimulation
  , valueForAttributeNamed
  , setValue_forAttributeNamed
  , particleTexture
  , setParticleTexture
  , particleBlendMode
  , setParticleBlendMode
  , particleColor
  , setParticleColor
  , particleColorRedRange
  , setParticleColorRedRange
  , particleColorGreenRange
  , setParticleColorGreenRange
  , particleColorBlueRange
  , setParticleColorBlueRange
  , particleColorAlphaRange
  , setParticleColorAlphaRange
  , particleColorRedSpeed
  , setParticleColorRedSpeed
  , particleColorGreenSpeed
  , setParticleColorGreenSpeed
  , particleColorBlueSpeed
  , setParticleColorBlueSpeed
  , particleColorAlphaSpeed
  , setParticleColorAlphaSpeed
  , particleColorSequence
  , setParticleColorSequence
  , particleColorBlendFactor
  , setParticleColorBlendFactor
  , particleColorBlendFactorRange
  , setParticleColorBlendFactorRange
  , particleColorBlendFactorSpeed
  , setParticleColorBlendFactorSpeed
  , particleColorBlendFactorSequence
  , setParticleColorBlendFactorSequence
  , particleSpeed
  , setParticleSpeed
  , particleSpeedRange
  , setParticleSpeedRange
  , emissionAngle
  , setEmissionAngle
  , emissionAngleRange
  , setEmissionAngleRange
  , xAcceleration
  , setXAcceleration
  , yAcceleration
  , setYAcceleration
  , particleBirthRate
  , setParticleBirthRate
  , numParticlesToEmit
  , setNumParticlesToEmit
  , particleLifetime
  , setParticleLifetime
  , particleLifetimeRange
  , setParticleLifetimeRange
  , particleRotation
  , setParticleRotation
  , particleRotationRange
  , setParticleRotationRange
  , particleRotationSpeed
  , setParticleRotationSpeed
  , particleScale
  , setParticleScale
  , particleScaleRange
  , setParticleScaleRange
  , particleScaleSpeed
  , setParticleScaleSpeed
  , particleScaleSequence
  , setParticleScaleSequence
  , particleAlpha
  , setParticleAlpha
  , particleAlphaRange
  , setParticleAlphaRange
  , particleAlphaSpeed
  , setParticleAlphaSpeed
  , particleAlphaSequence
  , setParticleAlphaSequence
  , particleAction
  , setParticleAction
  , fieldBitMask
  , setFieldBitMask
  , targetNode
  , setTargetNode
  , shader
  , setShader
  , attributeValues
  , setAttributeValues
  , particleZPosition
  , setParticleZPosition
  , particleRenderOrder
  , setParticleRenderOrder
  , particleZPositionRange
  , setParticleZPositionRange
  , particleZPositionSpeed
  , setParticleZPositionSpeed
  , advanceSimulationTimeSelector
  , resetSimulationSelector
  , valueForAttributeNamedSelector
  , setValue_forAttributeNamedSelector
  , particleTextureSelector
  , setParticleTextureSelector
  , particleBlendModeSelector
  , setParticleBlendModeSelector
  , particleColorSelector
  , setParticleColorSelector
  , particleColorRedRangeSelector
  , setParticleColorRedRangeSelector
  , particleColorGreenRangeSelector
  , setParticleColorGreenRangeSelector
  , particleColorBlueRangeSelector
  , setParticleColorBlueRangeSelector
  , particleColorAlphaRangeSelector
  , setParticleColorAlphaRangeSelector
  , particleColorRedSpeedSelector
  , setParticleColorRedSpeedSelector
  , particleColorGreenSpeedSelector
  , setParticleColorGreenSpeedSelector
  , particleColorBlueSpeedSelector
  , setParticleColorBlueSpeedSelector
  , particleColorAlphaSpeedSelector
  , setParticleColorAlphaSpeedSelector
  , particleColorSequenceSelector
  , setParticleColorSequenceSelector
  , particleColorBlendFactorSelector
  , setParticleColorBlendFactorSelector
  , particleColorBlendFactorRangeSelector
  , setParticleColorBlendFactorRangeSelector
  , particleColorBlendFactorSpeedSelector
  , setParticleColorBlendFactorSpeedSelector
  , particleColorBlendFactorSequenceSelector
  , setParticleColorBlendFactorSequenceSelector
  , particleSpeedSelector
  , setParticleSpeedSelector
  , particleSpeedRangeSelector
  , setParticleSpeedRangeSelector
  , emissionAngleSelector
  , setEmissionAngleSelector
  , emissionAngleRangeSelector
  , setEmissionAngleRangeSelector
  , xAccelerationSelector
  , setXAccelerationSelector
  , yAccelerationSelector
  , setYAccelerationSelector
  , particleBirthRateSelector
  , setParticleBirthRateSelector
  , numParticlesToEmitSelector
  , setNumParticlesToEmitSelector
  , particleLifetimeSelector
  , setParticleLifetimeSelector
  , particleLifetimeRangeSelector
  , setParticleLifetimeRangeSelector
  , particleRotationSelector
  , setParticleRotationSelector
  , particleRotationRangeSelector
  , setParticleRotationRangeSelector
  , particleRotationSpeedSelector
  , setParticleRotationSpeedSelector
  , particleScaleSelector
  , setParticleScaleSelector
  , particleScaleRangeSelector
  , setParticleScaleRangeSelector
  , particleScaleSpeedSelector
  , setParticleScaleSpeedSelector
  , particleScaleSequenceSelector
  , setParticleScaleSequenceSelector
  , particleAlphaSelector
  , setParticleAlphaSelector
  , particleAlphaRangeSelector
  , setParticleAlphaRangeSelector
  , particleAlphaSpeedSelector
  , setParticleAlphaSpeedSelector
  , particleAlphaSequenceSelector
  , setParticleAlphaSequenceSelector
  , particleActionSelector
  , setParticleActionSelector
  , fieldBitMaskSelector
  , setFieldBitMaskSelector
  , targetNodeSelector
  , setTargetNodeSelector
  , shaderSelector
  , setShaderSelector
  , attributeValuesSelector
  , setAttributeValuesSelector
  , particleZPositionSelector
  , setParticleZPositionSelector
  , particleRenderOrderSelector
  , setParticleRenderOrderSelector
  , particleZPositionRangeSelector
  , setParticleZPositionRangeSelector
  , particleZPositionSpeedSelector
  , setParticleZPositionSpeedSelector

  -- * Enum types
  , SKBlendMode(SKBlendMode)
  , pattern SKBlendModeAlpha
  , pattern SKBlendModeAdd
  , pattern SKBlendModeSubtract
  , pattern SKBlendModeMultiply
  , pattern SKBlendModeMultiplyX2
  , pattern SKBlendModeScreen
  , pattern SKBlendModeReplace
  , pattern SKBlendModeMultiplyAlpha
  , SKParticleRenderOrder(SKParticleRenderOrder)
  , pattern SKParticleRenderOrderOldestLast
  , pattern SKParticleRenderOrderOldestFirst
  , pattern SKParticleRenderOrderDontCare

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

import ObjC.SpriteKit.Internal.Classes
import ObjC.SpriteKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The particle simulation is stepped automatically each frame when present in the scene. This allows the user to manually advance the simulation by a fixed amount of time. Useful for pre-populating particles before adding them to the scene.
--
-- ObjC selector: @- advanceSimulationTime:@
advanceSimulationTime :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
advanceSimulationTime skEmitterNode  sec =
  sendMsg skEmitterNode (mkSelector "advanceSimulationTime:") retVoid [argCDouble (fromIntegral sec)]

-- | @- resetSimulation@
resetSimulation :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO ()
resetSimulation skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "resetSimulation") retVoid []

-- | @- valueForAttributeNamed:@
valueForAttributeNamed :: (IsSKEmitterNode skEmitterNode, IsNSString key) => skEmitterNode -> key -> IO (Id SKAttributeValue)
valueForAttributeNamed skEmitterNode  key =
withObjCPtr key $ \raw_key ->
    sendMsg skEmitterNode (mkSelector "valueForAttributeNamed:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())] >>= retainedObject . castPtr

-- | @- setValue:forAttributeNamed:@
setValue_forAttributeNamed :: (IsSKEmitterNode skEmitterNode, IsSKAttributeValue value, IsNSString key) => skEmitterNode -> value -> key -> IO ()
setValue_forAttributeNamed skEmitterNode  value key =
withObjCPtr value $ \raw_value ->
  withObjCPtr key $ \raw_key ->
      sendMsg skEmitterNode (mkSelector "setValue:forAttributeNamed:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | The texture to be used for the particles.
--
-- ObjC selector: @- particleTexture@
particleTexture :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO (Id SKTexture)
particleTexture skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "particleTexture") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The texture to be used for the particles.
--
-- ObjC selector: @- setParticleTexture:@
setParticleTexture :: (IsSKEmitterNode skEmitterNode, IsSKTexture value) => skEmitterNode -> value -> IO ()
setParticleTexture skEmitterNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skEmitterNode (mkSelector "setParticleTexture:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The blend mode for each particle. Defaults to SKBlendModeAlpha.
--
-- ObjC selector: @- particleBlendMode@
particleBlendMode :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO SKBlendMode
particleBlendMode skEmitterNode  =
  fmap (coerce :: CLong -> SKBlendMode) $ sendMsg skEmitterNode (mkSelector "particleBlendMode") retCLong []

-- | The blend mode for each particle. Defaults to SKBlendModeAlpha.
--
-- ObjC selector: @- setParticleBlendMode:@
setParticleBlendMode :: IsSKEmitterNode skEmitterNode => skEmitterNode -> SKBlendMode -> IO ()
setParticleBlendMode skEmitterNode  value =
  sendMsg skEmitterNode (mkSelector "setParticleBlendMode:") retVoid [argCLong (coerce value)]

-- | The starting color for each particle. Defaults to clear.
--
-- ObjC selector: @- particleColor@
particleColor :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO (Id NSColor)
particleColor skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "particleColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The starting color for each particle. Defaults to clear.
--
-- ObjC selector: @- setParticleColor:@
setParticleColor :: (IsSKEmitterNode skEmitterNode, IsNSColor value) => skEmitterNode -> value -> IO ()
setParticleColor skEmitterNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skEmitterNode (mkSelector "setParticleColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The random variance about each color component for each particle. Defaults to 0.0.
--
-- ObjC selector: @- particleColorRedRange@
particleColorRedRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleColorRedRange skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "particleColorRedRange") retCDouble []

-- | The random variance about each color component for each particle. Defaults to 0.0.
--
-- ObjC selector: @- setParticleColorRedRange:@
setParticleColorRedRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleColorRedRange skEmitterNode  value =
  sendMsg skEmitterNode (mkSelector "setParticleColorRedRange:") retVoid [argCDouble (fromIntegral value)]

-- | @- particleColorGreenRange@
particleColorGreenRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleColorGreenRange skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "particleColorGreenRange") retCDouble []

-- | @- setParticleColorGreenRange:@
setParticleColorGreenRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleColorGreenRange skEmitterNode  value =
  sendMsg skEmitterNode (mkSelector "setParticleColorGreenRange:") retVoid [argCDouble (fromIntegral value)]

-- | @- particleColorBlueRange@
particleColorBlueRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleColorBlueRange skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "particleColorBlueRange") retCDouble []

-- | @- setParticleColorBlueRange:@
setParticleColorBlueRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleColorBlueRange skEmitterNode  value =
  sendMsg skEmitterNode (mkSelector "setParticleColorBlueRange:") retVoid [argCDouble (fromIntegral value)]

-- | @- particleColorAlphaRange@
particleColorAlphaRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleColorAlphaRange skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "particleColorAlphaRange") retCDouble []

-- | @- setParticleColorAlphaRange:@
setParticleColorAlphaRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleColorAlphaRange skEmitterNode  value =
  sendMsg skEmitterNode (mkSelector "setParticleColorAlphaRange:") retVoid [argCDouble (fromIntegral value)]

-- | The rate at which to modify each color component for each particle (per second).
--
-- ObjC selector: @- particleColorRedSpeed@
particleColorRedSpeed :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleColorRedSpeed skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "particleColorRedSpeed") retCDouble []

-- | The rate at which to modify each color component for each particle (per second).
--
-- ObjC selector: @- setParticleColorRedSpeed:@
setParticleColorRedSpeed :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleColorRedSpeed skEmitterNode  value =
  sendMsg skEmitterNode (mkSelector "setParticleColorRedSpeed:") retVoid [argCDouble (fromIntegral value)]

-- | @- particleColorGreenSpeed@
particleColorGreenSpeed :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleColorGreenSpeed skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "particleColorGreenSpeed") retCDouble []

-- | @- setParticleColorGreenSpeed:@
setParticleColorGreenSpeed :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleColorGreenSpeed skEmitterNode  value =
  sendMsg skEmitterNode (mkSelector "setParticleColorGreenSpeed:") retVoid [argCDouble (fromIntegral value)]

-- | @- particleColorBlueSpeed@
particleColorBlueSpeed :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleColorBlueSpeed skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "particleColorBlueSpeed") retCDouble []

-- | @- setParticleColorBlueSpeed:@
setParticleColorBlueSpeed :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleColorBlueSpeed skEmitterNode  value =
  sendMsg skEmitterNode (mkSelector "setParticleColorBlueSpeed:") retVoid [argCDouble (fromIntegral value)]

-- | @- particleColorAlphaSpeed@
particleColorAlphaSpeed :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleColorAlphaSpeed skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "particleColorAlphaSpeed") retCDouble []

-- | @- setParticleColorAlphaSpeed:@
setParticleColorAlphaSpeed :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleColorAlphaSpeed skEmitterNode  value =
  sendMsg skEmitterNode (mkSelector "setParticleColorAlphaSpeed:") retVoid [argCDouble (fromIntegral value)]

-- | @- particleColorSequence@
particleColorSequence :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO (Id SKKeyframeSequence)
particleColorSequence skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "particleColorSequence") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setParticleColorSequence:@
setParticleColorSequence :: (IsSKEmitterNode skEmitterNode, IsSKKeyframeSequence value) => skEmitterNode -> value -> IO ()
setParticleColorSequence skEmitterNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skEmitterNode (mkSelector "setParticleColorSequence:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The starting color blend for each particle. Behaves the same as SKSpriteNode. Defaults to 0.0.
--
-- ObjC selector: @- particleColorBlendFactor@
particleColorBlendFactor :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleColorBlendFactor skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "particleColorBlendFactor") retCDouble []

-- | The starting color blend for each particle. Behaves the same as SKSpriteNode. Defaults to 0.0.
--
-- ObjC selector: @- setParticleColorBlendFactor:@
setParticleColorBlendFactor :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleColorBlendFactor skEmitterNode  value =
  sendMsg skEmitterNode (mkSelector "setParticleColorBlendFactor:") retVoid [argCDouble (fromIntegral value)]

-- | The random variance about the starting color blend for each particle. Defaults to 0.0.
--
-- ObjC selector: @- particleColorBlendFactorRange@
particleColorBlendFactorRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleColorBlendFactorRange skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "particleColorBlendFactorRange") retCDouble []

-- | The random variance about the starting color blend for each particle. Defaults to 0.0.
--
-- ObjC selector: @- setParticleColorBlendFactorRange:@
setParticleColorBlendFactorRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleColorBlendFactorRange skEmitterNode  value =
  sendMsg skEmitterNode (mkSelector "setParticleColorBlendFactorRange:") retVoid [argCDouble (fromIntegral value)]

-- | The rate at which to modify the color blend for each particle. Defaults to 0.0.
--
-- ObjC selector: @- particleColorBlendFactorSpeed@
particleColorBlendFactorSpeed :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleColorBlendFactorSpeed skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "particleColorBlendFactorSpeed") retCDouble []

-- | The rate at which to modify the color blend for each particle. Defaults to 0.0.
--
-- ObjC selector: @- setParticleColorBlendFactorSpeed:@
setParticleColorBlendFactorSpeed :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleColorBlendFactorSpeed skEmitterNode  value =
  sendMsg skEmitterNode (mkSelector "setParticleColorBlendFactorSpeed:") retVoid [argCDouble (fromIntegral value)]

-- | @- particleColorBlendFactorSequence@
particleColorBlendFactorSequence :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO (Id SKKeyframeSequence)
particleColorBlendFactorSequence skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "particleColorBlendFactorSequence") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setParticleColorBlendFactorSequence:@
setParticleColorBlendFactorSequence :: (IsSKEmitterNode skEmitterNode, IsSKKeyframeSequence value) => skEmitterNode -> value -> IO ()
setParticleColorBlendFactorSequence skEmitterNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skEmitterNode (mkSelector "setParticleColorBlendFactorSequence:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The starting speed for each particle along its emission vector. Defaults to 0.0.
--
-- ObjC selector: @- particleSpeed@
particleSpeed :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleSpeed skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "particleSpeed") retCDouble []

-- | The starting speed for each particle along its emission vector. Defaults to 0.0.
--
-- ObjC selector: @- setParticleSpeed:@
setParticleSpeed :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleSpeed skEmitterNode  value =
  sendMsg skEmitterNode (mkSelector "setParticleSpeed:") retVoid [argCDouble (fromIntegral value)]

-- | The random variance about the starting speed for each particle along its emission vector. Defaults to 0.0.
--
-- ObjC selector: @- particleSpeedRange@
particleSpeedRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleSpeedRange skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "particleSpeedRange") retCDouble []

-- | The random variance about the starting speed for each particle along its emission vector. Defaults to 0.0.
--
-- ObjC selector: @- setParticleSpeedRange:@
setParticleSpeedRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleSpeedRange skEmitterNode  value =
  sendMsg skEmitterNode (mkSelector "setParticleSpeedRange:") retVoid [argCDouble (fromIntegral value)]

-- | The angle at which to emit each new particle, in radians. Defaults to 0.0.
--
-- ObjC selector: @- emissionAngle@
emissionAngle :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
emissionAngle skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "emissionAngle") retCDouble []

-- | The angle at which to emit each new particle, in radians. Defaults to 0.0.
--
-- ObjC selector: @- setEmissionAngle:@
setEmissionAngle :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setEmissionAngle skEmitterNode  value =
  sendMsg skEmitterNode (mkSelector "setEmissionAngle:") retVoid [argCDouble (fromIntegral value)]

-- | The random variance about the angle at which to emit each new particle, in radians. Defaults to 0.0.
--
-- ObjC selector: @- emissionAngleRange@
emissionAngleRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
emissionAngleRange skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "emissionAngleRange") retCDouble []

-- | The random variance about the angle at which to emit each new particle, in radians. Defaults to 0.0.
--
-- ObjC selector: @- setEmissionAngleRange:@
setEmissionAngleRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setEmissionAngleRange skEmitterNode  value =
  sendMsg skEmitterNode (mkSelector "setEmissionAngleRange:") retVoid [argCDouble (fromIntegral value)]

-- | The acceleration to apply to each particles velocity. Useful for simulating effects such as wind or gravity. Defaults to 0.0.
--
-- ObjC selector: @- xAcceleration@
xAcceleration :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
xAcceleration skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "xAcceleration") retCDouble []

-- | The acceleration to apply to each particles velocity. Useful for simulating effects such as wind or gravity. Defaults to 0.0.
--
-- ObjC selector: @- setXAcceleration:@
setXAcceleration :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setXAcceleration skEmitterNode  value =
  sendMsg skEmitterNode (mkSelector "setXAcceleration:") retVoid [argCDouble (fromIntegral value)]

-- | @- yAcceleration@
yAcceleration :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
yAcceleration skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "yAcceleration") retCDouble []

-- | @- setYAcceleration:@
setYAcceleration :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setYAcceleration skEmitterNode  value =
  sendMsg skEmitterNode (mkSelector "setYAcceleration:") retVoid [argCDouble (fromIntegral value)]

-- | The rate at which new particles are generated, in particles per second. Defaults to 0.0.
--
-- ObjC selector: @- particleBirthRate@
particleBirthRate :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleBirthRate skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "particleBirthRate") retCDouble []

-- | The rate at which new particles are generated, in particles per second. Defaults to 0.0.
--
-- ObjC selector: @- setParticleBirthRate:@
setParticleBirthRate :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleBirthRate skEmitterNode  value =
  sendMsg skEmitterNode (mkSelector "setParticleBirthRate:") retVoid [argCDouble (fromIntegral value)]

-- | The number of particles that will be emitted. If set to 0, there is no limit. Defaults to 0.
--
-- ObjC selector: @- numParticlesToEmit@
numParticlesToEmit :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CULong
numParticlesToEmit skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "numParticlesToEmit") retCULong []

-- | The number of particles that will be emitted. If set to 0, there is no limit. Defaults to 0.
--
-- ObjC selector: @- setNumParticlesToEmit:@
setNumParticlesToEmit :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CULong -> IO ()
setNumParticlesToEmit skEmitterNode  value =
  sendMsg skEmitterNode (mkSelector "setNumParticlesToEmit:") retVoid [argCULong (fromIntegral value)]

-- | The lifetime of each particle, in seconds. Defaults to 0.0.
--
-- ObjC selector: @- particleLifetime@
particleLifetime :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleLifetime skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "particleLifetime") retCDouble []

-- | The lifetime of each particle, in seconds. Defaults to 0.0.
--
-- ObjC selector: @- setParticleLifetime:@
setParticleLifetime :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleLifetime skEmitterNode  value =
  sendMsg skEmitterNode (mkSelector "setParticleLifetime:") retVoid [argCDouble (fromIntegral value)]

-- | The random variance about the lifetime of each particle, in seconds. Defaults to 0.0.
--
-- ObjC selector: @- particleLifetimeRange@
particleLifetimeRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleLifetimeRange skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "particleLifetimeRange") retCDouble []

-- | The random variance about the lifetime of each particle, in seconds. Defaults to 0.0.
--
-- ObjC selector: @- setParticleLifetimeRange:@
setParticleLifetimeRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleLifetimeRange skEmitterNode  value =
  sendMsg skEmitterNode (mkSelector "setParticleLifetimeRange:") retVoid [argCDouble (fromIntegral value)]

-- | The starting z-rotation for each particle. Defaults to 0.0.
--
-- ObjC selector: @- particleRotation@
particleRotation :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleRotation skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "particleRotation") retCDouble []

-- | The starting z-rotation for each particle. Defaults to 0.0.
--
-- ObjC selector: @- setParticleRotation:@
setParticleRotation :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleRotation skEmitterNode  value =
  sendMsg skEmitterNode (mkSelector "setParticleRotation:") retVoid [argCDouble (fromIntegral value)]

-- | The random variance about the starting z-rotation for each particle. Defaults to 0.0.
--
-- ObjC selector: @- particleRotationRange@
particleRotationRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleRotationRange skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "particleRotationRange") retCDouble []

-- | The random variance about the starting z-rotation for each particle. Defaults to 0.0.
--
-- ObjC selector: @- setParticleRotationRange:@
setParticleRotationRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleRotationRange skEmitterNode  value =
  sendMsg skEmitterNode (mkSelector "setParticleRotationRange:") retVoid [argCDouble (fromIntegral value)]

-- | The rate at which to modify the z-rotation for each particle. Defaults to 0.0.
--
-- ObjC selector: @- particleRotationSpeed@
particleRotationSpeed :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleRotationSpeed skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "particleRotationSpeed") retCDouble []

-- | The rate at which to modify the z-rotation for each particle. Defaults to 0.0.
--
-- ObjC selector: @- setParticleRotationSpeed:@
setParticleRotationSpeed :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleRotationSpeed skEmitterNode  value =
  sendMsg skEmitterNode (mkSelector "setParticleRotationSpeed:") retVoid [argCDouble (fromIntegral value)]

-- | The starting scale for each particle. Defaults to 1.0.
--
-- ObjC selector: @- particleScale@
particleScale :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleScale skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "particleScale") retCDouble []

-- | The starting scale for each particle. Defaults to 1.0.
--
-- ObjC selector: @- setParticleScale:@
setParticleScale :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleScale skEmitterNode  value =
  sendMsg skEmitterNode (mkSelector "setParticleScale:") retVoid [argCDouble (fromIntegral value)]

-- | The random variance about the starting scale for each particle. Defaults to 0.0.
--
-- ObjC selector: @- particleScaleRange@
particleScaleRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleScaleRange skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "particleScaleRange") retCDouble []

-- | The random variance about the starting scale for each particle. Defaults to 0.0.
--
-- ObjC selector: @- setParticleScaleRange:@
setParticleScaleRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleScaleRange skEmitterNode  value =
  sendMsg skEmitterNode (mkSelector "setParticleScaleRange:") retVoid [argCDouble (fromIntegral value)]

-- | The rate at which to modify the scale for each particle. Defaults to 0.0.
--
-- ObjC selector: @- particleScaleSpeed@
particleScaleSpeed :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleScaleSpeed skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "particleScaleSpeed") retCDouble []

-- | The rate at which to modify the scale for each particle. Defaults to 0.0.
--
-- ObjC selector: @- setParticleScaleSpeed:@
setParticleScaleSpeed :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleScaleSpeed skEmitterNode  value =
  sendMsg skEmitterNode (mkSelector "setParticleScaleSpeed:") retVoid [argCDouble (fromIntegral value)]

-- | @- particleScaleSequence@
particleScaleSequence :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO (Id SKKeyframeSequence)
particleScaleSequence skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "particleScaleSequence") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setParticleScaleSequence:@
setParticleScaleSequence :: (IsSKEmitterNode skEmitterNode, IsSKKeyframeSequence value) => skEmitterNode -> value -> IO ()
setParticleScaleSequence skEmitterNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skEmitterNode (mkSelector "setParticleScaleSequence:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The starting alpha for each particle. Defaults to 1.0.
--
-- ObjC selector: @- particleAlpha@
particleAlpha :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleAlpha skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "particleAlpha") retCDouble []

-- | The starting alpha for each particle. Defaults to 1.0.
--
-- ObjC selector: @- setParticleAlpha:@
setParticleAlpha :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleAlpha skEmitterNode  value =
  sendMsg skEmitterNode (mkSelector "setParticleAlpha:") retVoid [argCDouble (fromIntegral value)]

-- | The random variance about the starting alpha for each particle. Defaults to 1.0.
--
-- ObjC selector: @- particleAlphaRange@
particleAlphaRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleAlphaRange skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "particleAlphaRange") retCDouble []

-- | The random variance about the starting alpha for each particle. Defaults to 1.0.
--
-- ObjC selector: @- setParticleAlphaRange:@
setParticleAlphaRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleAlphaRange skEmitterNode  value =
  sendMsg skEmitterNode (mkSelector "setParticleAlphaRange:") retVoid [argCDouble (fromIntegral value)]

-- | The rate at which to modify the alpha for each particle. Defaults to 1.0.
--
-- ObjC selector: @- particleAlphaSpeed@
particleAlphaSpeed :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleAlphaSpeed skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "particleAlphaSpeed") retCDouble []

-- | The rate at which to modify the alpha for each particle. Defaults to 1.0.
--
-- ObjC selector: @- setParticleAlphaSpeed:@
setParticleAlphaSpeed :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleAlphaSpeed skEmitterNode  value =
  sendMsg skEmitterNode (mkSelector "setParticleAlphaSpeed:") retVoid [argCDouble (fromIntegral value)]

-- | @- particleAlphaSequence@
particleAlphaSequence :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO (Id SKKeyframeSequence)
particleAlphaSequence skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "particleAlphaSequence") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setParticleAlphaSequence:@
setParticleAlphaSequence :: (IsSKEmitterNode skEmitterNode, IsSKKeyframeSequence value) => skEmitterNode -> value -> IO ()
setParticleAlphaSequence skEmitterNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skEmitterNode (mkSelector "setParticleAlphaSequence:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Specifies an action executed by new particles.
--
-- ObjC selector: @- particleAction@
particleAction :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO (Id SKAction)
particleAction skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "particleAction") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Specifies an action executed by new particles.
--
-- ObjC selector: @- setParticleAction:@
setParticleAction :: (IsSKEmitterNode skEmitterNode, IsSKAction value) => skEmitterNode -> value -> IO ()
setParticleAction skEmitterNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skEmitterNode (mkSelector "setParticleAction:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Defines what logical 'categories' of fields this particles emitted respond to. Defaults to all bits set (all categories). Can be forced off via affectedByGravity.
--
-- ObjC selector: @- fieldBitMask@
fieldBitMask :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CUInt
fieldBitMask skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "fieldBitMask") retCUInt []

-- | Defines what logical 'categories' of fields this particles emitted respond to. Defaults to all bits set (all categories). Can be forced off via affectedByGravity.
--
-- ObjC selector: @- setFieldBitMask:@
setFieldBitMask :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CUInt -> IO ()
setFieldBitMask skEmitterNode  value =
  sendMsg skEmitterNode (mkSelector "setFieldBitMask:") retVoid [argCUInt (fromIntegral value)]

-- | Normally the particles are rendered as if they were a child of the SKEmitterNode, they can also be rendered as if they were a child of any other node in the scene by setting the targetNode property. Defaults to nil (standard behavior).
--
-- ObjC selector: @- targetNode@
targetNode :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO (Id SKNode)
targetNode skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "targetNode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Normally the particles are rendered as if they were a child of the SKEmitterNode, they can also be rendered as if they were a child of any other node in the scene by setting the targetNode property. Defaults to nil (standard behavior).
--
-- ObjC selector: @- setTargetNode:@
setTargetNode :: (IsSKEmitterNode skEmitterNode, IsSKNode value) => skEmitterNode -> value -> IO ()
setTargetNode skEmitterNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skEmitterNode (mkSelector "setTargetNode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- shader@
shader :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO (Id SKShader)
shader skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "shader") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setShader:@
setShader :: (IsSKEmitterNode skEmitterNode, IsSKShader value) => skEmitterNode -> value -> IO ()
setShader skEmitterNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skEmitterNode (mkSelector "setShader:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Optional dictionary of SKAttributeValues Attributes can be used with custom SKShaders.
--
-- ObjC selector: @- attributeValues@
attributeValues :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO (Id NSDictionary)
attributeValues skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "attributeValues") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Optional dictionary of SKAttributeValues Attributes can be used with custom SKShaders.
--
-- ObjC selector: @- setAttributeValues:@
setAttributeValues :: (IsSKEmitterNode skEmitterNode, IsNSDictionary value) => skEmitterNode -> value -> IO ()
setAttributeValues skEmitterNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skEmitterNode (mkSelector "setAttributeValues:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The starting z-position for each particle. Defaults to 0.0.
--
-- ObjC selector: @- particleZPosition@
particleZPosition :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleZPosition skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "particleZPosition") retCDouble []

-- | The starting z-position for each particle. Defaults to 0.0.
--
-- ObjC selector: @- setParticleZPosition:@
setParticleZPosition :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleZPosition skEmitterNode  value =
  sendMsg skEmitterNode (mkSelector "setParticleZPosition:") retVoid [argCDouble (fromIntegral value)]

-- | The order in which particles will be rendered. Defaults to SKParticleRenderOrderOldestLast.
--
-- ObjC selector: @- particleRenderOrder@
particleRenderOrder :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO SKParticleRenderOrder
particleRenderOrder skEmitterNode  =
  fmap (coerce :: CULong -> SKParticleRenderOrder) $ sendMsg skEmitterNode (mkSelector "particleRenderOrder") retCULong []

-- | The order in which particles will be rendered. Defaults to SKParticleRenderOrderOldestLast.
--
-- ObjC selector: @- setParticleRenderOrder:@
setParticleRenderOrder :: IsSKEmitterNode skEmitterNode => skEmitterNode -> SKParticleRenderOrder -> IO ()
setParticleRenderOrder skEmitterNode  value =
  sendMsg skEmitterNode (mkSelector "setParticleRenderOrder:") retVoid [argCULong (coerce value)]

-- | The random variance about the starting z-position for each particle. Defaults to 0.0.
--
-- ObjC selector: @- particleZPositionRange@
particleZPositionRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleZPositionRange skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "particleZPositionRange") retCDouble []

-- | The random variance about the starting z-position for each particle. Defaults to 0.0.
--
-- ObjC selector: @- setParticleZPositionRange:@
setParticleZPositionRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleZPositionRange skEmitterNode  value =
  sendMsg skEmitterNode (mkSelector "setParticleZPositionRange:") retVoid [argCDouble (fromIntegral value)]

-- | The rate at which to modify the z-position for each particle. Defaults to 0.0.
--
-- ObjC selector: @- particleZPositionSpeed@
particleZPositionSpeed :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleZPositionSpeed skEmitterNode  =
  sendMsg skEmitterNode (mkSelector "particleZPositionSpeed") retCDouble []

-- | The rate at which to modify the z-position for each particle. Defaults to 0.0.
--
-- ObjC selector: @- setParticleZPositionSpeed:@
setParticleZPositionSpeed :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleZPositionSpeed skEmitterNode  value =
  sendMsg skEmitterNode (mkSelector "setParticleZPositionSpeed:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @advanceSimulationTime:@
advanceSimulationTimeSelector :: Selector
advanceSimulationTimeSelector = mkSelector "advanceSimulationTime:"

-- | @Selector@ for @resetSimulation@
resetSimulationSelector :: Selector
resetSimulationSelector = mkSelector "resetSimulation"

-- | @Selector@ for @valueForAttributeNamed:@
valueForAttributeNamedSelector :: Selector
valueForAttributeNamedSelector = mkSelector "valueForAttributeNamed:"

-- | @Selector@ for @setValue:forAttributeNamed:@
setValue_forAttributeNamedSelector :: Selector
setValue_forAttributeNamedSelector = mkSelector "setValue:forAttributeNamed:"

-- | @Selector@ for @particleTexture@
particleTextureSelector :: Selector
particleTextureSelector = mkSelector "particleTexture"

-- | @Selector@ for @setParticleTexture:@
setParticleTextureSelector :: Selector
setParticleTextureSelector = mkSelector "setParticleTexture:"

-- | @Selector@ for @particleBlendMode@
particleBlendModeSelector :: Selector
particleBlendModeSelector = mkSelector "particleBlendMode"

-- | @Selector@ for @setParticleBlendMode:@
setParticleBlendModeSelector :: Selector
setParticleBlendModeSelector = mkSelector "setParticleBlendMode:"

-- | @Selector@ for @particleColor@
particleColorSelector :: Selector
particleColorSelector = mkSelector "particleColor"

-- | @Selector@ for @setParticleColor:@
setParticleColorSelector :: Selector
setParticleColorSelector = mkSelector "setParticleColor:"

-- | @Selector@ for @particleColorRedRange@
particleColorRedRangeSelector :: Selector
particleColorRedRangeSelector = mkSelector "particleColorRedRange"

-- | @Selector@ for @setParticleColorRedRange:@
setParticleColorRedRangeSelector :: Selector
setParticleColorRedRangeSelector = mkSelector "setParticleColorRedRange:"

-- | @Selector@ for @particleColorGreenRange@
particleColorGreenRangeSelector :: Selector
particleColorGreenRangeSelector = mkSelector "particleColorGreenRange"

-- | @Selector@ for @setParticleColorGreenRange:@
setParticleColorGreenRangeSelector :: Selector
setParticleColorGreenRangeSelector = mkSelector "setParticleColorGreenRange:"

-- | @Selector@ for @particleColorBlueRange@
particleColorBlueRangeSelector :: Selector
particleColorBlueRangeSelector = mkSelector "particleColorBlueRange"

-- | @Selector@ for @setParticleColorBlueRange:@
setParticleColorBlueRangeSelector :: Selector
setParticleColorBlueRangeSelector = mkSelector "setParticleColorBlueRange:"

-- | @Selector@ for @particleColorAlphaRange@
particleColorAlphaRangeSelector :: Selector
particleColorAlphaRangeSelector = mkSelector "particleColorAlphaRange"

-- | @Selector@ for @setParticleColorAlphaRange:@
setParticleColorAlphaRangeSelector :: Selector
setParticleColorAlphaRangeSelector = mkSelector "setParticleColorAlphaRange:"

-- | @Selector@ for @particleColorRedSpeed@
particleColorRedSpeedSelector :: Selector
particleColorRedSpeedSelector = mkSelector "particleColorRedSpeed"

-- | @Selector@ for @setParticleColorRedSpeed:@
setParticleColorRedSpeedSelector :: Selector
setParticleColorRedSpeedSelector = mkSelector "setParticleColorRedSpeed:"

-- | @Selector@ for @particleColorGreenSpeed@
particleColorGreenSpeedSelector :: Selector
particleColorGreenSpeedSelector = mkSelector "particleColorGreenSpeed"

-- | @Selector@ for @setParticleColorGreenSpeed:@
setParticleColorGreenSpeedSelector :: Selector
setParticleColorGreenSpeedSelector = mkSelector "setParticleColorGreenSpeed:"

-- | @Selector@ for @particleColorBlueSpeed@
particleColorBlueSpeedSelector :: Selector
particleColorBlueSpeedSelector = mkSelector "particleColorBlueSpeed"

-- | @Selector@ for @setParticleColorBlueSpeed:@
setParticleColorBlueSpeedSelector :: Selector
setParticleColorBlueSpeedSelector = mkSelector "setParticleColorBlueSpeed:"

-- | @Selector@ for @particleColorAlphaSpeed@
particleColorAlphaSpeedSelector :: Selector
particleColorAlphaSpeedSelector = mkSelector "particleColorAlphaSpeed"

-- | @Selector@ for @setParticleColorAlphaSpeed:@
setParticleColorAlphaSpeedSelector :: Selector
setParticleColorAlphaSpeedSelector = mkSelector "setParticleColorAlphaSpeed:"

-- | @Selector@ for @particleColorSequence@
particleColorSequenceSelector :: Selector
particleColorSequenceSelector = mkSelector "particleColorSequence"

-- | @Selector@ for @setParticleColorSequence:@
setParticleColorSequenceSelector :: Selector
setParticleColorSequenceSelector = mkSelector "setParticleColorSequence:"

-- | @Selector@ for @particleColorBlendFactor@
particleColorBlendFactorSelector :: Selector
particleColorBlendFactorSelector = mkSelector "particleColorBlendFactor"

-- | @Selector@ for @setParticleColorBlendFactor:@
setParticleColorBlendFactorSelector :: Selector
setParticleColorBlendFactorSelector = mkSelector "setParticleColorBlendFactor:"

-- | @Selector@ for @particleColorBlendFactorRange@
particleColorBlendFactorRangeSelector :: Selector
particleColorBlendFactorRangeSelector = mkSelector "particleColorBlendFactorRange"

-- | @Selector@ for @setParticleColorBlendFactorRange:@
setParticleColorBlendFactorRangeSelector :: Selector
setParticleColorBlendFactorRangeSelector = mkSelector "setParticleColorBlendFactorRange:"

-- | @Selector@ for @particleColorBlendFactorSpeed@
particleColorBlendFactorSpeedSelector :: Selector
particleColorBlendFactorSpeedSelector = mkSelector "particleColorBlendFactorSpeed"

-- | @Selector@ for @setParticleColorBlendFactorSpeed:@
setParticleColorBlendFactorSpeedSelector :: Selector
setParticleColorBlendFactorSpeedSelector = mkSelector "setParticleColorBlendFactorSpeed:"

-- | @Selector@ for @particleColorBlendFactorSequence@
particleColorBlendFactorSequenceSelector :: Selector
particleColorBlendFactorSequenceSelector = mkSelector "particleColorBlendFactorSequence"

-- | @Selector@ for @setParticleColorBlendFactorSequence:@
setParticleColorBlendFactorSequenceSelector :: Selector
setParticleColorBlendFactorSequenceSelector = mkSelector "setParticleColorBlendFactorSequence:"

-- | @Selector@ for @particleSpeed@
particleSpeedSelector :: Selector
particleSpeedSelector = mkSelector "particleSpeed"

-- | @Selector@ for @setParticleSpeed:@
setParticleSpeedSelector :: Selector
setParticleSpeedSelector = mkSelector "setParticleSpeed:"

-- | @Selector@ for @particleSpeedRange@
particleSpeedRangeSelector :: Selector
particleSpeedRangeSelector = mkSelector "particleSpeedRange"

-- | @Selector@ for @setParticleSpeedRange:@
setParticleSpeedRangeSelector :: Selector
setParticleSpeedRangeSelector = mkSelector "setParticleSpeedRange:"

-- | @Selector@ for @emissionAngle@
emissionAngleSelector :: Selector
emissionAngleSelector = mkSelector "emissionAngle"

-- | @Selector@ for @setEmissionAngle:@
setEmissionAngleSelector :: Selector
setEmissionAngleSelector = mkSelector "setEmissionAngle:"

-- | @Selector@ for @emissionAngleRange@
emissionAngleRangeSelector :: Selector
emissionAngleRangeSelector = mkSelector "emissionAngleRange"

-- | @Selector@ for @setEmissionAngleRange:@
setEmissionAngleRangeSelector :: Selector
setEmissionAngleRangeSelector = mkSelector "setEmissionAngleRange:"

-- | @Selector@ for @xAcceleration@
xAccelerationSelector :: Selector
xAccelerationSelector = mkSelector "xAcceleration"

-- | @Selector@ for @setXAcceleration:@
setXAccelerationSelector :: Selector
setXAccelerationSelector = mkSelector "setXAcceleration:"

-- | @Selector@ for @yAcceleration@
yAccelerationSelector :: Selector
yAccelerationSelector = mkSelector "yAcceleration"

-- | @Selector@ for @setYAcceleration:@
setYAccelerationSelector :: Selector
setYAccelerationSelector = mkSelector "setYAcceleration:"

-- | @Selector@ for @particleBirthRate@
particleBirthRateSelector :: Selector
particleBirthRateSelector = mkSelector "particleBirthRate"

-- | @Selector@ for @setParticleBirthRate:@
setParticleBirthRateSelector :: Selector
setParticleBirthRateSelector = mkSelector "setParticleBirthRate:"

-- | @Selector@ for @numParticlesToEmit@
numParticlesToEmitSelector :: Selector
numParticlesToEmitSelector = mkSelector "numParticlesToEmit"

-- | @Selector@ for @setNumParticlesToEmit:@
setNumParticlesToEmitSelector :: Selector
setNumParticlesToEmitSelector = mkSelector "setNumParticlesToEmit:"

-- | @Selector@ for @particleLifetime@
particleLifetimeSelector :: Selector
particleLifetimeSelector = mkSelector "particleLifetime"

-- | @Selector@ for @setParticleLifetime:@
setParticleLifetimeSelector :: Selector
setParticleLifetimeSelector = mkSelector "setParticleLifetime:"

-- | @Selector@ for @particleLifetimeRange@
particleLifetimeRangeSelector :: Selector
particleLifetimeRangeSelector = mkSelector "particleLifetimeRange"

-- | @Selector@ for @setParticleLifetimeRange:@
setParticleLifetimeRangeSelector :: Selector
setParticleLifetimeRangeSelector = mkSelector "setParticleLifetimeRange:"

-- | @Selector@ for @particleRotation@
particleRotationSelector :: Selector
particleRotationSelector = mkSelector "particleRotation"

-- | @Selector@ for @setParticleRotation:@
setParticleRotationSelector :: Selector
setParticleRotationSelector = mkSelector "setParticleRotation:"

-- | @Selector@ for @particleRotationRange@
particleRotationRangeSelector :: Selector
particleRotationRangeSelector = mkSelector "particleRotationRange"

-- | @Selector@ for @setParticleRotationRange:@
setParticleRotationRangeSelector :: Selector
setParticleRotationRangeSelector = mkSelector "setParticleRotationRange:"

-- | @Selector@ for @particleRotationSpeed@
particleRotationSpeedSelector :: Selector
particleRotationSpeedSelector = mkSelector "particleRotationSpeed"

-- | @Selector@ for @setParticleRotationSpeed:@
setParticleRotationSpeedSelector :: Selector
setParticleRotationSpeedSelector = mkSelector "setParticleRotationSpeed:"

-- | @Selector@ for @particleScale@
particleScaleSelector :: Selector
particleScaleSelector = mkSelector "particleScale"

-- | @Selector@ for @setParticleScale:@
setParticleScaleSelector :: Selector
setParticleScaleSelector = mkSelector "setParticleScale:"

-- | @Selector@ for @particleScaleRange@
particleScaleRangeSelector :: Selector
particleScaleRangeSelector = mkSelector "particleScaleRange"

-- | @Selector@ for @setParticleScaleRange:@
setParticleScaleRangeSelector :: Selector
setParticleScaleRangeSelector = mkSelector "setParticleScaleRange:"

-- | @Selector@ for @particleScaleSpeed@
particleScaleSpeedSelector :: Selector
particleScaleSpeedSelector = mkSelector "particleScaleSpeed"

-- | @Selector@ for @setParticleScaleSpeed:@
setParticleScaleSpeedSelector :: Selector
setParticleScaleSpeedSelector = mkSelector "setParticleScaleSpeed:"

-- | @Selector@ for @particleScaleSequence@
particleScaleSequenceSelector :: Selector
particleScaleSequenceSelector = mkSelector "particleScaleSequence"

-- | @Selector@ for @setParticleScaleSequence:@
setParticleScaleSequenceSelector :: Selector
setParticleScaleSequenceSelector = mkSelector "setParticleScaleSequence:"

-- | @Selector@ for @particleAlpha@
particleAlphaSelector :: Selector
particleAlphaSelector = mkSelector "particleAlpha"

-- | @Selector@ for @setParticleAlpha:@
setParticleAlphaSelector :: Selector
setParticleAlphaSelector = mkSelector "setParticleAlpha:"

-- | @Selector@ for @particleAlphaRange@
particleAlphaRangeSelector :: Selector
particleAlphaRangeSelector = mkSelector "particleAlphaRange"

-- | @Selector@ for @setParticleAlphaRange:@
setParticleAlphaRangeSelector :: Selector
setParticleAlphaRangeSelector = mkSelector "setParticleAlphaRange:"

-- | @Selector@ for @particleAlphaSpeed@
particleAlphaSpeedSelector :: Selector
particleAlphaSpeedSelector = mkSelector "particleAlphaSpeed"

-- | @Selector@ for @setParticleAlphaSpeed:@
setParticleAlphaSpeedSelector :: Selector
setParticleAlphaSpeedSelector = mkSelector "setParticleAlphaSpeed:"

-- | @Selector@ for @particleAlphaSequence@
particleAlphaSequenceSelector :: Selector
particleAlphaSequenceSelector = mkSelector "particleAlphaSequence"

-- | @Selector@ for @setParticleAlphaSequence:@
setParticleAlphaSequenceSelector :: Selector
setParticleAlphaSequenceSelector = mkSelector "setParticleAlphaSequence:"

-- | @Selector@ for @particleAction@
particleActionSelector :: Selector
particleActionSelector = mkSelector "particleAction"

-- | @Selector@ for @setParticleAction:@
setParticleActionSelector :: Selector
setParticleActionSelector = mkSelector "setParticleAction:"

-- | @Selector@ for @fieldBitMask@
fieldBitMaskSelector :: Selector
fieldBitMaskSelector = mkSelector "fieldBitMask"

-- | @Selector@ for @setFieldBitMask:@
setFieldBitMaskSelector :: Selector
setFieldBitMaskSelector = mkSelector "setFieldBitMask:"

-- | @Selector@ for @targetNode@
targetNodeSelector :: Selector
targetNodeSelector = mkSelector "targetNode"

-- | @Selector@ for @setTargetNode:@
setTargetNodeSelector :: Selector
setTargetNodeSelector = mkSelector "setTargetNode:"

-- | @Selector@ for @shader@
shaderSelector :: Selector
shaderSelector = mkSelector "shader"

-- | @Selector@ for @setShader:@
setShaderSelector :: Selector
setShaderSelector = mkSelector "setShader:"

-- | @Selector@ for @attributeValues@
attributeValuesSelector :: Selector
attributeValuesSelector = mkSelector "attributeValues"

-- | @Selector@ for @setAttributeValues:@
setAttributeValuesSelector :: Selector
setAttributeValuesSelector = mkSelector "setAttributeValues:"

-- | @Selector@ for @particleZPosition@
particleZPositionSelector :: Selector
particleZPositionSelector = mkSelector "particleZPosition"

-- | @Selector@ for @setParticleZPosition:@
setParticleZPositionSelector :: Selector
setParticleZPositionSelector = mkSelector "setParticleZPosition:"

-- | @Selector@ for @particleRenderOrder@
particleRenderOrderSelector :: Selector
particleRenderOrderSelector = mkSelector "particleRenderOrder"

-- | @Selector@ for @setParticleRenderOrder:@
setParticleRenderOrderSelector :: Selector
setParticleRenderOrderSelector = mkSelector "setParticleRenderOrder:"

-- | @Selector@ for @particleZPositionRange@
particleZPositionRangeSelector :: Selector
particleZPositionRangeSelector = mkSelector "particleZPositionRange"

-- | @Selector@ for @setParticleZPositionRange:@
setParticleZPositionRangeSelector :: Selector
setParticleZPositionRangeSelector = mkSelector "setParticleZPositionRange:"

-- | @Selector@ for @particleZPositionSpeed@
particleZPositionSpeedSelector :: Selector
particleZPositionSpeedSelector = mkSelector "particleZPositionSpeed"

-- | @Selector@ for @setParticleZPositionSpeed:@
setParticleZPositionSpeedSelector :: Selector
setParticleZPositionSpeedSelector = mkSelector "setParticleZPositionSpeed:"

