{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , attributeValuesSelector
  , emissionAngleRangeSelector
  , emissionAngleSelector
  , fieldBitMaskSelector
  , numParticlesToEmitSelector
  , particleActionSelector
  , particleAlphaRangeSelector
  , particleAlphaSelector
  , particleAlphaSequenceSelector
  , particleAlphaSpeedSelector
  , particleBirthRateSelector
  , particleBlendModeSelector
  , particleColorAlphaRangeSelector
  , particleColorAlphaSpeedSelector
  , particleColorBlendFactorRangeSelector
  , particleColorBlendFactorSelector
  , particleColorBlendFactorSequenceSelector
  , particleColorBlendFactorSpeedSelector
  , particleColorBlueRangeSelector
  , particleColorBlueSpeedSelector
  , particleColorGreenRangeSelector
  , particleColorGreenSpeedSelector
  , particleColorRedRangeSelector
  , particleColorRedSpeedSelector
  , particleColorSelector
  , particleColorSequenceSelector
  , particleLifetimeRangeSelector
  , particleLifetimeSelector
  , particleRenderOrderSelector
  , particleRotationRangeSelector
  , particleRotationSelector
  , particleRotationSpeedSelector
  , particleScaleRangeSelector
  , particleScaleSelector
  , particleScaleSequenceSelector
  , particleScaleSpeedSelector
  , particleSpeedRangeSelector
  , particleSpeedSelector
  , particleTextureSelector
  , particleZPositionRangeSelector
  , particleZPositionSelector
  , particleZPositionSpeedSelector
  , resetSimulationSelector
  , setAttributeValuesSelector
  , setEmissionAngleRangeSelector
  , setEmissionAngleSelector
  , setFieldBitMaskSelector
  , setNumParticlesToEmitSelector
  , setParticleActionSelector
  , setParticleAlphaRangeSelector
  , setParticleAlphaSelector
  , setParticleAlphaSequenceSelector
  , setParticleAlphaSpeedSelector
  , setParticleBirthRateSelector
  , setParticleBlendModeSelector
  , setParticleColorAlphaRangeSelector
  , setParticleColorAlphaSpeedSelector
  , setParticleColorBlendFactorRangeSelector
  , setParticleColorBlendFactorSelector
  , setParticleColorBlendFactorSequenceSelector
  , setParticleColorBlendFactorSpeedSelector
  , setParticleColorBlueRangeSelector
  , setParticleColorBlueSpeedSelector
  , setParticleColorGreenRangeSelector
  , setParticleColorGreenSpeedSelector
  , setParticleColorRedRangeSelector
  , setParticleColorRedSpeedSelector
  , setParticleColorSelector
  , setParticleColorSequenceSelector
  , setParticleLifetimeRangeSelector
  , setParticleLifetimeSelector
  , setParticleRenderOrderSelector
  , setParticleRotationRangeSelector
  , setParticleRotationSelector
  , setParticleRotationSpeedSelector
  , setParticleScaleRangeSelector
  , setParticleScaleSelector
  , setParticleScaleSequenceSelector
  , setParticleScaleSpeedSelector
  , setParticleSpeedRangeSelector
  , setParticleSpeedSelector
  , setParticleTextureSelector
  , setParticleZPositionRangeSelector
  , setParticleZPositionSelector
  , setParticleZPositionSpeedSelector
  , setShaderSelector
  , setTargetNodeSelector
  , setValue_forAttributeNamedSelector
  , setXAccelerationSelector
  , setYAccelerationSelector
  , shaderSelector
  , targetNodeSelector
  , valueForAttributeNamedSelector
  , xAccelerationSelector
  , yAccelerationSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
advanceSimulationTime skEmitterNode sec =
  sendMessage skEmitterNode advanceSimulationTimeSelector sec

-- | @- resetSimulation@
resetSimulation :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO ()
resetSimulation skEmitterNode =
  sendMessage skEmitterNode resetSimulationSelector

-- | @- valueForAttributeNamed:@
valueForAttributeNamed :: (IsSKEmitterNode skEmitterNode, IsNSString key) => skEmitterNode -> key -> IO (Id SKAttributeValue)
valueForAttributeNamed skEmitterNode key =
  sendMessage skEmitterNode valueForAttributeNamedSelector (toNSString key)

-- | @- setValue:forAttributeNamed:@
setValue_forAttributeNamed :: (IsSKEmitterNode skEmitterNode, IsSKAttributeValue value, IsNSString key) => skEmitterNode -> value -> key -> IO ()
setValue_forAttributeNamed skEmitterNode value key =
  sendMessage skEmitterNode setValue_forAttributeNamedSelector (toSKAttributeValue value) (toNSString key)

-- | The texture to be used for the particles.
--
-- ObjC selector: @- particleTexture@
particleTexture :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO (Id SKTexture)
particleTexture skEmitterNode =
  sendMessage skEmitterNode particleTextureSelector

-- | The texture to be used for the particles.
--
-- ObjC selector: @- setParticleTexture:@
setParticleTexture :: (IsSKEmitterNode skEmitterNode, IsSKTexture value) => skEmitterNode -> value -> IO ()
setParticleTexture skEmitterNode value =
  sendMessage skEmitterNode setParticleTextureSelector (toSKTexture value)

-- | The blend mode for each particle. Defaults to SKBlendModeAlpha.
--
-- ObjC selector: @- particleBlendMode@
particleBlendMode :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO SKBlendMode
particleBlendMode skEmitterNode =
  sendMessage skEmitterNode particleBlendModeSelector

-- | The blend mode for each particle. Defaults to SKBlendModeAlpha.
--
-- ObjC selector: @- setParticleBlendMode:@
setParticleBlendMode :: IsSKEmitterNode skEmitterNode => skEmitterNode -> SKBlendMode -> IO ()
setParticleBlendMode skEmitterNode value =
  sendMessage skEmitterNode setParticleBlendModeSelector value

-- | The starting color for each particle. Defaults to clear.
--
-- ObjC selector: @- particleColor@
particleColor :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO (Id NSColor)
particleColor skEmitterNode =
  sendMessage skEmitterNode particleColorSelector

-- | The starting color for each particle. Defaults to clear.
--
-- ObjC selector: @- setParticleColor:@
setParticleColor :: (IsSKEmitterNode skEmitterNode, IsNSColor value) => skEmitterNode -> value -> IO ()
setParticleColor skEmitterNode value =
  sendMessage skEmitterNode setParticleColorSelector (toNSColor value)

-- | The random variance about each color component for each particle. Defaults to 0.0.
--
-- ObjC selector: @- particleColorRedRange@
particleColorRedRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleColorRedRange skEmitterNode =
  sendMessage skEmitterNode particleColorRedRangeSelector

-- | The random variance about each color component for each particle. Defaults to 0.0.
--
-- ObjC selector: @- setParticleColorRedRange:@
setParticleColorRedRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleColorRedRange skEmitterNode value =
  sendMessage skEmitterNode setParticleColorRedRangeSelector value

-- | @- particleColorGreenRange@
particleColorGreenRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleColorGreenRange skEmitterNode =
  sendMessage skEmitterNode particleColorGreenRangeSelector

-- | @- setParticleColorGreenRange:@
setParticleColorGreenRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleColorGreenRange skEmitterNode value =
  sendMessage skEmitterNode setParticleColorGreenRangeSelector value

-- | @- particleColorBlueRange@
particleColorBlueRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleColorBlueRange skEmitterNode =
  sendMessage skEmitterNode particleColorBlueRangeSelector

-- | @- setParticleColorBlueRange:@
setParticleColorBlueRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleColorBlueRange skEmitterNode value =
  sendMessage skEmitterNode setParticleColorBlueRangeSelector value

-- | @- particleColorAlphaRange@
particleColorAlphaRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleColorAlphaRange skEmitterNode =
  sendMessage skEmitterNode particleColorAlphaRangeSelector

-- | @- setParticleColorAlphaRange:@
setParticleColorAlphaRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleColorAlphaRange skEmitterNode value =
  sendMessage skEmitterNode setParticleColorAlphaRangeSelector value

-- | The rate at which to modify each color component for each particle (per second).
--
-- ObjC selector: @- particleColorRedSpeed@
particleColorRedSpeed :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleColorRedSpeed skEmitterNode =
  sendMessage skEmitterNode particleColorRedSpeedSelector

-- | The rate at which to modify each color component for each particle (per second).
--
-- ObjC selector: @- setParticleColorRedSpeed:@
setParticleColorRedSpeed :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleColorRedSpeed skEmitterNode value =
  sendMessage skEmitterNode setParticleColorRedSpeedSelector value

-- | @- particleColorGreenSpeed@
particleColorGreenSpeed :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleColorGreenSpeed skEmitterNode =
  sendMessage skEmitterNode particleColorGreenSpeedSelector

-- | @- setParticleColorGreenSpeed:@
setParticleColorGreenSpeed :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleColorGreenSpeed skEmitterNode value =
  sendMessage skEmitterNode setParticleColorGreenSpeedSelector value

-- | @- particleColorBlueSpeed@
particleColorBlueSpeed :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleColorBlueSpeed skEmitterNode =
  sendMessage skEmitterNode particleColorBlueSpeedSelector

-- | @- setParticleColorBlueSpeed:@
setParticleColorBlueSpeed :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleColorBlueSpeed skEmitterNode value =
  sendMessage skEmitterNode setParticleColorBlueSpeedSelector value

-- | @- particleColorAlphaSpeed@
particleColorAlphaSpeed :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleColorAlphaSpeed skEmitterNode =
  sendMessage skEmitterNode particleColorAlphaSpeedSelector

-- | @- setParticleColorAlphaSpeed:@
setParticleColorAlphaSpeed :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleColorAlphaSpeed skEmitterNode value =
  sendMessage skEmitterNode setParticleColorAlphaSpeedSelector value

-- | @- particleColorSequence@
particleColorSequence :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO (Id SKKeyframeSequence)
particleColorSequence skEmitterNode =
  sendMessage skEmitterNode particleColorSequenceSelector

-- | @- setParticleColorSequence:@
setParticleColorSequence :: (IsSKEmitterNode skEmitterNode, IsSKKeyframeSequence value) => skEmitterNode -> value -> IO ()
setParticleColorSequence skEmitterNode value =
  sendMessage skEmitterNode setParticleColorSequenceSelector (toSKKeyframeSequence value)

-- | The starting color blend for each particle. Behaves the same as SKSpriteNode. Defaults to 0.0.
--
-- ObjC selector: @- particleColorBlendFactor@
particleColorBlendFactor :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleColorBlendFactor skEmitterNode =
  sendMessage skEmitterNode particleColorBlendFactorSelector

-- | The starting color blend for each particle. Behaves the same as SKSpriteNode. Defaults to 0.0.
--
-- ObjC selector: @- setParticleColorBlendFactor:@
setParticleColorBlendFactor :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleColorBlendFactor skEmitterNode value =
  sendMessage skEmitterNode setParticleColorBlendFactorSelector value

-- | The random variance about the starting color blend for each particle. Defaults to 0.0.
--
-- ObjC selector: @- particleColorBlendFactorRange@
particleColorBlendFactorRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleColorBlendFactorRange skEmitterNode =
  sendMessage skEmitterNode particleColorBlendFactorRangeSelector

-- | The random variance about the starting color blend for each particle. Defaults to 0.0.
--
-- ObjC selector: @- setParticleColorBlendFactorRange:@
setParticleColorBlendFactorRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleColorBlendFactorRange skEmitterNode value =
  sendMessage skEmitterNode setParticleColorBlendFactorRangeSelector value

-- | The rate at which to modify the color blend for each particle. Defaults to 0.0.
--
-- ObjC selector: @- particleColorBlendFactorSpeed@
particleColorBlendFactorSpeed :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleColorBlendFactorSpeed skEmitterNode =
  sendMessage skEmitterNode particleColorBlendFactorSpeedSelector

-- | The rate at which to modify the color blend for each particle. Defaults to 0.0.
--
-- ObjC selector: @- setParticleColorBlendFactorSpeed:@
setParticleColorBlendFactorSpeed :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleColorBlendFactorSpeed skEmitterNode value =
  sendMessage skEmitterNode setParticleColorBlendFactorSpeedSelector value

-- | @- particleColorBlendFactorSequence@
particleColorBlendFactorSequence :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO (Id SKKeyframeSequence)
particleColorBlendFactorSequence skEmitterNode =
  sendMessage skEmitterNode particleColorBlendFactorSequenceSelector

-- | @- setParticleColorBlendFactorSequence:@
setParticleColorBlendFactorSequence :: (IsSKEmitterNode skEmitterNode, IsSKKeyframeSequence value) => skEmitterNode -> value -> IO ()
setParticleColorBlendFactorSequence skEmitterNode value =
  sendMessage skEmitterNode setParticleColorBlendFactorSequenceSelector (toSKKeyframeSequence value)

-- | The starting speed for each particle along its emission vector. Defaults to 0.0.
--
-- ObjC selector: @- particleSpeed@
particleSpeed :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleSpeed skEmitterNode =
  sendMessage skEmitterNode particleSpeedSelector

-- | The starting speed for each particle along its emission vector. Defaults to 0.0.
--
-- ObjC selector: @- setParticleSpeed:@
setParticleSpeed :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleSpeed skEmitterNode value =
  sendMessage skEmitterNode setParticleSpeedSelector value

-- | The random variance about the starting speed for each particle along its emission vector. Defaults to 0.0.
--
-- ObjC selector: @- particleSpeedRange@
particleSpeedRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleSpeedRange skEmitterNode =
  sendMessage skEmitterNode particleSpeedRangeSelector

-- | The random variance about the starting speed for each particle along its emission vector. Defaults to 0.0.
--
-- ObjC selector: @- setParticleSpeedRange:@
setParticleSpeedRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleSpeedRange skEmitterNode value =
  sendMessage skEmitterNode setParticleSpeedRangeSelector value

-- | The angle at which to emit each new particle, in radians. Defaults to 0.0.
--
-- ObjC selector: @- emissionAngle@
emissionAngle :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
emissionAngle skEmitterNode =
  sendMessage skEmitterNode emissionAngleSelector

-- | The angle at which to emit each new particle, in radians. Defaults to 0.0.
--
-- ObjC selector: @- setEmissionAngle:@
setEmissionAngle :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setEmissionAngle skEmitterNode value =
  sendMessage skEmitterNode setEmissionAngleSelector value

-- | The random variance about the angle at which to emit each new particle, in radians. Defaults to 0.0.
--
-- ObjC selector: @- emissionAngleRange@
emissionAngleRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
emissionAngleRange skEmitterNode =
  sendMessage skEmitterNode emissionAngleRangeSelector

-- | The random variance about the angle at which to emit each new particle, in radians. Defaults to 0.0.
--
-- ObjC selector: @- setEmissionAngleRange:@
setEmissionAngleRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setEmissionAngleRange skEmitterNode value =
  sendMessage skEmitterNode setEmissionAngleRangeSelector value

-- | The acceleration to apply to each particles velocity. Useful for simulating effects such as wind or gravity. Defaults to 0.0.
--
-- ObjC selector: @- xAcceleration@
xAcceleration :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
xAcceleration skEmitterNode =
  sendMessage skEmitterNode xAccelerationSelector

-- | The acceleration to apply to each particles velocity. Useful for simulating effects such as wind or gravity. Defaults to 0.0.
--
-- ObjC selector: @- setXAcceleration:@
setXAcceleration :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setXAcceleration skEmitterNode value =
  sendMessage skEmitterNode setXAccelerationSelector value

-- | @- yAcceleration@
yAcceleration :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
yAcceleration skEmitterNode =
  sendMessage skEmitterNode yAccelerationSelector

-- | @- setYAcceleration:@
setYAcceleration :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setYAcceleration skEmitterNode value =
  sendMessage skEmitterNode setYAccelerationSelector value

-- | The rate at which new particles are generated, in particles per second. Defaults to 0.0.
--
-- ObjC selector: @- particleBirthRate@
particleBirthRate :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleBirthRate skEmitterNode =
  sendMessage skEmitterNode particleBirthRateSelector

-- | The rate at which new particles are generated, in particles per second. Defaults to 0.0.
--
-- ObjC selector: @- setParticleBirthRate:@
setParticleBirthRate :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleBirthRate skEmitterNode value =
  sendMessage skEmitterNode setParticleBirthRateSelector value

-- | The number of particles that will be emitted. If set to 0, there is no limit. Defaults to 0.
--
-- ObjC selector: @- numParticlesToEmit@
numParticlesToEmit :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CULong
numParticlesToEmit skEmitterNode =
  sendMessage skEmitterNode numParticlesToEmitSelector

-- | The number of particles that will be emitted. If set to 0, there is no limit. Defaults to 0.
--
-- ObjC selector: @- setNumParticlesToEmit:@
setNumParticlesToEmit :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CULong -> IO ()
setNumParticlesToEmit skEmitterNode value =
  sendMessage skEmitterNode setNumParticlesToEmitSelector value

-- | The lifetime of each particle, in seconds. Defaults to 0.0.
--
-- ObjC selector: @- particleLifetime@
particleLifetime :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleLifetime skEmitterNode =
  sendMessage skEmitterNode particleLifetimeSelector

-- | The lifetime of each particle, in seconds. Defaults to 0.0.
--
-- ObjC selector: @- setParticleLifetime:@
setParticleLifetime :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleLifetime skEmitterNode value =
  sendMessage skEmitterNode setParticleLifetimeSelector value

-- | The random variance about the lifetime of each particle, in seconds. Defaults to 0.0.
--
-- ObjC selector: @- particleLifetimeRange@
particleLifetimeRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleLifetimeRange skEmitterNode =
  sendMessage skEmitterNode particleLifetimeRangeSelector

-- | The random variance about the lifetime of each particle, in seconds. Defaults to 0.0.
--
-- ObjC selector: @- setParticleLifetimeRange:@
setParticleLifetimeRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleLifetimeRange skEmitterNode value =
  sendMessage skEmitterNode setParticleLifetimeRangeSelector value

-- | The starting z-rotation for each particle. Defaults to 0.0.
--
-- ObjC selector: @- particleRotation@
particleRotation :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleRotation skEmitterNode =
  sendMessage skEmitterNode particleRotationSelector

-- | The starting z-rotation for each particle. Defaults to 0.0.
--
-- ObjC selector: @- setParticleRotation:@
setParticleRotation :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleRotation skEmitterNode value =
  sendMessage skEmitterNode setParticleRotationSelector value

-- | The random variance about the starting z-rotation for each particle. Defaults to 0.0.
--
-- ObjC selector: @- particleRotationRange@
particleRotationRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleRotationRange skEmitterNode =
  sendMessage skEmitterNode particleRotationRangeSelector

-- | The random variance about the starting z-rotation for each particle. Defaults to 0.0.
--
-- ObjC selector: @- setParticleRotationRange:@
setParticleRotationRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleRotationRange skEmitterNode value =
  sendMessage skEmitterNode setParticleRotationRangeSelector value

-- | The rate at which to modify the z-rotation for each particle. Defaults to 0.0.
--
-- ObjC selector: @- particleRotationSpeed@
particleRotationSpeed :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleRotationSpeed skEmitterNode =
  sendMessage skEmitterNode particleRotationSpeedSelector

-- | The rate at which to modify the z-rotation for each particle. Defaults to 0.0.
--
-- ObjC selector: @- setParticleRotationSpeed:@
setParticleRotationSpeed :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleRotationSpeed skEmitterNode value =
  sendMessage skEmitterNode setParticleRotationSpeedSelector value

-- | The starting scale for each particle. Defaults to 1.0.
--
-- ObjC selector: @- particleScale@
particleScale :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleScale skEmitterNode =
  sendMessage skEmitterNode particleScaleSelector

-- | The starting scale for each particle. Defaults to 1.0.
--
-- ObjC selector: @- setParticleScale:@
setParticleScale :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleScale skEmitterNode value =
  sendMessage skEmitterNode setParticleScaleSelector value

-- | The random variance about the starting scale for each particle. Defaults to 0.0.
--
-- ObjC selector: @- particleScaleRange@
particleScaleRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleScaleRange skEmitterNode =
  sendMessage skEmitterNode particleScaleRangeSelector

-- | The random variance about the starting scale for each particle. Defaults to 0.0.
--
-- ObjC selector: @- setParticleScaleRange:@
setParticleScaleRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleScaleRange skEmitterNode value =
  sendMessage skEmitterNode setParticleScaleRangeSelector value

-- | The rate at which to modify the scale for each particle. Defaults to 0.0.
--
-- ObjC selector: @- particleScaleSpeed@
particleScaleSpeed :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleScaleSpeed skEmitterNode =
  sendMessage skEmitterNode particleScaleSpeedSelector

-- | The rate at which to modify the scale for each particle. Defaults to 0.0.
--
-- ObjC selector: @- setParticleScaleSpeed:@
setParticleScaleSpeed :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleScaleSpeed skEmitterNode value =
  sendMessage skEmitterNode setParticleScaleSpeedSelector value

-- | @- particleScaleSequence@
particleScaleSequence :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO (Id SKKeyframeSequence)
particleScaleSequence skEmitterNode =
  sendMessage skEmitterNode particleScaleSequenceSelector

-- | @- setParticleScaleSequence:@
setParticleScaleSequence :: (IsSKEmitterNode skEmitterNode, IsSKKeyframeSequence value) => skEmitterNode -> value -> IO ()
setParticleScaleSequence skEmitterNode value =
  sendMessage skEmitterNode setParticleScaleSequenceSelector (toSKKeyframeSequence value)

-- | The starting alpha for each particle. Defaults to 1.0.
--
-- ObjC selector: @- particleAlpha@
particleAlpha :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleAlpha skEmitterNode =
  sendMessage skEmitterNode particleAlphaSelector

-- | The starting alpha for each particle. Defaults to 1.0.
--
-- ObjC selector: @- setParticleAlpha:@
setParticleAlpha :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleAlpha skEmitterNode value =
  sendMessage skEmitterNode setParticleAlphaSelector value

-- | The random variance about the starting alpha for each particle. Defaults to 1.0.
--
-- ObjC selector: @- particleAlphaRange@
particleAlphaRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleAlphaRange skEmitterNode =
  sendMessage skEmitterNode particleAlphaRangeSelector

-- | The random variance about the starting alpha for each particle. Defaults to 1.0.
--
-- ObjC selector: @- setParticleAlphaRange:@
setParticleAlphaRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleAlphaRange skEmitterNode value =
  sendMessage skEmitterNode setParticleAlphaRangeSelector value

-- | The rate at which to modify the alpha for each particle. Defaults to 1.0.
--
-- ObjC selector: @- particleAlphaSpeed@
particleAlphaSpeed :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleAlphaSpeed skEmitterNode =
  sendMessage skEmitterNode particleAlphaSpeedSelector

-- | The rate at which to modify the alpha for each particle. Defaults to 1.0.
--
-- ObjC selector: @- setParticleAlphaSpeed:@
setParticleAlphaSpeed :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleAlphaSpeed skEmitterNode value =
  sendMessage skEmitterNode setParticleAlphaSpeedSelector value

-- | @- particleAlphaSequence@
particleAlphaSequence :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO (Id SKKeyframeSequence)
particleAlphaSequence skEmitterNode =
  sendMessage skEmitterNode particleAlphaSequenceSelector

-- | @- setParticleAlphaSequence:@
setParticleAlphaSequence :: (IsSKEmitterNode skEmitterNode, IsSKKeyframeSequence value) => skEmitterNode -> value -> IO ()
setParticleAlphaSequence skEmitterNode value =
  sendMessage skEmitterNode setParticleAlphaSequenceSelector (toSKKeyframeSequence value)

-- | Specifies an action executed by new particles.
--
-- ObjC selector: @- particleAction@
particleAction :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO (Id SKAction)
particleAction skEmitterNode =
  sendMessage skEmitterNode particleActionSelector

-- | Specifies an action executed by new particles.
--
-- ObjC selector: @- setParticleAction:@
setParticleAction :: (IsSKEmitterNode skEmitterNode, IsSKAction value) => skEmitterNode -> value -> IO ()
setParticleAction skEmitterNode value =
  sendMessage skEmitterNode setParticleActionSelector (toSKAction value)

-- | Defines what logical 'categories' of fields this particles emitted respond to. Defaults to all bits set (all categories). Can be forced off via affectedByGravity.
--
-- ObjC selector: @- fieldBitMask@
fieldBitMask :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CUInt
fieldBitMask skEmitterNode =
  sendMessage skEmitterNode fieldBitMaskSelector

-- | Defines what logical 'categories' of fields this particles emitted respond to. Defaults to all bits set (all categories). Can be forced off via affectedByGravity.
--
-- ObjC selector: @- setFieldBitMask:@
setFieldBitMask :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CUInt -> IO ()
setFieldBitMask skEmitterNode value =
  sendMessage skEmitterNode setFieldBitMaskSelector value

-- | Normally the particles are rendered as if they were a child of the SKEmitterNode, they can also be rendered as if they were a child of any other node in the scene by setting the targetNode property. Defaults to nil (standard behavior).
--
-- ObjC selector: @- targetNode@
targetNode :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO (Id SKNode)
targetNode skEmitterNode =
  sendMessage skEmitterNode targetNodeSelector

-- | Normally the particles are rendered as if they were a child of the SKEmitterNode, they can also be rendered as if they were a child of any other node in the scene by setting the targetNode property. Defaults to nil (standard behavior).
--
-- ObjC selector: @- setTargetNode:@
setTargetNode :: (IsSKEmitterNode skEmitterNode, IsSKNode value) => skEmitterNode -> value -> IO ()
setTargetNode skEmitterNode value =
  sendMessage skEmitterNode setTargetNodeSelector (toSKNode value)

-- | @- shader@
shader :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO (Id SKShader)
shader skEmitterNode =
  sendMessage skEmitterNode shaderSelector

-- | @- setShader:@
setShader :: (IsSKEmitterNode skEmitterNode, IsSKShader value) => skEmitterNode -> value -> IO ()
setShader skEmitterNode value =
  sendMessage skEmitterNode setShaderSelector (toSKShader value)

-- | Optional dictionary of SKAttributeValues Attributes can be used with custom SKShaders.
--
-- ObjC selector: @- attributeValues@
attributeValues :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO (Id NSDictionary)
attributeValues skEmitterNode =
  sendMessage skEmitterNode attributeValuesSelector

-- | Optional dictionary of SKAttributeValues Attributes can be used with custom SKShaders.
--
-- ObjC selector: @- setAttributeValues:@
setAttributeValues :: (IsSKEmitterNode skEmitterNode, IsNSDictionary value) => skEmitterNode -> value -> IO ()
setAttributeValues skEmitterNode value =
  sendMessage skEmitterNode setAttributeValuesSelector (toNSDictionary value)

-- | The starting z-position for each particle. Defaults to 0.0.
--
-- ObjC selector: @- particleZPosition@
particleZPosition :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleZPosition skEmitterNode =
  sendMessage skEmitterNode particleZPositionSelector

-- | The starting z-position for each particle. Defaults to 0.0.
--
-- ObjC selector: @- setParticleZPosition:@
setParticleZPosition :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleZPosition skEmitterNode value =
  sendMessage skEmitterNode setParticleZPositionSelector value

-- | The order in which particles will be rendered. Defaults to SKParticleRenderOrderOldestLast.
--
-- ObjC selector: @- particleRenderOrder@
particleRenderOrder :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO SKParticleRenderOrder
particleRenderOrder skEmitterNode =
  sendMessage skEmitterNode particleRenderOrderSelector

-- | The order in which particles will be rendered. Defaults to SKParticleRenderOrderOldestLast.
--
-- ObjC selector: @- setParticleRenderOrder:@
setParticleRenderOrder :: IsSKEmitterNode skEmitterNode => skEmitterNode -> SKParticleRenderOrder -> IO ()
setParticleRenderOrder skEmitterNode value =
  sendMessage skEmitterNode setParticleRenderOrderSelector value

-- | The random variance about the starting z-position for each particle. Defaults to 0.0.
--
-- ObjC selector: @- particleZPositionRange@
particleZPositionRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleZPositionRange skEmitterNode =
  sendMessage skEmitterNode particleZPositionRangeSelector

-- | The random variance about the starting z-position for each particle. Defaults to 0.0.
--
-- ObjC selector: @- setParticleZPositionRange:@
setParticleZPositionRange :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleZPositionRange skEmitterNode value =
  sendMessage skEmitterNode setParticleZPositionRangeSelector value

-- | The rate at which to modify the z-position for each particle. Defaults to 0.0.
--
-- ObjC selector: @- particleZPositionSpeed@
particleZPositionSpeed :: IsSKEmitterNode skEmitterNode => skEmitterNode -> IO CDouble
particleZPositionSpeed skEmitterNode =
  sendMessage skEmitterNode particleZPositionSpeedSelector

-- | The rate at which to modify the z-position for each particle. Defaults to 0.0.
--
-- ObjC selector: @- setParticleZPositionSpeed:@
setParticleZPositionSpeed :: IsSKEmitterNode skEmitterNode => skEmitterNode -> CDouble -> IO ()
setParticleZPositionSpeed skEmitterNode value =
  sendMessage skEmitterNode setParticleZPositionSpeedSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @advanceSimulationTime:@
advanceSimulationTimeSelector :: Selector '[CDouble] ()
advanceSimulationTimeSelector = mkSelector "advanceSimulationTime:"

-- | @Selector@ for @resetSimulation@
resetSimulationSelector :: Selector '[] ()
resetSimulationSelector = mkSelector "resetSimulation"

-- | @Selector@ for @valueForAttributeNamed:@
valueForAttributeNamedSelector :: Selector '[Id NSString] (Id SKAttributeValue)
valueForAttributeNamedSelector = mkSelector "valueForAttributeNamed:"

-- | @Selector@ for @setValue:forAttributeNamed:@
setValue_forAttributeNamedSelector :: Selector '[Id SKAttributeValue, Id NSString] ()
setValue_forAttributeNamedSelector = mkSelector "setValue:forAttributeNamed:"

-- | @Selector@ for @particleTexture@
particleTextureSelector :: Selector '[] (Id SKTexture)
particleTextureSelector = mkSelector "particleTexture"

-- | @Selector@ for @setParticleTexture:@
setParticleTextureSelector :: Selector '[Id SKTexture] ()
setParticleTextureSelector = mkSelector "setParticleTexture:"

-- | @Selector@ for @particleBlendMode@
particleBlendModeSelector :: Selector '[] SKBlendMode
particleBlendModeSelector = mkSelector "particleBlendMode"

-- | @Selector@ for @setParticleBlendMode:@
setParticleBlendModeSelector :: Selector '[SKBlendMode] ()
setParticleBlendModeSelector = mkSelector "setParticleBlendMode:"

-- | @Selector@ for @particleColor@
particleColorSelector :: Selector '[] (Id NSColor)
particleColorSelector = mkSelector "particleColor"

-- | @Selector@ for @setParticleColor:@
setParticleColorSelector :: Selector '[Id NSColor] ()
setParticleColorSelector = mkSelector "setParticleColor:"

-- | @Selector@ for @particleColorRedRange@
particleColorRedRangeSelector :: Selector '[] CDouble
particleColorRedRangeSelector = mkSelector "particleColorRedRange"

-- | @Selector@ for @setParticleColorRedRange:@
setParticleColorRedRangeSelector :: Selector '[CDouble] ()
setParticleColorRedRangeSelector = mkSelector "setParticleColorRedRange:"

-- | @Selector@ for @particleColorGreenRange@
particleColorGreenRangeSelector :: Selector '[] CDouble
particleColorGreenRangeSelector = mkSelector "particleColorGreenRange"

-- | @Selector@ for @setParticleColorGreenRange:@
setParticleColorGreenRangeSelector :: Selector '[CDouble] ()
setParticleColorGreenRangeSelector = mkSelector "setParticleColorGreenRange:"

-- | @Selector@ for @particleColorBlueRange@
particleColorBlueRangeSelector :: Selector '[] CDouble
particleColorBlueRangeSelector = mkSelector "particleColorBlueRange"

-- | @Selector@ for @setParticleColorBlueRange:@
setParticleColorBlueRangeSelector :: Selector '[CDouble] ()
setParticleColorBlueRangeSelector = mkSelector "setParticleColorBlueRange:"

-- | @Selector@ for @particleColorAlphaRange@
particleColorAlphaRangeSelector :: Selector '[] CDouble
particleColorAlphaRangeSelector = mkSelector "particleColorAlphaRange"

-- | @Selector@ for @setParticleColorAlphaRange:@
setParticleColorAlphaRangeSelector :: Selector '[CDouble] ()
setParticleColorAlphaRangeSelector = mkSelector "setParticleColorAlphaRange:"

-- | @Selector@ for @particleColorRedSpeed@
particleColorRedSpeedSelector :: Selector '[] CDouble
particleColorRedSpeedSelector = mkSelector "particleColorRedSpeed"

-- | @Selector@ for @setParticleColorRedSpeed:@
setParticleColorRedSpeedSelector :: Selector '[CDouble] ()
setParticleColorRedSpeedSelector = mkSelector "setParticleColorRedSpeed:"

-- | @Selector@ for @particleColorGreenSpeed@
particleColorGreenSpeedSelector :: Selector '[] CDouble
particleColorGreenSpeedSelector = mkSelector "particleColorGreenSpeed"

-- | @Selector@ for @setParticleColorGreenSpeed:@
setParticleColorGreenSpeedSelector :: Selector '[CDouble] ()
setParticleColorGreenSpeedSelector = mkSelector "setParticleColorGreenSpeed:"

-- | @Selector@ for @particleColorBlueSpeed@
particleColorBlueSpeedSelector :: Selector '[] CDouble
particleColorBlueSpeedSelector = mkSelector "particleColorBlueSpeed"

-- | @Selector@ for @setParticleColorBlueSpeed:@
setParticleColorBlueSpeedSelector :: Selector '[CDouble] ()
setParticleColorBlueSpeedSelector = mkSelector "setParticleColorBlueSpeed:"

-- | @Selector@ for @particleColorAlphaSpeed@
particleColorAlphaSpeedSelector :: Selector '[] CDouble
particleColorAlphaSpeedSelector = mkSelector "particleColorAlphaSpeed"

-- | @Selector@ for @setParticleColorAlphaSpeed:@
setParticleColorAlphaSpeedSelector :: Selector '[CDouble] ()
setParticleColorAlphaSpeedSelector = mkSelector "setParticleColorAlphaSpeed:"

-- | @Selector@ for @particleColorSequence@
particleColorSequenceSelector :: Selector '[] (Id SKKeyframeSequence)
particleColorSequenceSelector = mkSelector "particleColorSequence"

-- | @Selector@ for @setParticleColorSequence:@
setParticleColorSequenceSelector :: Selector '[Id SKKeyframeSequence] ()
setParticleColorSequenceSelector = mkSelector "setParticleColorSequence:"

-- | @Selector@ for @particleColorBlendFactor@
particleColorBlendFactorSelector :: Selector '[] CDouble
particleColorBlendFactorSelector = mkSelector "particleColorBlendFactor"

-- | @Selector@ for @setParticleColorBlendFactor:@
setParticleColorBlendFactorSelector :: Selector '[CDouble] ()
setParticleColorBlendFactorSelector = mkSelector "setParticleColorBlendFactor:"

-- | @Selector@ for @particleColorBlendFactorRange@
particleColorBlendFactorRangeSelector :: Selector '[] CDouble
particleColorBlendFactorRangeSelector = mkSelector "particleColorBlendFactorRange"

-- | @Selector@ for @setParticleColorBlendFactorRange:@
setParticleColorBlendFactorRangeSelector :: Selector '[CDouble] ()
setParticleColorBlendFactorRangeSelector = mkSelector "setParticleColorBlendFactorRange:"

-- | @Selector@ for @particleColorBlendFactorSpeed@
particleColorBlendFactorSpeedSelector :: Selector '[] CDouble
particleColorBlendFactorSpeedSelector = mkSelector "particleColorBlendFactorSpeed"

-- | @Selector@ for @setParticleColorBlendFactorSpeed:@
setParticleColorBlendFactorSpeedSelector :: Selector '[CDouble] ()
setParticleColorBlendFactorSpeedSelector = mkSelector "setParticleColorBlendFactorSpeed:"

-- | @Selector@ for @particleColorBlendFactorSequence@
particleColorBlendFactorSequenceSelector :: Selector '[] (Id SKKeyframeSequence)
particleColorBlendFactorSequenceSelector = mkSelector "particleColorBlendFactorSequence"

-- | @Selector@ for @setParticleColorBlendFactorSequence:@
setParticleColorBlendFactorSequenceSelector :: Selector '[Id SKKeyframeSequence] ()
setParticleColorBlendFactorSequenceSelector = mkSelector "setParticleColorBlendFactorSequence:"

-- | @Selector@ for @particleSpeed@
particleSpeedSelector :: Selector '[] CDouble
particleSpeedSelector = mkSelector "particleSpeed"

-- | @Selector@ for @setParticleSpeed:@
setParticleSpeedSelector :: Selector '[CDouble] ()
setParticleSpeedSelector = mkSelector "setParticleSpeed:"

-- | @Selector@ for @particleSpeedRange@
particleSpeedRangeSelector :: Selector '[] CDouble
particleSpeedRangeSelector = mkSelector "particleSpeedRange"

-- | @Selector@ for @setParticleSpeedRange:@
setParticleSpeedRangeSelector :: Selector '[CDouble] ()
setParticleSpeedRangeSelector = mkSelector "setParticleSpeedRange:"

-- | @Selector@ for @emissionAngle@
emissionAngleSelector :: Selector '[] CDouble
emissionAngleSelector = mkSelector "emissionAngle"

-- | @Selector@ for @setEmissionAngle:@
setEmissionAngleSelector :: Selector '[CDouble] ()
setEmissionAngleSelector = mkSelector "setEmissionAngle:"

-- | @Selector@ for @emissionAngleRange@
emissionAngleRangeSelector :: Selector '[] CDouble
emissionAngleRangeSelector = mkSelector "emissionAngleRange"

-- | @Selector@ for @setEmissionAngleRange:@
setEmissionAngleRangeSelector :: Selector '[CDouble] ()
setEmissionAngleRangeSelector = mkSelector "setEmissionAngleRange:"

-- | @Selector@ for @xAcceleration@
xAccelerationSelector :: Selector '[] CDouble
xAccelerationSelector = mkSelector "xAcceleration"

-- | @Selector@ for @setXAcceleration:@
setXAccelerationSelector :: Selector '[CDouble] ()
setXAccelerationSelector = mkSelector "setXAcceleration:"

-- | @Selector@ for @yAcceleration@
yAccelerationSelector :: Selector '[] CDouble
yAccelerationSelector = mkSelector "yAcceleration"

-- | @Selector@ for @setYAcceleration:@
setYAccelerationSelector :: Selector '[CDouble] ()
setYAccelerationSelector = mkSelector "setYAcceleration:"

-- | @Selector@ for @particleBirthRate@
particleBirthRateSelector :: Selector '[] CDouble
particleBirthRateSelector = mkSelector "particleBirthRate"

-- | @Selector@ for @setParticleBirthRate:@
setParticleBirthRateSelector :: Selector '[CDouble] ()
setParticleBirthRateSelector = mkSelector "setParticleBirthRate:"

-- | @Selector@ for @numParticlesToEmit@
numParticlesToEmitSelector :: Selector '[] CULong
numParticlesToEmitSelector = mkSelector "numParticlesToEmit"

-- | @Selector@ for @setNumParticlesToEmit:@
setNumParticlesToEmitSelector :: Selector '[CULong] ()
setNumParticlesToEmitSelector = mkSelector "setNumParticlesToEmit:"

-- | @Selector@ for @particleLifetime@
particleLifetimeSelector :: Selector '[] CDouble
particleLifetimeSelector = mkSelector "particleLifetime"

-- | @Selector@ for @setParticleLifetime:@
setParticleLifetimeSelector :: Selector '[CDouble] ()
setParticleLifetimeSelector = mkSelector "setParticleLifetime:"

-- | @Selector@ for @particleLifetimeRange@
particleLifetimeRangeSelector :: Selector '[] CDouble
particleLifetimeRangeSelector = mkSelector "particleLifetimeRange"

-- | @Selector@ for @setParticleLifetimeRange:@
setParticleLifetimeRangeSelector :: Selector '[CDouble] ()
setParticleLifetimeRangeSelector = mkSelector "setParticleLifetimeRange:"

-- | @Selector@ for @particleRotation@
particleRotationSelector :: Selector '[] CDouble
particleRotationSelector = mkSelector "particleRotation"

-- | @Selector@ for @setParticleRotation:@
setParticleRotationSelector :: Selector '[CDouble] ()
setParticleRotationSelector = mkSelector "setParticleRotation:"

-- | @Selector@ for @particleRotationRange@
particleRotationRangeSelector :: Selector '[] CDouble
particleRotationRangeSelector = mkSelector "particleRotationRange"

-- | @Selector@ for @setParticleRotationRange:@
setParticleRotationRangeSelector :: Selector '[CDouble] ()
setParticleRotationRangeSelector = mkSelector "setParticleRotationRange:"

-- | @Selector@ for @particleRotationSpeed@
particleRotationSpeedSelector :: Selector '[] CDouble
particleRotationSpeedSelector = mkSelector "particleRotationSpeed"

-- | @Selector@ for @setParticleRotationSpeed:@
setParticleRotationSpeedSelector :: Selector '[CDouble] ()
setParticleRotationSpeedSelector = mkSelector "setParticleRotationSpeed:"

-- | @Selector@ for @particleScale@
particleScaleSelector :: Selector '[] CDouble
particleScaleSelector = mkSelector "particleScale"

-- | @Selector@ for @setParticleScale:@
setParticleScaleSelector :: Selector '[CDouble] ()
setParticleScaleSelector = mkSelector "setParticleScale:"

-- | @Selector@ for @particleScaleRange@
particleScaleRangeSelector :: Selector '[] CDouble
particleScaleRangeSelector = mkSelector "particleScaleRange"

-- | @Selector@ for @setParticleScaleRange:@
setParticleScaleRangeSelector :: Selector '[CDouble] ()
setParticleScaleRangeSelector = mkSelector "setParticleScaleRange:"

-- | @Selector@ for @particleScaleSpeed@
particleScaleSpeedSelector :: Selector '[] CDouble
particleScaleSpeedSelector = mkSelector "particleScaleSpeed"

-- | @Selector@ for @setParticleScaleSpeed:@
setParticleScaleSpeedSelector :: Selector '[CDouble] ()
setParticleScaleSpeedSelector = mkSelector "setParticleScaleSpeed:"

-- | @Selector@ for @particleScaleSequence@
particleScaleSequenceSelector :: Selector '[] (Id SKKeyframeSequence)
particleScaleSequenceSelector = mkSelector "particleScaleSequence"

-- | @Selector@ for @setParticleScaleSequence:@
setParticleScaleSequenceSelector :: Selector '[Id SKKeyframeSequence] ()
setParticleScaleSequenceSelector = mkSelector "setParticleScaleSequence:"

-- | @Selector@ for @particleAlpha@
particleAlphaSelector :: Selector '[] CDouble
particleAlphaSelector = mkSelector "particleAlpha"

-- | @Selector@ for @setParticleAlpha:@
setParticleAlphaSelector :: Selector '[CDouble] ()
setParticleAlphaSelector = mkSelector "setParticleAlpha:"

-- | @Selector@ for @particleAlphaRange@
particleAlphaRangeSelector :: Selector '[] CDouble
particleAlphaRangeSelector = mkSelector "particleAlphaRange"

-- | @Selector@ for @setParticleAlphaRange:@
setParticleAlphaRangeSelector :: Selector '[CDouble] ()
setParticleAlphaRangeSelector = mkSelector "setParticleAlphaRange:"

-- | @Selector@ for @particleAlphaSpeed@
particleAlphaSpeedSelector :: Selector '[] CDouble
particleAlphaSpeedSelector = mkSelector "particleAlphaSpeed"

-- | @Selector@ for @setParticleAlphaSpeed:@
setParticleAlphaSpeedSelector :: Selector '[CDouble] ()
setParticleAlphaSpeedSelector = mkSelector "setParticleAlphaSpeed:"

-- | @Selector@ for @particleAlphaSequence@
particleAlphaSequenceSelector :: Selector '[] (Id SKKeyframeSequence)
particleAlphaSequenceSelector = mkSelector "particleAlphaSequence"

-- | @Selector@ for @setParticleAlphaSequence:@
setParticleAlphaSequenceSelector :: Selector '[Id SKKeyframeSequence] ()
setParticleAlphaSequenceSelector = mkSelector "setParticleAlphaSequence:"

-- | @Selector@ for @particleAction@
particleActionSelector :: Selector '[] (Id SKAction)
particleActionSelector = mkSelector "particleAction"

-- | @Selector@ for @setParticleAction:@
setParticleActionSelector :: Selector '[Id SKAction] ()
setParticleActionSelector = mkSelector "setParticleAction:"

-- | @Selector@ for @fieldBitMask@
fieldBitMaskSelector :: Selector '[] CUInt
fieldBitMaskSelector = mkSelector "fieldBitMask"

-- | @Selector@ for @setFieldBitMask:@
setFieldBitMaskSelector :: Selector '[CUInt] ()
setFieldBitMaskSelector = mkSelector "setFieldBitMask:"

-- | @Selector@ for @targetNode@
targetNodeSelector :: Selector '[] (Id SKNode)
targetNodeSelector = mkSelector "targetNode"

-- | @Selector@ for @setTargetNode:@
setTargetNodeSelector :: Selector '[Id SKNode] ()
setTargetNodeSelector = mkSelector "setTargetNode:"

-- | @Selector@ for @shader@
shaderSelector :: Selector '[] (Id SKShader)
shaderSelector = mkSelector "shader"

-- | @Selector@ for @setShader:@
setShaderSelector :: Selector '[Id SKShader] ()
setShaderSelector = mkSelector "setShader:"

-- | @Selector@ for @attributeValues@
attributeValuesSelector :: Selector '[] (Id NSDictionary)
attributeValuesSelector = mkSelector "attributeValues"

-- | @Selector@ for @setAttributeValues:@
setAttributeValuesSelector :: Selector '[Id NSDictionary] ()
setAttributeValuesSelector = mkSelector "setAttributeValues:"

-- | @Selector@ for @particleZPosition@
particleZPositionSelector :: Selector '[] CDouble
particleZPositionSelector = mkSelector "particleZPosition"

-- | @Selector@ for @setParticleZPosition:@
setParticleZPositionSelector :: Selector '[CDouble] ()
setParticleZPositionSelector = mkSelector "setParticleZPosition:"

-- | @Selector@ for @particleRenderOrder@
particleRenderOrderSelector :: Selector '[] SKParticleRenderOrder
particleRenderOrderSelector = mkSelector "particleRenderOrder"

-- | @Selector@ for @setParticleRenderOrder:@
setParticleRenderOrderSelector :: Selector '[SKParticleRenderOrder] ()
setParticleRenderOrderSelector = mkSelector "setParticleRenderOrder:"

-- | @Selector@ for @particleZPositionRange@
particleZPositionRangeSelector :: Selector '[] CDouble
particleZPositionRangeSelector = mkSelector "particleZPositionRange"

-- | @Selector@ for @setParticleZPositionRange:@
setParticleZPositionRangeSelector :: Selector '[CDouble] ()
setParticleZPositionRangeSelector = mkSelector "setParticleZPositionRange:"

-- | @Selector@ for @particleZPositionSpeed@
particleZPositionSpeedSelector :: Selector '[] CDouble
particleZPositionSpeedSelector = mkSelector "particleZPositionSpeed"

-- | @Selector@ for @setParticleZPositionSpeed:@
setParticleZPositionSpeedSelector :: Selector '[CDouble] ()
setParticleZPositionSpeedSelector = mkSelector "setParticleZPositionSpeed:"

