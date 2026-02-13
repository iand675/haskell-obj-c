{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNParticleSystem
--
-- The SCNParticleSystem class represents a system of particles.
--
-- Generated bindings for @SCNParticleSystem@.
module ObjC.SceneKit.SCNParticleSystem
  ( SCNParticleSystem
  , IsSCNParticleSystem(..)
  , particleSystem
  , particleSystemNamed_inDirectory
  , reset
  , handleEvent_forProperties_withBlock
  , addModifierForProperties_atStage_withBlock
  , removeModifiersOfStage
  , removeAllModifiers
  , emissionDuration
  , setEmissionDuration
  , emissionDurationVariation
  , setEmissionDurationVariation
  , idleDuration
  , setIdleDuration
  , idleDurationVariation
  , setIdleDurationVariation
  , loops
  , setLoops
  , birthRate
  , setBirthRate
  , birthRateVariation
  , setBirthRateVariation
  , warmupDuration
  , setWarmupDuration
  , emitterShape
  , setEmitterShape
  , birthLocation
  , setBirthLocation
  , birthDirection
  , setBirthDirection
  , spreadingAngle
  , setSpreadingAngle
  , emittingDirection
  , setEmittingDirection
  , orientationDirection
  , setOrientationDirection
  , acceleration
  , setAcceleration
  , local
  , setLocal
  , particleAngle
  , setParticleAngle
  , particleAngleVariation
  , setParticleAngleVariation
  , particleVelocity
  , setParticleVelocity
  , particleVelocityVariation
  , setParticleVelocityVariation
  , particleAngularVelocity
  , setParticleAngularVelocity
  , particleAngularVelocityVariation
  , setParticleAngularVelocityVariation
  , particleLifeSpan
  , setParticleLifeSpan
  , particleLifeSpanVariation
  , setParticleLifeSpanVariation
  , systemSpawnedOnDying
  , setSystemSpawnedOnDying
  , systemSpawnedOnCollision
  , setSystemSpawnedOnCollision
  , systemSpawnedOnLiving
  , setSystemSpawnedOnLiving
  , particleImage
  , setParticleImage
  , imageSequenceColumnCount
  , setImageSequenceColumnCount
  , imageSequenceRowCount
  , setImageSequenceRowCount
  , imageSequenceInitialFrame
  , setImageSequenceInitialFrame
  , imageSequenceInitialFrameVariation
  , setImageSequenceInitialFrameVariation
  , imageSequenceFrameRate
  , setImageSequenceFrameRate
  , imageSequenceFrameRateVariation
  , setImageSequenceFrameRateVariation
  , imageSequenceAnimationMode
  , setImageSequenceAnimationMode
  , particleColor
  , setParticleColor
  , particleColorVariation
  , setParticleColorVariation
  , particleSize
  , setParticleSize
  , particleSizeVariation
  , setParticleSizeVariation
  , particleIntensity
  , setParticleIntensity
  , particleIntensityVariation
  , setParticleIntensityVariation
  , blendMode
  , setBlendMode
  , blackPassEnabled
  , setBlackPassEnabled
  , orientationMode
  , setOrientationMode
  , sortingMode
  , setSortingMode
  , lightingEnabled
  , setLightingEnabled
  , affectedByGravity
  , setAffectedByGravity
  , affectedByPhysicsFields
  , setAffectedByPhysicsFields
  , particleDiesOnCollision
  , setParticleDiesOnCollision
  , colliderNodes
  , setColliderNodes
  , particleMass
  , setParticleMass
  , particleMassVariation
  , setParticleMassVariation
  , particleBounce
  , setParticleBounce
  , particleBounceVariation
  , setParticleBounceVariation
  , particleFriction
  , setParticleFriction
  , particleFrictionVariation
  , setParticleFrictionVariation
  , particleCharge
  , setParticleCharge
  , particleChargeVariation
  , setParticleChargeVariation
  , dampingFactor
  , setDampingFactor
  , speedFactor
  , setSpeedFactor
  , stretchFactor
  , setStretchFactor
  , fresnelExponent
  , setFresnelExponent
  , writesToDepthBuffer
  , setWritesToDepthBuffer
  , propertyControllers
  , setPropertyControllers
  , accelerationSelector
  , addModifierForProperties_atStage_withBlockSelector
  , affectedByGravitySelector
  , affectedByPhysicsFieldsSelector
  , birthDirectionSelector
  , birthLocationSelector
  , birthRateSelector
  , birthRateVariationSelector
  , blackPassEnabledSelector
  , blendModeSelector
  , colliderNodesSelector
  , dampingFactorSelector
  , emissionDurationSelector
  , emissionDurationVariationSelector
  , emitterShapeSelector
  , emittingDirectionSelector
  , fresnelExponentSelector
  , handleEvent_forProperties_withBlockSelector
  , idleDurationSelector
  , idleDurationVariationSelector
  , imageSequenceAnimationModeSelector
  , imageSequenceColumnCountSelector
  , imageSequenceFrameRateSelector
  , imageSequenceFrameRateVariationSelector
  , imageSequenceInitialFrameSelector
  , imageSequenceInitialFrameVariationSelector
  , imageSequenceRowCountSelector
  , lightingEnabledSelector
  , localSelector
  , loopsSelector
  , orientationDirectionSelector
  , orientationModeSelector
  , particleAngleSelector
  , particleAngleVariationSelector
  , particleAngularVelocitySelector
  , particleAngularVelocityVariationSelector
  , particleBounceSelector
  , particleBounceVariationSelector
  , particleChargeSelector
  , particleChargeVariationSelector
  , particleColorSelector
  , particleColorVariationSelector
  , particleDiesOnCollisionSelector
  , particleFrictionSelector
  , particleFrictionVariationSelector
  , particleImageSelector
  , particleIntensitySelector
  , particleIntensityVariationSelector
  , particleLifeSpanSelector
  , particleLifeSpanVariationSelector
  , particleMassSelector
  , particleMassVariationSelector
  , particleSizeSelector
  , particleSizeVariationSelector
  , particleSystemNamed_inDirectorySelector
  , particleSystemSelector
  , particleVelocitySelector
  , particleVelocityVariationSelector
  , propertyControllersSelector
  , removeAllModifiersSelector
  , removeModifiersOfStageSelector
  , resetSelector
  , setAccelerationSelector
  , setAffectedByGravitySelector
  , setAffectedByPhysicsFieldsSelector
  , setBirthDirectionSelector
  , setBirthLocationSelector
  , setBirthRateSelector
  , setBirthRateVariationSelector
  , setBlackPassEnabledSelector
  , setBlendModeSelector
  , setColliderNodesSelector
  , setDampingFactorSelector
  , setEmissionDurationSelector
  , setEmissionDurationVariationSelector
  , setEmitterShapeSelector
  , setEmittingDirectionSelector
  , setFresnelExponentSelector
  , setIdleDurationSelector
  , setIdleDurationVariationSelector
  , setImageSequenceAnimationModeSelector
  , setImageSequenceColumnCountSelector
  , setImageSequenceFrameRateSelector
  , setImageSequenceFrameRateVariationSelector
  , setImageSequenceInitialFrameSelector
  , setImageSequenceInitialFrameVariationSelector
  , setImageSequenceRowCountSelector
  , setLightingEnabledSelector
  , setLocalSelector
  , setLoopsSelector
  , setOrientationDirectionSelector
  , setOrientationModeSelector
  , setParticleAngleSelector
  , setParticleAngleVariationSelector
  , setParticleAngularVelocitySelector
  , setParticleAngularVelocityVariationSelector
  , setParticleBounceSelector
  , setParticleBounceVariationSelector
  , setParticleChargeSelector
  , setParticleChargeVariationSelector
  , setParticleColorSelector
  , setParticleColorVariationSelector
  , setParticleDiesOnCollisionSelector
  , setParticleFrictionSelector
  , setParticleFrictionVariationSelector
  , setParticleImageSelector
  , setParticleIntensitySelector
  , setParticleIntensityVariationSelector
  , setParticleLifeSpanSelector
  , setParticleLifeSpanVariationSelector
  , setParticleMassSelector
  , setParticleMassVariationSelector
  , setParticleSizeSelector
  , setParticleSizeVariationSelector
  , setParticleVelocitySelector
  , setParticleVelocityVariationSelector
  , setPropertyControllersSelector
  , setSortingModeSelector
  , setSpeedFactorSelector
  , setSpreadingAngleSelector
  , setStretchFactorSelector
  , setSystemSpawnedOnCollisionSelector
  , setSystemSpawnedOnDyingSelector
  , setSystemSpawnedOnLivingSelector
  , setWarmupDurationSelector
  , setWritesToDepthBufferSelector
  , sortingModeSelector
  , speedFactorSelector
  , spreadingAngleSelector
  , stretchFactorSelector
  , systemSpawnedOnCollisionSelector
  , systemSpawnedOnDyingSelector
  , systemSpawnedOnLivingSelector
  , warmupDurationSelector
  , writesToDepthBufferSelector

  -- * Enum types
  , SCNParticleBirthDirection(SCNParticleBirthDirection)
  , pattern SCNParticleBirthDirectionConstant
  , pattern SCNParticleBirthDirectionSurfaceNormal
  , pattern SCNParticleBirthDirectionRandom
  , SCNParticleBirthLocation(SCNParticleBirthLocation)
  , pattern SCNParticleBirthLocationSurface
  , pattern SCNParticleBirthLocationVolume
  , pattern SCNParticleBirthLocationVertex
  , SCNParticleBlendMode(SCNParticleBlendMode)
  , pattern SCNParticleBlendModeAdditive
  , pattern SCNParticleBlendModeSubtract
  , pattern SCNParticleBlendModeMultiply
  , pattern SCNParticleBlendModeScreen
  , pattern SCNParticleBlendModeAlpha
  , pattern SCNParticleBlendModeReplace
  , SCNParticleEvent(SCNParticleEvent)
  , pattern SCNParticleEventBirth
  , pattern SCNParticleEventDeath
  , pattern SCNParticleEventCollision
  , SCNParticleImageSequenceAnimationMode(SCNParticleImageSequenceAnimationMode)
  , pattern SCNParticleImageSequenceAnimationModeRepeat
  , pattern SCNParticleImageSequenceAnimationModeClamp
  , pattern SCNParticleImageSequenceAnimationModeAutoReverse
  , SCNParticleModifierStage(SCNParticleModifierStage)
  , pattern SCNParticleModifierStagePreDynamics
  , pattern SCNParticleModifierStagePostDynamics
  , pattern SCNParticleModifierStagePreCollision
  , pattern SCNParticleModifierStagePostCollision
  , SCNParticleOrientationMode(SCNParticleOrientationMode)
  , pattern SCNParticleOrientationModeBillboardScreenAligned
  , pattern SCNParticleOrientationModeBillboardViewAligned
  , pattern SCNParticleOrientationModeFree
  , pattern SCNParticleOrientationModeBillboardYAligned
  , SCNParticleSortingMode(SCNParticleSortingMode)
  , pattern SCNParticleSortingModeNone
  , pattern SCNParticleSortingModeProjectedDepth
  , pattern SCNParticleSortingModeDistance
  , pattern SCNParticleSortingModeOldestFirst
  , pattern SCNParticleSortingModeYoungestFirst

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.SceneKit.Internal.Structs
import ObjC.SceneKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ particleSystem@
particleSystem :: IO (Id SCNParticleSystem)
particleSystem  =
  do
    cls' <- getRequiredClass "SCNParticleSystem"
    sendClassMessage cls' particleSystemSelector

-- | @+ particleSystemNamed:inDirectory:@
particleSystemNamed_inDirectory :: (IsNSString name, IsNSString directory) => name -> directory -> IO (Id SCNParticleSystem)
particleSystemNamed_inDirectory name directory =
  do
    cls' <- getRequiredClass "SCNParticleSystem"
    sendClassMessage cls' particleSystemNamed_inDirectorySelector (toNSString name) (toNSString directory)

-- | @- reset@
reset :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO ()
reset scnParticleSystem =
  sendMessage scnParticleSystem resetSelector

-- | @- handleEvent:forProperties:withBlock:@
handleEvent_forProperties_withBlock :: (IsSCNParticleSystem scnParticleSystem, IsNSArray properties) => scnParticleSystem -> SCNParticleEvent -> properties -> Ptr () -> IO ()
handleEvent_forProperties_withBlock scnParticleSystem event properties block =
  sendMessage scnParticleSystem handleEvent_forProperties_withBlockSelector event (toNSArray properties) block

-- | @- addModifierForProperties:atStage:withBlock:@
addModifierForProperties_atStage_withBlock :: (IsSCNParticleSystem scnParticleSystem, IsNSArray properties) => scnParticleSystem -> properties -> SCNParticleModifierStage -> Ptr () -> IO ()
addModifierForProperties_atStage_withBlock scnParticleSystem properties stage block =
  sendMessage scnParticleSystem addModifierForProperties_atStage_withBlockSelector (toNSArray properties) stage block

-- | @- removeModifiersOfStage:@
removeModifiersOfStage :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> SCNParticleModifierStage -> IO ()
removeModifiersOfStage scnParticleSystem stage =
  sendMessage scnParticleSystem removeModifiersOfStageSelector stage

-- | @- removeAllModifiers@
removeAllModifiers :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO ()
removeAllModifiers scnParticleSystem =
  sendMessage scnParticleSystem removeAllModifiersSelector

-- | @- emissionDuration@
emissionDuration :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
emissionDuration scnParticleSystem =
  sendMessage scnParticleSystem emissionDurationSelector

-- | @- setEmissionDuration:@
setEmissionDuration :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setEmissionDuration scnParticleSystem value =
  sendMessage scnParticleSystem setEmissionDurationSelector value

-- | @- emissionDurationVariation@
emissionDurationVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
emissionDurationVariation scnParticleSystem =
  sendMessage scnParticleSystem emissionDurationVariationSelector

-- | @- setEmissionDurationVariation:@
setEmissionDurationVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setEmissionDurationVariation scnParticleSystem value =
  sendMessage scnParticleSystem setEmissionDurationVariationSelector value

-- | @- idleDuration@
idleDuration :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
idleDuration scnParticleSystem =
  sendMessage scnParticleSystem idleDurationSelector

-- | @- setIdleDuration:@
setIdleDuration :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setIdleDuration scnParticleSystem value =
  sendMessage scnParticleSystem setIdleDurationSelector value

-- | @- idleDurationVariation@
idleDurationVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
idleDurationVariation scnParticleSystem =
  sendMessage scnParticleSystem idleDurationVariationSelector

-- | @- setIdleDurationVariation:@
setIdleDurationVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setIdleDurationVariation scnParticleSystem value =
  sendMessage scnParticleSystem setIdleDurationVariationSelector value

-- | @- loops@
loops :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO Bool
loops scnParticleSystem =
  sendMessage scnParticleSystem loopsSelector

-- | @- setLoops:@
setLoops :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> Bool -> IO ()
setLoops scnParticleSystem value =
  sendMessage scnParticleSystem setLoopsSelector value

-- | @- birthRate@
birthRate :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
birthRate scnParticleSystem =
  sendMessage scnParticleSystem birthRateSelector

-- | @- setBirthRate:@
setBirthRate :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setBirthRate scnParticleSystem value =
  sendMessage scnParticleSystem setBirthRateSelector value

-- | @- birthRateVariation@
birthRateVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
birthRateVariation scnParticleSystem =
  sendMessage scnParticleSystem birthRateVariationSelector

-- | @- setBirthRateVariation:@
setBirthRateVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setBirthRateVariation scnParticleSystem value =
  sendMessage scnParticleSystem setBirthRateVariationSelector value

-- | @- warmupDuration@
warmupDuration :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
warmupDuration scnParticleSystem =
  sendMessage scnParticleSystem warmupDurationSelector

-- | @- setWarmupDuration:@
setWarmupDuration :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setWarmupDuration scnParticleSystem value =
  sendMessage scnParticleSystem setWarmupDurationSelector value

-- | @- emitterShape@
emitterShape :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO (Id SCNGeometry)
emitterShape scnParticleSystem =
  sendMessage scnParticleSystem emitterShapeSelector

-- | @- setEmitterShape:@
setEmitterShape :: (IsSCNParticleSystem scnParticleSystem, IsSCNGeometry value) => scnParticleSystem -> value -> IO ()
setEmitterShape scnParticleSystem value =
  sendMessage scnParticleSystem setEmitterShapeSelector (toSCNGeometry value)

-- | @- birthLocation@
birthLocation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO SCNParticleBirthLocation
birthLocation scnParticleSystem =
  sendMessage scnParticleSystem birthLocationSelector

-- | @- setBirthLocation:@
setBirthLocation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> SCNParticleBirthLocation -> IO ()
setBirthLocation scnParticleSystem value =
  sendMessage scnParticleSystem setBirthLocationSelector value

-- | @- birthDirection@
birthDirection :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO SCNParticleBirthDirection
birthDirection scnParticleSystem =
  sendMessage scnParticleSystem birthDirectionSelector

-- | @- setBirthDirection:@
setBirthDirection :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> SCNParticleBirthDirection -> IO ()
setBirthDirection scnParticleSystem value =
  sendMessage scnParticleSystem setBirthDirectionSelector value

-- | @- spreadingAngle@
spreadingAngle :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
spreadingAngle scnParticleSystem =
  sendMessage scnParticleSystem spreadingAngleSelector

-- | @- setSpreadingAngle:@
setSpreadingAngle :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setSpreadingAngle scnParticleSystem value =
  sendMessage scnParticleSystem setSpreadingAngleSelector value

-- | @- emittingDirection@
emittingDirection :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO SCNVector3
emittingDirection scnParticleSystem =
  sendMessage scnParticleSystem emittingDirectionSelector

-- | @- setEmittingDirection:@
setEmittingDirection :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> SCNVector3 -> IO ()
setEmittingDirection scnParticleSystem value =
  sendMessage scnParticleSystem setEmittingDirectionSelector value

-- | @- orientationDirection@
orientationDirection :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO SCNVector3
orientationDirection scnParticleSystem =
  sendMessage scnParticleSystem orientationDirectionSelector

-- | @- setOrientationDirection:@
setOrientationDirection :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> SCNVector3 -> IO ()
setOrientationDirection scnParticleSystem value =
  sendMessage scnParticleSystem setOrientationDirectionSelector value

-- | @- acceleration@
acceleration :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO SCNVector3
acceleration scnParticleSystem =
  sendMessage scnParticleSystem accelerationSelector

-- | @- setAcceleration:@
setAcceleration :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> SCNVector3 -> IO ()
setAcceleration scnParticleSystem value =
  sendMessage scnParticleSystem setAccelerationSelector value

-- | @- local@
local :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO Bool
local scnParticleSystem =
  sendMessage scnParticleSystem localSelector

-- | @- setLocal:@
setLocal :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> Bool -> IO ()
setLocal scnParticleSystem value =
  sendMessage scnParticleSystem setLocalSelector value

-- | @- particleAngle@
particleAngle :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
particleAngle scnParticleSystem =
  sendMessage scnParticleSystem particleAngleSelector

-- | @- setParticleAngle:@
setParticleAngle :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setParticleAngle scnParticleSystem value =
  sendMessage scnParticleSystem setParticleAngleSelector value

-- | @- particleAngleVariation@
particleAngleVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
particleAngleVariation scnParticleSystem =
  sendMessage scnParticleSystem particleAngleVariationSelector

-- | @- setParticleAngleVariation:@
setParticleAngleVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setParticleAngleVariation scnParticleSystem value =
  sendMessage scnParticleSystem setParticleAngleVariationSelector value

-- | @- particleVelocity@
particleVelocity :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
particleVelocity scnParticleSystem =
  sendMessage scnParticleSystem particleVelocitySelector

-- | @- setParticleVelocity:@
setParticleVelocity :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setParticleVelocity scnParticleSystem value =
  sendMessage scnParticleSystem setParticleVelocitySelector value

-- | @- particleVelocityVariation@
particleVelocityVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
particleVelocityVariation scnParticleSystem =
  sendMessage scnParticleSystem particleVelocityVariationSelector

-- | @- setParticleVelocityVariation:@
setParticleVelocityVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setParticleVelocityVariation scnParticleSystem value =
  sendMessage scnParticleSystem setParticleVelocityVariationSelector value

-- | @- particleAngularVelocity@
particleAngularVelocity :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
particleAngularVelocity scnParticleSystem =
  sendMessage scnParticleSystem particleAngularVelocitySelector

-- | @- setParticleAngularVelocity:@
setParticleAngularVelocity :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setParticleAngularVelocity scnParticleSystem value =
  sendMessage scnParticleSystem setParticleAngularVelocitySelector value

-- | @- particleAngularVelocityVariation@
particleAngularVelocityVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
particleAngularVelocityVariation scnParticleSystem =
  sendMessage scnParticleSystem particleAngularVelocityVariationSelector

-- | @- setParticleAngularVelocityVariation:@
setParticleAngularVelocityVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setParticleAngularVelocityVariation scnParticleSystem value =
  sendMessage scnParticleSystem setParticleAngularVelocityVariationSelector value

-- | @- particleLifeSpan@
particleLifeSpan :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
particleLifeSpan scnParticleSystem =
  sendMessage scnParticleSystem particleLifeSpanSelector

-- | @- setParticleLifeSpan:@
setParticleLifeSpan :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setParticleLifeSpan scnParticleSystem value =
  sendMessage scnParticleSystem setParticleLifeSpanSelector value

-- | @- particleLifeSpanVariation@
particleLifeSpanVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
particleLifeSpanVariation scnParticleSystem =
  sendMessage scnParticleSystem particleLifeSpanVariationSelector

-- | @- setParticleLifeSpanVariation:@
setParticleLifeSpanVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setParticleLifeSpanVariation scnParticleSystem value =
  sendMessage scnParticleSystem setParticleLifeSpanVariationSelector value

-- | @- systemSpawnedOnDying@
systemSpawnedOnDying :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO (Id SCNParticleSystem)
systemSpawnedOnDying scnParticleSystem =
  sendMessage scnParticleSystem systemSpawnedOnDyingSelector

-- | @- setSystemSpawnedOnDying:@
setSystemSpawnedOnDying :: (IsSCNParticleSystem scnParticleSystem, IsSCNParticleSystem value) => scnParticleSystem -> value -> IO ()
setSystemSpawnedOnDying scnParticleSystem value =
  sendMessage scnParticleSystem setSystemSpawnedOnDyingSelector (toSCNParticleSystem value)

-- | @- systemSpawnedOnCollision@
systemSpawnedOnCollision :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO (Id SCNParticleSystem)
systemSpawnedOnCollision scnParticleSystem =
  sendMessage scnParticleSystem systemSpawnedOnCollisionSelector

-- | @- setSystemSpawnedOnCollision:@
setSystemSpawnedOnCollision :: (IsSCNParticleSystem scnParticleSystem, IsSCNParticleSystem value) => scnParticleSystem -> value -> IO ()
setSystemSpawnedOnCollision scnParticleSystem value =
  sendMessage scnParticleSystem setSystemSpawnedOnCollisionSelector (toSCNParticleSystem value)

-- | @- systemSpawnedOnLiving@
systemSpawnedOnLiving :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO (Id SCNParticleSystem)
systemSpawnedOnLiving scnParticleSystem =
  sendMessage scnParticleSystem systemSpawnedOnLivingSelector

-- | @- setSystemSpawnedOnLiving:@
setSystemSpawnedOnLiving :: (IsSCNParticleSystem scnParticleSystem, IsSCNParticleSystem value) => scnParticleSystem -> value -> IO ()
setSystemSpawnedOnLiving scnParticleSystem value =
  sendMessage scnParticleSystem setSystemSpawnedOnLivingSelector (toSCNParticleSystem value)

-- | @- particleImage@
particleImage :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO RawId
particleImage scnParticleSystem =
  sendMessage scnParticleSystem particleImageSelector

-- | @- setParticleImage:@
setParticleImage :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> RawId -> IO ()
setParticleImage scnParticleSystem value =
  sendMessage scnParticleSystem setParticleImageSelector value

-- | @- imageSequenceColumnCount@
imageSequenceColumnCount :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CULong
imageSequenceColumnCount scnParticleSystem =
  sendMessage scnParticleSystem imageSequenceColumnCountSelector

-- | @- setImageSequenceColumnCount:@
setImageSequenceColumnCount :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CULong -> IO ()
setImageSequenceColumnCount scnParticleSystem value =
  sendMessage scnParticleSystem setImageSequenceColumnCountSelector value

-- | @- imageSequenceRowCount@
imageSequenceRowCount :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CULong
imageSequenceRowCount scnParticleSystem =
  sendMessage scnParticleSystem imageSequenceRowCountSelector

-- | @- setImageSequenceRowCount:@
setImageSequenceRowCount :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CULong -> IO ()
setImageSequenceRowCount scnParticleSystem value =
  sendMessage scnParticleSystem setImageSequenceRowCountSelector value

-- | @- imageSequenceInitialFrame@
imageSequenceInitialFrame :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
imageSequenceInitialFrame scnParticleSystem =
  sendMessage scnParticleSystem imageSequenceInitialFrameSelector

-- | @- setImageSequenceInitialFrame:@
setImageSequenceInitialFrame :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setImageSequenceInitialFrame scnParticleSystem value =
  sendMessage scnParticleSystem setImageSequenceInitialFrameSelector value

-- | @- imageSequenceInitialFrameVariation@
imageSequenceInitialFrameVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
imageSequenceInitialFrameVariation scnParticleSystem =
  sendMessage scnParticleSystem imageSequenceInitialFrameVariationSelector

-- | @- setImageSequenceInitialFrameVariation:@
setImageSequenceInitialFrameVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setImageSequenceInitialFrameVariation scnParticleSystem value =
  sendMessage scnParticleSystem setImageSequenceInitialFrameVariationSelector value

-- | @- imageSequenceFrameRate@
imageSequenceFrameRate :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
imageSequenceFrameRate scnParticleSystem =
  sendMessage scnParticleSystem imageSequenceFrameRateSelector

-- | @- setImageSequenceFrameRate:@
setImageSequenceFrameRate :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setImageSequenceFrameRate scnParticleSystem value =
  sendMessage scnParticleSystem setImageSequenceFrameRateSelector value

-- | @- imageSequenceFrameRateVariation@
imageSequenceFrameRateVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
imageSequenceFrameRateVariation scnParticleSystem =
  sendMessage scnParticleSystem imageSequenceFrameRateVariationSelector

-- | @- setImageSequenceFrameRateVariation:@
setImageSequenceFrameRateVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setImageSequenceFrameRateVariation scnParticleSystem value =
  sendMessage scnParticleSystem setImageSequenceFrameRateVariationSelector value

-- | @- imageSequenceAnimationMode@
imageSequenceAnimationMode :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO SCNParticleImageSequenceAnimationMode
imageSequenceAnimationMode scnParticleSystem =
  sendMessage scnParticleSystem imageSequenceAnimationModeSelector

-- | @- setImageSequenceAnimationMode:@
setImageSequenceAnimationMode :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> SCNParticleImageSequenceAnimationMode -> IO ()
setImageSequenceAnimationMode scnParticleSystem value =
  sendMessage scnParticleSystem setImageSequenceAnimationModeSelector value

-- | @- particleColor@
particleColor :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO (Id NSColor)
particleColor scnParticleSystem =
  sendMessage scnParticleSystem particleColorSelector

-- | @- setParticleColor:@
setParticleColor :: (IsSCNParticleSystem scnParticleSystem, IsNSColor value) => scnParticleSystem -> value -> IO ()
setParticleColor scnParticleSystem value =
  sendMessage scnParticleSystem setParticleColorSelector (toNSColor value)

-- | @- particleColorVariation@
particleColorVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO SCNVector4
particleColorVariation scnParticleSystem =
  sendMessage scnParticleSystem particleColorVariationSelector

-- | @- setParticleColorVariation:@
setParticleColorVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> SCNVector4 -> IO ()
setParticleColorVariation scnParticleSystem value =
  sendMessage scnParticleSystem setParticleColorVariationSelector value

-- | @- particleSize@
particleSize :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
particleSize scnParticleSystem =
  sendMessage scnParticleSystem particleSizeSelector

-- | @- setParticleSize:@
setParticleSize :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setParticleSize scnParticleSystem value =
  sendMessage scnParticleSystem setParticleSizeSelector value

-- | @- particleSizeVariation@
particleSizeVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
particleSizeVariation scnParticleSystem =
  sendMessage scnParticleSystem particleSizeVariationSelector

-- | @- setParticleSizeVariation:@
setParticleSizeVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setParticleSizeVariation scnParticleSystem value =
  sendMessage scnParticleSystem setParticleSizeVariationSelector value

-- | @- particleIntensity@
particleIntensity :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
particleIntensity scnParticleSystem =
  sendMessage scnParticleSystem particleIntensitySelector

-- | @- setParticleIntensity:@
setParticleIntensity :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setParticleIntensity scnParticleSystem value =
  sendMessage scnParticleSystem setParticleIntensitySelector value

-- | @- particleIntensityVariation@
particleIntensityVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
particleIntensityVariation scnParticleSystem =
  sendMessage scnParticleSystem particleIntensityVariationSelector

-- | @- setParticleIntensityVariation:@
setParticleIntensityVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setParticleIntensityVariation scnParticleSystem value =
  sendMessage scnParticleSystem setParticleIntensityVariationSelector value

-- | @- blendMode@
blendMode :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO SCNParticleBlendMode
blendMode scnParticleSystem =
  sendMessage scnParticleSystem blendModeSelector

-- | @- setBlendMode:@
setBlendMode :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> SCNParticleBlendMode -> IO ()
setBlendMode scnParticleSystem value =
  sendMessage scnParticleSystem setBlendModeSelector value

-- | @- blackPassEnabled@
blackPassEnabled :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO Bool
blackPassEnabled scnParticleSystem =
  sendMessage scnParticleSystem blackPassEnabledSelector

-- | @- setBlackPassEnabled:@
setBlackPassEnabled :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> Bool -> IO ()
setBlackPassEnabled scnParticleSystem value =
  sendMessage scnParticleSystem setBlackPassEnabledSelector value

-- | @- orientationMode@
orientationMode :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO SCNParticleOrientationMode
orientationMode scnParticleSystem =
  sendMessage scnParticleSystem orientationModeSelector

-- | @- setOrientationMode:@
setOrientationMode :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> SCNParticleOrientationMode -> IO ()
setOrientationMode scnParticleSystem value =
  sendMessage scnParticleSystem setOrientationModeSelector value

-- | @- sortingMode@
sortingMode :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO SCNParticleSortingMode
sortingMode scnParticleSystem =
  sendMessage scnParticleSystem sortingModeSelector

-- | @- setSortingMode:@
setSortingMode :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> SCNParticleSortingMode -> IO ()
setSortingMode scnParticleSystem value =
  sendMessage scnParticleSystem setSortingModeSelector value

-- | @- lightingEnabled@
lightingEnabled :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO Bool
lightingEnabled scnParticleSystem =
  sendMessage scnParticleSystem lightingEnabledSelector

-- | @- setLightingEnabled:@
setLightingEnabled :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> Bool -> IO ()
setLightingEnabled scnParticleSystem value =
  sendMessage scnParticleSystem setLightingEnabledSelector value

-- | @- affectedByGravity@
affectedByGravity :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO Bool
affectedByGravity scnParticleSystem =
  sendMessage scnParticleSystem affectedByGravitySelector

-- | @- setAffectedByGravity:@
setAffectedByGravity :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> Bool -> IO ()
setAffectedByGravity scnParticleSystem value =
  sendMessage scnParticleSystem setAffectedByGravitySelector value

-- | @- affectedByPhysicsFields@
affectedByPhysicsFields :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO Bool
affectedByPhysicsFields scnParticleSystem =
  sendMessage scnParticleSystem affectedByPhysicsFieldsSelector

-- | @- setAffectedByPhysicsFields:@
setAffectedByPhysicsFields :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> Bool -> IO ()
setAffectedByPhysicsFields scnParticleSystem value =
  sendMessage scnParticleSystem setAffectedByPhysicsFieldsSelector value

-- | @- particleDiesOnCollision@
particleDiesOnCollision :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO Bool
particleDiesOnCollision scnParticleSystem =
  sendMessage scnParticleSystem particleDiesOnCollisionSelector

-- | @- setParticleDiesOnCollision:@
setParticleDiesOnCollision :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> Bool -> IO ()
setParticleDiesOnCollision scnParticleSystem value =
  sendMessage scnParticleSystem setParticleDiesOnCollisionSelector value

-- | @- colliderNodes@
colliderNodes :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO (Id NSArray)
colliderNodes scnParticleSystem =
  sendMessage scnParticleSystem colliderNodesSelector

-- | @- setColliderNodes:@
setColliderNodes :: (IsSCNParticleSystem scnParticleSystem, IsNSArray value) => scnParticleSystem -> value -> IO ()
setColliderNodes scnParticleSystem value =
  sendMessage scnParticleSystem setColliderNodesSelector (toNSArray value)

-- | @- particleMass@
particleMass :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
particleMass scnParticleSystem =
  sendMessage scnParticleSystem particleMassSelector

-- | @- setParticleMass:@
setParticleMass :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setParticleMass scnParticleSystem value =
  sendMessage scnParticleSystem setParticleMassSelector value

-- | @- particleMassVariation@
particleMassVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
particleMassVariation scnParticleSystem =
  sendMessage scnParticleSystem particleMassVariationSelector

-- | @- setParticleMassVariation:@
setParticleMassVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setParticleMassVariation scnParticleSystem value =
  sendMessage scnParticleSystem setParticleMassVariationSelector value

-- | @- particleBounce@
particleBounce :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
particleBounce scnParticleSystem =
  sendMessage scnParticleSystem particleBounceSelector

-- | @- setParticleBounce:@
setParticleBounce :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setParticleBounce scnParticleSystem value =
  sendMessage scnParticleSystem setParticleBounceSelector value

-- | @- particleBounceVariation@
particleBounceVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
particleBounceVariation scnParticleSystem =
  sendMessage scnParticleSystem particleBounceVariationSelector

-- | @- setParticleBounceVariation:@
setParticleBounceVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setParticleBounceVariation scnParticleSystem value =
  sendMessage scnParticleSystem setParticleBounceVariationSelector value

-- | @- particleFriction@
particleFriction :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
particleFriction scnParticleSystem =
  sendMessage scnParticleSystem particleFrictionSelector

-- | @- setParticleFriction:@
setParticleFriction :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setParticleFriction scnParticleSystem value =
  sendMessage scnParticleSystem setParticleFrictionSelector value

-- | @- particleFrictionVariation@
particleFrictionVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
particleFrictionVariation scnParticleSystem =
  sendMessage scnParticleSystem particleFrictionVariationSelector

-- | @- setParticleFrictionVariation:@
setParticleFrictionVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setParticleFrictionVariation scnParticleSystem value =
  sendMessage scnParticleSystem setParticleFrictionVariationSelector value

-- | @- particleCharge@
particleCharge :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
particleCharge scnParticleSystem =
  sendMessage scnParticleSystem particleChargeSelector

-- | @- setParticleCharge:@
setParticleCharge :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setParticleCharge scnParticleSystem value =
  sendMessage scnParticleSystem setParticleChargeSelector value

-- | @- particleChargeVariation@
particleChargeVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
particleChargeVariation scnParticleSystem =
  sendMessage scnParticleSystem particleChargeVariationSelector

-- | @- setParticleChargeVariation:@
setParticleChargeVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setParticleChargeVariation scnParticleSystem value =
  sendMessage scnParticleSystem setParticleChargeVariationSelector value

-- | @- dampingFactor@
dampingFactor :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
dampingFactor scnParticleSystem =
  sendMessage scnParticleSystem dampingFactorSelector

-- | @- setDampingFactor:@
setDampingFactor :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setDampingFactor scnParticleSystem value =
  sendMessage scnParticleSystem setDampingFactorSelector value

-- | @- speedFactor@
speedFactor :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
speedFactor scnParticleSystem =
  sendMessage scnParticleSystem speedFactorSelector

-- | @- setSpeedFactor:@
setSpeedFactor :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setSpeedFactor scnParticleSystem value =
  sendMessage scnParticleSystem setSpeedFactorSelector value

-- | @- stretchFactor@
stretchFactor :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
stretchFactor scnParticleSystem =
  sendMessage scnParticleSystem stretchFactorSelector

-- | @- setStretchFactor:@
setStretchFactor :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setStretchFactor scnParticleSystem value =
  sendMessage scnParticleSystem setStretchFactorSelector value

-- | @- fresnelExponent@
fresnelExponent :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
fresnelExponent scnParticleSystem =
  sendMessage scnParticleSystem fresnelExponentSelector

-- | @- setFresnelExponent:@
setFresnelExponent :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setFresnelExponent scnParticleSystem value =
  sendMessage scnParticleSystem setFresnelExponentSelector value

-- | writeToDepthBuffer
--
-- Determines whether the receiver writes to the depth buffer when rendered. Defaults to NO.
--
-- ObjC selector: @- writesToDepthBuffer@
writesToDepthBuffer :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO Bool
writesToDepthBuffer scnParticleSystem =
  sendMessage scnParticleSystem writesToDepthBufferSelector

-- | writeToDepthBuffer
--
-- Determines whether the receiver writes to the depth buffer when rendered. Defaults to NO.
--
-- ObjC selector: @- setWritesToDepthBuffer:@
setWritesToDepthBuffer :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> Bool -> IO ()
setWritesToDepthBuffer scnParticleSystem value =
  sendMessage scnParticleSystem setWritesToDepthBufferSelector value

-- | @- propertyControllers@
propertyControllers :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO (Id NSDictionary)
propertyControllers scnParticleSystem =
  sendMessage scnParticleSystem propertyControllersSelector

-- | @- setPropertyControllers:@
setPropertyControllers :: (IsSCNParticleSystem scnParticleSystem, IsNSDictionary value) => scnParticleSystem -> value -> IO ()
setPropertyControllers scnParticleSystem value =
  sendMessage scnParticleSystem setPropertyControllersSelector (toNSDictionary value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @particleSystem@
particleSystemSelector :: Selector '[] (Id SCNParticleSystem)
particleSystemSelector = mkSelector "particleSystem"

-- | @Selector@ for @particleSystemNamed:inDirectory:@
particleSystemNamed_inDirectorySelector :: Selector '[Id NSString, Id NSString] (Id SCNParticleSystem)
particleSystemNamed_inDirectorySelector = mkSelector "particleSystemNamed:inDirectory:"

-- | @Selector@ for @reset@
resetSelector :: Selector '[] ()
resetSelector = mkSelector "reset"

-- | @Selector@ for @handleEvent:forProperties:withBlock:@
handleEvent_forProperties_withBlockSelector :: Selector '[SCNParticleEvent, Id NSArray, Ptr ()] ()
handleEvent_forProperties_withBlockSelector = mkSelector "handleEvent:forProperties:withBlock:"

-- | @Selector@ for @addModifierForProperties:atStage:withBlock:@
addModifierForProperties_atStage_withBlockSelector :: Selector '[Id NSArray, SCNParticleModifierStage, Ptr ()] ()
addModifierForProperties_atStage_withBlockSelector = mkSelector "addModifierForProperties:atStage:withBlock:"

-- | @Selector@ for @removeModifiersOfStage:@
removeModifiersOfStageSelector :: Selector '[SCNParticleModifierStage] ()
removeModifiersOfStageSelector = mkSelector "removeModifiersOfStage:"

-- | @Selector@ for @removeAllModifiers@
removeAllModifiersSelector :: Selector '[] ()
removeAllModifiersSelector = mkSelector "removeAllModifiers"

-- | @Selector@ for @emissionDuration@
emissionDurationSelector :: Selector '[] CDouble
emissionDurationSelector = mkSelector "emissionDuration"

-- | @Selector@ for @setEmissionDuration:@
setEmissionDurationSelector :: Selector '[CDouble] ()
setEmissionDurationSelector = mkSelector "setEmissionDuration:"

-- | @Selector@ for @emissionDurationVariation@
emissionDurationVariationSelector :: Selector '[] CDouble
emissionDurationVariationSelector = mkSelector "emissionDurationVariation"

-- | @Selector@ for @setEmissionDurationVariation:@
setEmissionDurationVariationSelector :: Selector '[CDouble] ()
setEmissionDurationVariationSelector = mkSelector "setEmissionDurationVariation:"

-- | @Selector@ for @idleDuration@
idleDurationSelector :: Selector '[] CDouble
idleDurationSelector = mkSelector "idleDuration"

-- | @Selector@ for @setIdleDuration:@
setIdleDurationSelector :: Selector '[CDouble] ()
setIdleDurationSelector = mkSelector "setIdleDuration:"

-- | @Selector@ for @idleDurationVariation@
idleDurationVariationSelector :: Selector '[] CDouble
idleDurationVariationSelector = mkSelector "idleDurationVariation"

-- | @Selector@ for @setIdleDurationVariation:@
setIdleDurationVariationSelector :: Selector '[CDouble] ()
setIdleDurationVariationSelector = mkSelector "setIdleDurationVariation:"

-- | @Selector@ for @loops@
loopsSelector :: Selector '[] Bool
loopsSelector = mkSelector "loops"

-- | @Selector@ for @setLoops:@
setLoopsSelector :: Selector '[Bool] ()
setLoopsSelector = mkSelector "setLoops:"

-- | @Selector@ for @birthRate@
birthRateSelector :: Selector '[] CDouble
birthRateSelector = mkSelector "birthRate"

-- | @Selector@ for @setBirthRate:@
setBirthRateSelector :: Selector '[CDouble] ()
setBirthRateSelector = mkSelector "setBirthRate:"

-- | @Selector@ for @birthRateVariation@
birthRateVariationSelector :: Selector '[] CDouble
birthRateVariationSelector = mkSelector "birthRateVariation"

-- | @Selector@ for @setBirthRateVariation:@
setBirthRateVariationSelector :: Selector '[CDouble] ()
setBirthRateVariationSelector = mkSelector "setBirthRateVariation:"

-- | @Selector@ for @warmupDuration@
warmupDurationSelector :: Selector '[] CDouble
warmupDurationSelector = mkSelector "warmupDuration"

-- | @Selector@ for @setWarmupDuration:@
setWarmupDurationSelector :: Selector '[CDouble] ()
setWarmupDurationSelector = mkSelector "setWarmupDuration:"

-- | @Selector@ for @emitterShape@
emitterShapeSelector :: Selector '[] (Id SCNGeometry)
emitterShapeSelector = mkSelector "emitterShape"

-- | @Selector@ for @setEmitterShape:@
setEmitterShapeSelector :: Selector '[Id SCNGeometry] ()
setEmitterShapeSelector = mkSelector "setEmitterShape:"

-- | @Selector@ for @birthLocation@
birthLocationSelector :: Selector '[] SCNParticleBirthLocation
birthLocationSelector = mkSelector "birthLocation"

-- | @Selector@ for @setBirthLocation:@
setBirthLocationSelector :: Selector '[SCNParticleBirthLocation] ()
setBirthLocationSelector = mkSelector "setBirthLocation:"

-- | @Selector@ for @birthDirection@
birthDirectionSelector :: Selector '[] SCNParticleBirthDirection
birthDirectionSelector = mkSelector "birthDirection"

-- | @Selector@ for @setBirthDirection:@
setBirthDirectionSelector :: Selector '[SCNParticleBirthDirection] ()
setBirthDirectionSelector = mkSelector "setBirthDirection:"

-- | @Selector@ for @spreadingAngle@
spreadingAngleSelector :: Selector '[] CDouble
spreadingAngleSelector = mkSelector "spreadingAngle"

-- | @Selector@ for @setSpreadingAngle:@
setSpreadingAngleSelector :: Selector '[CDouble] ()
setSpreadingAngleSelector = mkSelector "setSpreadingAngle:"

-- | @Selector@ for @emittingDirection@
emittingDirectionSelector :: Selector '[] SCNVector3
emittingDirectionSelector = mkSelector "emittingDirection"

-- | @Selector@ for @setEmittingDirection:@
setEmittingDirectionSelector :: Selector '[SCNVector3] ()
setEmittingDirectionSelector = mkSelector "setEmittingDirection:"

-- | @Selector@ for @orientationDirection@
orientationDirectionSelector :: Selector '[] SCNVector3
orientationDirectionSelector = mkSelector "orientationDirection"

-- | @Selector@ for @setOrientationDirection:@
setOrientationDirectionSelector :: Selector '[SCNVector3] ()
setOrientationDirectionSelector = mkSelector "setOrientationDirection:"

-- | @Selector@ for @acceleration@
accelerationSelector :: Selector '[] SCNVector3
accelerationSelector = mkSelector "acceleration"

-- | @Selector@ for @setAcceleration:@
setAccelerationSelector :: Selector '[SCNVector3] ()
setAccelerationSelector = mkSelector "setAcceleration:"

-- | @Selector@ for @local@
localSelector :: Selector '[] Bool
localSelector = mkSelector "local"

-- | @Selector@ for @setLocal:@
setLocalSelector :: Selector '[Bool] ()
setLocalSelector = mkSelector "setLocal:"

-- | @Selector@ for @particleAngle@
particleAngleSelector :: Selector '[] CDouble
particleAngleSelector = mkSelector "particleAngle"

-- | @Selector@ for @setParticleAngle:@
setParticleAngleSelector :: Selector '[CDouble] ()
setParticleAngleSelector = mkSelector "setParticleAngle:"

-- | @Selector@ for @particleAngleVariation@
particleAngleVariationSelector :: Selector '[] CDouble
particleAngleVariationSelector = mkSelector "particleAngleVariation"

-- | @Selector@ for @setParticleAngleVariation:@
setParticleAngleVariationSelector :: Selector '[CDouble] ()
setParticleAngleVariationSelector = mkSelector "setParticleAngleVariation:"

-- | @Selector@ for @particleVelocity@
particleVelocitySelector :: Selector '[] CDouble
particleVelocitySelector = mkSelector "particleVelocity"

-- | @Selector@ for @setParticleVelocity:@
setParticleVelocitySelector :: Selector '[CDouble] ()
setParticleVelocitySelector = mkSelector "setParticleVelocity:"

-- | @Selector@ for @particleVelocityVariation@
particleVelocityVariationSelector :: Selector '[] CDouble
particleVelocityVariationSelector = mkSelector "particleVelocityVariation"

-- | @Selector@ for @setParticleVelocityVariation:@
setParticleVelocityVariationSelector :: Selector '[CDouble] ()
setParticleVelocityVariationSelector = mkSelector "setParticleVelocityVariation:"

-- | @Selector@ for @particleAngularVelocity@
particleAngularVelocitySelector :: Selector '[] CDouble
particleAngularVelocitySelector = mkSelector "particleAngularVelocity"

-- | @Selector@ for @setParticleAngularVelocity:@
setParticleAngularVelocitySelector :: Selector '[CDouble] ()
setParticleAngularVelocitySelector = mkSelector "setParticleAngularVelocity:"

-- | @Selector@ for @particleAngularVelocityVariation@
particleAngularVelocityVariationSelector :: Selector '[] CDouble
particleAngularVelocityVariationSelector = mkSelector "particleAngularVelocityVariation"

-- | @Selector@ for @setParticleAngularVelocityVariation:@
setParticleAngularVelocityVariationSelector :: Selector '[CDouble] ()
setParticleAngularVelocityVariationSelector = mkSelector "setParticleAngularVelocityVariation:"

-- | @Selector@ for @particleLifeSpan@
particleLifeSpanSelector :: Selector '[] CDouble
particleLifeSpanSelector = mkSelector "particleLifeSpan"

-- | @Selector@ for @setParticleLifeSpan:@
setParticleLifeSpanSelector :: Selector '[CDouble] ()
setParticleLifeSpanSelector = mkSelector "setParticleLifeSpan:"

-- | @Selector@ for @particleLifeSpanVariation@
particleLifeSpanVariationSelector :: Selector '[] CDouble
particleLifeSpanVariationSelector = mkSelector "particleLifeSpanVariation"

-- | @Selector@ for @setParticleLifeSpanVariation:@
setParticleLifeSpanVariationSelector :: Selector '[CDouble] ()
setParticleLifeSpanVariationSelector = mkSelector "setParticleLifeSpanVariation:"

-- | @Selector@ for @systemSpawnedOnDying@
systemSpawnedOnDyingSelector :: Selector '[] (Id SCNParticleSystem)
systemSpawnedOnDyingSelector = mkSelector "systemSpawnedOnDying"

-- | @Selector@ for @setSystemSpawnedOnDying:@
setSystemSpawnedOnDyingSelector :: Selector '[Id SCNParticleSystem] ()
setSystemSpawnedOnDyingSelector = mkSelector "setSystemSpawnedOnDying:"

-- | @Selector@ for @systemSpawnedOnCollision@
systemSpawnedOnCollisionSelector :: Selector '[] (Id SCNParticleSystem)
systemSpawnedOnCollisionSelector = mkSelector "systemSpawnedOnCollision"

-- | @Selector@ for @setSystemSpawnedOnCollision:@
setSystemSpawnedOnCollisionSelector :: Selector '[Id SCNParticleSystem] ()
setSystemSpawnedOnCollisionSelector = mkSelector "setSystemSpawnedOnCollision:"

-- | @Selector@ for @systemSpawnedOnLiving@
systemSpawnedOnLivingSelector :: Selector '[] (Id SCNParticleSystem)
systemSpawnedOnLivingSelector = mkSelector "systemSpawnedOnLiving"

-- | @Selector@ for @setSystemSpawnedOnLiving:@
setSystemSpawnedOnLivingSelector :: Selector '[Id SCNParticleSystem] ()
setSystemSpawnedOnLivingSelector = mkSelector "setSystemSpawnedOnLiving:"

-- | @Selector@ for @particleImage@
particleImageSelector :: Selector '[] RawId
particleImageSelector = mkSelector "particleImage"

-- | @Selector@ for @setParticleImage:@
setParticleImageSelector :: Selector '[RawId] ()
setParticleImageSelector = mkSelector "setParticleImage:"

-- | @Selector@ for @imageSequenceColumnCount@
imageSequenceColumnCountSelector :: Selector '[] CULong
imageSequenceColumnCountSelector = mkSelector "imageSequenceColumnCount"

-- | @Selector@ for @setImageSequenceColumnCount:@
setImageSequenceColumnCountSelector :: Selector '[CULong] ()
setImageSequenceColumnCountSelector = mkSelector "setImageSequenceColumnCount:"

-- | @Selector@ for @imageSequenceRowCount@
imageSequenceRowCountSelector :: Selector '[] CULong
imageSequenceRowCountSelector = mkSelector "imageSequenceRowCount"

-- | @Selector@ for @setImageSequenceRowCount:@
setImageSequenceRowCountSelector :: Selector '[CULong] ()
setImageSequenceRowCountSelector = mkSelector "setImageSequenceRowCount:"

-- | @Selector@ for @imageSequenceInitialFrame@
imageSequenceInitialFrameSelector :: Selector '[] CDouble
imageSequenceInitialFrameSelector = mkSelector "imageSequenceInitialFrame"

-- | @Selector@ for @setImageSequenceInitialFrame:@
setImageSequenceInitialFrameSelector :: Selector '[CDouble] ()
setImageSequenceInitialFrameSelector = mkSelector "setImageSequenceInitialFrame:"

-- | @Selector@ for @imageSequenceInitialFrameVariation@
imageSequenceInitialFrameVariationSelector :: Selector '[] CDouble
imageSequenceInitialFrameVariationSelector = mkSelector "imageSequenceInitialFrameVariation"

-- | @Selector@ for @setImageSequenceInitialFrameVariation:@
setImageSequenceInitialFrameVariationSelector :: Selector '[CDouble] ()
setImageSequenceInitialFrameVariationSelector = mkSelector "setImageSequenceInitialFrameVariation:"

-- | @Selector@ for @imageSequenceFrameRate@
imageSequenceFrameRateSelector :: Selector '[] CDouble
imageSequenceFrameRateSelector = mkSelector "imageSequenceFrameRate"

-- | @Selector@ for @setImageSequenceFrameRate:@
setImageSequenceFrameRateSelector :: Selector '[CDouble] ()
setImageSequenceFrameRateSelector = mkSelector "setImageSequenceFrameRate:"

-- | @Selector@ for @imageSequenceFrameRateVariation@
imageSequenceFrameRateVariationSelector :: Selector '[] CDouble
imageSequenceFrameRateVariationSelector = mkSelector "imageSequenceFrameRateVariation"

-- | @Selector@ for @setImageSequenceFrameRateVariation:@
setImageSequenceFrameRateVariationSelector :: Selector '[CDouble] ()
setImageSequenceFrameRateVariationSelector = mkSelector "setImageSequenceFrameRateVariation:"

-- | @Selector@ for @imageSequenceAnimationMode@
imageSequenceAnimationModeSelector :: Selector '[] SCNParticleImageSequenceAnimationMode
imageSequenceAnimationModeSelector = mkSelector "imageSequenceAnimationMode"

-- | @Selector@ for @setImageSequenceAnimationMode:@
setImageSequenceAnimationModeSelector :: Selector '[SCNParticleImageSequenceAnimationMode] ()
setImageSequenceAnimationModeSelector = mkSelector "setImageSequenceAnimationMode:"

-- | @Selector@ for @particleColor@
particleColorSelector :: Selector '[] (Id NSColor)
particleColorSelector = mkSelector "particleColor"

-- | @Selector@ for @setParticleColor:@
setParticleColorSelector :: Selector '[Id NSColor] ()
setParticleColorSelector = mkSelector "setParticleColor:"

-- | @Selector@ for @particleColorVariation@
particleColorVariationSelector :: Selector '[] SCNVector4
particleColorVariationSelector = mkSelector "particleColorVariation"

-- | @Selector@ for @setParticleColorVariation:@
setParticleColorVariationSelector :: Selector '[SCNVector4] ()
setParticleColorVariationSelector = mkSelector "setParticleColorVariation:"

-- | @Selector@ for @particleSize@
particleSizeSelector :: Selector '[] CDouble
particleSizeSelector = mkSelector "particleSize"

-- | @Selector@ for @setParticleSize:@
setParticleSizeSelector :: Selector '[CDouble] ()
setParticleSizeSelector = mkSelector "setParticleSize:"

-- | @Selector@ for @particleSizeVariation@
particleSizeVariationSelector :: Selector '[] CDouble
particleSizeVariationSelector = mkSelector "particleSizeVariation"

-- | @Selector@ for @setParticleSizeVariation:@
setParticleSizeVariationSelector :: Selector '[CDouble] ()
setParticleSizeVariationSelector = mkSelector "setParticleSizeVariation:"

-- | @Selector@ for @particleIntensity@
particleIntensitySelector :: Selector '[] CDouble
particleIntensitySelector = mkSelector "particleIntensity"

-- | @Selector@ for @setParticleIntensity:@
setParticleIntensitySelector :: Selector '[CDouble] ()
setParticleIntensitySelector = mkSelector "setParticleIntensity:"

-- | @Selector@ for @particleIntensityVariation@
particleIntensityVariationSelector :: Selector '[] CDouble
particleIntensityVariationSelector = mkSelector "particleIntensityVariation"

-- | @Selector@ for @setParticleIntensityVariation:@
setParticleIntensityVariationSelector :: Selector '[CDouble] ()
setParticleIntensityVariationSelector = mkSelector "setParticleIntensityVariation:"

-- | @Selector@ for @blendMode@
blendModeSelector :: Selector '[] SCNParticleBlendMode
blendModeSelector = mkSelector "blendMode"

-- | @Selector@ for @setBlendMode:@
setBlendModeSelector :: Selector '[SCNParticleBlendMode] ()
setBlendModeSelector = mkSelector "setBlendMode:"

-- | @Selector@ for @blackPassEnabled@
blackPassEnabledSelector :: Selector '[] Bool
blackPassEnabledSelector = mkSelector "blackPassEnabled"

-- | @Selector@ for @setBlackPassEnabled:@
setBlackPassEnabledSelector :: Selector '[Bool] ()
setBlackPassEnabledSelector = mkSelector "setBlackPassEnabled:"

-- | @Selector@ for @orientationMode@
orientationModeSelector :: Selector '[] SCNParticleOrientationMode
orientationModeSelector = mkSelector "orientationMode"

-- | @Selector@ for @setOrientationMode:@
setOrientationModeSelector :: Selector '[SCNParticleOrientationMode] ()
setOrientationModeSelector = mkSelector "setOrientationMode:"

-- | @Selector@ for @sortingMode@
sortingModeSelector :: Selector '[] SCNParticleSortingMode
sortingModeSelector = mkSelector "sortingMode"

-- | @Selector@ for @setSortingMode:@
setSortingModeSelector :: Selector '[SCNParticleSortingMode] ()
setSortingModeSelector = mkSelector "setSortingMode:"

-- | @Selector@ for @lightingEnabled@
lightingEnabledSelector :: Selector '[] Bool
lightingEnabledSelector = mkSelector "lightingEnabled"

-- | @Selector@ for @setLightingEnabled:@
setLightingEnabledSelector :: Selector '[Bool] ()
setLightingEnabledSelector = mkSelector "setLightingEnabled:"

-- | @Selector@ for @affectedByGravity@
affectedByGravitySelector :: Selector '[] Bool
affectedByGravitySelector = mkSelector "affectedByGravity"

-- | @Selector@ for @setAffectedByGravity:@
setAffectedByGravitySelector :: Selector '[Bool] ()
setAffectedByGravitySelector = mkSelector "setAffectedByGravity:"

-- | @Selector@ for @affectedByPhysicsFields@
affectedByPhysicsFieldsSelector :: Selector '[] Bool
affectedByPhysicsFieldsSelector = mkSelector "affectedByPhysicsFields"

-- | @Selector@ for @setAffectedByPhysicsFields:@
setAffectedByPhysicsFieldsSelector :: Selector '[Bool] ()
setAffectedByPhysicsFieldsSelector = mkSelector "setAffectedByPhysicsFields:"

-- | @Selector@ for @particleDiesOnCollision@
particleDiesOnCollisionSelector :: Selector '[] Bool
particleDiesOnCollisionSelector = mkSelector "particleDiesOnCollision"

-- | @Selector@ for @setParticleDiesOnCollision:@
setParticleDiesOnCollisionSelector :: Selector '[Bool] ()
setParticleDiesOnCollisionSelector = mkSelector "setParticleDiesOnCollision:"

-- | @Selector@ for @colliderNodes@
colliderNodesSelector :: Selector '[] (Id NSArray)
colliderNodesSelector = mkSelector "colliderNodes"

-- | @Selector@ for @setColliderNodes:@
setColliderNodesSelector :: Selector '[Id NSArray] ()
setColliderNodesSelector = mkSelector "setColliderNodes:"

-- | @Selector@ for @particleMass@
particleMassSelector :: Selector '[] CDouble
particleMassSelector = mkSelector "particleMass"

-- | @Selector@ for @setParticleMass:@
setParticleMassSelector :: Selector '[CDouble] ()
setParticleMassSelector = mkSelector "setParticleMass:"

-- | @Selector@ for @particleMassVariation@
particleMassVariationSelector :: Selector '[] CDouble
particleMassVariationSelector = mkSelector "particleMassVariation"

-- | @Selector@ for @setParticleMassVariation:@
setParticleMassVariationSelector :: Selector '[CDouble] ()
setParticleMassVariationSelector = mkSelector "setParticleMassVariation:"

-- | @Selector@ for @particleBounce@
particleBounceSelector :: Selector '[] CDouble
particleBounceSelector = mkSelector "particleBounce"

-- | @Selector@ for @setParticleBounce:@
setParticleBounceSelector :: Selector '[CDouble] ()
setParticleBounceSelector = mkSelector "setParticleBounce:"

-- | @Selector@ for @particleBounceVariation@
particleBounceVariationSelector :: Selector '[] CDouble
particleBounceVariationSelector = mkSelector "particleBounceVariation"

-- | @Selector@ for @setParticleBounceVariation:@
setParticleBounceVariationSelector :: Selector '[CDouble] ()
setParticleBounceVariationSelector = mkSelector "setParticleBounceVariation:"

-- | @Selector@ for @particleFriction@
particleFrictionSelector :: Selector '[] CDouble
particleFrictionSelector = mkSelector "particleFriction"

-- | @Selector@ for @setParticleFriction:@
setParticleFrictionSelector :: Selector '[CDouble] ()
setParticleFrictionSelector = mkSelector "setParticleFriction:"

-- | @Selector@ for @particleFrictionVariation@
particleFrictionVariationSelector :: Selector '[] CDouble
particleFrictionVariationSelector = mkSelector "particleFrictionVariation"

-- | @Selector@ for @setParticleFrictionVariation:@
setParticleFrictionVariationSelector :: Selector '[CDouble] ()
setParticleFrictionVariationSelector = mkSelector "setParticleFrictionVariation:"

-- | @Selector@ for @particleCharge@
particleChargeSelector :: Selector '[] CDouble
particleChargeSelector = mkSelector "particleCharge"

-- | @Selector@ for @setParticleCharge:@
setParticleChargeSelector :: Selector '[CDouble] ()
setParticleChargeSelector = mkSelector "setParticleCharge:"

-- | @Selector@ for @particleChargeVariation@
particleChargeVariationSelector :: Selector '[] CDouble
particleChargeVariationSelector = mkSelector "particleChargeVariation"

-- | @Selector@ for @setParticleChargeVariation:@
setParticleChargeVariationSelector :: Selector '[CDouble] ()
setParticleChargeVariationSelector = mkSelector "setParticleChargeVariation:"

-- | @Selector@ for @dampingFactor@
dampingFactorSelector :: Selector '[] CDouble
dampingFactorSelector = mkSelector "dampingFactor"

-- | @Selector@ for @setDampingFactor:@
setDampingFactorSelector :: Selector '[CDouble] ()
setDampingFactorSelector = mkSelector "setDampingFactor:"

-- | @Selector@ for @speedFactor@
speedFactorSelector :: Selector '[] CDouble
speedFactorSelector = mkSelector "speedFactor"

-- | @Selector@ for @setSpeedFactor:@
setSpeedFactorSelector :: Selector '[CDouble] ()
setSpeedFactorSelector = mkSelector "setSpeedFactor:"

-- | @Selector@ for @stretchFactor@
stretchFactorSelector :: Selector '[] CDouble
stretchFactorSelector = mkSelector "stretchFactor"

-- | @Selector@ for @setStretchFactor:@
setStretchFactorSelector :: Selector '[CDouble] ()
setStretchFactorSelector = mkSelector "setStretchFactor:"

-- | @Selector@ for @fresnelExponent@
fresnelExponentSelector :: Selector '[] CDouble
fresnelExponentSelector = mkSelector "fresnelExponent"

-- | @Selector@ for @setFresnelExponent:@
setFresnelExponentSelector :: Selector '[CDouble] ()
setFresnelExponentSelector = mkSelector "setFresnelExponent:"

-- | @Selector@ for @writesToDepthBuffer@
writesToDepthBufferSelector :: Selector '[] Bool
writesToDepthBufferSelector = mkSelector "writesToDepthBuffer"

-- | @Selector@ for @setWritesToDepthBuffer:@
setWritesToDepthBufferSelector :: Selector '[Bool] ()
setWritesToDepthBufferSelector = mkSelector "setWritesToDepthBuffer:"

-- | @Selector@ for @propertyControllers@
propertyControllersSelector :: Selector '[] (Id NSDictionary)
propertyControllersSelector = mkSelector "propertyControllers"

-- | @Selector@ for @setPropertyControllers:@
setPropertyControllersSelector :: Selector '[Id NSDictionary] ()
setPropertyControllersSelector = mkSelector "setPropertyControllers:"

