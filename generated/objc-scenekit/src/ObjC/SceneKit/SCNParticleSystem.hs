{-# LANGUAGE PatternSynonyms #-}
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
  , particleSystemSelector
  , particleSystemNamed_inDirectorySelector
  , resetSelector
  , handleEvent_forProperties_withBlockSelector
  , addModifierForProperties_atStage_withBlockSelector
  , removeModifiersOfStageSelector
  , removeAllModifiersSelector
  , emissionDurationSelector
  , setEmissionDurationSelector
  , emissionDurationVariationSelector
  , setEmissionDurationVariationSelector
  , idleDurationSelector
  , setIdleDurationSelector
  , idleDurationVariationSelector
  , setIdleDurationVariationSelector
  , loopsSelector
  , setLoopsSelector
  , birthRateSelector
  , setBirthRateSelector
  , birthRateVariationSelector
  , setBirthRateVariationSelector
  , warmupDurationSelector
  , setWarmupDurationSelector
  , emitterShapeSelector
  , setEmitterShapeSelector
  , birthLocationSelector
  , setBirthLocationSelector
  , birthDirectionSelector
  , setBirthDirectionSelector
  , spreadingAngleSelector
  , setSpreadingAngleSelector
  , emittingDirectionSelector
  , setEmittingDirectionSelector
  , orientationDirectionSelector
  , setOrientationDirectionSelector
  , accelerationSelector
  , setAccelerationSelector
  , localSelector
  , setLocalSelector
  , particleAngleSelector
  , setParticleAngleSelector
  , particleAngleVariationSelector
  , setParticleAngleVariationSelector
  , particleVelocitySelector
  , setParticleVelocitySelector
  , particleVelocityVariationSelector
  , setParticleVelocityVariationSelector
  , particleAngularVelocitySelector
  , setParticleAngularVelocitySelector
  , particleAngularVelocityVariationSelector
  , setParticleAngularVelocityVariationSelector
  , particleLifeSpanSelector
  , setParticleLifeSpanSelector
  , particleLifeSpanVariationSelector
  , setParticleLifeSpanVariationSelector
  , systemSpawnedOnDyingSelector
  , setSystemSpawnedOnDyingSelector
  , systemSpawnedOnCollisionSelector
  , setSystemSpawnedOnCollisionSelector
  , systemSpawnedOnLivingSelector
  , setSystemSpawnedOnLivingSelector
  , particleImageSelector
  , setParticleImageSelector
  , imageSequenceColumnCountSelector
  , setImageSequenceColumnCountSelector
  , imageSequenceRowCountSelector
  , setImageSequenceRowCountSelector
  , imageSequenceInitialFrameSelector
  , setImageSequenceInitialFrameSelector
  , imageSequenceInitialFrameVariationSelector
  , setImageSequenceInitialFrameVariationSelector
  , imageSequenceFrameRateSelector
  , setImageSequenceFrameRateSelector
  , imageSequenceFrameRateVariationSelector
  , setImageSequenceFrameRateVariationSelector
  , imageSequenceAnimationModeSelector
  , setImageSequenceAnimationModeSelector
  , particleColorSelector
  , setParticleColorSelector
  , particleColorVariationSelector
  , setParticleColorVariationSelector
  , particleSizeSelector
  , setParticleSizeSelector
  , particleSizeVariationSelector
  , setParticleSizeVariationSelector
  , particleIntensitySelector
  , setParticleIntensitySelector
  , particleIntensityVariationSelector
  , setParticleIntensityVariationSelector
  , blendModeSelector
  , setBlendModeSelector
  , blackPassEnabledSelector
  , setBlackPassEnabledSelector
  , orientationModeSelector
  , setOrientationModeSelector
  , sortingModeSelector
  , setSortingModeSelector
  , lightingEnabledSelector
  , setLightingEnabledSelector
  , affectedByGravitySelector
  , setAffectedByGravitySelector
  , affectedByPhysicsFieldsSelector
  , setAffectedByPhysicsFieldsSelector
  , particleDiesOnCollisionSelector
  , setParticleDiesOnCollisionSelector
  , colliderNodesSelector
  , setColliderNodesSelector
  , particleMassSelector
  , setParticleMassSelector
  , particleMassVariationSelector
  , setParticleMassVariationSelector
  , particleBounceSelector
  , setParticleBounceSelector
  , particleBounceVariationSelector
  , setParticleBounceVariationSelector
  , particleFrictionSelector
  , setParticleFrictionSelector
  , particleFrictionVariationSelector
  , setParticleFrictionVariationSelector
  , particleChargeSelector
  , setParticleChargeSelector
  , particleChargeVariationSelector
  , setParticleChargeVariationSelector
  , dampingFactorSelector
  , setDampingFactorSelector
  , speedFactorSelector
  , setSpeedFactorSelector
  , stretchFactorSelector
  , setStretchFactorSelector
  , fresnelExponentSelector
  , setFresnelExponentSelector
  , writesToDepthBufferSelector
  , setWritesToDepthBufferSelector
  , propertyControllersSelector
  , setPropertyControllersSelector

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
    sendClassMsg cls' (mkSelector "particleSystem") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ particleSystemNamed:inDirectory:@
particleSystemNamed_inDirectory :: (IsNSString name, IsNSString directory) => name -> directory -> IO (Id SCNParticleSystem)
particleSystemNamed_inDirectory name directory =
  do
    cls' <- getRequiredClass "SCNParticleSystem"
    withObjCPtr name $ \raw_name ->
      withObjCPtr directory $ \raw_directory ->
        sendClassMsg cls' (mkSelector "particleSystemNamed:inDirectory:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_directory :: Ptr ())] >>= retainedObject . castPtr

-- | @- reset@
reset :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO ()
reset scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "reset") retVoid []

-- | @- handleEvent:forProperties:withBlock:@
handleEvent_forProperties_withBlock :: (IsSCNParticleSystem scnParticleSystem, IsNSArray properties) => scnParticleSystem -> SCNParticleEvent -> properties -> Ptr () -> IO ()
handleEvent_forProperties_withBlock scnParticleSystem  event properties block =
withObjCPtr properties $ \raw_properties ->
    sendMsg scnParticleSystem (mkSelector "handleEvent:forProperties:withBlock:") retVoid [argCLong (coerce event), argPtr (castPtr raw_properties :: Ptr ()), argPtr (castPtr block :: Ptr ())]

-- | @- addModifierForProperties:atStage:withBlock:@
addModifierForProperties_atStage_withBlock :: (IsSCNParticleSystem scnParticleSystem, IsNSArray properties) => scnParticleSystem -> properties -> SCNParticleModifierStage -> Ptr () -> IO ()
addModifierForProperties_atStage_withBlock scnParticleSystem  properties stage block =
withObjCPtr properties $ \raw_properties ->
    sendMsg scnParticleSystem (mkSelector "addModifierForProperties:atStage:withBlock:") retVoid [argPtr (castPtr raw_properties :: Ptr ()), argCLong (coerce stage), argPtr (castPtr block :: Ptr ())]

-- | @- removeModifiersOfStage:@
removeModifiersOfStage :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> SCNParticleModifierStage -> IO ()
removeModifiersOfStage scnParticleSystem  stage =
  sendMsg scnParticleSystem (mkSelector "removeModifiersOfStage:") retVoid [argCLong (coerce stage)]

-- | @- removeAllModifiers@
removeAllModifiers :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO ()
removeAllModifiers scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "removeAllModifiers") retVoid []

-- | @- emissionDuration@
emissionDuration :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
emissionDuration scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "emissionDuration") retCDouble []

-- | @- setEmissionDuration:@
setEmissionDuration :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setEmissionDuration scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setEmissionDuration:") retVoid [argCDouble (fromIntegral value)]

-- | @- emissionDurationVariation@
emissionDurationVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
emissionDurationVariation scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "emissionDurationVariation") retCDouble []

-- | @- setEmissionDurationVariation:@
setEmissionDurationVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setEmissionDurationVariation scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setEmissionDurationVariation:") retVoid [argCDouble (fromIntegral value)]

-- | @- idleDuration@
idleDuration :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
idleDuration scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "idleDuration") retCDouble []

-- | @- setIdleDuration:@
setIdleDuration :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setIdleDuration scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setIdleDuration:") retVoid [argCDouble (fromIntegral value)]

-- | @- idleDurationVariation@
idleDurationVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
idleDurationVariation scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "idleDurationVariation") retCDouble []

-- | @- setIdleDurationVariation:@
setIdleDurationVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setIdleDurationVariation scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setIdleDurationVariation:") retVoid [argCDouble (fromIntegral value)]

-- | @- loops@
loops :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO Bool
loops scnParticleSystem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnParticleSystem (mkSelector "loops") retCULong []

-- | @- setLoops:@
setLoops :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> Bool -> IO ()
setLoops scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setLoops:") retVoid [argCULong (if value then 1 else 0)]

-- | @- birthRate@
birthRate :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
birthRate scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "birthRate") retCDouble []

-- | @- setBirthRate:@
setBirthRate :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setBirthRate scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setBirthRate:") retVoid [argCDouble (fromIntegral value)]

-- | @- birthRateVariation@
birthRateVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
birthRateVariation scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "birthRateVariation") retCDouble []

-- | @- setBirthRateVariation:@
setBirthRateVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setBirthRateVariation scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setBirthRateVariation:") retVoid [argCDouble (fromIntegral value)]

-- | @- warmupDuration@
warmupDuration :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
warmupDuration scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "warmupDuration") retCDouble []

-- | @- setWarmupDuration:@
setWarmupDuration :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setWarmupDuration scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setWarmupDuration:") retVoid [argCDouble (fromIntegral value)]

-- | @- emitterShape@
emitterShape :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO (Id SCNGeometry)
emitterShape scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "emitterShape") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEmitterShape:@
setEmitterShape :: (IsSCNParticleSystem scnParticleSystem, IsSCNGeometry value) => scnParticleSystem -> value -> IO ()
setEmitterShape scnParticleSystem  value =
withObjCPtr value $ \raw_value ->
    sendMsg scnParticleSystem (mkSelector "setEmitterShape:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- birthLocation@
birthLocation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO SCNParticleBirthLocation
birthLocation scnParticleSystem  =
  fmap (coerce :: CLong -> SCNParticleBirthLocation) $ sendMsg scnParticleSystem (mkSelector "birthLocation") retCLong []

-- | @- setBirthLocation:@
setBirthLocation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> SCNParticleBirthLocation -> IO ()
setBirthLocation scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setBirthLocation:") retVoid [argCLong (coerce value)]

-- | @- birthDirection@
birthDirection :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO SCNParticleBirthDirection
birthDirection scnParticleSystem  =
  fmap (coerce :: CLong -> SCNParticleBirthDirection) $ sendMsg scnParticleSystem (mkSelector "birthDirection") retCLong []

-- | @- setBirthDirection:@
setBirthDirection :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> SCNParticleBirthDirection -> IO ()
setBirthDirection scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setBirthDirection:") retVoid [argCLong (coerce value)]

-- | @- spreadingAngle@
spreadingAngle :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
spreadingAngle scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "spreadingAngle") retCDouble []

-- | @- setSpreadingAngle:@
setSpreadingAngle :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setSpreadingAngle scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setSpreadingAngle:") retVoid [argCDouble (fromIntegral value)]

-- | @- emittingDirection@
emittingDirection :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO SCNVector3
emittingDirection scnParticleSystem  =
  sendMsgStret scnParticleSystem (mkSelector "emittingDirection") retSCNVector3 []

-- | @- setEmittingDirection:@
setEmittingDirection :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> SCNVector3 -> IO ()
setEmittingDirection scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setEmittingDirection:") retVoid [argSCNVector3 value]

-- | @- orientationDirection@
orientationDirection :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO SCNVector3
orientationDirection scnParticleSystem  =
  sendMsgStret scnParticleSystem (mkSelector "orientationDirection") retSCNVector3 []

-- | @- setOrientationDirection:@
setOrientationDirection :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> SCNVector3 -> IO ()
setOrientationDirection scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setOrientationDirection:") retVoid [argSCNVector3 value]

-- | @- acceleration@
acceleration :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO SCNVector3
acceleration scnParticleSystem  =
  sendMsgStret scnParticleSystem (mkSelector "acceleration") retSCNVector3 []

-- | @- setAcceleration:@
setAcceleration :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> SCNVector3 -> IO ()
setAcceleration scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setAcceleration:") retVoid [argSCNVector3 value]

-- | @- local@
local :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO Bool
local scnParticleSystem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnParticleSystem (mkSelector "local") retCULong []

-- | @- setLocal:@
setLocal :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> Bool -> IO ()
setLocal scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setLocal:") retVoid [argCULong (if value then 1 else 0)]

-- | @- particleAngle@
particleAngle :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
particleAngle scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "particleAngle") retCDouble []

-- | @- setParticleAngle:@
setParticleAngle :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setParticleAngle scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setParticleAngle:") retVoid [argCDouble (fromIntegral value)]

-- | @- particleAngleVariation@
particleAngleVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
particleAngleVariation scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "particleAngleVariation") retCDouble []

-- | @- setParticleAngleVariation:@
setParticleAngleVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setParticleAngleVariation scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setParticleAngleVariation:") retVoid [argCDouble (fromIntegral value)]

-- | @- particleVelocity@
particleVelocity :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
particleVelocity scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "particleVelocity") retCDouble []

-- | @- setParticleVelocity:@
setParticleVelocity :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setParticleVelocity scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setParticleVelocity:") retVoid [argCDouble (fromIntegral value)]

-- | @- particleVelocityVariation@
particleVelocityVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
particleVelocityVariation scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "particleVelocityVariation") retCDouble []

-- | @- setParticleVelocityVariation:@
setParticleVelocityVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setParticleVelocityVariation scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setParticleVelocityVariation:") retVoid [argCDouble (fromIntegral value)]

-- | @- particleAngularVelocity@
particleAngularVelocity :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
particleAngularVelocity scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "particleAngularVelocity") retCDouble []

-- | @- setParticleAngularVelocity:@
setParticleAngularVelocity :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setParticleAngularVelocity scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setParticleAngularVelocity:") retVoid [argCDouble (fromIntegral value)]

-- | @- particleAngularVelocityVariation@
particleAngularVelocityVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
particleAngularVelocityVariation scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "particleAngularVelocityVariation") retCDouble []

-- | @- setParticleAngularVelocityVariation:@
setParticleAngularVelocityVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setParticleAngularVelocityVariation scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setParticleAngularVelocityVariation:") retVoid [argCDouble (fromIntegral value)]

-- | @- particleLifeSpan@
particleLifeSpan :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
particleLifeSpan scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "particleLifeSpan") retCDouble []

-- | @- setParticleLifeSpan:@
setParticleLifeSpan :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setParticleLifeSpan scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setParticleLifeSpan:") retVoid [argCDouble (fromIntegral value)]

-- | @- particleLifeSpanVariation@
particleLifeSpanVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
particleLifeSpanVariation scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "particleLifeSpanVariation") retCDouble []

-- | @- setParticleLifeSpanVariation:@
setParticleLifeSpanVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setParticleLifeSpanVariation scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setParticleLifeSpanVariation:") retVoid [argCDouble (fromIntegral value)]

-- | @- systemSpawnedOnDying@
systemSpawnedOnDying :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO (Id SCNParticleSystem)
systemSpawnedOnDying scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "systemSpawnedOnDying") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSystemSpawnedOnDying:@
setSystemSpawnedOnDying :: (IsSCNParticleSystem scnParticleSystem, IsSCNParticleSystem value) => scnParticleSystem -> value -> IO ()
setSystemSpawnedOnDying scnParticleSystem  value =
withObjCPtr value $ \raw_value ->
    sendMsg scnParticleSystem (mkSelector "setSystemSpawnedOnDying:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- systemSpawnedOnCollision@
systemSpawnedOnCollision :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO (Id SCNParticleSystem)
systemSpawnedOnCollision scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "systemSpawnedOnCollision") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSystemSpawnedOnCollision:@
setSystemSpawnedOnCollision :: (IsSCNParticleSystem scnParticleSystem, IsSCNParticleSystem value) => scnParticleSystem -> value -> IO ()
setSystemSpawnedOnCollision scnParticleSystem  value =
withObjCPtr value $ \raw_value ->
    sendMsg scnParticleSystem (mkSelector "setSystemSpawnedOnCollision:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- systemSpawnedOnLiving@
systemSpawnedOnLiving :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO (Id SCNParticleSystem)
systemSpawnedOnLiving scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "systemSpawnedOnLiving") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSystemSpawnedOnLiving:@
setSystemSpawnedOnLiving :: (IsSCNParticleSystem scnParticleSystem, IsSCNParticleSystem value) => scnParticleSystem -> value -> IO ()
setSystemSpawnedOnLiving scnParticleSystem  value =
withObjCPtr value $ \raw_value ->
    sendMsg scnParticleSystem (mkSelector "setSystemSpawnedOnLiving:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- particleImage@
particleImage :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO RawId
particleImage scnParticleSystem  =
  fmap (RawId . castPtr) $ sendMsg scnParticleSystem (mkSelector "particleImage") (retPtr retVoid) []

-- | @- setParticleImage:@
setParticleImage :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> RawId -> IO ()
setParticleImage scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setParticleImage:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- imageSequenceColumnCount@
imageSequenceColumnCount :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CULong
imageSequenceColumnCount scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "imageSequenceColumnCount") retCULong []

-- | @- setImageSequenceColumnCount:@
setImageSequenceColumnCount :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CULong -> IO ()
setImageSequenceColumnCount scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setImageSequenceColumnCount:") retVoid [argCULong (fromIntegral value)]

-- | @- imageSequenceRowCount@
imageSequenceRowCount :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CULong
imageSequenceRowCount scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "imageSequenceRowCount") retCULong []

-- | @- setImageSequenceRowCount:@
setImageSequenceRowCount :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CULong -> IO ()
setImageSequenceRowCount scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setImageSequenceRowCount:") retVoid [argCULong (fromIntegral value)]

-- | @- imageSequenceInitialFrame@
imageSequenceInitialFrame :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
imageSequenceInitialFrame scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "imageSequenceInitialFrame") retCDouble []

-- | @- setImageSequenceInitialFrame:@
setImageSequenceInitialFrame :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setImageSequenceInitialFrame scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setImageSequenceInitialFrame:") retVoid [argCDouble (fromIntegral value)]

-- | @- imageSequenceInitialFrameVariation@
imageSequenceInitialFrameVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
imageSequenceInitialFrameVariation scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "imageSequenceInitialFrameVariation") retCDouble []

-- | @- setImageSequenceInitialFrameVariation:@
setImageSequenceInitialFrameVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setImageSequenceInitialFrameVariation scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setImageSequenceInitialFrameVariation:") retVoid [argCDouble (fromIntegral value)]

-- | @- imageSequenceFrameRate@
imageSequenceFrameRate :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
imageSequenceFrameRate scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "imageSequenceFrameRate") retCDouble []

-- | @- setImageSequenceFrameRate:@
setImageSequenceFrameRate :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setImageSequenceFrameRate scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setImageSequenceFrameRate:") retVoid [argCDouble (fromIntegral value)]

-- | @- imageSequenceFrameRateVariation@
imageSequenceFrameRateVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
imageSequenceFrameRateVariation scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "imageSequenceFrameRateVariation") retCDouble []

-- | @- setImageSequenceFrameRateVariation:@
setImageSequenceFrameRateVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setImageSequenceFrameRateVariation scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setImageSequenceFrameRateVariation:") retVoid [argCDouble (fromIntegral value)]

-- | @- imageSequenceAnimationMode@
imageSequenceAnimationMode :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO SCNParticleImageSequenceAnimationMode
imageSequenceAnimationMode scnParticleSystem  =
  fmap (coerce :: CLong -> SCNParticleImageSequenceAnimationMode) $ sendMsg scnParticleSystem (mkSelector "imageSequenceAnimationMode") retCLong []

-- | @- setImageSequenceAnimationMode:@
setImageSequenceAnimationMode :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> SCNParticleImageSequenceAnimationMode -> IO ()
setImageSequenceAnimationMode scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setImageSequenceAnimationMode:") retVoid [argCLong (coerce value)]

-- | @- particleColor@
particleColor :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO (Id NSColor)
particleColor scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "particleColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setParticleColor:@
setParticleColor :: (IsSCNParticleSystem scnParticleSystem, IsNSColor value) => scnParticleSystem -> value -> IO ()
setParticleColor scnParticleSystem  value =
withObjCPtr value $ \raw_value ->
    sendMsg scnParticleSystem (mkSelector "setParticleColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- particleColorVariation@
particleColorVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO SCNVector4
particleColorVariation scnParticleSystem  =
  sendMsgStret scnParticleSystem (mkSelector "particleColorVariation") retSCNVector4 []

-- | @- setParticleColorVariation:@
setParticleColorVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> SCNVector4 -> IO ()
setParticleColorVariation scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setParticleColorVariation:") retVoid [argSCNVector4 value]

-- | @- particleSize@
particleSize :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
particleSize scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "particleSize") retCDouble []

-- | @- setParticleSize:@
setParticleSize :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setParticleSize scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setParticleSize:") retVoid [argCDouble (fromIntegral value)]

-- | @- particleSizeVariation@
particleSizeVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
particleSizeVariation scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "particleSizeVariation") retCDouble []

-- | @- setParticleSizeVariation:@
setParticleSizeVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setParticleSizeVariation scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setParticleSizeVariation:") retVoid [argCDouble (fromIntegral value)]

-- | @- particleIntensity@
particleIntensity :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
particleIntensity scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "particleIntensity") retCDouble []

-- | @- setParticleIntensity:@
setParticleIntensity :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setParticleIntensity scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setParticleIntensity:") retVoid [argCDouble (fromIntegral value)]

-- | @- particleIntensityVariation@
particleIntensityVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
particleIntensityVariation scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "particleIntensityVariation") retCDouble []

-- | @- setParticleIntensityVariation:@
setParticleIntensityVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setParticleIntensityVariation scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setParticleIntensityVariation:") retVoid [argCDouble (fromIntegral value)]

-- | @- blendMode@
blendMode :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO SCNParticleBlendMode
blendMode scnParticleSystem  =
  fmap (coerce :: CLong -> SCNParticleBlendMode) $ sendMsg scnParticleSystem (mkSelector "blendMode") retCLong []

-- | @- setBlendMode:@
setBlendMode :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> SCNParticleBlendMode -> IO ()
setBlendMode scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setBlendMode:") retVoid [argCLong (coerce value)]

-- | @- blackPassEnabled@
blackPassEnabled :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO Bool
blackPassEnabled scnParticleSystem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnParticleSystem (mkSelector "blackPassEnabled") retCULong []

-- | @- setBlackPassEnabled:@
setBlackPassEnabled :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> Bool -> IO ()
setBlackPassEnabled scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setBlackPassEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- orientationMode@
orientationMode :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO SCNParticleOrientationMode
orientationMode scnParticleSystem  =
  fmap (coerce :: CLong -> SCNParticleOrientationMode) $ sendMsg scnParticleSystem (mkSelector "orientationMode") retCLong []

-- | @- setOrientationMode:@
setOrientationMode :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> SCNParticleOrientationMode -> IO ()
setOrientationMode scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setOrientationMode:") retVoid [argCLong (coerce value)]

-- | @- sortingMode@
sortingMode :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO SCNParticleSortingMode
sortingMode scnParticleSystem  =
  fmap (coerce :: CLong -> SCNParticleSortingMode) $ sendMsg scnParticleSystem (mkSelector "sortingMode") retCLong []

-- | @- setSortingMode:@
setSortingMode :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> SCNParticleSortingMode -> IO ()
setSortingMode scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setSortingMode:") retVoid [argCLong (coerce value)]

-- | @- lightingEnabled@
lightingEnabled :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO Bool
lightingEnabled scnParticleSystem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnParticleSystem (mkSelector "lightingEnabled") retCULong []

-- | @- setLightingEnabled:@
setLightingEnabled :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> Bool -> IO ()
setLightingEnabled scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setLightingEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- affectedByGravity@
affectedByGravity :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO Bool
affectedByGravity scnParticleSystem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnParticleSystem (mkSelector "affectedByGravity") retCULong []

-- | @- setAffectedByGravity:@
setAffectedByGravity :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> Bool -> IO ()
setAffectedByGravity scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setAffectedByGravity:") retVoid [argCULong (if value then 1 else 0)]

-- | @- affectedByPhysicsFields@
affectedByPhysicsFields :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO Bool
affectedByPhysicsFields scnParticleSystem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnParticleSystem (mkSelector "affectedByPhysicsFields") retCULong []

-- | @- setAffectedByPhysicsFields:@
setAffectedByPhysicsFields :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> Bool -> IO ()
setAffectedByPhysicsFields scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setAffectedByPhysicsFields:") retVoid [argCULong (if value then 1 else 0)]

-- | @- particleDiesOnCollision@
particleDiesOnCollision :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO Bool
particleDiesOnCollision scnParticleSystem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnParticleSystem (mkSelector "particleDiesOnCollision") retCULong []

-- | @- setParticleDiesOnCollision:@
setParticleDiesOnCollision :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> Bool -> IO ()
setParticleDiesOnCollision scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setParticleDiesOnCollision:") retVoid [argCULong (if value then 1 else 0)]

-- | @- colliderNodes@
colliderNodes :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO (Id NSArray)
colliderNodes scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "colliderNodes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setColliderNodes:@
setColliderNodes :: (IsSCNParticleSystem scnParticleSystem, IsNSArray value) => scnParticleSystem -> value -> IO ()
setColliderNodes scnParticleSystem  value =
withObjCPtr value $ \raw_value ->
    sendMsg scnParticleSystem (mkSelector "setColliderNodes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- particleMass@
particleMass :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
particleMass scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "particleMass") retCDouble []

-- | @- setParticleMass:@
setParticleMass :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setParticleMass scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setParticleMass:") retVoid [argCDouble (fromIntegral value)]

-- | @- particleMassVariation@
particleMassVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
particleMassVariation scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "particleMassVariation") retCDouble []

-- | @- setParticleMassVariation:@
setParticleMassVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setParticleMassVariation scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setParticleMassVariation:") retVoid [argCDouble (fromIntegral value)]

-- | @- particleBounce@
particleBounce :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
particleBounce scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "particleBounce") retCDouble []

-- | @- setParticleBounce:@
setParticleBounce :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setParticleBounce scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setParticleBounce:") retVoid [argCDouble (fromIntegral value)]

-- | @- particleBounceVariation@
particleBounceVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
particleBounceVariation scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "particleBounceVariation") retCDouble []

-- | @- setParticleBounceVariation:@
setParticleBounceVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setParticleBounceVariation scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setParticleBounceVariation:") retVoid [argCDouble (fromIntegral value)]

-- | @- particleFriction@
particleFriction :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
particleFriction scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "particleFriction") retCDouble []

-- | @- setParticleFriction:@
setParticleFriction :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setParticleFriction scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setParticleFriction:") retVoid [argCDouble (fromIntegral value)]

-- | @- particleFrictionVariation@
particleFrictionVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
particleFrictionVariation scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "particleFrictionVariation") retCDouble []

-- | @- setParticleFrictionVariation:@
setParticleFrictionVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setParticleFrictionVariation scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setParticleFrictionVariation:") retVoid [argCDouble (fromIntegral value)]

-- | @- particleCharge@
particleCharge :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
particleCharge scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "particleCharge") retCDouble []

-- | @- setParticleCharge:@
setParticleCharge :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setParticleCharge scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setParticleCharge:") retVoid [argCDouble (fromIntegral value)]

-- | @- particleChargeVariation@
particleChargeVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
particleChargeVariation scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "particleChargeVariation") retCDouble []

-- | @- setParticleChargeVariation:@
setParticleChargeVariation :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setParticleChargeVariation scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setParticleChargeVariation:") retVoid [argCDouble (fromIntegral value)]

-- | @- dampingFactor@
dampingFactor :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
dampingFactor scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "dampingFactor") retCDouble []

-- | @- setDampingFactor:@
setDampingFactor :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setDampingFactor scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setDampingFactor:") retVoid [argCDouble (fromIntegral value)]

-- | @- speedFactor@
speedFactor :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
speedFactor scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "speedFactor") retCDouble []

-- | @- setSpeedFactor:@
setSpeedFactor :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setSpeedFactor scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setSpeedFactor:") retVoid [argCDouble (fromIntegral value)]

-- | @- stretchFactor@
stretchFactor :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
stretchFactor scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "stretchFactor") retCDouble []

-- | @- setStretchFactor:@
setStretchFactor :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setStretchFactor scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setStretchFactor:") retVoid [argCDouble (fromIntegral value)]

-- | @- fresnelExponent@
fresnelExponent :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO CDouble
fresnelExponent scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "fresnelExponent") retCDouble []

-- | @- setFresnelExponent:@
setFresnelExponent :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> CDouble -> IO ()
setFresnelExponent scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setFresnelExponent:") retVoid [argCDouble (fromIntegral value)]

-- | writeToDepthBuffer
--
-- Determines whether the receiver writes to the depth buffer when rendered. Defaults to NO.
--
-- ObjC selector: @- writesToDepthBuffer@
writesToDepthBuffer :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO Bool
writesToDepthBuffer scnParticleSystem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnParticleSystem (mkSelector "writesToDepthBuffer") retCULong []

-- | writeToDepthBuffer
--
-- Determines whether the receiver writes to the depth buffer when rendered. Defaults to NO.
--
-- ObjC selector: @- setWritesToDepthBuffer:@
setWritesToDepthBuffer :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> Bool -> IO ()
setWritesToDepthBuffer scnParticleSystem  value =
  sendMsg scnParticleSystem (mkSelector "setWritesToDepthBuffer:") retVoid [argCULong (if value then 1 else 0)]

-- | @- propertyControllers@
propertyControllers :: IsSCNParticleSystem scnParticleSystem => scnParticleSystem -> IO (Id NSDictionary)
propertyControllers scnParticleSystem  =
  sendMsg scnParticleSystem (mkSelector "propertyControllers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPropertyControllers:@
setPropertyControllers :: (IsSCNParticleSystem scnParticleSystem, IsNSDictionary value) => scnParticleSystem -> value -> IO ()
setPropertyControllers scnParticleSystem  value =
withObjCPtr value $ \raw_value ->
    sendMsg scnParticleSystem (mkSelector "setPropertyControllers:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @particleSystem@
particleSystemSelector :: Selector
particleSystemSelector = mkSelector "particleSystem"

-- | @Selector@ for @particleSystemNamed:inDirectory:@
particleSystemNamed_inDirectorySelector :: Selector
particleSystemNamed_inDirectorySelector = mkSelector "particleSystemNamed:inDirectory:"

-- | @Selector@ for @reset@
resetSelector :: Selector
resetSelector = mkSelector "reset"

-- | @Selector@ for @handleEvent:forProperties:withBlock:@
handleEvent_forProperties_withBlockSelector :: Selector
handleEvent_forProperties_withBlockSelector = mkSelector "handleEvent:forProperties:withBlock:"

-- | @Selector@ for @addModifierForProperties:atStage:withBlock:@
addModifierForProperties_atStage_withBlockSelector :: Selector
addModifierForProperties_atStage_withBlockSelector = mkSelector "addModifierForProperties:atStage:withBlock:"

-- | @Selector@ for @removeModifiersOfStage:@
removeModifiersOfStageSelector :: Selector
removeModifiersOfStageSelector = mkSelector "removeModifiersOfStage:"

-- | @Selector@ for @removeAllModifiers@
removeAllModifiersSelector :: Selector
removeAllModifiersSelector = mkSelector "removeAllModifiers"

-- | @Selector@ for @emissionDuration@
emissionDurationSelector :: Selector
emissionDurationSelector = mkSelector "emissionDuration"

-- | @Selector@ for @setEmissionDuration:@
setEmissionDurationSelector :: Selector
setEmissionDurationSelector = mkSelector "setEmissionDuration:"

-- | @Selector@ for @emissionDurationVariation@
emissionDurationVariationSelector :: Selector
emissionDurationVariationSelector = mkSelector "emissionDurationVariation"

-- | @Selector@ for @setEmissionDurationVariation:@
setEmissionDurationVariationSelector :: Selector
setEmissionDurationVariationSelector = mkSelector "setEmissionDurationVariation:"

-- | @Selector@ for @idleDuration@
idleDurationSelector :: Selector
idleDurationSelector = mkSelector "idleDuration"

-- | @Selector@ for @setIdleDuration:@
setIdleDurationSelector :: Selector
setIdleDurationSelector = mkSelector "setIdleDuration:"

-- | @Selector@ for @idleDurationVariation@
idleDurationVariationSelector :: Selector
idleDurationVariationSelector = mkSelector "idleDurationVariation"

-- | @Selector@ for @setIdleDurationVariation:@
setIdleDurationVariationSelector :: Selector
setIdleDurationVariationSelector = mkSelector "setIdleDurationVariation:"

-- | @Selector@ for @loops@
loopsSelector :: Selector
loopsSelector = mkSelector "loops"

-- | @Selector@ for @setLoops:@
setLoopsSelector :: Selector
setLoopsSelector = mkSelector "setLoops:"

-- | @Selector@ for @birthRate@
birthRateSelector :: Selector
birthRateSelector = mkSelector "birthRate"

-- | @Selector@ for @setBirthRate:@
setBirthRateSelector :: Selector
setBirthRateSelector = mkSelector "setBirthRate:"

-- | @Selector@ for @birthRateVariation@
birthRateVariationSelector :: Selector
birthRateVariationSelector = mkSelector "birthRateVariation"

-- | @Selector@ for @setBirthRateVariation:@
setBirthRateVariationSelector :: Selector
setBirthRateVariationSelector = mkSelector "setBirthRateVariation:"

-- | @Selector@ for @warmupDuration@
warmupDurationSelector :: Selector
warmupDurationSelector = mkSelector "warmupDuration"

-- | @Selector@ for @setWarmupDuration:@
setWarmupDurationSelector :: Selector
setWarmupDurationSelector = mkSelector "setWarmupDuration:"

-- | @Selector@ for @emitterShape@
emitterShapeSelector :: Selector
emitterShapeSelector = mkSelector "emitterShape"

-- | @Selector@ for @setEmitterShape:@
setEmitterShapeSelector :: Selector
setEmitterShapeSelector = mkSelector "setEmitterShape:"

-- | @Selector@ for @birthLocation@
birthLocationSelector :: Selector
birthLocationSelector = mkSelector "birthLocation"

-- | @Selector@ for @setBirthLocation:@
setBirthLocationSelector :: Selector
setBirthLocationSelector = mkSelector "setBirthLocation:"

-- | @Selector@ for @birthDirection@
birthDirectionSelector :: Selector
birthDirectionSelector = mkSelector "birthDirection"

-- | @Selector@ for @setBirthDirection:@
setBirthDirectionSelector :: Selector
setBirthDirectionSelector = mkSelector "setBirthDirection:"

-- | @Selector@ for @spreadingAngle@
spreadingAngleSelector :: Selector
spreadingAngleSelector = mkSelector "spreadingAngle"

-- | @Selector@ for @setSpreadingAngle:@
setSpreadingAngleSelector :: Selector
setSpreadingAngleSelector = mkSelector "setSpreadingAngle:"

-- | @Selector@ for @emittingDirection@
emittingDirectionSelector :: Selector
emittingDirectionSelector = mkSelector "emittingDirection"

-- | @Selector@ for @setEmittingDirection:@
setEmittingDirectionSelector :: Selector
setEmittingDirectionSelector = mkSelector "setEmittingDirection:"

-- | @Selector@ for @orientationDirection@
orientationDirectionSelector :: Selector
orientationDirectionSelector = mkSelector "orientationDirection"

-- | @Selector@ for @setOrientationDirection:@
setOrientationDirectionSelector :: Selector
setOrientationDirectionSelector = mkSelector "setOrientationDirection:"

-- | @Selector@ for @acceleration@
accelerationSelector :: Selector
accelerationSelector = mkSelector "acceleration"

-- | @Selector@ for @setAcceleration:@
setAccelerationSelector :: Selector
setAccelerationSelector = mkSelector "setAcceleration:"

-- | @Selector@ for @local@
localSelector :: Selector
localSelector = mkSelector "local"

-- | @Selector@ for @setLocal:@
setLocalSelector :: Selector
setLocalSelector = mkSelector "setLocal:"

-- | @Selector@ for @particleAngle@
particleAngleSelector :: Selector
particleAngleSelector = mkSelector "particleAngle"

-- | @Selector@ for @setParticleAngle:@
setParticleAngleSelector :: Selector
setParticleAngleSelector = mkSelector "setParticleAngle:"

-- | @Selector@ for @particleAngleVariation@
particleAngleVariationSelector :: Selector
particleAngleVariationSelector = mkSelector "particleAngleVariation"

-- | @Selector@ for @setParticleAngleVariation:@
setParticleAngleVariationSelector :: Selector
setParticleAngleVariationSelector = mkSelector "setParticleAngleVariation:"

-- | @Selector@ for @particleVelocity@
particleVelocitySelector :: Selector
particleVelocitySelector = mkSelector "particleVelocity"

-- | @Selector@ for @setParticleVelocity:@
setParticleVelocitySelector :: Selector
setParticleVelocitySelector = mkSelector "setParticleVelocity:"

-- | @Selector@ for @particleVelocityVariation@
particleVelocityVariationSelector :: Selector
particleVelocityVariationSelector = mkSelector "particleVelocityVariation"

-- | @Selector@ for @setParticleVelocityVariation:@
setParticleVelocityVariationSelector :: Selector
setParticleVelocityVariationSelector = mkSelector "setParticleVelocityVariation:"

-- | @Selector@ for @particleAngularVelocity@
particleAngularVelocitySelector :: Selector
particleAngularVelocitySelector = mkSelector "particleAngularVelocity"

-- | @Selector@ for @setParticleAngularVelocity:@
setParticleAngularVelocitySelector :: Selector
setParticleAngularVelocitySelector = mkSelector "setParticleAngularVelocity:"

-- | @Selector@ for @particleAngularVelocityVariation@
particleAngularVelocityVariationSelector :: Selector
particleAngularVelocityVariationSelector = mkSelector "particleAngularVelocityVariation"

-- | @Selector@ for @setParticleAngularVelocityVariation:@
setParticleAngularVelocityVariationSelector :: Selector
setParticleAngularVelocityVariationSelector = mkSelector "setParticleAngularVelocityVariation:"

-- | @Selector@ for @particleLifeSpan@
particleLifeSpanSelector :: Selector
particleLifeSpanSelector = mkSelector "particleLifeSpan"

-- | @Selector@ for @setParticleLifeSpan:@
setParticleLifeSpanSelector :: Selector
setParticleLifeSpanSelector = mkSelector "setParticleLifeSpan:"

-- | @Selector@ for @particleLifeSpanVariation@
particleLifeSpanVariationSelector :: Selector
particleLifeSpanVariationSelector = mkSelector "particleLifeSpanVariation"

-- | @Selector@ for @setParticleLifeSpanVariation:@
setParticleLifeSpanVariationSelector :: Selector
setParticleLifeSpanVariationSelector = mkSelector "setParticleLifeSpanVariation:"

-- | @Selector@ for @systemSpawnedOnDying@
systemSpawnedOnDyingSelector :: Selector
systemSpawnedOnDyingSelector = mkSelector "systemSpawnedOnDying"

-- | @Selector@ for @setSystemSpawnedOnDying:@
setSystemSpawnedOnDyingSelector :: Selector
setSystemSpawnedOnDyingSelector = mkSelector "setSystemSpawnedOnDying:"

-- | @Selector@ for @systemSpawnedOnCollision@
systemSpawnedOnCollisionSelector :: Selector
systemSpawnedOnCollisionSelector = mkSelector "systemSpawnedOnCollision"

-- | @Selector@ for @setSystemSpawnedOnCollision:@
setSystemSpawnedOnCollisionSelector :: Selector
setSystemSpawnedOnCollisionSelector = mkSelector "setSystemSpawnedOnCollision:"

-- | @Selector@ for @systemSpawnedOnLiving@
systemSpawnedOnLivingSelector :: Selector
systemSpawnedOnLivingSelector = mkSelector "systemSpawnedOnLiving"

-- | @Selector@ for @setSystemSpawnedOnLiving:@
setSystemSpawnedOnLivingSelector :: Selector
setSystemSpawnedOnLivingSelector = mkSelector "setSystemSpawnedOnLiving:"

-- | @Selector@ for @particleImage@
particleImageSelector :: Selector
particleImageSelector = mkSelector "particleImage"

-- | @Selector@ for @setParticleImage:@
setParticleImageSelector :: Selector
setParticleImageSelector = mkSelector "setParticleImage:"

-- | @Selector@ for @imageSequenceColumnCount@
imageSequenceColumnCountSelector :: Selector
imageSequenceColumnCountSelector = mkSelector "imageSequenceColumnCount"

-- | @Selector@ for @setImageSequenceColumnCount:@
setImageSequenceColumnCountSelector :: Selector
setImageSequenceColumnCountSelector = mkSelector "setImageSequenceColumnCount:"

-- | @Selector@ for @imageSequenceRowCount@
imageSequenceRowCountSelector :: Selector
imageSequenceRowCountSelector = mkSelector "imageSequenceRowCount"

-- | @Selector@ for @setImageSequenceRowCount:@
setImageSequenceRowCountSelector :: Selector
setImageSequenceRowCountSelector = mkSelector "setImageSequenceRowCount:"

-- | @Selector@ for @imageSequenceInitialFrame@
imageSequenceInitialFrameSelector :: Selector
imageSequenceInitialFrameSelector = mkSelector "imageSequenceInitialFrame"

-- | @Selector@ for @setImageSequenceInitialFrame:@
setImageSequenceInitialFrameSelector :: Selector
setImageSequenceInitialFrameSelector = mkSelector "setImageSequenceInitialFrame:"

-- | @Selector@ for @imageSequenceInitialFrameVariation@
imageSequenceInitialFrameVariationSelector :: Selector
imageSequenceInitialFrameVariationSelector = mkSelector "imageSequenceInitialFrameVariation"

-- | @Selector@ for @setImageSequenceInitialFrameVariation:@
setImageSequenceInitialFrameVariationSelector :: Selector
setImageSequenceInitialFrameVariationSelector = mkSelector "setImageSequenceInitialFrameVariation:"

-- | @Selector@ for @imageSequenceFrameRate@
imageSequenceFrameRateSelector :: Selector
imageSequenceFrameRateSelector = mkSelector "imageSequenceFrameRate"

-- | @Selector@ for @setImageSequenceFrameRate:@
setImageSequenceFrameRateSelector :: Selector
setImageSequenceFrameRateSelector = mkSelector "setImageSequenceFrameRate:"

-- | @Selector@ for @imageSequenceFrameRateVariation@
imageSequenceFrameRateVariationSelector :: Selector
imageSequenceFrameRateVariationSelector = mkSelector "imageSequenceFrameRateVariation"

-- | @Selector@ for @setImageSequenceFrameRateVariation:@
setImageSequenceFrameRateVariationSelector :: Selector
setImageSequenceFrameRateVariationSelector = mkSelector "setImageSequenceFrameRateVariation:"

-- | @Selector@ for @imageSequenceAnimationMode@
imageSequenceAnimationModeSelector :: Selector
imageSequenceAnimationModeSelector = mkSelector "imageSequenceAnimationMode"

-- | @Selector@ for @setImageSequenceAnimationMode:@
setImageSequenceAnimationModeSelector :: Selector
setImageSequenceAnimationModeSelector = mkSelector "setImageSequenceAnimationMode:"

-- | @Selector@ for @particleColor@
particleColorSelector :: Selector
particleColorSelector = mkSelector "particleColor"

-- | @Selector@ for @setParticleColor:@
setParticleColorSelector :: Selector
setParticleColorSelector = mkSelector "setParticleColor:"

-- | @Selector@ for @particleColorVariation@
particleColorVariationSelector :: Selector
particleColorVariationSelector = mkSelector "particleColorVariation"

-- | @Selector@ for @setParticleColorVariation:@
setParticleColorVariationSelector :: Selector
setParticleColorVariationSelector = mkSelector "setParticleColorVariation:"

-- | @Selector@ for @particleSize@
particleSizeSelector :: Selector
particleSizeSelector = mkSelector "particleSize"

-- | @Selector@ for @setParticleSize:@
setParticleSizeSelector :: Selector
setParticleSizeSelector = mkSelector "setParticleSize:"

-- | @Selector@ for @particleSizeVariation@
particleSizeVariationSelector :: Selector
particleSizeVariationSelector = mkSelector "particleSizeVariation"

-- | @Selector@ for @setParticleSizeVariation:@
setParticleSizeVariationSelector :: Selector
setParticleSizeVariationSelector = mkSelector "setParticleSizeVariation:"

-- | @Selector@ for @particleIntensity@
particleIntensitySelector :: Selector
particleIntensitySelector = mkSelector "particleIntensity"

-- | @Selector@ for @setParticleIntensity:@
setParticleIntensitySelector :: Selector
setParticleIntensitySelector = mkSelector "setParticleIntensity:"

-- | @Selector@ for @particleIntensityVariation@
particleIntensityVariationSelector :: Selector
particleIntensityVariationSelector = mkSelector "particleIntensityVariation"

-- | @Selector@ for @setParticleIntensityVariation:@
setParticleIntensityVariationSelector :: Selector
setParticleIntensityVariationSelector = mkSelector "setParticleIntensityVariation:"

-- | @Selector@ for @blendMode@
blendModeSelector :: Selector
blendModeSelector = mkSelector "blendMode"

-- | @Selector@ for @setBlendMode:@
setBlendModeSelector :: Selector
setBlendModeSelector = mkSelector "setBlendMode:"

-- | @Selector@ for @blackPassEnabled@
blackPassEnabledSelector :: Selector
blackPassEnabledSelector = mkSelector "blackPassEnabled"

-- | @Selector@ for @setBlackPassEnabled:@
setBlackPassEnabledSelector :: Selector
setBlackPassEnabledSelector = mkSelector "setBlackPassEnabled:"

-- | @Selector@ for @orientationMode@
orientationModeSelector :: Selector
orientationModeSelector = mkSelector "orientationMode"

-- | @Selector@ for @setOrientationMode:@
setOrientationModeSelector :: Selector
setOrientationModeSelector = mkSelector "setOrientationMode:"

-- | @Selector@ for @sortingMode@
sortingModeSelector :: Selector
sortingModeSelector = mkSelector "sortingMode"

-- | @Selector@ for @setSortingMode:@
setSortingModeSelector :: Selector
setSortingModeSelector = mkSelector "setSortingMode:"

-- | @Selector@ for @lightingEnabled@
lightingEnabledSelector :: Selector
lightingEnabledSelector = mkSelector "lightingEnabled"

-- | @Selector@ for @setLightingEnabled:@
setLightingEnabledSelector :: Selector
setLightingEnabledSelector = mkSelector "setLightingEnabled:"

-- | @Selector@ for @affectedByGravity@
affectedByGravitySelector :: Selector
affectedByGravitySelector = mkSelector "affectedByGravity"

-- | @Selector@ for @setAffectedByGravity:@
setAffectedByGravitySelector :: Selector
setAffectedByGravitySelector = mkSelector "setAffectedByGravity:"

-- | @Selector@ for @affectedByPhysicsFields@
affectedByPhysicsFieldsSelector :: Selector
affectedByPhysicsFieldsSelector = mkSelector "affectedByPhysicsFields"

-- | @Selector@ for @setAffectedByPhysicsFields:@
setAffectedByPhysicsFieldsSelector :: Selector
setAffectedByPhysicsFieldsSelector = mkSelector "setAffectedByPhysicsFields:"

-- | @Selector@ for @particleDiesOnCollision@
particleDiesOnCollisionSelector :: Selector
particleDiesOnCollisionSelector = mkSelector "particleDiesOnCollision"

-- | @Selector@ for @setParticleDiesOnCollision:@
setParticleDiesOnCollisionSelector :: Selector
setParticleDiesOnCollisionSelector = mkSelector "setParticleDiesOnCollision:"

-- | @Selector@ for @colliderNodes@
colliderNodesSelector :: Selector
colliderNodesSelector = mkSelector "colliderNodes"

-- | @Selector@ for @setColliderNodes:@
setColliderNodesSelector :: Selector
setColliderNodesSelector = mkSelector "setColliderNodes:"

-- | @Selector@ for @particleMass@
particleMassSelector :: Selector
particleMassSelector = mkSelector "particleMass"

-- | @Selector@ for @setParticleMass:@
setParticleMassSelector :: Selector
setParticleMassSelector = mkSelector "setParticleMass:"

-- | @Selector@ for @particleMassVariation@
particleMassVariationSelector :: Selector
particleMassVariationSelector = mkSelector "particleMassVariation"

-- | @Selector@ for @setParticleMassVariation:@
setParticleMassVariationSelector :: Selector
setParticleMassVariationSelector = mkSelector "setParticleMassVariation:"

-- | @Selector@ for @particleBounce@
particleBounceSelector :: Selector
particleBounceSelector = mkSelector "particleBounce"

-- | @Selector@ for @setParticleBounce:@
setParticleBounceSelector :: Selector
setParticleBounceSelector = mkSelector "setParticleBounce:"

-- | @Selector@ for @particleBounceVariation@
particleBounceVariationSelector :: Selector
particleBounceVariationSelector = mkSelector "particleBounceVariation"

-- | @Selector@ for @setParticleBounceVariation:@
setParticleBounceVariationSelector :: Selector
setParticleBounceVariationSelector = mkSelector "setParticleBounceVariation:"

-- | @Selector@ for @particleFriction@
particleFrictionSelector :: Selector
particleFrictionSelector = mkSelector "particleFriction"

-- | @Selector@ for @setParticleFriction:@
setParticleFrictionSelector :: Selector
setParticleFrictionSelector = mkSelector "setParticleFriction:"

-- | @Selector@ for @particleFrictionVariation@
particleFrictionVariationSelector :: Selector
particleFrictionVariationSelector = mkSelector "particleFrictionVariation"

-- | @Selector@ for @setParticleFrictionVariation:@
setParticleFrictionVariationSelector :: Selector
setParticleFrictionVariationSelector = mkSelector "setParticleFrictionVariation:"

-- | @Selector@ for @particleCharge@
particleChargeSelector :: Selector
particleChargeSelector = mkSelector "particleCharge"

-- | @Selector@ for @setParticleCharge:@
setParticleChargeSelector :: Selector
setParticleChargeSelector = mkSelector "setParticleCharge:"

-- | @Selector@ for @particleChargeVariation@
particleChargeVariationSelector :: Selector
particleChargeVariationSelector = mkSelector "particleChargeVariation"

-- | @Selector@ for @setParticleChargeVariation:@
setParticleChargeVariationSelector :: Selector
setParticleChargeVariationSelector = mkSelector "setParticleChargeVariation:"

-- | @Selector@ for @dampingFactor@
dampingFactorSelector :: Selector
dampingFactorSelector = mkSelector "dampingFactor"

-- | @Selector@ for @setDampingFactor:@
setDampingFactorSelector :: Selector
setDampingFactorSelector = mkSelector "setDampingFactor:"

-- | @Selector@ for @speedFactor@
speedFactorSelector :: Selector
speedFactorSelector = mkSelector "speedFactor"

-- | @Selector@ for @setSpeedFactor:@
setSpeedFactorSelector :: Selector
setSpeedFactorSelector = mkSelector "setSpeedFactor:"

-- | @Selector@ for @stretchFactor@
stretchFactorSelector :: Selector
stretchFactorSelector = mkSelector "stretchFactor"

-- | @Selector@ for @setStretchFactor:@
setStretchFactorSelector :: Selector
setStretchFactorSelector = mkSelector "setStretchFactor:"

-- | @Selector@ for @fresnelExponent@
fresnelExponentSelector :: Selector
fresnelExponentSelector = mkSelector "fresnelExponent"

-- | @Selector@ for @setFresnelExponent:@
setFresnelExponentSelector :: Selector
setFresnelExponentSelector = mkSelector "setFresnelExponent:"

-- | @Selector@ for @writesToDepthBuffer@
writesToDepthBufferSelector :: Selector
writesToDepthBufferSelector = mkSelector "writesToDepthBuffer"

-- | @Selector@ for @setWritesToDepthBuffer:@
setWritesToDepthBufferSelector :: Selector
setWritesToDepthBufferSelector = mkSelector "setWritesToDepthBuffer:"

-- | @Selector@ for @propertyControllers@
propertyControllersSelector :: Selector
propertyControllersSelector = mkSelector "propertyControllers"

-- | @Selector@ for @setPropertyControllers:@
setPropertyControllersSelector :: Selector
setPropertyControllersSelector = mkSelector "setPropertyControllers:"

