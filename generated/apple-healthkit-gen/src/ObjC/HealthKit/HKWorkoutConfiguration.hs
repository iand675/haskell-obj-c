{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKWorkoutConfiguration
--
-- An HKWorkoutConfiguration is an object that can be used to describe the workout activity.
--
-- Generated bindings for @HKWorkoutConfiguration@.
module ObjC.HealthKit.HKWorkoutConfiguration
  ( HKWorkoutConfiguration
  , IsHKWorkoutConfiguration(..)
  , activityType
  , setActivityType
  , locationType
  , setLocationType
  , swimmingLocationType
  , setSwimmingLocationType
  , lapLength
  , setLapLength
  , activityTypeSelector
  , lapLengthSelector
  , locationTypeSelector
  , setActivityTypeSelector
  , setLapLengthSelector
  , setLocationTypeSelector
  , setSwimmingLocationTypeSelector
  , swimmingLocationTypeSelector

  -- * Enum types
  , HKWorkoutActivityType(HKWorkoutActivityType)
  , pattern HKWorkoutActivityTypeAmericanFootball
  , pattern HKWorkoutActivityTypeArchery
  , pattern HKWorkoutActivityTypeAustralianFootball
  , pattern HKWorkoutActivityTypeBadminton
  , pattern HKWorkoutActivityTypeBaseball
  , pattern HKWorkoutActivityTypeBasketball
  , pattern HKWorkoutActivityTypeBowling
  , pattern HKWorkoutActivityTypeBoxing
  , pattern HKWorkoutActivityTypeClimbing
  , pattern HKWorkoutActivityTypeCricket
  , pattern HKWorkoutActivityTypeCrossTraining
  , pattern HKWorkoutActivityTypeCurling
  , pattern HKWorkoutActivityTypeCycling
  , pattern HKWorkoutActivityTypeDance
  , pattern HKWorkoutActivityTypeDanceInspiredTraining
  , pattern HKWorkoutActivityTypeElliptical
  , pattern HKWorkoutActivityTypeEquestrianSports
  , pattern HKWorkoutActivityTypeFencing
  , pattern HKWorkoutActivityTypeFishing
  , pattern HKWorkoutActivityTypeFunctionalStrengthTraining
  , pattern HKWorkoutActivityTypeGolf
  , pattern HKWorkoutActivityTypeGymnastics
  , pattern HKWorkoutActivityTypeHandball
  , pattern HKWorkoutActivityTypeHiking
  , pattern HKWorkoutActivityTypeHockey
  , pattern HKWorkoutActivityTypeHunting
  , pattern HKWorkoutActivityTypeLacrosse
  , pattern HKWorkoutActivityTypeMartialArts
  , pattern HKWorkoutActivityTypeMindAndBody
  , pattern HKWorkoutActivityTypeMixedMetabolicCardioTraining
  , pattern HKWorkoutActivityTypePaddleSports
  , pattern HKWorkoutActivityTypePlay
  , pattern HKWorkoutActivityTypePreparationAndRecovery
  , pattern HKWorkoutActivityTypeRacquetball
  , pattern HKWorkoutActivityTypeRowing
  , pattern HKWorkoutActivityTypeRugby
  , pattern HKWorkoutActivityTypeRunning
  , pattern HKWorkoutActivityTypeSailing
  , pattern HKWorkoutActivityTypeSkatingSports
  , pattern HKWorkoutActivityTypeSnowSports
  , pattern HKWorkoutActivityTypeSoccer
  , pattern HKWorkoutActivityTypeSoftball
  , pattern HKWorkoutActivityTypeSquash
  , pattern HKWorkoutActivityTypeStairClimbing
  , pattern HKWorkoutActivityTypeSurfingSports
  , pattern HKWorkoutActivityTypeSwimming
  , pattern HKWorkoutActivityTypeTableTennis
  , pattern HKWorkoutActivityTypeTennis
  , pattern HKWorkoutActivityTypeTrackAndField
  , pattern HKWorkoutActivityTypeTraditionalStrengthTraining
  , pattern HKWorkoutActivityTypeVolleyball
  , pattern HKWorkoutActivityTypeWalking
  , pattern HKWorkoutActivityTypeWaterFitness
  , pattern HKWorkoutActivityTypeWaterPolo
  , pattern HKWorkoutActivityTypeWaterSports
  , pattern HKWorkoutActivityTypeWrestling
  , pattern HKWorkoutActivityTypeYoga
  , pattern HKWorkoutActivityTypeBarre
  , pattern HKWorkoutActivityTypeCoreTraining
  , pattern HKWorkoutActivityTypeCrossCountrySkiing
  , pattern HKWorkoutActivityTypeDownhillSkiing
  , pattern HKWorkoutActivityTypeFlexibility
  , pattern HKWorkoutActivityTypeHighIntensityIntervalTraining
  , pattern HKWorkoutActivityTypeJumpRope
  , pattern HKWorkoutActivityTypeKickboxing
  , pattern HKWorkoutActivityTypePilates
  , pattern HKWorkoutActivityTypeSnowboarding
  , pattern HKWorkoutActivityTypeStairs
  , pattern HKWorkoutActivityTypeStepTraining
  , pattern HKWorkoutActivityTypeWheelchairWalkPace
  , pattern HKWorkoutActivityTypeWheelchairRunPace
  , pattern HKWorkoutActivityTypeTaiChi
  , pattern HKWorkoutActivityTypeMixedCardio
  , pattern HKWorkoutActivityTypeHandCycling
  , pattern HKWorkoutActivityTypeDiscSports
  , pattern HKWorkoutActivityTypeFitnessGaming
  , pattern HKWorkoutActivityTypeCardioDance
  , pattern HKWorkoutActivityTypeSocialDance
  , pattern HKWorkoutActivityTypePickleball
  , pattern HKWorkoutActivityTypeCooldown
  , pattern HKWorkoutActivityTypeSwimBikeRun
  , pattern HKWorkoutActivityTypeTransition
  , pattern HKWorkoutActivityTypeUnderwaterDiving
  , pattern HKWorkoutActivityTypeOther
  , HKWorkoutSessionLocationType(HKWorkoutSessionLocationType)
  , pattern HKWorkoutSessionLocationTypeUnknown
  , pattern HKWorkoutSessionLocationTypeIndoor
  , pattern HKWorkoutSessionLocationTypeOutdoor
  , HKWorkoutSwimmingLocationType(HKWorkoutSwimmingLocationType)
  , pattern HKWorkoutSwimmingLocationTypeUnknown
  , pattern HKWorkoutSwimmingLocationTypePool
  , pattern HKWorkoutSwimmingLocationTypeOpenWater

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.HealthKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | activityType
--
-- Indicates the type of workout for the configuration.
--
-- ObjC selector: @- activityType@
activityType :: IsHKWorkoutConfiguration hkWorkoutConfiguration => hkWorkoutConfiguration -> IO HKWorkoutActivityType
activityType hkWorkoutConfiguration =
  sendMessage hkWorkoutConfiguration activityTypeSelector

-- | activityType
--
-- Indicates the type of workout for the configuration.
--
-- ObjC selector: @- setActivityType:@
setActivityType :: IsHKWorkoutConfiguration hkWorkoutConfiguration => hkWorkoutConfiguration -> HKWorkoutActivityType -> IO ()
setActivityType hkWorkoutConfiguration value =
  sendMessage hkWorkoutConfiguration setActivityTypeSelector value

-- | locationType
--
-- Indicates the type of location (indoors vs. outdoors) for the configuration.
--
-- ObjC selector: @- locationType@
locationType :: IsHKWorkoutConfiguration hkWorkoutConfiguration => hkWorkoutConfiguration -> IO HKWorkoutSessionLocationType
locationType hkWorkoutConfiguration =
  sendMessage hkWorkoutConfiguration locationTypeSelector

-- | locationType
--
-- Indicates the type of location (indoors vs. outdoors) for the configuration.
--
-- ObjC selector: @- setLocationType:@
setLocationType :: IsHKWorkoutConfiguration hkWorkoutConfiguration => hkWorkoutConfiguration -> HKWorkoutSessionLocationType -> IO ()
setLocationType hkWorkoutConfiguration value =
  sendMessage hkWorkoutConfiguration setLocationTypeSelector value

-- | swimmingLocationType
--
-- Indicates the type of swimming location (pool vs. open water) where the workout will take place.
--
-- ObjC selector: @- swimmingLocationType@
swimmingLocationType :: IsHKWorkoutConfiguration hkWorkoutConfiguration => hkWorkoutConfiguration -> IO HKWorkoutSwimmingLocationType
swimmingLocationType hkWorkoutConfiguration =
  sendMessage hkWorkoutConfiguration swimmingLocationTypeSelector

-- | swimmingLocationType
--
-- Indicates the type of swimming location (pool vs. open water) where the workout will take place.
--
-- ObjC selector: @- setSwimmingLocationType:@
setSwimmingLocationType :: IsHKWorkoutConfiguration hkWorkoutConfiguration => hkWorkoutConfiguration -> HKWorkoutSwimmingLocationType -> IO ()
setSwimmingLocationType hkWorkoutConfiguration value =
  sendMessage hkWorkoutConfiguration setSwimmingLocationTypeSelector value

-- | lapLength
--
-- Indicates the length of the pool, when the workout location type is pool.
--
-- This metric represents the length of the pool where the workout takes place. It should be a quantity with                a unit representing length.
--
-- ObjC selector: @- lapLength@
lapLength :: IsHKWorkoutConfiguration hkWorkoutConfiguration => hkWorkoutConfiguration -> IO (Id HKQuantity)
lapLength hkWorkoutConfiguration =
  sendMessage hkWorkoutConfiguration lapLengthSelector

-- | lapLength
--
-- Indicates the length of the pool, when the workout location type is pool.
--
-- This metric represents the length of the pool where the workout takes place. It should be a quantity with                a unit representing length.
--
-- ObjC selector: @- setLapLength:@
setLapLength :: (IsHKWorkoutConfiguration hkWorkoutConfiguration, IsHKQuantity value) => hkWorkoutConfiguration -> value -> IO ()
setLapLength hkWorkoutConfiguration value =
  sendMessage hkWorkoutConfiguration setLapLengthSelector (toHKQuantity value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @activityType@
activityTypeSelector :: Selector '[] HKWorkoutActivityType
activityTypeSelector = mkSelector "activityType"

-- | @Selector@ for @setActivityType:@
setActivityTypeSelector :: Selector '[HKWorkoutActivityType] ()
setActivityTypeSelector = mkSelector "setActivityType:"

-- | @Selector@ for @locationType@
locationTypeSelector :: Selector '[] HKWorkoutSessionLocationType
locationTypeSelector = mkSelector "locationType"

-- | @Selector@ for @setLocationType:@
setLocationTypeSelector :: Selector '[HKWorkoutSessionLocationType] ()
setLocationTypeSelector = mkSelector "setLocationType:"

-- | @Selector@ for @swimmingLocationType@
swimmingLocationTypeSelector :: Selector '[] HKWorkoutSwimmingLocationType
swimmingLocationTypeSelector = mkSelector "swimmingLocationType"

-- | @Selector@ for @setSwimmingLocationType:@
setSwimmingLocationTypeSelector :: Selector '[HKWorkoutSwimmingLocationType] ()
setSwimmingLocationTypeSelector = mkSelector "setSwimmingLocationType:"

-- | @Selector@ for @lapLength@
lapLengthSelector :: Selector '[] (Id HKQuantity)
lapLengthSelector = mkSelector "lapLength"

-- | @Selector@ for @setLapLength:@
setLapLengthSelector :: Selector '[Id HKQuantity] ()
setLapLengthSelector = mkSelector "setLapLength:"

