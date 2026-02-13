{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @HKWorkoutEffortRelationship@.
module ObjC.HealthKit.HKWorkoutEffortRelationship
  ( HKWorkoutEffortRelationship
  , IsHKWorkoutEffortRelationship(..)
  , workout
  , activity
  , samples
  , activitySelector
  , samplesSelector
  , workoutSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | workout
--
-- ObjC selector: @- workout@
workout :: IsHKWorkoutEffortRelationship hkWorkoutEffortRelationship => hkWorkoutEffortRelationship -> IO (Id HKWorkout)
workout hkWorkoutEffortRelationship =
  sendMessage hkWorkoutEffortRelationship workoutSelector

-- | activity
--
-- ObjC selector: @- activity@
activity :: IsHKWorkoutEffortRelationship hkWorkoutEffortRelationship => hkWorkoutEffortRelationship -> IO (Id HKWorkoutActivity)
activity hkWorkoutEffortRelationship =
  sendMessage hkWorkoutEffortRelationship activitySelector

-- | samples
--
-- The samples related to the workout but not any sub-activities
--
-- ObjC selector: @- samples@
samples :: IsHKWorkoutEffortRelationship hkWorkoutEffortRelationship => hkWorkoutEffortRelationship -> IO (Id NSArray)
samples hkWorkoutEffortRelationship =
  sendMessage hkWorkoutEffortRelationship samplesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @workout@
workoutSelector :: Selector '[] (Id HKWorkout)
workoutSelector = mkSelector "workout"

-- | @Selector@ for @activity@
activitySelector :: Selector '[] (Id HKWorkoutActivity)
activitySelector = mkSelector "activity"

-- | @Selector@ for @samples@
samplesSelector :: Selector '[] (Id NSArray)
samplesSelector = mkSelector "samples"

