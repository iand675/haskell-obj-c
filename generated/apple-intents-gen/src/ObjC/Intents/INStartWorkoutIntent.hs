{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INStartWorkoutIntent@.
module ObjC.Intents.INStartWorkoutIntent
  ( INStartWorkoutIntent
  , IsINStartWorkoutIntent(..)
  , initWithWorkoutName_goalValue_workoutGoalUnitType_workoutLocationType_isOpenEnded
  , workoutName
  , goalValue
  , workoutGoalUnitType
  , workoutLocationType
  , isOpenEnded
  , goalValueSelector
  , initWithWorkoutName_goalValue_workoutGoalUnitType_workoutLocationType_isOpenEndedSelector
  , isOpenEndedSelector
  , workoutGoalUnitTypeSelector
  , workoutLocationTypeSelector
  , workoutNameSelector

  -- * Enum types
  , INWorkoutGoalUnitType(INWorkoutGoalUnitType)
  , pattern INWorkoutGoalUnitTypeUnknown
  , pattern INWorkoutGoalUnitTypeInch
  , pattern INWorkoutGoalUnitTypeMeter
  , pattern INWorkoutGoalUnitTypeFoot
  , pattern INWorkoutGoalUnitTypeMile
  , pattern INWorkoutGoalUnitTypeYard
  , pattern INWorkoutGoalUnitTypeSecond
  , pattern INWorkoutGoalUnitTypeMinute
  , pattern INWorkoutGoalUnitTypeHour
  , pattern INWorkoutGoalUnitTypeJoule
  , pattern INWorkoutGoalUnitTypeKiloCalorie
  , INWorkoutLocationType(INWorkoutLocationType)
  , pattern INWorkoutLocationTypeUnknown
  , pattern INWorkoutLocationTypeOutdoor
  , pattern INWorkoutLocationTypeIndoor

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithWorkoutName:goalValue:workoutGoalUnitType:workoutLocationType:isOpenEnded:@
initWithWorkoutName_goalValue_workoutGoalUnitType_workoutLocationType_isOpenEnded :: (IsINStartWorkoutIntent inStartWorkoutIntent, IsINSpeakableString workoutName, IsNSNumber goalValue, IsNSNumber isOpenEnded) => inStartWorkoutIntent -> workoutName -> goalValue -> INWorkoutGoalUnitType -> INWorkoutLocationType -> isOpenEnded -> IO (Id INStartWorkoutIntent)
initWithWorkoutName_goalValue_workoutGoalUnitType_workoutLocationType_isOpenEnded inStartWorkoutIntent workoutName goalValue workoutGoalUnitType workoutLocationType isOpenEnded =
  sendOwnedMessage inStartWorkoutIntent initWithWorkoutName_goalValue_workoutGoalUnitType_workoutLocationType_isOpenEndedSelector (toINSpeakableString workoutName) (toNSNumber goalValue) workoutGoalUnitType workoutLocationType (toNSNumber isOpenEnded)

-- | @- workoutName@
workoutName :: IsINStartWorkoutIntent inStartWorkoutIntent => inStartWorkoutIntent -> IO (Id INSpeakableString)
workoutName inStartWorkoutIntent =
  sendMessage inStartWorkoutIntent workoutNameSelector

-- | @- goalValue@
goalValue :: IsINStartWorkoutIntent inStartWorkoutIntent => inStartWorkoutIntent -> IO (Id NSNumber)
goalValue inStartWorkoutIntent =
  sendMessage inStartWorkoutIntent goalValueSelector

-- | @- workoutGoalUnitType@
workoutGoalUnitType :: IsINStartWorkoutIntent inStartWorkoutIntent => inStartWorkoutIntent -> IO INWorkoutGoalUnitType
workoutGoalUnitType inStartWorkoutIntent =
  sendMessage inStartWorkoutIntent workoutGoalUnitTypeSelector

-- | @- workoutLocationType@
workoutLocationType :: IsINStartWorkoutIntent inStartWorkoutIntent => inStartWorkoutIntent -> IO INWorkoutLocationType
workoutLocationType inStartWorkoutIntent =
  sendMessage inStartWorkoutIntent workoutLocationTypeSelector

-- | @- isOpenEnded@
isOpenEnded :: IsINStartWorkoutIntent inStartWorkoutIntent => inStartWorkoutIntent -> IO (Id NSNumber)
isOpenEnded inStartWorkoutIntent =
  sendMessage inStartWorkoutIntent isOpenEndedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithWorkoutName:goalValue:workoutGoalUnitType:workoutLocationType:isOpenEnded:@
initWithWorkoutName_goalValue_workoutGoalUnitType_workoutLocationType_isOpenEndedSelector :: Selector '[Id INSpeakableString, Id NSNumber, INWorkoutGoalUnitType, INWorkoutLocationType, Id NSNumber] (Id INStartWorkoutIntent)
initWithWorkoutName_goalValue_workoutGoalUnitType_workoutLocationType_isOpenEndedSelector = mkSelector "initWithWorkoutName:goalValue:workoutGoalUnitType:workoutLocationType:isOpenEnded:"

-- | @Selector@ for @workoutName@
workoutNameSelector :: Selector '[] (Id INSpeakableString)
workoutNameSelector = mkSelector "workoutName"

-- | @Selector@ for @goalValue@
goalValueSelector :: Selector '[] (Id NSNumber)
goalValueSelector = mkSelector "goalValue"

-- | @Selector@ for @workoutGoalUnitType@
workoutGoalUnitTypeSelector :: Selector '[] INWorkoutGoalUnitType
workoutGoalUnitTypeSelector = mkSelector "workoutGoalUnitType"

-- | @Selector@ for @workoutLocationType@
workoutLocationTypeSelector :: Selector '[] INWorkoutLocationType
workoutLocationTypeSelector = mkSelector "workoutLocationType"

-- | @Selector@ for @isOpenEnded@
isOpenEndedSelector :: Selector '[] (Id NSNumber)
isOpenEndedSelector = mkSelector "isOpenEnded"

