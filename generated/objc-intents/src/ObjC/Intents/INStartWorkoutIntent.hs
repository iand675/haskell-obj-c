{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INStartWorkoutIntent@.
module ObjC.Intents.INStartWorkoutIntent
  ( INStartWorkoutIntent
  , IsINStartWorkoutIntent(..)
  , initWithWorkoutName_goalValue_workoutGoalUnitType_workoutLocationType_isOpenEnded
  , workoutName
  , workoutGoalUnitType
  , workoutLocationType
  , initWithWorkoutName_goalValue_workoutGoalUnitType_workoutLocationType_isOpenEndedSelector
  , workoutNameSelector
  , workoutGoalUnitTypeSelector
  , workoutLocationTypeSelector

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

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithWorkoutName:goalValue:workoutGoalUnitType:workoutLocationType:isOpenEnded:@
initWithWorkoutName_goalValue_workoutGoalUnitType_workoutLocationType_isOpenEnded :: (IsINStartWorkoutIntent inStartWorkoutIntent, IsINSpeakableString workoutName, IsNSNumber goalValue, IsNSNumber isOpenEnded) => inStartWorkoutIntent -> workoutName -> goalValue -> INWorkoutGoalUnitType -> INWorkoutLocationType -> isOpenEnded -> IO (Id INStartWorkoutIntent)
initWithWorkoutName_goalValue_workoutGoalUnitType_workoutLocationType_isOpenEnded inStartWorkoutIntent  workoutName goalValue workoutGoalUnitType workoutLocationType isOpenEnded =
withObjCPtr workoutName $ \raw_workoutName ->
  withObjCPtr goalValue $ \raw_goalValue ->
    withObjCPtr isOpenEnded $ \raw_isOpenEnded ->
        sendMsg inStartWorkoutIntent (mkSelector "initWithWorkoutName:goalValue:workoutGoalUnitType:workoutLocationType:isOpenEnded:") (retPtr retVoid) [argPtr (castPtr raw_workoutName :: Ptr ()), argPtr (castPtr raw_goalValue :: Ptr ()), argCLong (coerce workoutGoalUnitType), argCLong (coerce workoutLocationType), argPtr (castPtr raw_isOpenEnded :: Ptr ())] >>= ownedObject . castPtr

-- | @- workoutName@
workoutName :: IsINStartWorkoutIntent inStartWorkoutIntent => inStartWorkoutIntent -> IO (Id INSpeakableString)
workoutName inStartWorkoutIntent  =
  sendMsg inStartWorkoutIntent (mkSelector "workoutName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- workoutGoalUnitType@
workoutGoalUnitType :: IsINStartWorkoutIntent inStartWorkoutIntent => inStartWorkoutIntent -> IO INWorkoutGoalUnitType
workoutGoalUnitType inStartWorkoutIntent  =
  fmap (coerce :: CLong -> INWorkoutGoalUnitType) $ sendMsg inStartWorkoutIntent (mkSelector "workoutGoalUnitType") retCLong []

-- | @- workoutLocationType@
workoutLocationType :: IsINStartWorkoutIntent inStartWorkoutIntent => inStartWorkoutIntent -> IO INWorkoutLocationType
workoutLocationType inStartWorkoutIntent  =
  fmap (coerce :: CLong -> INWorkoutLocationType) $ sendMsg inStartWorkoutIntent (mkSelector "workoutLocationType") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithWorkoutName:goalValue:workoutGoalUnitType:workoutLocationType:isOpenEnded:@
initWithWorkoutName_goalValue_workoutGoalUnitType_workoutLocationType_isOpenEndedSelector :: Selector
initWithWorkoutName_goalValue_workoutGoalUnitType_workoutLocationType_isOpenEndedSelector = mkSelector "initWithWorkoutName:goalValue:workoutGoalUnitType:workoutLocationType:isOpenEnded:"

-- | @Selector@ for @workoutName@
workoutNameSelector :: Selector
workoutNameSelector = mkSelector "workoutName"

-- | @Selector@ for @workoutGoalUnitType@
workoutGoalUnitTypeSelector :: Selector
workoutGoalUnitTypeSelector = mkSelector "workoutGoalUnitType"

-- | @Selector@ for @workoutLocationType@
workoutLocationTypeSelector :: Selector
workoutLocationTypeSelector = mkSelector "workoutLocationType"

