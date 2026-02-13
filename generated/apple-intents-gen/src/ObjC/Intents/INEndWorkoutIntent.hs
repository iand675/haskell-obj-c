{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INEndWorkoutIntent@.
module ObjC.Intents.INEndWorkoutIntent
  ( INEndWorkoutIntent
  , IsINEndWorkoutIntent(..)
  , initWithWorkoutName
  , workoutName
  , initWithWorkoutNameSelector
  , workoutNameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithWorkoutName:@
initWithWorkoutName :: (IsINEndWorkoutIntent inEndWorkoutIntent, IsINSpeakableString workoutName) => inEndWorkoutIntent -> workoutName -> IO (Id INEndWorkoutIntent)
initWithWorkoutName inEndWorkoutIntent workoutName =
  sendOwnedMessage inEndWorkoutIntent initWithWorkoutNameSelector (toINSpeakableString workoutName)

-- | @- workoutName@
workoutName :: IsINEndWorkoutIntent inEndWorkoutIntent => inEndWorkoutIntent -> IO (Id INSpeakableString)
workoutName inEndWorkoutIntent =
  sendMessage inEndWorkoutIntent workoutNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithWorkoutName:@
initWithWorkoutNameSelector :: Selector '[Id INSpeakableString] (Id INEndWorkoutIntent)
initWithWorkoutNameSelector = mkSelector "initWithWorkoutName:"

-- | @Selector@ for @workoutName@
workoutNameSelector :: Selector '[] (Id INSpeakableString)
workoutNameSelector = mkSelector "workoutName"

