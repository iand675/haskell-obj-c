{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INPauseWorkoutIntent@.
module ObjC.Intents.INPauseWorkoutIntent
  ( INPauseWorkoutIntent
  , IsINPauseWorkoutIntent(..)
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
initWithWorkoutName :: (IsINPauseWorkoutIntent inPauseWorkoutIntent, IsINSpeakableString workoutName) => inPauseWorkoutIntent -> workoutName -> IO (Id INPauseWorkoutIntent)
initWithWorkoutName inPauseWorkoutIntent workoutName =
  sendOwnedMessage inPauseWorkoutIntent initWithWorkoutNameSelector (toINSpeakableString workoutName)

-- | @- workoutName@
workoutName :: IsINPauseWorkoutIntent inPauseWorkoutIntent => inPauseWorkoutIntent -> IO (Id INSpeakableString)
workoutName inPauseWorkoutIntent =
  sendMessage inPauseWorkoutIntent workoutNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithWorkoutName:@
initWithWorkoutNameSelector :: Selector '[Id INSpeakableString] (Id INPauseWorkoutIntent)
initWithWorkoutNameSelector = mkSelector "initWithWorkoutName:"

-- | @Selector@ for @workoutName@
workoutNameSelector :: Selector '[] (Id INSpeakableString)
workoutNameSelector = mkSelector "workoutName"

