{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INResumeWorkoutIntent@.
module ObjC.Intents.INResumeWorkoutIntent
  ( INResumeWorkoutIntent
  , IsINResumeWorkoutIntent(..)
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
initWithWorkoutName :: (IsINResumeWorkoutIntent inResumeWorkoutIntent, IsINSpeakableString workoutName) => inResumeWorkoutIntent -> workoutName -> IO (Id INResumeWorkoutIntent)
initWithWorkoutName inResumeWorkoutIntent workoutName =
  sendOwnedMessage inResumeWorkoutIntent initWithWorkoutNameSelector (toINSpeakableString workoutName)

-- | @- workoutName@
workoutName :: IsINResumeWorkoutIntent inResumeWorkoutIntent => inResumeWorkoutIntent -> IO (Id INSpeakableString)
workoutName inResumeWorkoutIntent =
  sendMessage inResumeWorkoutIntent workoutNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithWorkoutName:@
initWithWorkoutNameSelector :: Selector '[Id INSpeakableString] (Id INResumeWorkoutIntent)
initWithWorkoutNameSelector = mkSelector "initWithWorkoutName:"

-- | @Selector@ for @workoutName@
workoutNameSelector :: Selector '[] (Id INSpeakableString)
workoutNameSelector = mkSelector "workoutName"

