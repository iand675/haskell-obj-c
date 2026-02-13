{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INCancelWorkoutIntent@.
module ObjC.Intents.INCancelWorkoutIntent
  ( INCancelWorkoutIntent
  , IsINCancelWorkoutIntent(..)
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
initWithWorkoutName :: (IsINCancelWorkoutIntent inCancelWorkoutIntent, IsINSpeakableString workoutName) => inCancelWorkoutIntent -> workoutName -> IO (Id INCancelWorkoutIntent)
initWithWorkoutName inCancelWorkoutIntent workoutName =
  sendOwnedMessage inCancelWorkoutIntent initWithWorkoutNameSelector (toINSpeakableString workoutName)

-- | @- workoutName@
workoutName :: IsINCancelWorkoutIntent inCancelWorkoutIntent => inCancelWorkoutIntent -> IO (Id INSpeakableString)
workoutName inCancelWorkoutIntent =
  sendMessage inCancelWorkoutIntent workoutNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithWorkoutName:@
initWithWorkoutNameSelector :: Selector '[Id INSpeakableString] (Id INCancelWorkoutIntent)
initWithWorkoutNameSelector = mkSelector "initWithWorkoutName:"

-- | @Selector@ for @workoutName@
workoutNameSelector :: Selector '[] (Id INSpeakableString)
workoutNameSelector = mkSelector "workoutName"

