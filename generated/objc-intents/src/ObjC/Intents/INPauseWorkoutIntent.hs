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
import ObjC.Foundation.Internal.Classes

-- | @- initWithWorkoutName:@
initWithWorkoutName :: (IsINPauseWorkoutIntent inPauseWorkoutIntent, IsINSpeakableString workoutName) => inPauseWorkoutIntent -> workoutName -> IO (Id INPauseWorkoutIntent)
initWithWorkoutName inPauseWorkoutIntent  workoutName =
withObjCPtr workoutName $ \raw_workoutName ->
    sendMsg inPauseWorkoutIntent (mkSelector "initWithWorkoutName:") (retPtr retVoid) [argPtr (castPtr raw_workoutName :: Ptr ())] >>= ownedObject . castPtr

-- | @- workoutName@
workoutName :: IsINPauseWorkoutIntent inPauseWorkoutIntent => inPauseWorkoutIntent -> IO (Id INSpeakableString)
workoutName inPauseWorkoutIntent  =
  sendMsg inPauseWorkoutIntent (mkSelector "workoutName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithWorkoutName:@
initWithWorkoutNameSelector :: Selector
initWithWorkoutNameSelector = mkSelector "initWithWorkoutName:"

-- | @Selector@ for @workoutName@
workoutNameSelector :: Selector
workoutNameSelector = mkSelector "workoutName"

