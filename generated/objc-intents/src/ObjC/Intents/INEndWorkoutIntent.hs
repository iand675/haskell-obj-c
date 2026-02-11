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
initWithWorkoutName :: (IsINEndWorkoutIntent inEndWorkoutIntent, IsINSpeakableString workoutName) => inEndWorkoutIntent -> workoutName -> IO (Id INEndWorkoutIntent)
initWithWorkoutName inEndWorkoutIntent  workoutName =
withObjCPtr workoutName $ \raw_workoutName ->
    sendMsg inEndWorkoutIntent (mkSelector "initWithWorkoutName:") (retPtr retVoid) [argPtr (castPtr raw_workoutName :: Ptr ())] >>= ownedObject . castPtr

-- | @- workoutName@
workoutName :: IsINEndWorkoutIntent inEndWorkoutIntent => inEndWorkoutIntent -> IO (Id INSpeakableString)
workoutName inEndWorkoutIntent  =
  sendMsg inEndWorkoutIntent (mkSelector "workoutName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithWorkoutName:@
initWithWorkoutNameSelector :: Selector
initWithWorkoutNameSelector = mkSelector "initWithWorkoutName:"

-- | @Selector@ for @workoutName@
workoutNameSelector :: Selector
workoutNameSelector = mkSelector "workoutName"

