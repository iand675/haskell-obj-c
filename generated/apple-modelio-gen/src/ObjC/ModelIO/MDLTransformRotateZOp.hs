{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MDLTransformRotateZOp@.
module ObjC.ModelIO.MDLTransformRotateZOp
  ( MDLTransformRotateZOp
  , IsMDLTransformRotateZOp(..)
  , name
  , animatedValue
  , animatedValueSelector
  , nameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ModelIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- name@
name :: IsMDLTransformRotateZOp mdlTransformRotateZOp => mdlTransformRotateZOp -> IO (Id NSString)
name mdlTransformRotateZOp =
  sendMessage mdlTransformRotateZOp nameSelector

-- | @- animatedValue@
animatedValue :: IsMDLTransformRotateZOp mdlTransformRotateZOp => mdlTransformRotateZOp -> IO (Id MDLAnimatedScalar)
animatedValue mdlTransformRotateZOp =
  sendMessage mdlTransformRotateZOp animatedValueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @animatedValue@
animatedValueSelector :: Selector '[] (Id MDLAnimatedScalar)
animatedValueSelector = mkSelector "animatedValue"

