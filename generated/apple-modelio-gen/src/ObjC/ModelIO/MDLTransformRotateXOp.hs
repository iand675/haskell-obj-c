{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MDLTransformRotateXOp@.
module ObjC.ModelIO.MDLTransformRotateXOp
  ( MDLTransformRotateXOp
  , IsMDLTransformRotateXOp(..)
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
name :: IsMDLTransformRotateXOp mdlTransformRotateXOp => mdlTransformRotateXOp -> IO (Id NSString)
name mdlTransformRotateXOp =
  sendMessage mdlTransformRotateXOp nameSelector

-- | @- animatedValue@
animatedValue :: IsMDLTransformRotateXOp mdlTransformRotateXOp => mdlTransformRotateXOp -> IO (Id MDLAnimatedScalar)
animatedValue mdlTransformRotateXOp =
  sendMessage mdlTransformRotateXOp animatedValueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @animatedValue@
animatedValueSelector :: Selector '[] (Id MDLAnimatedScalar)
animatedValueSelector = mkSelector "animatedValue"

