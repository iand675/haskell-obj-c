{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MDLTransformRotateYOp@.
module ObjC.ModelIO.MDLTransformRotateYOp
  ( MDLTransformRotateYOp
  , IsMDLTransformRotateYOp(..)
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
name :: IsMDLTransformRotateYOp mdlTransformRotateYOp => mdlTransformRotateYOp -> IO (Id NSString)
name mdlTransformRotateYOp =
  sendMessage mdlTransformRotateYOp nameSelector

-- | @- animatedValue@
animatedValue :: IsMDLTransformRotateYOp mdlTransformRotateYOp => mdlTransformRotateYOp -> IO (Id MDLAnimatedScalar)
animatedValue mdlTransformRotateYOp =
  sendMessage mdlTransformRotateYOp animatedValueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @animatedValue@
animatedValueSelector :: Selector '[] (Id MDLAnimatedScalar)
animatedValueSelector = mkSelector "animatedValue"

