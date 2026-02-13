{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MDLTransformMatrixOp@.
module ObjC.ModelIO.MDLTransformMatrixOp
  ( MDLTransformMatrixOp
  , IsMDLTransformMatrixOp(..)
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
name :: IsMDLTransformMatrixOp mdlTransformMatrixOp => mdlTransformMatrixOp -> IO (Id NSString)
name mdlTransformMatrixOp =
  sendMessage mdlTransformMatrixOp nameSelector

-- | @- animatedValue@
animatedValue :: IsMDLTransformMatrixOp mdlTransformMatrixOp => mdlTransformMatrixOp -> IO (Id MDLAnimatedMatrix4x4)
animatedValue mdlTransformMatrixOp =
  sendMessage mdlTransformMatrixOp animatedValueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @animatedValue@
animatedValueSelector :: Selector '[] (Id MDLAnimatedMatrix4x4)
animatedValueSelector = mkSelector "animatedValue"

