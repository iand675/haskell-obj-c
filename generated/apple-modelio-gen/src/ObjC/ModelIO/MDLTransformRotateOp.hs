{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MDLTransformRotateOp@.
module ObjC.ModelIO.MDLTransformRotateOp
  ( MDLTransformRotateOp
  , IsMDLTransformRotateOp(..)
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
name :: IsMDLTransformRotateOp mdlTransformRotateOp => mdlTransformRotateOp -> IO (Id NSString)
name mdlTransformRotateOp =
  sendMessage mdlTransformRotateOp nameSelector

-- | @- animatedValue@
animatedValue :: IsMDLTransformRotateOp mdlTransformRotateOp => mdlTransformRotateOp -> IO (Id MDLAnimatedVector3)
animatedValue mdlTransformRotateOp =
  sendMessage mdlTransformRotateOp animatedValueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @animatedValue@
animatedValueSelector :: Selector '[] (Id MDLAnimatedVector3)
animatedValueSelector = mkSelector "animatedValue"

