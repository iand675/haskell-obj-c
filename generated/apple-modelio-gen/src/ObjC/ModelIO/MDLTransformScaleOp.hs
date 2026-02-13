{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MDLTransformScaleOp@.
module ObjC.ModelIO.MDLTransformScaleOp
  ( MDLTransformScaleOp
  , IsMDLTransformScaleOp(..)
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
name :: IsMDLTransformScaleOp mdlTransformScaleOp => mdlTransformScaleOp -> IO (Id NSString)
name mdlTransformScaleOp =
  sendMessage mdlTransformScaleOp nameSelector

-- | @- animatedValue@
animatedValue :: IsMDLTransformScaleOp mdlTransformScaleOp => mdlTransformScaleOp -> IO (Id MDLAnimatedVector3)
animatedValue mdlTransformScaleOp =
  sendMessage mdlTransformScaleOp animatedValueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @animatedValue@
animatedValueSelector :: Selector '[] (Id MDLAnimatedVector3)
animatedValueSelector = mkSelector "animatedValue"

