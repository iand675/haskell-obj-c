{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MDLTransformOrientOp@.
module ObjC.ModelIO.MDLTransformOrientOp
  ( MDLTransformOrientOp
  , IsMDLTransformOrientOp(..)
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
name :: IsMDLTransformOrientOp mdlTransformOrientOp => mdlTransformOrientOp -> IO (Id NSString)
name mdlTransformOrientOp =
  sendMessage mdlTransformOrientOp nameSelector

-- | @- animatedValue@
animatedValue :: IsMDLTransformOrientOp mdlTransformOrientOp => mdlTransformOrientOp -> IO (Id MDLAnimatedQuaternion)
animatedValue mdlTransformOrientOp =
  sendMessage mdlTransformOrientOp animatedValueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @animatedValue@
animatedValueSelector :: Selector '[] (Id MDLAnimatedQuaternion)
animatedValueSelector = mkSelector "animatedValue"

