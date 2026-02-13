{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MDLTransformTranslateOp@.
module ObjC.ModelIO.MDLTransformTranslateOp
  ( MDLTransformTranslateOp
  , IsMDLTransformTranslateOp(..)
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
name :: IsMDLTransformTranslateOp mdlTransformTranslateOp => mdlTransformTranslateOp -> IO (Id NSString)
name mdlTransformTranslateOp =
  sendMessage mdlTransformTranslateOp nameSelector

-- | @- animatedValue@
animatedValue :: IsMDLTransformTranslateOp mdlTransformTranslateOp => mdlTransformTranslateOp -> IO (Id MDLAnimatedVector3)
animatedValue mdlTransformTranslateOp =
  sendMessage mdlTransformTranslateOp animatedValueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @animatedValue@
animatedValueSelector :: Selector '[] (Id MDLAnimatedVector3)
animatedValueSelector = mkSelector "animatedValue"

