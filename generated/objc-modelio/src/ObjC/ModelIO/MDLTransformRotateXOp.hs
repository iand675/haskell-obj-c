{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MDLTransformRotateXOp@.
module ObjC.ModelIO.MDLTransformRotateXOp
  ( MDLTransformRotateXOp
  , IsMDLTransformRotateXOp(..)
  , name
  , animatedValue
  , nameSelector
  , animatedValueSelector


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

import ObjC.ModelIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- name@
name :: IsMDLTransformRotateXOp mdlTransformRotateXOp => mdlTransformRotateXOp -> IO (Id NSString)
name mdlTransformRotateXOp  =
  sendMsg mdlTransformRotateXOp (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- animatedValue@
animatedValue :: IsMDLTransformRotateXOp mdlTransformRotateXOp => mdlTransformRotateXOp -> IO (Id MDLAnimatedScalar)
animatedValue mdlTransformRotateXOp  =
  sendMsg mdlTransformRotateXOp (mkSelector "animatedValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @animatedValue@
animatedValueSelector :: Selector
animatedValueSelector = mkSelector "animatedValue"

