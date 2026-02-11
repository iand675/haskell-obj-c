{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MDLTransformMatrixOp@.
module ObjC.ModelIO.MDLTransformMatrixOp
  ( MDLTransformMatrixOp
  , IsMDLTransformMatrixOp(..)
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
name :: IsMDLTransformMatrixOp mdlTransformMatrixOp => mdlTransformMatrixOp -> IO (Id NSString)
name mdlTransformMatrixOp  =
  sendMsg mdlTransformMatrixOp (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- animatedValue@
animatedValue :: IsMDLTransformMatrixOp mdlTransformMatrixOp => mdlTransformMatrixOp -> IO (Id MDLAnimatedMatrix4x4)
animatedValue mdlTransformMatrixOp  =
  sendMsg mdlTransformMatrixOp (mkSelector "animatedValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @animatedValue@
animatedValueSelector :: Selector
animatedValueSelector = mkSelector "animatedValue"

