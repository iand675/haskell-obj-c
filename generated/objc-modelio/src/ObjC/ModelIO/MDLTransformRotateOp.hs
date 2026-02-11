{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MDLTransformRotateOp@.
module ObjC.ModelIO.MDLTransformRotateOp
  ( MDLTransformRotateOp
  , IsMDLTransformRotateOp(..)
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
name :: IsMDLTransformRotateOp mdlTransformRotateOp => mdlTransformRotateOp -> IO (Id NSString)
name mdlTransformRotateOp  =
  sendMsg mdlTransformRotateOp (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- animatedValue@
animatedValue :: IsMDLTransformRotateOp mdlTransformRotateOp => mdlTransformRotateOp -> IO (Id MDLAnimatedVector3)
animatedValue mdlTransformRotateOp  =
  sendMsg mdlTransformRotateOp (mkSelector "animatedValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @animatedValue@
animatedValueSelector :: Selector
animatedValueSelector = mkSelector "animatedValue"

