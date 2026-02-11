{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CIRenderInfo@.
module ObjC.CoreImage.CIRenderInfo
  ( CIRenderInfo
  , IsCIRenderInfo(..)
  , kernelExecutionTime
  , kernelCompileTime
  , passCount
  , pixelsProcessed
  , kernelExecutionTimeSelector
  , kernelCompileTimeSelector
  , passCountSelector
  , pixelsProcessedSelector


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

import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- kernelExecutionTime@
kernelExecutionTime :: IsCIRenderInfo ciRenderInfo => ciRenderInfo -> IO CDouble
kernelExecutionTime ciRenderInfo  =
  sendMsg ciRenderInfo (mkSelector "kernelExecutionTime") retCDouble []

-- | @- kernelCompileTime@
kernelCompileTime :: IsCIRenderInfo ciRenderInfo => ciRenderInfo -> IO CDouble
kernelCompileTime ciRenderInfo  =
  sendMsg ciRenderInfo (mkSelector "kernelCompileTime") retCDouble []

-- | @- passCount@
passCount :: IsCIRenderInfo ciRenderInfo => ciRenderInfo -> IO CLong
passCount ciRenderInfo  =
  sendMsg ciRenderInfo (mkSelector "passCount") retCLong []

-- | @- pixelsProcessed@
pixelsProcessed :: IsCIRenderInfo ciRenderInfo => ciRenderInfo -> IO CLong
pixelsProcessed ciRenderInfo  =
  sendMsg ciRenderInfo (mkSelector "pixelsProcessed") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @kernelExecutionTime@
kernelExecutionTimeSelector :: Selector
kernelExecutionTimeSelector = mkSelector "kernelExecutionTime"

-- | @Selector@ for @kernelCompileTime@
kernelCompileTimeSelector :: Selector
kernelCompileTimeSelector = mkSelector "kernelCompileTime"

-- | @Selector@ for @passCount@
passCountSelector :: Selector
passCountSelector = mkSelector "passCount"

-- | @Selector@ for @pixelsProcessed@
pixelsProcessedSelector :: Selector
pixelsProcessedSelector = mkSelector "pixelsProcessed"

