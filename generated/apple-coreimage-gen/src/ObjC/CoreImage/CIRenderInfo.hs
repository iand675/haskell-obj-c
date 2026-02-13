{-# LANGUAGE DataKinds #-}
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
  , kernelCompileTimeSelector
  , kernelExecutionTimeSelector
  , passCountSelector
  , pixelsProcessedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- kernelExecutionTime@
kernelExecutionTime :: IsCIRenderInfo ciRenderInfo => ciRenderInfo -> IO CDouble
kernelExecutionTime ciRenderInfo =
  sendMessage ciRenderInfo kernelExecutionTimeSelector

-- | @- kernelCompileTime@
kernelCompileTime :: IsCIRenderInfo ciRenderInfo => ciRenderInfo -> IO CDouble
kernelCompileTime ciRenderInfo =
  sendMessage ciRenderInfo kernelCompileTimeSelector

-- | @- passCount@
passCount :: IsCIRenderInfo ciRenderInfo => ciRenderInfo -> IO CLong
passCount ciRenderInfo =
  sendMessage ciRenderInfo passCountSelector

-- | @- pixelsProcessed@
pixelsProcessed :: IsCIRenderInfo ciRenderInfo => ciRenderInfo -> IO CLong
pixelsProcessed ciRenderInfo =
  sendMessage ciRenderInfo pixelsProcessedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @kernelExecutionTime@
kernelExecutionTimeSelector :: Selector '[] CDouble
kernelExecutionTimeSelector = mkSelector "kernelExecutionTime"

-- | @Selector@ for @kernelCompileTime@
kernelCompileTimeSelector :: Selector '[] CDouble
kernelCompileTimeSelector = mkSelector "kernelCompileTime"

-- | @Selector@ for @passCount@
passCountSelector :: Selector '[] CLong
passCountSelector = mkSelector "passCount"

-- | @Selector@ for @pixelsProcessed@
pixelsProcessedSelector :: Selector '[] CLong
pixelsProcessedSelector = mkSelector "pixelsProcessed"

