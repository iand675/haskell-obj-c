{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSMatrixUnaryKernel
--
-- This depends on Metal.framework
--
-- A MPSMatrixUnaryKernel consumes one MPSMatrix and produces one MPSMatrix.
--
-- Generated bindings for @MPSMatrixUnaryKernel@.
module ObjC.MetalPerformanceShaders.MPSMatrixUnaryKernel
  ( MPSMatrixUnaryKernel
  , IsMPSMatrixUnaryKernel(..)
  , batchStart
  , setBatchStart
  , batchSize
  , setBatchSize
  , batchStartSelector
  , setBatchStartSelector
  , batchSizeSelector
  , setBatchSizeSelector


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

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | batchStart
--
-- The index of the first matrix in the batch.  This property is              modifiable and defaults to 0 at initialization time.  If              batch processing should begin at a different matrix this value              should be modified prior to encoding the kernel.
--
-- ObjC selector: @- batchStart@
batchStart :: IsMPSMatrixUnaryKernel mpsMatrixUnaryKernel => mpsMatrixUnaryKernel -> IO CULong
batchStart mpsMatrixUnaryKernel  =
  sendMsg mpsMatrixUnaryKernel (mkSelector "batchStart") retCULong []

-- | batchStart
--
-- The index of the first matrix in the batch.  This property is              modifiable and defaults to 0 at initialization time.  If              batch processing should begin at a different matrix this value              should be modified prior to encoding the kernel.
--
-- ObjC selector: @- setBatchStart:@
setBatchStart :: IsMPSMatrixUnaryKernel mpsMatrixUnaryKernel => mpsMatrixUnaryKernel -> CULong -> IO ()
setBatchStart mpsMatrixUnaryKernel  value =
  sendMsg mpsMatrixUnaryKernel (mkSelector "setBatchStart:") retVoid [argCULong (fromIntegral value)]

-- | batchSize
--
-- The number of matrices in the batch to process.  This property              is modifiable and by default allows all matrices available at              encoding time to be processed.  If a single matrix should be              processed set this value to 1.
--
-- ObjC selector: @- batchSize@
batchSize :: IsMPSMatrixUnaryKernel mpsMatrixUnaryKernel => mpsMatrixUnaryKernel -> IO CULong
batchSize mpsMatrixUnaryKernel  =
  sendMsg mpsMatrixUnaryKernel (mkSelector "batchSize") retCULong []

-- | batchSize
--
-- The number of matrices in the batch to process.  This property              is modifiable and by default allows all matrices available at              encoding time to be processed.  If a single matrix should be              processed set this value to 1.
--
-- ObjC selector: @- setBatchSize:@
setBatchSize :: IsMPSMatrixUnaryKernel mpsMatrixUnaryKernel => mpsMatrixUnaryKernel -> CULong -> IO ()
setBatchSize mpsMatrixUnaryKernel  value =
  sendMsg mpsMatrixUnaryKernel (mkSelector "setBatchSize:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @batchStart@
batchStartSelector :: Selector
batchStartSelector = mkSelector "batchStart"

-- | @Selector@ for @setBatchStart:@
setBatchStartSelector :: Selector
setBatchStartSelector = mkSelector "setBatchStart:"

-- | @Selector@ for @batchSize@
batchSizeSelector :: Selector
batchSizeSelector = mkSelector "batchSize"

-- | @Selector@ for @setBatchSize:@
setBatchSizeSelector :: Selector
setBatchSizeSelector = mkSelector "setBatchSize:"

