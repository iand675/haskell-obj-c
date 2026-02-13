{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSMatrixBinaryKernel
--
-- This depends on Metal.framework
--
-- A MPSMatrixBinaryKernel consumes two MPSMatrix objects and produces              one MPSMatrix object.
--
-- Generated bindings for @MPSMatrixBinaryKernel@.
module ObjC.MetalPerformanceShaders.MPSMatrixBinaryKernel
  ( MPSMatrixBinaryKernel
  , IsMPSMatrixBinaryKernel(..)
  , batchStart
  , setBatchStart
  , batchSize
  , setBatchSize
  , batchSizeSelector
  , batchStartSelector
  , setBatchSizeSelector
  , setBatchStartSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | batchStart
--
-- The index of the first matrix in the batch.  This property is              modifiable and defaults to 0 at initialization time.  If              batch processing should begin at a different matrix this value              should be modified prior to encoding the kernel.
--
-- ObjC selector: @- batchStart@
batchStart :: IsMPSMatrixBinaryKernel mpsMatrixBinaryKernel => mpsMatrixBinaryKernel -> IO CULong
batchStart mpsMatrixBinaryKernel =
  sendMessage mpsMatrixBinaryKernel batchStartSelector

-- | batchStart
--
-- The index of the first matrix in the batch.  This property is              modifiable and defaults to 0 at initialization time.  If              batch processing should begin at a different matrix this value              should be modified prior to encoding the kernel.
--
-- ObjC selector: @- setBatchStart:@
setBatchStart :: IsMPSMatrixBinaryKernel mpsMatrixBinaryKernel => mpsMatrixBinaryKernel -> CULong -> IO ()
setBatchStart mpsMatrixBinaryKernel value =
  sendMessage mpsMatrixBinaryKernel setBatchStartSelector value

-- | batchSize
--
-- The number of matrices in the batch to process.  This property              is modifiable and by default allows all matrices available at              encoding time to be processed.  If a single matrix should be              processed set this value to 1.
--
-- ObjC selector: @- batchSize@
batchSize :: IsMPSMatrixBinaryKernel mpsMatrixBinaryKernel => mpsMatrixBinaryKernel -> IO CULong
batchSize mpsMatrixBinaryKernel =
  sendMessage mpsMatrixBinaryKernel batchSizeSelector

-- | batchSize
--
-- The number of matrices in the batch to process.  This property              is modifiable and by default allows all matrices available at              encoding time to be processed.  If a single matrix should be              processed set this value to 1.
--
-- ObjC selector: @- setBatchSize:@
setBatchSize :: IsMPSMatrixBinaryKernel mpsMatrixBinaryKernel => mpsMatrixBinaryKernel -> CULong -> IO ()
setBatchSize mpsMatrixBinaryKernel value =
  sendMessage mpsMatrixBinaryKernel setBatchSizeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @batchStart@
batchStartSelector :: Selector '[] CULong
batchStartSelector = mkSelector "batchStart"

-- | @Selector@ for @setBatchStart:@
setBatchStartSelector :: Selector '[CULong] ()
setBatchStartSelector = mkSelector "setBatchStart:"

-- | @Selector@ for @batchSize@
batchSizeSelector :: Selector '[] CULong
batchSizeSelector = mkSelector "batchSize"

-- | @Selector@ for @setBatchSize:@
setBatchSizeSelector :: Selector '[CULong] ()
setBatchSizeSelector = mkSelector "setBatchSize:"

