{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MDLVertexAttributeData
--
-- convenience object to quickly access vertex attribute data
--
-- created by MDLMesh's vertexAttributeData selector             Setting values on this object has no effect on the             underlying objects.
--
-- Generated bindings for @MDLVertexAttributeData@.
module ObjC.ModelIO.MDLVertexAttributeData
  ( MDLVertexAttributeData
  , IsMDLVertexAttributeData(..)
  , map_
  , setMap
  , dataStart
  , setDataStart
  , stride
  , setStride
  , format
  , setFormat
  , bufferSize
  , setBufferSize
  , bufferSizeSelector
  , dataStartSelector
  , formatSelector
  , mapSelector
  , setBufferSizeSelector
  , setDataStartSelector
  , setFormatSelector
  , setMapSelector
  , setStrideSelector
  , strideSelector

  -- * Enum types
  , MDLVertexFormat(MDLVertexFormat)
  , pattern MDLVertexFormatInvalid
  , pattern MDLVertexFormatPackedBit
  , pattern MDLVertexFormatUCharBits
  , pattern MDLVertexFormatCharBits
  , pattern MDLVertexFormatUCharNormalizedBits
  , pattern MDLVertexFormatCharNormalizedBits
  , pattern MDLVertexFormatUShortBits
  , pattern MDLVertexFormatShortBits
  , pattern MDLVertexFormatUShortNormalizedBits
  , pattern MDLVertexFormatShortNormalizedBits
  , pattern MDLVertexFormatUIntBits
  , pattern MDLVertexFormatIntBits
  , pattern MDLVertexFormatHalfBits
  , pattern MDLVertexFormatFloatBits
  , pattern MDLVertexFormatUChar
  , pattern MDLVertexFormatUChar2
  , pattern MDLVertexFormatUChar3
  , pattern MDLVertexFormatUChar4
  , pattern MDLVertexFormatChar
  , pattern MDLVertexFormatChar2
  , pattern MDLVertexFormatChar3
  , pattern MDLVertexFormatChar4
  , pattern MDLVertexFormatUCharNormalized
  , pattern MDLVertexFormatUChar2Normalized
  , pattern MDLVertexFormatUChar3Normalized
  , pattern MDLVertexFormatUChar4Normalized
  , pattern MDLVertexFormatCharNormalized
  , pattern MDLVertexFormatChar2Normalized
  , pattern MDLVertexFormatChar3Normalized
  , pattern MDLVertexFormatChar4Normalized
  , pattern MDLVertexFormatUShort
  , pattern MDLVertexFormatUShort2
  , pattern MDLVertexFormatUShort3
  , pattern MDLVertexFormatUShort4
  , pattern MDLVertexFormatShort
  , pattern MDLVertexFormatShort2
  , pattern MDLVertexFormatShort3
  , pattern MDLVertexFormatShort4
  , pattern MDLVertexFormatUShortNormalized
  , pattern MDLVertexFormatUShort2Normalized
  , pattern MDLVertexFormatUShort3Normalized
  , pattern MDLVertexFormatUShort4Normalized
  , pattern MDLVertexFormatShortNormalized
  , pattern MDLVertexFormatShort2Normalized
  , pattern MDLVertexFormatShort3Normalized
  , pattern MDLVertexFormatShort4Normalized
  , pattern MDLVertexFormatUInt
  , pattern MDLVertexFormatUInt2
  , pattern MDLVertexFormatUInt3
  , pattern MDLVertexFormatUInt4
  , pattern MDLVertexFormatInt
  , pattern MDLVertexFormatInt2
  , pattern MDLVertexFormatInt3
  , pattern MDLVertexFormatInt4
  , pattern MDLVertexFormatHalf
  , pattern MDLVertexFormatHalf2
  , pattern MDLVertexFormatHalf3
  , pattern MDLVertexFormatHalf4
  , pattern MDLVertexFormatFloat
  , pattern MDLVertexFormatFloat2
  , pattern MDLVertexFormatFloat3
  , pattern MDLVertexFormatFloat4
  , pattern MDLVertexFormatInt1010102Normalized
  , pattern MDLVertexFormatUInt1010102Normalized

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ModelIO.Internal.Classes
import ObjC.ModelIO.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- map@
map_ :: IsMDLVertexAttributeData mdlVertexAttributeData => mdlVertexAttributeData -> IO (Id MDLMeshBufferMap)
map_ mdlVertexAttributeData =
  sendMessage mdlVertexAttributeData mapSelector

-- | @- setMap:@
setMap :: (IsMDLVertexAttributeData mdlVertexAttributeData, IsMDLMeshBufferMap value) => mdlVertexAttributeData -> value -> IO ()
setMap mdlVertexAttributeData value =
  sendMessage mdlVertexAttributeData setMapSelector (toMDLMeshBufferMap value)

-- | @- dataStart@
dataStart :: IsMDLVertexAttributeData mdlVertexAttributeData => mdlVertexAttributeData -> IO (Ptr ())
dataStart mdlVertexAttributeData =
  sendMessage mdlVertexAttributeData dataStartSelector

-- | @- setDataStart:@
setDataStart :: IsMDLVertexAttributeData mdlVertexAttributeData => mdlVertexAttributeData -> Ptr () -> IO ()
setDataStart mdlVertexAttributeData value =
  sendMessage mdlVertexAttributeData setDataStartSelector value

-- | @- stride@
stride :: IsMDLVertexAttributeData mdlVertexAttributeData => mdlVertexAttributeData -> IO CULong
stride mdlVertexAttributeData =
  sendMessage mdlVertexAttributeData strideSelector

-- | @- setStride:@
setStride :: IsMDLVertexAttributeData mdlVertexAttributeData => mdlVertexAttributeData -> CULong -> IO ()
setStride mdlVertexAttributeData value =
  sendMessage mdlVertexAttributeData setStrideSelector value

-- | @- format@
format :: IsMDLVertexAttributeData mdlVertexAttributeData => mdlVertexAttributeData -> IO MDLVertexFormat
format mdlVertexAttributeData =
  sendMessage mdlVertexAttributeData formatSelector

-- | @- setFormat:@
setFormat :: IsMDLVertexAttributeData mdlVertexAttributeData => mdlVertexAttributeData -> MDLVertexFormat -> IO ()
setFormat mdlVertexAttributeData value =
  sendMessage mdlVertexAttributeData setFormatSelector value

-- | @- bufferSize@
bufferSize :: IsMDLVertexAttributeData mdlVertexAttributeData => mdlVertexAttributeData -> IO CULong
bufferSize mdlVertexAttributeData =
  sendMessage mdlVertexAttributeData bufferSizeSelector

-- | @- setBufferSize:@
setBufferSize :: IsMDLVertexAttributeData mdlVertexAttributeData => mdlVertexAttributeData -> CULong -> IO ()
setBufferSize mdlVertexAttributeData value =
  sendMessage mdlVertexAttributeData setBufferSizeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @map@
mapSelector :: Selector '[] (Id MDLMeshBufferMap)
mapSelector = mkSelector "map"

-- | @Selector@ for @setMap:@
setMapSelector :: Selector '[Id MDLMeshBufferMap] ()
setMapSelector = mkSelector "setMap:"

-- | @Selector@ for @dataStart@
dataStartSelector :: Selector '[] (Ptr ())
dataStartSelector = mkSelector "dataStart"

-- | @Selector@ for @setDataStart:@
setDataStartSelector :: Selector '[Ptr ()] ()
setDataStartSelector = mkSelector "setDataStart:"

-- | @Selector@ for @stride@
strideSelector :: Selector '[] CULong
strideSelector = mkSelector "stride"

-- | @Selector@ for @setStride:@
setStrideSelector :: Selector '[CULong] ()
setStrideSelector = mkSelector "setStride:"

-- | @Selector@ for @format@
formatSelector :: Selector '[] MDLVertexFormat
formatSelector = mkSelector "format"

-- | @Selector@ for @setFormat:@
setFormatSelector :: Selector '[MDLVertexFormat] ()
setFormatSelector = mkSelector "setFormat:"

-- | @Selector@ for @bufferSize@
bufferSizeSelector :: Selector '[] CULong
bufferSizeSelector = mkSelector "bufferSize"

-- | @Selector@ for @setBufferSize:@
setBufferSizeSelector :: Selector '[CULong] ()
setBufferSizeSelector = mkSelector "setBufferSize:"

