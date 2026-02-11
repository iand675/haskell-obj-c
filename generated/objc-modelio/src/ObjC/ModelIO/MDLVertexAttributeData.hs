{-# LANGUAGE PatternSynonyms #-}
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
  , mapSelector
  , setMapSelector
  , dataStartSelector
  , setDataStartSelector
  , strideSelector
  , setStrideSelector
  , formatSelector
  , setFormatSelector
  , bufferSizeSelector
  , setBufferSizeSelector

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
import ObjC.ModelIO.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- map@
map_ :: IsMDLVertexAttributeData mdlVertexAttributeData => mdlVertexAttributeData -> IO (Id MDLMeshBufferMap)
map_ mdlVertexAttributeData  =
  sendMsg mdlVertexAttributeData (mkSelector "map") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMap:@
setMap :: (IsMDLVertexAttributeData mdlVertexAttributeData, IsMDLMeshBufferMap value) => mdlVertexAttributeData -> value -> IO ()
setMap mdlVertexAttributeData  value =
withObjCPtr value $ \raw_value ->
    sendMsg mdlVertexAttributeData (mkSelector "setMap:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- dataStart@
dataStart :: IsMDLVertexAttributeData mdlVertexAttributeData => mdlVertexAttributeData -> IO (Ptr ())
dataStart mdlVertexAttributeData  =
  fmap castPtr $ sendMsg mdlVertexAttributeData (mkSelector "dataStart") (retPtr retVoid) []

-- | @- setDataStart:@
setDataStart :: IsMDLVertexAttributeData mdlVertexAttributeData => mdlVertexAttributeData -> Ptr () -> IO ()
setDataStart mdlVertexAttributeData  value =
  sendMsg mdlVertexAttributeData (mkSelector "setDataStart:") retVoid [argPtr value]

-- | @- stride@
stride :: IsMDLVertexAttributeData mdlVertexAttributeData => mdlVertexAttributeData -> IO CULong
stride mdlVertexAttributeData  =
  sendMsg mdlVertexAttributeData (mkSelector "stride") retCULong []

-- | @- setStride:@
setStride :: IsMDLVertexAttributeData mdlVertexAttributeData => mdlVertexAttributeData -> CULong -> IO ()
setStride mdlVertexAttributeData  value =
  sendMsg mdlVertexAttributeData (mkSelector "setStride:") retVoid [argCULong (fromIntegral value)]

-- | @- format@
format :: IsMDLVertexAttributeData mdlVertexAttributeData => mdlVertexAttributeData -> IO MDLVertexFormat
format mdlVertexAttributeData  =
  fmap (coerce :: CULong -> MDLVertexFormat) $ sendMsg mdlVertexAttributeData (mkSelector "format") retCULong []

-- | @- setFormat:@
setFormat :: IsMDLVertexAttributeData mdlVertexAttributeData => mdlVertexAttributeData -> MDLVertexFormat -> IO ()
setFormat mdlVertexAttributeData  value =
  sendMsg mdlVertexAttributeData (mkSelector "setFormat:") retVoid [argCULong (coerce value)]

-- | @- bufferSize@
bufferSize :: IsMDLVertexAttributeData mdlVertexAttributeData => mdlVertexAttributeData -> IO CULong
bufferSize mdlVertexAttributeData  =
  sendMsg mdlVertexAttributeData (mkSelector "bufferSize") retCULong []

-- | @- setBufferSize:@
setBufferSize :: IsMDLVertexAttributeData mdlVertexAttributeData => mdlVertexAttributeData -> CULong -> IO ()
setBufferSize mdlVertexAttributeData  value =
  sendMsg mdlVertexAttributeData (mkSelector "setBufferSize:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @map@
mapSelector :: Selector
mapSelector = mkSelector "map"

-- | @Selector@ for @setMap:@
setMapSelector :: Selector
setMapSelector = mkSelector "setMap:"

-- | @Selector@ for @dataStart@
dataStartSelector :: Selector
dataStartSelector = mkSelector "dataStart"

-- | @Selector@ for @setDataStart:@
setDataStartSelector :: Selector
setDataStartSelector = mkSelector "setDataStart:"

-- | @Selector@ for @stride@
strideSelector :: Selector
strideSelector = mkSelector "stride"

-- | @Selector@ for @setStride:@
setStrideSelector :: Selector
setStrideSelector = mkSelector "setStride:"

-- | @Selector@ for @format@
formatSelector :: Selector
formatSelector = mkSelector "format"

-- | @Selector@ for @setFormat:@
setFormatSelector :: Selector
setFormatSelector = mkSelector "setFormat:"

-- | @Selector@ for @bufferSize@
bufferSizeSelector :: Selector
bufferSizeSelector = mkSelector "bufferSize"

-- | @Selector@ for @setBufferSize:@
setBufferSizeSelector :: Selector
setBufferSizeSelector = mkSelector "setBufferSize:"

