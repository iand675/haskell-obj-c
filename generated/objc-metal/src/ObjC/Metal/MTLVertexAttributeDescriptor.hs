{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLVertexAttributeDescriptor@.
module ObjC.Metal.MTLVertexAttributeDescriptor
  ( MTLVertexAttributeDescriptor
  , IsMTLVertexAttributeDescriptor(..)
  , format
  , setFormat
  , offset
  , setOffset
  , bufferIndex
  , setBufferIndex
  , formatSelector
  , setFormatSelector
  , offsetSelector
  , setOffsetSelector
  , bufferIndexSelector
  , setBufferIndexSelector

  -- * Enum types
  , MTLVertexFormat(MTLVertexFormat)
  , pattern MTLVertexFormatInvalid
  , pattern MTLVertexFormatUChar2
  , pattern MTLVertexFormatUChar3
  , pattern MTLVertexFormatUChar4
  , pattern MTLVertexFormatChar2
  , pattern MTLVertexFormatChar3
  , pattern MTLVertexFormatChar4
  , pattern MTLVertexFormatUChar2Normalized
  , pattern MTLVertexFormatUChar3Normalized
  , pattern MTLVertexFormatUChar4Normalized
  , pattern MTLVertexFormatChar2Normalized
  , pattern MTLVertexFormatChar3Normalized
  , pattern MTLVertexFormatChar4Normalized
  , pattern MTLVertexFormatUShort2
  , pattern MTLVertexFormatUShort3
  , pattern MTLVertexFormatUShort4
  , pattern MTLVertexFormatShort2
  , pattern MTLVertexFormatShort3
  , pattern MTLVertexFormatShort4
  , pattern MTLVertexFormatUShort2Normalized
  , pattern MTLVertexFormatUShort3Normalized
  , pattern MTLVertexFormatUShort4Normalized
  , pattern MTLVertexFormatShort2Normalized
  , pattern MTLVertexFormatShort3Normalized
  , pattern MTLVertexFormatShort4Normalized
  , pattern MTLVertexFormatHalf2
  , pattern MTLVertexFormatHalf3
  , pattern MTLVertexFormatHalf4
  , pattern MTLVertexFormatFloat
  , pattern MTLVertexFormatFloat2
  , pattern MTLVertexFormatFloat3
  , pattern MTLVertexFormatFloat4
  , pattern MTLVertexFormatInt
  , pattern MTLVertexFormatInt2
  , pattern MTLVertexFormatInt3
  , pattern MTLVertexFormatInt4
  , pattern MTLVertexFormatUInt
  , pattern MTLVertexFormatUInt2
  , pattern MTLVertexFormatUInt3
  , pattern MTLVertexFormatUInt4
  , pattern MTLVertexFormatInt1010102Normalized
  , pattern MTLVertexFormatUInt1010102Normalized
  , pattern MTLVertexFormatUChar4Normalized_BGRA
  , pattern MTLVertexFormatUChar
  , pattern MTLVertexFormatChar
  , pattern MTLVertexFormatUCharNormalized
  , pattern MTLVertexFormatCharNormalized
  , pattern MTLVertexFormatUShort
  , pattern MTLVertexFormatShort
  , pattern MTLVertexFormatUShortNormalized
  , pattern MTLVertexFormatShortNormalized
  , pattern MTLVertexFormatHalf
  , pattern MTLVertexFormatFloatRG11B10
  , pattern MTLVertexFormatFloatRGB9E5

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

import ObjC.Metal.Internal.Classes
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- format@
format :: IsMTLVertexAttributeDescriptor mtlVertexAttributeDescriptor => mtlVertexAttributeDescriptor -> IO MTLVertexFormat
format mtlVertexAttributeDescriptor  =
  fmap (coerce :: CULong -> MTLVertexFormat) $ sendMsg mtlVertexAttributeDescriptor (mkSelector "format") retCULong []

-- | @- setFormat:@
setFormat :: IsMTLVertexAttributeDescriptor mtlVertexAttributeDescriptor => mtlVertexAttributeDescriptor -> MTLVertexFormat -> IO ()
setFormat mtlVertexAttributeDescriptor  value =
  sendMsg mtlVertexAttributeDescriptor (mkSelector "setFormat:") retVoid [argCULong (coerce value)]

-- | @- offset@
offset :: IsMTLVertexAttributeDescriptor mtlVertexAttributeDescriptor => mtlVertexAttributeDescriptor -> IO CULong
offset mtlVertexAttributeDescriptor  =
  sendMsg mtlVertexAttributeDescriptor (mkSelector "offset") retCULong []

-- | @- setOffset:@
setOffset :: IsMTLVertexAttributeDescriptor mtlVertexAttributeDescriptor => mtlVertexAttributeDescriptor -> CULong -> IO ()
setOffset mtlVertexAttributeDescriptor  value =
  sendMsg mtlVertexAttributeDescriptor (mkSelector "setOffset:") retVoid [argCULong (fromIntegral value)]

-- | @- bufferIndex@
bufferIndex :: IsMTLVertexAttributeDescriptor mtlVertexAttributeDescriptor => mtlVertexAttributeDescriptor -> IO CULong
bufferIndex mtlVertexAttributeDescriptor  =
  sendMsg mtlVertexAttributeDescriptor (mkSelector "bufferIndex") retCULong []

-- | @- setBufferIndex:@
setBufferIndex :: IsMTLVertexAttributeDescriptor mtlVertexAttributeDescriptor => mtlVertexAttributeDescriptor -> CULong -> IO ()
setBufferIndex mtlVertexAttributeDescriptor  value =
  sendMsg mtlVertexAttributeDescriptor (mkSelector "setBufferIndex:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @format@
formatSelector :: Selector
formatSelector = mkSelector "format"

-- | @Selector@ for @setFormat:@
setFormatSelector :: Selector
setFormatSelector = mkSelector "setFormat:"

-- | @Selector@ for @offset@
offsetSelector :: Selector
offsetSelector = mkSelector "offset"

-- | @Selector@ for @setOffset:@
setOffsetSelector :: Selector
setOffsetSelector = mkSelector "setOffset:"

-- | @Selector@ for @bufferIndex@
bufferIndexSelector :: Selector
bufferIndexSelector = mkSelector "bufferIndex"

-- | @Selector@ for @setBufferIndex:@
setBufferIndexSelector :: Selector
setBufferIndexSelector = mkSelector "setBufferIndex:"

