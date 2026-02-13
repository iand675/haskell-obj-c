{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , bufferIndexSelector
  , formatSelector
  , offsetSelector
  , setBufferIndexSelector
  , setFormatSelector
  , setOffsetSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- format@
format :: IsMTLVertexAttributeDescriptor mtlVertexAttributeDescriptor => mtlVertexAttributeDescriptor -> IO MTLVertexFormat
format mtlVertexAttributeDescriptor =
  sendMessage mtlVertexAttributeDescriptor formatSelector

-- | @- setFormat:@
setFormat :: IsMTLVertexAttributeDescriptor mtlVertexAttributeDescriptor => mtlVertexAttributeDescriptor -> MTLVertexFormat -> IO ()
setFormat mtlVertexAttributeDescriptor value =
  sendMessage mtlVertexAttributeDescriptor setFormatSelector value

-- | @- offset@
offset :: IsMTLVertexAttributeDescriptor mtlVertexAttributeDescriptor => mtlVertexAttributeDescriptor -> IO CULong
offset mtlVertexAttributeDescriptor =
  sendMessage mtlVertexAttributeDescriptor offsetSelector

-- | @- setOffset:@
setOffset :: IsMTLVertexAttributeDescriptor mtlVertexAttributeDescriptor => mtlVertexAttributeDescriptor -> CULong -> IO ()
setOffset mtlVertexAttributeDescriptor value =
  sendMessage mtlVertexAttributeDescriptor setOffsetSelector value

-- | @- bufferIndex@
bufferIndex :: IsMTLVertexAttributeDescriptor mtlVertexAttributeDescriptor => mtlVertexAttributeDescriptor -> IO CULong
bufferIndex mtlVertexAttributeDescriptor =
  sendMessage mtlVertexAttributeDescriptor bufferIndexSelector

-- | @- setBufferIndex:@
setBufferIndex :: IsMTLVertexAttributeDescriptor mtlVertexAttributeDescriptor => mtlVertexAttributeDescriptor -> CULong -> IO ()
setBufferIndex mtlVertexAttributeDescriptor value =
  sendMessage mtlVertexAttributeDescriptor setBufferIndexSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @format@
formatSelector :: Selector '[] MTLVertexFormat
formatSelector = mkSelector "format"

-- | @Selector@ for @setFormat:@
setFormatSelector :: Selector '[MTLVertexFormat] ()
setFormatSelector = mkSelector "setFormat:"

-- | @Selector@ for @offset@
offsetSelector :: Selector '[] CULong
offsetSelector = mkSelector "offset"

-- | @Selector@ for @setOffset:@
setOffsetSelector :: Selector '[CULong] ()
setOffsetSelector = mkSelector "setOffset:"

-- | @Selector@ for @bufferIndex@
bufferIndexSelector :: Selector '[] CULong
bufferIndexSelector = mkSelector "bufferIndex"

-- | @Selector@ for @setBufferIndex:@
setBufferIndexSelector :: Selector '[CULong] ()
setBufferIndexSelector = mkSelector "setBufferIndex:"

