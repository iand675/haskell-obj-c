{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTLAttributeDescriptor@.
module ObjC.Metal.MTLAttributeDescriptor
  ( MTLAttributeDescriptor
  , IsMTLAttributeDescriptor(..)
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
  , MTLAttributeFormat(MTLAttributeFormat)
  , pattern MTLAttributeFormatInvalid
  , pattern MTLAttributeFormatUChar2
  , pattern MTLAttributeFormatUChar3
  , pattern MTLAttributeFormatUChar4
  , pattern MTLAttributeFormatChar2
  , pattern MTLAttributeFormatChar3
  , pattern MTLAttributeFormatChar4
  , pattern MTLAttributeFormatUChar2Normalized
  , pattern MTLAttributeFormatUChar3Normalized
  , pattern MTLAttributeFormatUChar4Normalized
  , pattern MTLAttributeFormatChar2Normalized
  , pattern MTLAttributeFormatChar3Normalized
  , pattern MTLAttributeFormatChar4Normalized
  , pattern MTLAttributeFormatUShort2
  , pattern MTLAttributeFormatUShort3
  , pattern MTLAttributeFormatUShort4
  , pattern MTLAttributeFormatShort2
  , pattern MTLAttributeFormatShort3
  , pattern MTLAttributeFormatShort4
  , pattern MTLAttributeFormatUShort2Normalized
  , pattern MTLAttributeFormatUShort3Normalized
  , pattern MTLAttributeFormatUShort4Normalized
  , pattern MTLAttributeFormatShort2Normalized
  , pattern MTLAttributeFormatShort3Normalized
  , pattern MTLAttributeFormatShort4Normalized
  , pattern MTLAttributeFormatHalf2
  , pattern MTLAttributeFormatHalf3
  , pattern MTLAttributeFormatHalf4
  , pattern MTLAttributeFormatFloat
  , pattern MTLAttributeFormatFloat2
  , pattern MTLAttributeFormatFloat3
  , pattern MTLAttributeFormatFloat4
  , pattern MTLAttributeFormatInt
  , pattern MTLAttributeFormatInt2
  , pattern MTLAttributeFormatInt3
  , pattern MTLAttributeFormatInt4
  , pattern MTLAttributeFormatUInt
  , pattern MTLAttributeFormatUInt2
  , pattern MTLAttributeFormatUInt3
  , pattern MTLAttributeFormatUInt4
  , pattern MTLAttributeFormatInt1010102Normalized
  , pattern MTLAttributeFormatUInt1010102Normalized
  , pattern MTLAttributeFormatUChar4Normalized_BGRA
  , pattern MTLAttributeFormatUChar
  , pattern MTLAttributeFormatChar
  , pattern MTLAttributeFormatUCharNormalized
  , pattern MTLAttributeFormatCharNormalized
  , pattern MTLAttributeFormatUShort
  , pattern MTLAttributeFormatShort
  , pattern MTLAttributeFormatUShortNormalized
  , pattern MTLAttributeFormatShortNormalized
  , pattern MTLAttributeFormatHalf
  , pattern MTLAttributeFormatFloatRG11B10
  , pattern MTLAttributeFormatFloatRGB9E5

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
format :: IsMTLAttributeDescriptor mtlAttributeDescriptor => mtlAttributeDescriptor -> IO MTLAttributeFormat
format mtlAttributeDescriptor =
  sendMessage mtlAttributeDescriptor formatSelector

-- | @- setFormat:@
setFormat :: IsMTLAttributeDescriptor mtlAttributeDescriptor => mtlAttributeDescriptor -> MTLAttributeFormat -> IO ()
setFormat mtlAttributeDescriptor value =
  sendMessage mtlAttributeDescriptor setFormatSelector value

-- | @- offset@
offset :: IsMTLAttributeDescriptor mtlAttributeDescriptor => mtlAttributeDescriptor -> IO CULong
offset mtlAttributeDescriptor =
  sendMessage mtlAttributeDescriptor offsetSelector

-- | @- setOffset:@
setOffset :: IsMTLAttributeDescriptor mtlAttributeDescriptor => mtlAttributeDescriptor -> CULong -> IO ()
setOffset mtlAttributeDescriptor value =
  sendMessage mtlAttributeDescriptor setOffsetSelector value

-- | @- bufferIndex@
bufferIndex :: IsMTLAttributeDescriptor mtlAttributeDescriptor => mtlAttributeDescriptor -> IO CULong
bufferIndex mtlAttributeDescriptor =
  sendMessage mtlAttributeDescriptor bufferIndexSelector

-- | @- setBufferIndex:@
setBufferIndex :: IsMTLAttributeDescriptor mtlAttributeDescriptor => mtlAttributeDescriptor -> CULong -> IO ()
setBufferIndex mtlAttributeDescriptor value =
  sendMessage mtlAttributeDescriptor setBufferIndexSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @format@
formatSelector :: Selector '[] MTLAttributeFormat
formatSelector = mkSelector "format"

-- | @Selector@ for @setFormat:@
setFormatSelector :: Selector '[MTLAttributeFormat] ()
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

