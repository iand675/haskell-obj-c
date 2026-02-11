{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MDLVertexAttribute
--
-- Structure with properties of a vertex attribute
--
-- Generated bindings for @MDLVertexAttribute@.
module ObjC.ModelIO.MDLVertexAttribute
  ( MDLVertexAttribute
  , IsMDLVertexAttribute(..)
  , initWithName_format_offset_bufferIndex
  , name
  , setName
  , format
  , setFormat
  , offset
  , setOffset
  , bufferIndex
  , setBufferIndex
  , time
  , setTime
  , initWithName_format_offset_bufferIndexSelector
  , nameSelector
  , setNameSelector
  , formatSelector
  , setFormatSelector
  , offsetSelector
  , setOffsetSelector
  , bufferIndexSelector
  , setBufferIndexSelector
  , timeSelector
  , setTimeSelector

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

-- | initWithName:format:offset:bufferIndex
--
-- Initialize attribute object with all properties
--
-- ObjC selector: @- initWithName:format:offset:bufferIndex:@
initWithName_format_offset_bufferIndex :: (IsMDLVertexAttribute mdlVertexAttribute, IsNSString name) => mdlVertexAttribute -> name -> MDLVertexFormat -> CULong -> CULong -> IO (Id MDLVertexAttribute)
initWithName_format_offset_bufferIndex mdlVertexAttribute  name format offset bufferIndex =
withObjCPtr name $ \raw_name ->
    sendMsg mdlVertexAttribute (mkSelector "initWithName:format:offset:bufferIndex:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argCULong (coerce format), argCULong (fromIntegral offset), argCULong (fromIntegral bufferIndex)] >>= ownedObject . castPtr

-- | name
--
-- Identifying name of the attribute derived from model file, or one of            the predefined MDLVertexAttribute strings
--
-- ObjC selector: @- name@
name :: IsMDLVertexAttribute mdlVertexAttribute => mdlVertexAttribute -> IO (Id NSString)
name mdlVertexAttribute  =
  sendMsg mdlVertexAttribute (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | name
--
-- Identifying name of the attribute derived from model file, or one of            the predefined MDLVertexAttribute strings
--
-- ObjC selector: @- setName:@
setName :: (IsMDLVertexAttribute mdlVertexAttribute, IsNSString value) => mdlVertexAttribute -> value -> IO ()
setName mdlVertexAttribute  value =
withObjCPtr value $ \raw_value ->
    sendMsg mdlVertexAttribute (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | format
--
-- Format (including number of components) of the attribute
--
-- If the value is MDLVertexFormatInvalid.   Other values of this              object will be ignored when setting the MDLVertexDescriptor object              in a Mesh. The initial value is MDLVertexFormatInvalid.
--
-- ObjC selector: @- format@
format :: IsMDLVertexAttribute mdlVertexAttribute => mdlVertexAttribute -> IO MDLVertexFormat
format mdlVertexAttribute  =
  fmap (coerce :: CULong -> MDLVertexFormat) $ sendMsg mdlVertexAttribute (mkSelector "format") retCULong []

-- | format
--
-- Format (including number of components) of the attribute
--
-- If the value is MDLVertexFormatInvalid.   Other values of this              object will be ignored when setting the MDLVertexDescriptor object              in a Mesh. The initial value is MDLVertexFormatInvalid.
--
-- ObjC selector: @- setFormat:@
setFormat :: IsMDLVertexAttribute mdlVertexAttribute => mdlVertexAttribute -> MDLVertexFormat -> IO ()
setFormat mdlVertexAttribute  value =
  sendMsg mdlVertexAttribute (mkSelector "setFormat:") retVoid [argCULong (coerce value)]

-- | offset
--
-- offset in bytes of the attrbute in each element of the vertex buffer
--
-- ObjC selector: @- offset@
offset :: IsMDLVertexAttribute mdlVertexAttribute => mdlVertexAttribute -> IO CULong
offset mdlVertexAttribute  =
  sendMsg mdlVertexAttribute (mkSelector "offset") retCULong []

-- | offset
--
-- offset in bytes of the attrbute in each element of the vertex buffer
--
-- ObjC selector: @- setOffset:@
setOffset :: IsMDLVertexAttribute mdlVertexAttribute => mdlVertexAttribute -> CULong -> IO ()
setOffset mdlVertexAttribute  value =
  sendMsg mdlVertexAttribute (mkSelector "setOffset:") retVoid [argCULong (fromIntegral value)]

-- | bufferIndex
--
-- index of the buffer in mesh's vertexBuffer array in which this            attribute resides
--
-- ObjC selector: @- bufferIndex@
bufferIndex :: IsMDLVertexAttribute mdlVertexAttribute => mdlVertexAttribute -> IO CULong
bufferIndex mdlVertexAttribute  =
  sendMsg mdlVertexAttribute (mkSelector "bufferIndex") retCULong []

-- | bufferIndex
--
-- index of the buffer in mesh's vertexBuffer array in which this            attribute resides
--
-- ObjC selector: @- setBufferIndex:@
setBufferIndex :: IsMDLVertexAttribute mdlVertexAttribute => mdlVertexAttribute -> CULong -> IO ()
setBufferIndex mdlVertexAttribute  value =
  sendMsg mdlVertexAttribute (mkSelector "setBufferIndex:") retVoid [argCULong (fromIntegral value)]

-- | time
--
-- the time the attribute is intended for.
--
-- morph targets would store their times here
--
-- ObjC selector: @- time@
time :: IsMDLVertexAttribute mdlVertexAttribute => mdlVertexAttribute -> IO CDouble
time mdlVertexAttribute  =
  sendMsg mdlVertexAttribute (mkSelector "time") retCDouble []

-- | time
--
-- the time the attribute is intended for.
--
-- morph targets would store their times here
--
-- ObjC selector: @- setTime:@
setTime :: IsMDLVertexAttribute mdlVertexAttribute => mdlVertexAttribute -> CDouble -> IO ()
setTime mdlVertexAttribute  value =
  sendMsg mdlVertexAttribute (mkSelector "setTime:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:format:offset:bufferIndex:@
initWithName_format_offset_bufferIndexSelector :: Selector
initWithName_format_offset_bufferIndexSelector = mkSelector "initWithName:format:offset:bufferIndex:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

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

-- | @Selector@ for @time@
timeSelector :: Selector
timeSelector = mkSelector "time"

-- | @Selector@ for @setTime:@
setTimeSelector :: Selector
setTimeSelector = mkSelector "setTime:"

