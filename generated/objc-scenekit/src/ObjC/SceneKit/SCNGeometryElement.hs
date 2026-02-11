{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNGeometryElement
--
-- A geometry element describes how vertices from a geometry source are connected together.
--
-- Generated bindings for @SCNGeometryElement@.
module ObjC.SceneKit.SCNGeometryElement
  ( SCNGeometryElement
  , IsSCNGeometryElement(..)
  , geometryElementWithData_primitiveType_primitiveCount_bytesPerIndex
  , geometryElementWithData_primitiveType_primitiveCount_indicesChannelCount_interleavedIndicesChannels_bytesPerIndex
  , geometryElementWithBuffer_primitiveType_primitiveCount_bytesPerIndex
  , geometryElementWithBuffer_primitiveType_primitiveCount_indicesChannelCount_interleavedIndicesChannels_bytesPerIndex
  , data_
  , primitiveType
  , primitiveCount
  , interleavedIndicesChannels
  , indicesChannelCount
  , bytesPerIndex
  , primitiveRange
  , setPrimitiveRange
  , pointSize
  , setPointSize
  , minimumPointScreenSpaceRadius
  , setMinimumPointScreenSpaceRadius
  , maximumPointScreenSpaceRadius
  , setMaximumPointScreenSpaceRadius
  , geometryElementWithData_primitiveType_primitiveCount_bytesPerIndexSelector
  , geometryElementWithData_primitiveType_primitiveCount_indicesChannelCount_interleavedIndicesChannels_bytesPerIndexSelector
  , geometryElementWithBuffer_primitiveType_primitiveCount_bytesPerIndexSelector
  , geometryElementWithBuffer_primitiveType_primitiveCount_indicesChannelCount_interleavedIndicesChannels_bytesPerIndexSelector
  , dataSelector
  , primitiveTypeSelector
  , primitiveCountSelector
  , interleavedIndicesChannelsSelector
  , indicesChannelCountSelector
  , bytesPerIndexSelector
  , primitiveRangeSelector
  , setPrimitiveRangeSelector
  , pointSizeSelector
  , setPointSizeSelector
  , minimumPointScreenSpaceRadiusSelector
  , setMinimumPointScreenSpaceRadiusSelector
  , maximumPointScreenSpaceRadiusSelector
  , setMaximumPointScreenSpaceRadiusSelector

  -- * Enum types
  , SCNGeometryPrimitiveType(SCNGeometryPrimitiveType)
  , pattern SCNGeometryPrimitiveTypeTriangles
  , pattern SCNGeometryPrimitiveTypeTriangleStrip
  , pattern SCNGeometryPrimitiveTypeLine
  , pattern SCNGeometryPrimitiveTypePoint
  , pattern SCNGeometryPrimitiveTypePolygon

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.SceneKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | geometryElementWithData:primitiveType:primitiveCount:bytesPerIndex:
--
-- Creates and returns a geometry element from the given data and data format info.
--
-- @data@ — The data that contains element indices. You can pass nil to use an implicit vertex ordering (0,1,2,…).
--
-- @primitiveType@ — The primitive type, as listed in the SCNGeometryPrimitiveType enumeration.
--
-- @primitiveCount@ — The number of primitives in the data.
--
-- @bytesPerIndex@ — The number of bytes that represent a single index value in the data.
--
-- ObjC selector: @+ geometryElementWithData:primitiveType:primitiveCount:bytesPerIndex:@
geometryElementWithData_primitiveType_primitiveCount_bytesPerIndex :: IsNSData data_ => data_ -> SCNGeometryPrimitiveType -> CLong -> CLong -> IO (Id SCNGeometryElement)
geometryElementWithData_primitiveType_primitiveCount_bytesPerIndex data_ primitiveType primitiveCount bytesPerIndex =
  do
    cls' <- getRequiredClass "SCNGeometryElement"
    withObjCPtr data_ $ \raw_data_ ->
      sendClassMsg cls' (mkSelector "geometryElementWithData:primitiveType:primitiveCount:bytesPerIndex:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argCLong (coerce primitiveType), argCLong (fromIntegral primitiveCount), argCLong (fromIntegral bytesPerIndex)] >>= retainedObject . castPtr

-- | geometryElementWithData:primitiveType:primitiveCount:indicesChannelCount:interleavedIndicesChannels:bytesPerIndex:
--
-- @data@ — The data that contains element indices. You can pass nil to use an implicit vertex ordering (0,1,2,…).
--
-- @primitiveType@ — The primitive type, as listed in the SCNGeometryPrimitiveType enumeration.
--
-- @primitiveCount@ — The number of primitives in the data.
--
-- @indicesChannelCount@ — The number of channels for the vertex indices.
--
-- @interleavedIndicesChannels@ — Whether the channels are interleaved.
--
-- @bytesPerIndex@ — The number of bytes that represent a single index value in the data.
--
-- ObjC selector: @+ geometryElementWithData:primitiveType:primitiveCount:indicesChannelCount:interleavedIndicesChannels:bytesPerIndex:@
geometryElementWithData_primitiveType_primitiveCount_indicesChannelCount_interleavedIndicesChannels_bytesPerIndex :: IsNSData data_ => data_ -> SCNGeometryPrimitiveType -> CLong -> CLong -> Bool -> CLong -> IO (Id SCNGeometryElement)
geometryElementWithData_primitiveType_primitiveCount_indicesChannelCount_interleavedIndicesChannels_bytesPerIndex data_ primitiveType primitiveCount indicesChannelCount interleavedIndicesChannels bytesPerIndex =
  do
    cls' <- getRequiredClass "SCNGeometryElement"
    withObjCPtr data_ $ \raw_data_ ->
      sendClassMsg cls' (mkSelector "geometryElementWithData:primitiveType:primitiveCount:indicesChannelCount:interleavedIndicesChannels:bytesPerIndex:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argCLong (coerce primitiveType), argCLong (fromIntegral primitiveCount), argCLong (fromIntegral indicesChannelCount), argCULong (if interleavedIndicesChannels then 1 else 0), argCLong (fromIntegral bytesPerIndex)] >>= retainedObject . castPtr

-- | geometryElementWithBuffer:primitiveType:primitiveCount:bytesPerIndex:
--
-- Creates and returns a geometry element from the given Metal buffer and parameters.
--
-- @buffer@ — The buffer that contains element indices.
--
-- @primitiveType@ — The primitive type, as listed in the SCNGeometryPrimitiveType enumeration.
--
-- @primitiveCount@ — The number of primitives in the data.
--
-- @bytesPerIndex@ — The number of bytes that represent a single index value in the data.
--
-- ObjC selector: @+ geometryElementWithBuffer:primitiveType:primitiveCount:bytesPerIndex:@
geometryElementWithBuffer_primitiveType_primitiveCount_bytesPerIndex :: RawId -> SCNGeometryPrimitiveType -> CLong -> CLong -> IO (Id SCNGeometryElement)
geometryElementWithBuffer_primitiveType_primitiveCount_bytesPerIndex buffer primitiveType primitiveCount bytesPerIndex =
  do
    cls' <- getRequiredClass "SCNGeometryElement"
    sendClassMsg cls' (mkSelector "geometryElementWithBuffer:primitiveType:primitiveCount:bytesPerIndex:") (retPtr retVoid) [argPtr (castPtr (unRawId buffer) :: Ptr ()), argCLong (coerce primitiveType), argCLong (fromIntegral primitiveCount), argCLong (fromIntegral bytesPerIndex)] >>= retainedObject . castPtr

-- | @+ geometryElementWithBuffer:primitiveType:primitiveCount:indicesChannelCount:interleavedIndicesChannels:bytesPerIndex:@
geometryElementWithBuffer_primitiveType_primitiveCount_indicesChannelCount_interleavedIndicesChannels_bytesPerIndex :: RawId -> SCNGeometryPrimitiveType -> CLong -> CLong -> Bool -> CLong -> IO (Id SCNGeometryElement)
geometryElementWithBuffer_primitiveType_primitiveCount_indicesChannelCount_interleavedIndicesChannels_bytesPerIndex buffer primitiveType primitiveCount indicesChannelCount interleavedIndicesChannels bytesPerIndex =
  do
    cls' <- getRequiredClass "SCNGeometryElement"
    sendClassMsg cls' (mkSelector "geometryElementWithBuffer:primitiveType:primitiveCount:indicesChannelCount:interleavedIndicesChannels:bytesPerIndex:") (retPtr retVoid) [argPtr (castPtr (unRawId buffer) :: Ptr ()), argCLong (coerce primitiveType), argCLong (fromIntegral primitiveCount), argCLong (fromIntegral indicesChannelCount), argCULong (if interleavedIndicesChannels then 1 else 0), argCLong (fromIntegral bytesPerIndex)] >>= retainedObject . castPtr

-- | data
--
-- The data for the geometry element
--
-- ObjC selector: @- data@
data_ :: IsSCNGeometryElement scnGeometryElement => scnGeometryElement -> IO (Id NSData)
data_ scnGeometryElement  =
  sendMsg scnGeometryElement (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | primitiveType
--
-- The type of the geometry element. Possible values are listed in the SCNGeometryPrimitiveType enumeration.
--
-- ObjC selector: @- primitiveType@
primitiveType :: IsSCNGeometryElement scnGeometryElement => scnGeometryElement -> IO SCNGeometryPrimitiveType
primitiveType scnGeometryElement  =
  fmap (coerce :: CLong -> SCNGeometryPrimitiveType) $ sendMsg scnGeometryElement (mkSelector "primitiveType") retCLong []

-- | primitiveCount
--
-- The number of primitives in the data.
--
-- ObjC selector: @- primitiveCount@
primitiveCount :: IsSCNGeometryElement scnGeometryElement => scnGeometryElement -> IO CLong
primitiveCount scnGeometryElement  =
  sendMsg scnGeometryElement (mkSelector "primitiveCount") retCLong []

-- | interleavedIndicesChannels
--
-- Determines whether the channels are interleaved.
--
-- ObjC selector: @- interleavedIndicesChannels@
interleavedIndicesChannels :: IsSCNGeometryElement scnGeometryElement => scnGeometryElement -> IO Bool
interleavedIndicesChannels scnGeometryElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnGeometryElement (mkSelector "interleavedIndicesChannels") retCULong []

-- | indicesChannelCount
--
-- The number of channels in the geometry element.
--
-- ObjC selector: @- indicesChannelCount@
indicesChannelCount :: IsSCNGeometryElement scnGeometryElement => scnGeometryElement -> IO CLong
indicesChannelCount scnGeometryElement  =
  sendMsg scnGeometryElement (mkSelector "indicesChannelCount") retCLong []

-- | bytesPerIndex
--
-- The number of bytes that represent an index value
--
-- ObjC selector: @- bytesPerIndex@
bytesPerIndex :: IsSCNGeometryElement scnGeometryElement => scnGeometryElement -> IO CLong
bytesPerIndex scnGeometryElement  =
  sendMsg scnGeometryElement (mkSelector "bytesPerIndex") retCLong []

-- | primitiveRange
--
-- Specifies the subrange of primitives to render within NSMakeRange(0, primitiveCount). Defaults to NSMakeRange(NSNotFound, 0).
--
-- When the location of the range is set to NSNotFound, the entire geometry element is rendered.
--
-- ObjC selector: @- primitiveRange@
primitiveRange :: IsSCNGeometryElement scnGeometryElement => scnGeometryElement -> IO NSRange
primitiveRange scnGeometryElement  =
  sendMsgStret scnGeometryElement (mkSelector "primitiveRange") retNSRange []

-- | primitiveRange
--
-- Specifies the subrange of primitives to render within NSMakeRange(0, primitiveCount). Defaults to NSMakeRange(NSNotFound, 0).
--
-- When the location of the range is set to NSNotFound, the entire geometry element is rendered.
--
-- ObjC selector: @- setPrimitiveRange:@
setPrimitiveRange :: IsSCNGeometryElement scnGeometryElement => scnGeometryElement -> NSRange -> IO ()
setPrimitiveRange scnGeometryElement  value =
  sendMsg scnGeometryElement (mkSelector "setPrimitiveRange:") retVoid [argNSRange value]

-- | pointSize
--
-- Specifies the size of the point in local space. Defaults to 1
--
-- ObjC selector: @- pointSize@
pointSize :: IsSCNGeometryElement scnGeometryElement => scnGeometryElement -> IO CDouble
pointSize scnGeometryElement  =
  sendMsg scnGeometryElement (mkSelector "pointSize") retCDouble []

-- | pointSize
--
-- Specifies the size of the point in local space. Defaults to 1
--
-- ObjC selector: @- setPointSize:@
setPointSize :: IsSCNGeometryElement scnGeometryElement => scnGeometryElement -> CDouble -> IO ()
setPointSize scnGeometryElement  value =
  sendMsg scnGeometryElement (mkSelector "setPointSize:") retVoid [argCDouble (fromIntegral value)]

-- | minimumPointScreenSpaceRadius
--
-- Specifies the minimum size in screen-space (in pixel). Defaults to 1
--
-- ObjC selector: @- minimumPointScreenSpaceRadius@
minimumPointScreenSpaceRadius :: IsSCNGeometryElement scnGeometryElement => scnGeometryElement -> IO CDouble
minimumPointScreenSpaceRadius scnGeometryElement  =
  sendMsg scnGeometryElement (mkSelector "minimumPointScreenSpaceRadius") retCDouble []

-- | minimumPointScreenSpaceRadius
--
-- Specifies the minimum size in screen-space (in pixel). Defaults to 1
--
-- ObjC selector: @- setMinimumPointScreenSpaceRadius:@
setMinimumPointScreenSpaceRadius :: IsSCNGeometryElement scnGeometryElement => scnGeometryElement -> CDouble -> IO ()
setMinimumPointScreenSpaceRadius scnGeometryElement  value =
  sendMsg scnGeometryElement (mkSelector "setMinimumPointScreenSpaceRadius:") retVoid [argCDouble (fromIntegral value)]

-- | maximumPointScreenSpaceRadius
--
-- Specifies the maximum size in screen-space (in pixel). Defaults to 1
--
-- ObjC selector: @- maximumPointScreenSpaceRadius@
maximumPointScreenSpaceRadius :: IsSCNGeometryElement scnGeometryElement => scnGeometryElement -> IO CDouble
maximumPointScreenSpaceRadius scnGeometryElement  =
  sendMsg scnGeometryElement (mkSelector "maximumPointScreenSpaceRadius") retCDouble []

-- | maximumPointScreenSpaceRadius
--
-- Specifies the maximum size in screen-space (in pixel). Defaults to 1
--
-- ObjC selector: @- setMaximumPointScreenSpaceRadius:@
setMaximumPointScreenSpaceRadius :: IsSCNGeometryElement scnGeometryElement => scnGeometryElement -> CDouble -> IO ()
setMaximumPointScreenSpaceRadius scnGeometryElement  value =
  sendMsg scnGeometryElement (mkSelector "setMaximumPointScreenSpaceRadius:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @geometryElementWithData:primitiveType:primitiveCount:bytesPerIndex:@
geometryElementWithData_primitiveType_primitiveCount_bytesPerIndexSelector :: Selector
geometryElementWithData_primitiveType_primitiveCount_bytesPerIndexSelector = mkSelector "geometryElementWithData:primitiveType:primitiveCount:bytesPerIndex:"

-- | @Selector@ for @geometryElementWithData:primitiveType:primitiveCount:indicesChannelCount:interleavedIndicesChannels:bytesPerIndex:@
geometryElementWithData_primitiveType_primitiveCount_indicesChannelCount_interleavedIndicesChannels_bytesPerIndexSelector :: Selector
geometryElementWithData_primitiveType_primitiveCount_indicesChannelCount_interleavedIndicesChannels_bytesPerIndexSelector = mkSelector "geometryElementWithData:primitiveType:primitiveCount:indicesChannelCount:interleavedIndicesChannels:bytesPerIndex:"

-- | @Selector@ for @geometryElementWithBuffer:primitiveType:primitiveCount:bytesPerIndex:@
geometryElementWithBuffer_primitiveType_primitiveCount_bytesPerIndexSelector :: Selector
geometryElementWithBuffer_primitiveType_primitiveCount_bytesPerIndexSelector = mkSelector "geometryElementWithBuffer:primitiveType:primitiveCount:bytesPerIndex:"

-- | @Selector@ for @geometryElementWithBuffer:primitiveType:primitiveCount:indicesChannelCount:interleavedIndicesChannels:bytesPerIndex:@
geometryElementWithBuffer_primitiveType_primitiveCount_indicesChannelCount_interleavedIndicesChannels_bytesPerIndexSelector :: Selector
geometryElementWithBuffer_primitiveType_primitiveCount_indicesChannelCount_interleavedIndicesChannels_bytesPerIndexSelector = mkSelector "geometryElementWithBuffer:primitiveType:primitiveCount:indicesChannelCount:interleavedIndicesChannels:bytesPerIndex:"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

-- | @Selector@ for @primitiveType@
primitiveTypeSelector :: Selector
primitiveTypeSelector = mkSelector "primitiveType"

-- | @Selector@ for @primitiveCount@
primitiveCountSelector :: Selector
primitiveCountSelector = mkSelector "primitiveCount"

-- | @Selector@ for @interleavedIndicesChannels@
interleavedIndicesChannelsSelector :: Selector
interleavedIndicesChannelsSelector = mkSelector "interleavedIndicesChannels"

-- | @Selector@ for @indicesChannelCount@
indicesChannelCountSelector :: Selector
indicesChannelCountSelector = mkSelector "indicesChannelCount"

-- | @Selector@ for @bytesPerIndex@
bytesPerIndexSelector :: Selector
bytesPerIndexSelector = mkSelector "bytesPerIndex"

-- | @Selector@ for @primitiveRange@
primitiveRangeSelector :: Selector
primitiveRangeSelector = mkSelector "primitiveRange"

-- | @Selector@ for @setPrimitiveRange:@
setPrimitiveRangeSelector :: Selector
setPrimitiveRangeSelector = mkSelector "setPrimitiveRange:"

-- | @Selector@ for @pointSize@
pointSizeSelector :: Selector
pointSizeSelector = mkSelector "pointSize"

-- | @Selector@ for @setPointSize:@
setPointSizeSelector :: Selector
setPointSizeSelector = mkSelector "setPointSize:"

-- | @Selector@ for @minimumPointScreenSpaceRadius@
minimumPointScreenSpaceRadiusSelector :: Selector
minimumPointScreenSpaceRadiusSelector = mkSelector "minimumPointScreenSpaceRadius"

-- | @Selector@ for @setMinimumPointScreenSpaceRadius:@
setMinimumPointScreenSpaceRadiusSelector :: Selector
setMinimumPointScreenSpaceRadiusSelector = mkSelector "setMinimumPointScreenSpaceRadius:"

-- | @Selector@ for @maximumPointScreenSpaceRadius@
maximumPointScreenSpaceRadiusSelector :: Selector
maximumPointScreenSpaceRadiusSelector = mkSelector "maximumPointScreenSpaceRadius"

-- | @Selector@ for @setMaximumPointScreenSpaceRadius:@
setMaximumPointScreenSpaceRadiusSelector :: Selector
setMaximumPointScreenSpaceRadiusSelector = mkSelector "setMaximumPointScreenSpaceRadius:"

