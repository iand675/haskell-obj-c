{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , bytesPerIndexSelector
  , dataSelector
  , geometryElementWithBuffer_primitiveType_primitiveCount_bytesPerIndexSelector
  , geometryElementWithBuffer_primitiveType_primitiveCount_indicesChannelCount_interleavedIndicesChannels_bytesPerIndexSelector
  , geometryElementWithData_primitiveType_primitiveCount_bytesPerIndexSelector
  , geometryElementWithData_primitiveType_primitiveCount_indicesChannelCount_interleavedIndicesChannels_bytesPerIndexSelector
  , indicesChannelCountSelector
  , interleavedIndicesChannelsSelector
  , maximumPointScreenSpaceRadiusSelector
  , minimumPointScreenSpaceRadiusSelector
  , pointSizeSelector
  , primitiveCountSelector
  , primitiveRangeSelector
  , primitiveTypeSelector
  , setMaximumPointScreenSpaceRadiusSelector
  , setMinimumPointScreenSpaceRadiusSelector
  , setPointSizeSelector
  , setPrimitiveRangeSelector

  -- * Enum types
  , SCNGeometryPrimitiveType(SCNGeometryPrimitiveType)
  , pattern SCNGeometryPrimitiveTypeTriangles
  , pattern SCNGeometryPrimitiveTypeTriangleStrip
  , pattern SCNGeometryPrimitiveTypeLine
  , pattern SCNGeometryPrimitiveTypePoint
  , pattern SCNGeometryPrimitiveTypePolygon

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' geometryElementWithData_primitiveType_primitiveCount_bytesPerIndexSelector (toNSData data_) primitiveType primitiveCount bytesPerIndex

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
    sendClassMessage cls' geometryElementWithData_primitiveType_primitiveCount_indicesChannelCount_interleavedIndicesChannels_bytesPerIndexSelector (toNSData data_) primitiveType primitiveCount indicesChannelCount interleavedIndicesChannels bytesPerIndex

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
    sendClassMessage cls' geometryElementWithBuffer_primitiveType_primitiveCount_bytesPerIndexSelector buffer primitiveType primitiveCount bytesPerIndex

-- | @+ geometryElementWithBuffer:primitiveType:primitiveCount:indicesChannelCount:interleavedIndicesChannels:bytesPerIndex:@
geometryElementWithBuffer_primitiveType_primitiveCount_indicesChannelCount_interleavedIndicesChannels_bytesPerIndex :: RawId -> SCNGeometryPrimitiveType -> CLong -> CLong -> Bool -> CLong -> IO (Id SCNGeometryElement)
geometryElementWithBuffer_primitiveType_primitiveCount_indicesChannelCount_interleavedIndicesChannels_bytesPerIndex buffer primitiveType primitiveCount indicesChannelCount interleavedIndicesChannels bytesPerIndex =
  do
    cls' <- getRequiredClass "SCNGeometryElement"
    sendClassMessage cls' geometryElementWithBuffer_primitiveType_primitiveCount_indicesChannelCount_interleavedIndicesChannels_bytesPerIndexSelector buffer primitiveType primitiveCount indicesChannelCount interleavedIndicesChannels bytesPerIndex

-- | data
--
-- The data for the geometry element
--
-- ObjC selector: @- data@
data_ :: IsSCNGeometryElement scnGeometryElement => scnGeometryElement -> IO (Id NSData)
data_ scnGeometryElement =
  sendMessage scnGeometryElement dataSelector

-- | primitiveType
--
-- The type of the geometry element. Possible values are listed in the SCNGeometryPrimitiveType enumeration.
--
-- ObjC selector: @- primitiveType@
primitiveType :: IsSCNGeometryElement scnGeometryElement => scnGeometryElement -> IO SCNGeometryPrimitiveType
primitiveType scnGeometryElement =
  sendMessage scnGeometryElement primitiveTypeSelector

-- | primitiveCount
--
-- The number of primitives in the data.
--
-- ObjC selector: @- primitiveCount@
primitiveCount :: IsSCNGeometryElement scnGeometryElement => scnGeometryElement -> IO CLong
primitiveCount scnGeometryElement =
  sendMessage scnGeometryElement primitiveCountSelector

-- | interleavedIndicesChannels
--
-- Determines whether the channels are interleaved.
--
-- ObjC selector: @- interleavedIndicesChannels@
interleavedIndicesChannels :: IsSCNGeometryElement scnGeometryElement => scnGeometryElement -> IO Bool
interleavedIndicesChannels scnGeometryElement =
  sendMessage scnGeometryElement interleavedIndicesChannelsSelector

-- | indicesChannelCount
--
-- The number of channels in the geometry element.
--
-- ObjC selector: @- indicesChannelCount@
indicesChannelCount :: IsSCNGeometryElement scnGeometryElement => scnGeometryElement -> IO CLong
indicesChannelCount scnGeometryElement =
  sendMessage scnGeometryElement indicesChannelCountSelector

-- | bytesPerIndex
--
-- The number of bytes that represent an index value
--
-- ObjC selector: @- bytesPerIndex@
bytesPerIndex :: IsSCNGeometryElement scnGeometryElement => scnGeometryElement -> IO CLong
bytesPerIndex scnGeometryElement =
  sendMessage scnGeometryElement bytesPerIndexSelector

-- | primitiveRange
--
-- Specifies the subrange of primitives to render within NSMakeRange(0, primitiveCount). Defaults to NSMakeRange(NSNotFound, 0).
--
-- When the location of the range is set to NSNotFound, the entire geometry element is rendered.
--
-- ObjC selector: @- primitiveRange@
primitiveRange :: IsSCNGeometryElement scnGeometryElement => scnGeometryElement -> IO NSRange
primitiveRange scnGeometryElement =
  sendMessage scnGeometryElement primitiveRangeSelector

-- | primitiveRange
--
-- Specifies the subrange of primitives to render within NSMakeRange(0, primitiveCount). Defaults to NSMakeRange(NSNotFound, 0).
--
-- When the location of the range is set to NSNotFound, the entire geometry element is rendered.
--
-- ObjC selector: @- setPrimitiveRange:@
setPrimitiveRange :: IsSCNGeometryElement scnGeometryElement => scnGeometryElement -> NSRange -> IO ()
setPrimitiveRange scnGeometryElement value =
  sendMessage scnGeometryElement setPrimitiveRangeSelector value

-- | pointSize
--
-- Specifies the size of the point in local space. Defaults to 1
--
-- ObjC selector: @- pointSize@
pointSize :: IsSCNGeometryElement scnGeometryElement => scnGeometryElement -> IO CDouble
pointSize scnGeometryElement =
  sendMessage scnGeometryElement pointSizeSelector

-- | pointSize
--
-- Specifies the size of the point in local space. Defaults to 1
--
-- ObjC selector: @- setPointSize:@
setPointSize :: IsSCNGeometryElement scnGeometryElement => scnGeometryElement -> CDouble -> IO ()
setPointSize scnGeometryElement value =
  sendMessage scnGeometryElement setPointSizeSelector value

-- | minimumPointScreenSpaceRadius
--
-- Specifies the minimum size in screen-space (in pixel). Defaults to 1
--
-- ObjC selector: @- minimumPointScreenSpaceRadius@
minimumPointScreenSpaceRadius :: IsSCNGeometryElement scnGeometryElement => scnGeometryElement -> IO CDouble
minimumPointScreenSpaceRadius scnGeometryElement =
  sendMessage scnGeometryElement minimumPointScreenSpaceRadiusSelector

-- | minimumPointScreenSpaceRadius
--
-- Specifies the minimum size in screen-space (in pixel). Defaults to 1
--
-- ObjC selector: @- setMinimumPointScreenSpaceRadius:@
setMinimumPointScreenSpaceRadius :: IsSCNGeometryElement scnGeometryElement => scnGeometryElement -> CDouble -> IO ()
setMinimumPointScreenSpaceRadius scnGeometryElement value =
  sendMessage scnGeometryElement setMinimumPointScreenSpaceRadiusSelector value

-- | maximumPointScreenSpaceRadius
--
-- Specifies the maximum size in screen-space (in pixel). Defaults to 1
--
-- ObjC selector: @- maximumPointScreenSpaceRadius@
maximumPointScreenSpaceRadius :: IsSCNGeometryElement scnGeometryElement => scnGeometryElement -> IO CDouble
maximumPointScreenSpaceRadius scnGeometryElement =
  sendMessage scnGeometryElement maximumPointScreenSpaceRadiusSelector

-- | maximumPointScreenSpaceRadius
--
-- Specifies the maximum size in screen-space (in pixel). Defaults to 1
--
-- ObjC selector: @- setMaximumPointScreenSpaceRadius:@
setMaximumPointScreenSpaceRadius :: IsSCNGeometryElement scnGeometryElement => scnGeometryElement -> CDouble -> IO ()
setMaximumPointScreenSpaceRadius scnGeometryElement value =
  sendMessage scnGeometryElement setMaximumPointScreenSpaceRadiusSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @geometryElementWithData:primitiveType:primitiveCount:bytesPerIndex:@
geometryElementWithData_primitiveType_primitiveCount_bytesPerIndexSelector :: Selector '[Id NSData, SCNGeometryPrimitiveType, CLong, CLong] (Id SCNGeometryElement)
geometryElementWithData_primitiveType_primitiveCount_bytesPerIndexSelector = mkSelector "geometryElementWithData:primitiveType:primitiveCount:bytesPerIndex:"

-- | @Selector@ for @geometryElementWithData:primitiveType:primitiveCount:indicesChannelCount:interleavedIndicesChannels:bytesPerIndex:@
geometryElementWithData_primitiveType_primitiveCount_indicesChannelCount_interleavedIndicesChannels_bytesPerIndexSelector :: Selector '[Id NSData, SCNGeometryPrimitiveType, CLong, CLong, Bool, CLong] (Id SCNGeometryElement)
geometryElementWithData_primitiveType_primitiveCount_indicesChannelCount_interleavedIndicesChannels_bytesPerIndexSelector = mkSelector "geometryElementWithData:primitiveType:primitiveCount:indicesChannelCount:interleavedIndicesChannels:bytesPerIndex:"

-- | @Selector@ for @geometryElementWithBuffer:primitiveType:primitiveCount:bytesPerIndex:@
geometryElementWithBuffer_primitiveType_primitiveCount_bytesPerIndexSelector :: Selector '[RawId, SCNGeometryPrimitiveType, CLong, CLong] (Id SCNGeometryElement)
geometryElementWithBuffer_primitiveType_primitiveCount_bytesPerIndexSelector = mkSelector "geometryElementWithBuffer:primitiveType:primitiveCount:bytesPerIndex:"

-- | @Selector@ for @geometryElementWithBuffer:primitiveType:primitiveCount:indicesChannelCount:interleavedIndicesChannels:bytesPerIndex:@
geometryElementWithBuffer_primitiveType_primitiveCount_indicesChannelCount_interleavedIndicesChannels_bytesPerIndexSelector :: Selector '[RawId, SCNGeometryPrimitiveType, CLong, CLong, Bool, CLong] (Id SCNGeometryElement)
geometryElementWithBuffer_primitiveType_primitiveCount_indicesChannelCount_interleavedIndicesChannels_bytesPerIndexSelector = mkSelector "geometryElementWithBuffer:primitiveType:primitiveCount:indicesChannelCount:interleavedIndicesChannels:bytesPerIndex:"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSData)
dataSelector = mkSelector "data"

-- | @Selector@ for @primitiveType@
primitiveTypeSelector :: Selector '[] SCNGeometryPrimitiveType
primitiveTypeSelector = mkSelector "primitiveType"

-- | @Selector@ for @primitiveCount@
primitiveCountSelector :: Selector '[] CLong
primitiveCountSelector = mkSelector "primitiveCount"

-- | @Selector@ for @interleavedIndicesChannels@
interleavedIndicesChannelsSelector :: Selector '[] Bool
interleavedIndicesChannelsSelector = mkSelector "interleavedIndicesChannels"

-- | @Selector@ for @indicesChannelCount@
indicesChannelCountSelector :: Selector '[] CLong
indicesChannelCountSelector = mkSelector "indicesChannelCount"

-- | @Selector@ for @bytesPerIndex@
bytesPerIndexSelector :: Selector '[] CLong
bytesPerIndexSelector = mkSelector "bytesPerIndex"

-- | @Selector@ for @primitiveRange@
primitiveRangeSelector :: Selector '[] NSRange
primitiveRangeSelector = mkSelector "primitiveRange"

-- | @Selector@ for @setPrimitiveRange:@
setPrimitiveRangeSelector :: Selector '[NSRange] ()
setPrimitiveRangeSelector = mkSelector "setPrimitiveRange:"

-- | @Selector@ for @pointSize@
pointSizeSelector :: Selector '[] CDouble
pointSizeSelector = mkSelector "pointSize"

-- | @Selector@ for @setPointSize:@
setPointSizeSelector :: Selector '[CDouble] ()
setPointSizeSelector = mkSelector "setPointSize:"

-- | @Selector@ for @minimumPointScreenSpaceRadius@
minimumPointScreenSpaceRadiusSelector :: Selector '[] CDouble
minimumPointScreenSpaceRadiusSelector = mkSelector "minimumPointScreenSpaceRadius"

-- | @Selector@ for @setMinimumPointScreenSpaceRadius:@
setMinimumPointScreenSpaceRadiusSelector :: Selector '[CDouble] ()
setMinimumPointScreenSpaceRadiusSelector = mkSelector "setMinimumPointScreenSpaceRadius:"

-- | @Selector@ for @maximumPointScreenSpaceRadius@
maximumPointScreenSpaceRadiusSelector :: Selector '[] CDouble
maximumPointScreenSpaceRadiusSelector = mkSelector "maximumPointScreenSpaceRadius"

-- | @Selector@ for @setMaximumPointScreenSpaceRadius:@
setMaximumPointScreenSpaceRadiusSelector :: Selector '[CDouble] ()
setMaximumPointScreenSpaceRadiusSelector = mkSelector "setMaximumPointScreenSpaceRadius:"

