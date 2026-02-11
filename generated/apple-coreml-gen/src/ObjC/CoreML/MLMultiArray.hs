{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Use @MLMultiArray@ to store a multi-dimensional value.
--
-- Unlike @MLShapedArray@ or @MLTensor@, @MLMultiArray@ can be used in Obj-C code. Unlike @MLTensor@, @MLMultiArray@ is always backed by a concrete storage.
--
-- The object has properties to define the interpretation of the storage.
--
-- @.dataType@ defines the interpretation of raw bytes into a numeric scalar value. For example, @MLMultiArrayDataTypeFloat32@ means the backing storage uses IEEE 754 Float32 encoding.
--
-- @.shape@ defines the multi-dimensional space. For example, 30 x 20 image with three color components (Red, Green, Blue) could be defined using the shape @[3, 20, 30]@.
--
-- @.strides@ defines the offset addressing of the scalar for a given coordinates. For example, the image above might use @[640, 32, 1]@ as the @strides@. Then, the scalar at (1, 10, 15) is stored at @640 * 1 + 32 * 10 + 1 * 15@, or 975th scalar in the storage. In general, the scalar offset for coordinates @index@ and strides @strides@ is:
--
-- ``` scalarOffset = sum_d index[d]*strides[d] ```
--
-- The backing storage can be a heap allocated buffer or CVPixelBuffer. Though CVPixelBuffer backing supports limited data types, @MLModel@ could share the storage with backend hardware such as Apple Neural Engine without copy.
--
-- Generated bindings for @MLMultiArray@.
module ObjC.CoreML.MLMultiArray
  ( MLMultiArray
  , IsMLMultiArray(..)
  , transferToMultiArray
  , objectAtIndexedSubscript
  , objectForKeyedSubscript
  , setObject_atIndexedSubscript
  , setObject_forKeyedSubscript
  , multiArrayByConcatenatingMultiArrays_alongAxis_dataType
  , getBytesWithHandler
  , initWithShape_dataType_error
  , initWithShape_dataType_strides
  , initWithDataPointer_shape_dataType_strides_deallocator_error
  , initWithPixelBuffer_shape
  , dataPointer
  , dataType
  , shape
  , strides
  , count
  , pixelBuffer
  , transferToMultiArraySelector
  , objectAtIndexedSubscriptSelector
  , objectForKeyedSubscriptSelector
  , setObject_atIndexedSubscriptSelector
  , setObject_forKeyedSubscriptSelector
  , multiArrayByConcatenatingMultiArrays_alongAxis_dataTypeSelector
  , getBytesWithHandlerSelector
  , initWithShape_dataType_errorSelector
  , initWithShape_dataType_stridesSelector
  , initWithDataPointer_shape_dataType_strides_deallocator_errorSelector
  , initWithPixelBuffer_shapeSelector
  , dataPointerSelector
  , dataTypeSelector
  , shapeSelector
  , stridesSelector
  , countSelector
  , pixelBufferSelector

  -- * Enum types
  , MLMultiArrayDataType(MLMultiArrayDataType)
  , pattern MLMultiArrayDataTypeDouble
  , pattern MLMultiArrayDataTypeFloat64
  , pattern MLMultiArrayDataTypeFloat32
  , pattern MLMultiArrayDataTypeFloat16
  , pattern MLMultiArrayDataTypeFloat
  , pattern MLMultiArrayDataTypeInt32
  , pattern MLMultiArrayDataTypeInt8

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

import ObjC.CoreML.Internal.Classes
import ObjC.CoreML.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Transfer the contents to the destination multi-array.
--
-- Numeric data will be up or down casted as needed. It can transfer to a multi-array with different layout (strides).
--
-- ```swift let sourceMultiArray: MLMultiArray = ... // shape is [2, 3] and data type is Float64
--
-- let newStrides = [4, 1] let destinationMultiArray = MLMultiArray(shape: [2, 3],                                           dataType: .float32,                                           strides: newStrides) sourceMultiArray.transfer(to: destinationMultiArray) ```
--
-- ```objc NSArray<NSNumber *> *shape = \@[\@2, \@3]; NSArray<NSNumber *> *sourceStrides = \@[\@3, \@1]; NSArray<NSNumber *> *destinationStrides = \@[\@4, \@1]; MLMultiArray *source = [[MLMultiArray alloc] initWithShape:shape                                                   dataType:MLMultiArrayDataTypeDouble                                                    strides:sourceStrides]; // Initialize source...
--
-- MLMultiArray *destination = [[MLMultiArray alloc] initWithShape:shape                                                        dataType:MLMultiArrayDataTypeFloat32                                                         strides:destinationStrides]; [source transferToMultiArray:destination]; ```
--
-- - Parameters:   - destinationMultiArray: The transfer destination.
--
-- ObjC selector: @- transferToMultiArray:@
transferToMultiArray :: (IsMLMultiArray mlMultiArray, IsMLMultiArray destinationMultiArray) => mlMultiArray -> destinationMultiArray -> IO ()
transferToMultiArray mlMultiArray  destinationMultiArray =
  withObjCPtr destinationMultiArray $ \raw_destinationMultiArray ->
      sendMsg mlMultiArray (mkSelector "transferToMultiArray:") retVoid [argPtr (castPtr raw_destinationMultiArray :: Ptr ())]

-- | Get a value by its linear index (assumes C-style index ordering)
--
-- ObjC selector: @- objectAtIndexedSubscript:@
objectAtIndexedSubscript :: IsMLMultiArray mlMultiArray => mlMultiArray -> CLong -> IO (Id NSNumber)
objectAtIndexedSubscript mlMultiArray  idx =
    sendMsg mlMultiArray (mkSelector "objectAtIndexedSubscript:") (retPtr retVoid) [argCLong idx] >>= retainedObject . castPtr

-- | Get a value by its multidimensional index (NSArray<NSNumber *>)
--
-- ObjC selector: @- objectForKeyedSubscript:@
objectForKeyedSubscript :: (IsMLMultiArray mlMultiArray, IsNSArray key) => mlMultiArray -> key -> IO (Id NSNumber)
objectForKeyedSubscript mlMultiArray  key =
  withObjCPtr key $ \raw_key ->
      sendMsg mlMultiArray (mkSelector "objectForKeyedSubscript:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())] >>= retainedObject . castPtr

-- | Set a value by its linear index (assumes C-style index ordering)
--
-- ObjC selector: @- setObject:atIndexedSubscript:@
setObject_atIndexedSubscript :: (IsMLMultiArray mlMultiArray, IsNSNumber obj_) => mlMultiArray -> obj_ -> CLong -> IO ()
setObject_atIndexedSubscript mlMultiArray  obj_ idx =
  withObjCPtr obj_ $ \raw_obj_ ->
      sendMsg mlMultiArray (mkSelector "setObject:atIndexedSubscript:") retVoid [argPtr (castPtr raw_obj_ :: Ptr ()), argCLong idx]

-- | Set a value by subindicies (NSArray<NSNumber *>)
--
-- ObjC selector: @- setObject:forKeyedSubscript:@
setObject_forKeyedSubscript :: (IsMLMultiArray mlMultiArray, IsNSNumber obj_, IsNSArray key) => mlMultiArray -> obj_ -> key -> IO ()
setObject_forKeyedSubscript mlMultiArray  obj_ key =
  withObjCPtr obj_ $ \raw_obj_ ->
    withObjCPtr key $ \raw_key ->
        sendMsg mlMultiArray (mkSelector "setObject:forKeyedSubscript:") retVoid [argPtr (castPtr raw_obj_ :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | Concatenate MLMultiArrays to form a new MLMultiArray.
--
-- All the source MLMultiArrays must have a same shape except the specified axis. The resultant MLMultiArray has the same shape as inputs except this axis, which dimension will be the sum of all the input dimensions of the axis.
--
-- For example,
--
-- ```swift // Swift let A = try MLMultiArray(shape: [2, 3], dataType: .int32) let B = try MLMultiArray(shape: [2, 2], dataType: .int32) let C = MLMultiArray(concatenating: [A, B], axis: 1, dataType: .int32) assert(C.shape == [2, 5]) ```
--
-- ```objc // Obj-C MLMultiArray *A = [[MLMultiArray alloc] initWithShape:\@[\@2, \@3] dataType:MLMultiArrayDataTypeInt32 error:NULL]; MLMultiArray *B = [[MLMultiArray alloc] initWithShape:\@[\@2, \@2] dataType:MLMultiArrayDataTypeInt32 error:NULL]; MLMultiArray *C = [MLMultiArray multiArrayByConcatenatingMultiArrays:\@[A, B] alongAxis:1 dataType:MLMultiArrayDataTypeInt32]; assert(C.shape == \@[\@2, \@5]) ```
--
-- Numeric data will be up or down casted as needed.
--
-- The method raises NSInvalidArgumentException if the shapes of input multi arrays are not compatible for concatenation.
--
-- - Parameters:   - multiArrays: Array of MLMultiArray instances to be concatenated.   - axis: Axis index with which the concatenation will performed. The value is wrapped by the dimension of the axis. For example, -1 is the last axis.   - dataType: The data type of the resultant MLMultiArray.
--
-- ObjC selector: @+ multiArrayByConcatenatingMultiArrays:alongAxis:dataType:@
multiArrayByConcatenatingMultiArrays_alongAxis_dataType :: IsNSArray multiArrays => multiArrays -> CLong -> MLMultiArrayDataType -> IO (Id MLMultiArray)
multiArrayByConcatenatingMultiArrays_alongAxis_dataType multiArrays axis dataType =
  do
    cls' <- getRequiredClass "MLMultiArray"
    withObjCPtr multiArrays $ \raw_multiArrays ->
      sendClassMsg cls' (mkSelector "multiArrayByConcatenatingMultiArrays:alongAxis:dataType:") (retPtr retVoid) [argPtr (castPtr raw_multiArrays :: Ptr ()), argCLong axis, argCLong (coerce dataType)] >>= retainedObject . castPtr

-- | Get the underlying buffer pointer to read.
--
-- The buffer pointer is valid only within the block.
--
-- ```objc MLMultiArray * A = [[MLMultiArray alloc] initWithShape:\@[\@3, \@2] dataType:MLMultiArrayDataTypeInt32 error:NULL]; A[\@[\@1, \@2]] = \@42; [A getBytesWithHandler:^(const void *bytes, NSInteger size) {     const int32_t *scalarBuffer = (const int32_t *)bytes;     const int strideY = A.strides[0].intValue;     // Print 42     NSLog("Scalar at (1, 2): %d", scalarBuffer[1 * strideY + 2]); }]; ``` - Parameters:   - handler: The block to receive the buffer pointer and its size in bytes.
--
-- ObjC selector: @- getBytesWithHandler:@
getBytesWithHandler :: IsMLMultiArray mlMultiArray => mlMultiArray -> Ptr () -> IO ()
getBytesWithHandler mlMultiArray  handler =
    sendMsg mlMultiArray (mkSelector "getBytesWithHandler:") retVoid [argPtr (castPtr handler :: Ptr ())]

-- | Creates the object.
--
-- The contents of the object are left uninitialized; the client must initialize it.
--
-- The scalars will use the first-major contiguous layout.
--
-- - Parameters:   - shape: The shape   - dataType: The data type   - error: Filled with error information on error.
--
-- ObjC selector: @- initWithShape:dataType:error:@
initWithShape_dataType_error :: (IsMLMultiArray mlMultiArray, IsNSArray shape, IsNSError error_) => mlMultiArray -> shape -> MLMultiArrayDataType -> error_ -> IO (Id MLMultiArray)
initWithShape_dataType_error mlMultiArray  shape dataType error_ =
  withObjCPtr shape $ \raw_shape ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mlMultiArray (mkSelector "initWithShape:dataType:error:") (retPtr retVoid) [argPtr (castPtr raw_shape :: Ptr ()), argCLong (coerce dataType), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | Creates the object with specified strides.
--
-- The contents of the object are left uninitialized; the client must initialize it.
--
-- ```swift let shape = [2, 3]; let strides = [4, 1]
--
-- let multiArray = MLMultiArray(shape: shape, dataType: .float32, strides: strides) XCTAssertEqual(multiArray.shape, shape as [NSNumber]) XCTAssertEqual(multiArray.strides, strides as [NSNumber]) ```
--
-- ```objc NSArray<NSNumber *> *shape = \@[\@2, \@3]; NSArray<NSNumber *> *strides = \@[\@4, \@1];
--
-- MLMultiArray *multiArray = [[MLMultiArray alloc] initWithShape:shape                                                       dataType:MLMultiArrayDataTypeFloat32                                                        strides:strides]; XCTAssertEqualObjects(multiArray.shape, shape); XCTAssertEqualObjects(multiArray.strides, strides); ```
--
-- - Parameters:   - shape: The shape   - dataType: The data type   - strides: The strides.
--
-- ObjC selector: @- initWithShape:dataType:strides:@
initWithShape_dataType_strides :: (IsMLMultiArray mlMultiArray, IsNSArray shape, IsNSArray strides) => mlMultiArray -> shape -> MLMultiArrayDataType -> strides -> IO (Id MLMultiArray)
initWithShape_dataType_strides mlMultiArray  shape dataType strides =
  withObjCPtr shape $ \raw_shape ->
    withObjCPtr strides $ \raw_strides ->
        sendMsg mlMultiArray (mkSelector "initWithShape:dataType:strides:") (retPtr retVoid) [argPtr (castPtr raw_shape :: Ptr ()), argCLong (coerce dataType), argPtr (castPtr raw_strides :: Ptr ())] >>= ownedObject . castPtr

-- | Creates the object with existing data without copy.
--
-- Use this initializer to reference the existing buffer as the storage without copy.
--
-- ```objc int32_t *buffer = malloc(sizeof(int32_t) * 2 * 3 * 4); MLMultiArray *multiArray = [[MLMultiArray alloc] initWithDataPointer:buffer                                                                shape:\@[\@2, \@3, \@4]                                                             dataType:MLMultiArrayDataTypeInt32                                                              strides:\@[\@12, \@4, \@1]                                                          deallocator:^(void *bytes) { free(bytes); }                                                                error:NULL]; ```
--
-- - Parameters:   - dataPointer: The pointer to the buffer.   - shape: The shape   - dataType: The data type   - strides: The strides.   - deallocator: Block to be called on the deallocation of the instance.   - error: Filled with error information on error.
--
-- ObjC selector: @- initWithDataPointer:shape:dataType:strides:deallocator:error:@
initWithDataPointer_shape_dataType_strides_deallocator_error :: (IsMLMultiArray mlMultiArray, IsNSArray shape, IsNSArray strides, IsNSError error_) => mlMultiArray -> Ptr () -> shape -> MLMultiArrayDataType -> strides -> Ptr () -> error_ -> IO (Id MLMultiArray)
initWithDataPointer_shape_dataType_strides_deallocator_error mlMultiArray  dataPointer shape dataType strides deallocator error_ =
  withObjCPtr shape $ \raw_shape ->
    withObjCPtr strides $ \raw_strides ->
      withObjCPtr error_ $ \raw_error_ ->
          sendMsg mlMultiArray (mkSelector "initWithDataPointer:shape:dataType:strides:deallocator:error:") (retPtr retVoid) [argPtr dataPointer, argPtr (castPtr raw_shape :: Ptr ()), argCLong (coerce dataType), argPtr (castPtr raw_strides :: Ptr ()), argPtr (castPtr deallocator :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | Create by wrapping a pixel buffer.
--
-- Use this initializer to create an IOSurface backed MLMultiArray, which can reduce the inference latency by avoiding the buffer copy.
--
-- The instance will own the pixel buffer and release it on the deallocation.
--
-- The pixel buffer's pixel format type must be either @kCVPixelFormatType_OneComponent16Half@ for @MLMultiArrayDataTypeFloat16@ or @kCVPixelFormatType_OneComponent8@ for @MLMultiArrayDataTypeInt8@.
--
-- ```objc CVPixelBufferRef pixelBuffer = NULL; NSDictionary* pixelBufferAttributes = \@{     (id)kCVPixelBufferIOSurfacePropertiesKey: \@{} };
--
-- // Since shape == [2, 3, 4], width is 4 (= shape[2]) and height is 6 (= shape[0] * shape[1]). CVPixelBufferCreate(kCFAllocatorDefault, 4, 6, kCVPixelFormatType_OneComponent16Half, (__bridge CFDictionaryRef)pixelBufferAttributes, &pixelBuffer); MLMultiArray *multiArray = [[MLMultiArray alloc] initWithPixelBuffer:pixelBuffer shape:\@[\@2, \@3, \@4]]; ```
--
-- - Parameters:   - pixelBuffer: The pixel buffer to be owned by the instance.   - shape: The shape of the MLMultiArray. The last dimension of @shape@ must match the pixel buffer's width. The product of the rest of the dimensions must match the height.
--
-- ObjC selector: @- initWithPixelBuffer:shape:@
initWithPixelBuffer_shape :: (IsMLMultiArray mlMultiArray, IsNSArray shape) => mlMultiArray -> Ptr () -> shape -> IO (Id MLMultiArray)
initWithPixelBuffer_shape mlMultiArray  pixelBuffer shape =
  withObjCPtr shape $ \raw_shape ->
      sendMsg mlMultiArray (mkSelector "initWithPixelBuffer:shape:") (retPtr retVoid) [argPtr pixelBuffer, argPtr (castPtr raw_shape :: Ptr ())] >>= ownedObject . castPtr

-- | Unsafe pointer to underlying buffer holding the data
--
-- ObjC selector: @- dataPointer@
dataPointer :: IsMLMultiArray mlMultiArray => mlMultiArray -> IO (Ptr ())
dataPointer mlMultiArray  =
    fmap castPtr $ sendMsg mlMultiArray (mkSelector "dataPointer") (retPtr retVoid) []

-- | Scalar's data type.
--
-- ObjC selector: @- dataType@
dataType :: IsMLMultiArray mlMultiArray => mlMultiArray -> IO MLMultiArrayDataType
dataType mlMultiArray  =
    fmap (coerce :: CLong -> MLMultiArrayDataType) $ sendMsg mlMultiArray (mkSelector "dataType") retCLong []

-- | Shape of the multi-dimensional space that this instance represents.
--
-- ObjC selector: @- shape@
shape :: IsMLMultiArray mlMultiArray => mlMultiArray -> IO (Id NSArray)
shape mlMultiArray  =
    sendMsg mlMultiArray (mkSelector "shape") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Strides.
--
-- It defines the offset of the scalar of a given coordinate index in the storage, which is: ``` scalarOffset = sum_d index[d]*strides[d] ```
--
-- ObjC selector: @- strides@
strides :: IsMLMultiArray mlMultiArray => mlMultiArray -> IO (Id NSArray)
strides mlMultiArray  =
    sendMsg mlMultiArray (mkSelector "strides") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Count of total number of addressable scalars.
--
-- The value is same as @product_d shape[d]@.
--
-- ObjC selector: @- count@
count :: IsMLMultiArray mlMultiArray => mlMultiArray -> IO CLong
count mlMultiArray  =
    sendMsg mlMultiArray (mkSelector "count") retCLong []

-- | Returns the backing pixel buffer if exists, otherwise nil.
--
-- ObjC selector: @- pixelBuffer@
pixelBuffer :: IsMLMultiArray mlMultiArray => mlMultiArray -> IO (Ptr ())
pixelBuffer mlMultiArray  =
    fmap castPtr $ sendMsg mlMultiArray (mkSelector "pixelBuffer") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @transferToMultiArray:@
transferToMultiArraySelector :: Selector
transferToMultiArraySelector = mkSelector "transferToMultiArray:"

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @objectForKeyedSubscript:@
objectForKeyedSubscriptSelector :: Selector
objectForKeyedSubscriptSelector = mkSelector "objectForKeyedSubscript:"

-- | @Selector@ for @setObject:atIndexedSubscript:@
setObject_atIndexedSubscriptSelector :: Selector
setObject_atIndexedSubscriptSelector = mkSelector "setObject:atIndexedSubscript:"

-- | @Selector@ for @setObject:forKeyedSubscript:@
setObject_forKeyedSubscriptSelector :: Selector
setObject_forKeyedSubscriptSelector = mkSelector "setObject:forKeyedSubscript:"

-- | @Selector@ for @multiArrayByConcatenatingMultiArrays:alongAxis:dataType:@
multiArrayByConcatenatingMultiArrays_alongAxis_dataTypeSelector :: Selector
multiArrayByConcatenatingMultiArrays_alongAxis_dataTypeSelector = mkSelector "multiArrayByConcatenatingMultiArrays:alongAxis:dataType:"

-- | @Selector@ for @getBytesWithHandler:@
getBytesWithHandlerSelector :: Selector
getBytesWithHandlerSelector = mkSelector "getBytesWithHandler:"

-- | @Selector@ for @initWithShape:dataType:error:@
initWithShape_dataType_errorSelector :: Selector
initWithShape_dataType_errorSelector = mkSelector "initWithShape:dataType:error:"

-- | @Selector@ for @initWithShape:dataType:strides:@
initWithShape_dataType_stridesSelector :: Selector
initWithShape_dataType_stridesSelector = mkSelector "initWithShape:dataType:strides:"

-- | @Selector@ for @initWithDataPointer:shape:dataType:strides:deallocator:error:@
initWithDataPointer_shape_dataType_strides_deallocator_errorSelector :: Selector
initWithDataPointer_shape_dataType_strides_deallocator_errorSelector = mkSelector "initWithDataPointer:shape:dataType:strides:deallocator:error:"

-- | @Selector@ for @initWithPixelBuffer:shape:@
initWithPixelBuffer_shapeSelector :: Selector
initWithPixelBuffer_shapeSelector = mkSelector "initWithPixelBuffer:shape:"

-- | @Selector@ for @dataPointer@
dataPointerSelector :: Selector
dataPointerSelector = mkSelector "dataPointer"

-- | @Selector@ for @dataType@
dataTypeSelector :: Selector
dataTypeSelector = mkSelector "dataType"

-- | @Selector@ for @shape@
shapeSelector :: Selector
shapeSelector = mkSelector "shape"

-- | @Selector@ for @strides@
stridesSelector :: Selector
stridesSelector = mkSelector "strides"

-- | @Selector@ for @count@
countSelector :: Selector
countSelector = mkSelector "count"

-- | @Selector@ for @pixelBuffer@
pixelBufferSelector :: Selector
pixelBufferSelector = mkSelector "pixelBuffer"

