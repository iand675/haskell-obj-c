{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The Core Image class that defines a vector object.
--
-- A @CIVector@ can store one or more @CGFloat@ in one object. They can store a group of float values for a variety of different uses such as coordinate points, direction vectors, geometric rectangles,  transform matrices, convolution weights, or just a list a parameter values.
--
-- You use @CIVector@ objects in conjunction with other Core Image classes, such as ``CIFilter-class``  and ``CIKernel``.  Many of the built-in Core Image filters have one or more @CIVector@ inputs that  you can set to affect the filter's behavior.
--
-- Generated bindings for @CIVector@.
module ObjC.CoreImage.CIVector
  ( CIVector
  , IsCIVector(..)
  , vectorWithValues_count
  , vectorWithX
  , vectorWithX_Y
  , vectorWithX_Y_Z
  , vectorWithX_Y_Z_W
  , vectorWithString
  , initWithValues_count
  , initWithX
  , initWithX_Y
  , initWithX_Y_Z
  , initWithX_Y_Z_W
  , initWithString
  , valueAtIndex
  , count
  , x
  , y
  , z
  , w
  , stringRepresentation
  , vectorWithValues_countSelector
  , vectorWithXSelector
  , vectorWithX_YSelector
  , vectorWithX_Y_ZSelector
  , vectorWithX_Y_Z_WSelector
  , vectorWithStringSelector
  , initWithValues_countSelector
  , initWithXSelector
  , initWithX_YSelector
  , initWithX_Y_ZSelector
  , initWithX_Y_Z_WSelector
  , initWithStringSelector
  , valueAtIndexSelector
  , countSelector
  , xSelector
  , ySelector
  , zSelector
  , wSelector
  , stringRepresentationSelector


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

import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a Core Image vector object that is initialized with the specified values. - Parameters:   - values: The pointer @CGFloat@ values to initialize the vector with.   - count: The number of @CGFloats@ specified by the @values@ parameter. - Returns:    An autoreleased ``CIVector`` object of length @count@.
--
-- ObjC selector: @+ vectorWithValues:count:@
vectorWithValues_count :: Const (Ptr CDouble) -> CULong -> IO (Id CIVector)
vectorWithValues_count values count =
  do
    cls' <- getRequiredClass "CIVector"
    sendClassMsg cls' (mkSelector "vectorWithValues:count:") (retPtr retVoid) [argPtr (unConst values), argCULong (fromIntegral count)] >>= retainedObject . castPtr

-- | Create a Core Image vector object that is initialized with one value. - Parameters:   - x: The value for the first position in the vector. - Returns:    An autoreleased ``CIVector`` object of length 1.
--
-- ObjC selector: @+ vectorWithX:@
vectorWithX :: CDouble -> IO (Id CIVector)
vectorWithX x =
  do
    cls' <- getRequiredClass "CIVector"
    sendClassMsg cls' (mkSelector "vectorWithX:") (retPtr retVoid) [argCDouble (fromIntegral x)] >>= retainedObject . castPtr

-- | Create a Core Image vector object that is initialized with two values. - Parameters:   - x: The value for the first position in the vector.   - y: The value for the second position in the vector. - Returns:    An autoreleased ``CIVector`` object of length 2.
--
-- ObjC selector: @+ vectorWithX:Y:@
vectorWithX_Y :: CDouble -> CDouble -> IO (Id CIVector)
vectorWithX_Y x y =
  do
    cls' <- getRequiredClass "CIVector"
    sendClassMsg cls' (mkSelector "vectorWithX:Y:") (retPtr retVoid) [argCDouble (fromIntegral x), argCDouble (fromIntegral y)] >>= retainedObject . castPtr

-- | Create a Core Image vector object that is initialized with three values. - Parameters:   - x: The value for the first position in the vector.   - y: The value for the second position in the vector.   - z: The value for the third position in the vector. - Returns:    An autoreleased ``CIVector`` object of length 3.
--
-- ObjC selector: @+ vectorWithX:Y:Z:@
vectorWithX_Y_Z :: CDouble -> CDouble -> CDouble -> IO (Id CIVector)
vectorWithX_Y_Z x y z =
  do
    cls' <- getRequiredClass "CIVector"
    sendClassMsg cls' (mkSelector "vectorWithX:Y:Z:") (retPtr retVoid) [argCDouble (fromIntegral x), argCDouble (fromIntegral y), argCDouble (fromIntegral z)] >>= retainedObject . castPtr

-- | Create a Core Image vector object that is initialized with four values. - Parameters:   - x: The value for the first position in the vector.   - y: The value for the second position in the vector.   - z: The value for the third position in the vector.   - w: The value for the forth position in the vector. - Returns:    An autoreleased ``CIVector`` object of length 4.
--
-- ObjC selector: @+ vectorWithX:Y:Z:W:@
vectorWithX_Y_Z_W :: CDouble -> CDouble -> CDouble -> CDouble -> IO (Id CIVector)
vectorWithX_Y_Z_W x y z w =
  do
    cls' <- getRequiredClass "CIVector"
    sendClassMsg cls' (mkSelector "vectorWithX:Y:Z:W:") (retPtr retVoid) [argCDouble (fromIntegral x), argCDouble (fromIntegral y), argCDouble (fromIntegral z), argCDouble (fromIntegral w)] >>= retainedObject . castPtr

-- | Create a Core Image vector object with values provided in a string representation. - Parameters:   - representation: A string that is in one of the formats returned by the @stringRepresentation@ method. - Returns:    An autoreleased ``CIVector`` object.
--
-- ObjC selector: @+ vectorWithString:@
vectorWithString :: IsNSString representation => representation -> IO (Id CIVector)
vectorWithString representation =
  do
    cls' <- getRequiredClass "CIVector"
    withObjCPtr representation $ \raw_representation ->
      sendClassMsg cls' (mkSelector "vectorWithString:") (retPtr retVoid) [argPtr (castPtr raw_representation :: Ptr ())] >>= retainedObject . castPtr

-- | Initialize a Core Image vector object with the specified the values. - Parameters:   - values: A pointer @CGFloat@ values for vector.   - count: The number of @CGFloats@ specified by the @values@ parameter. - Returns:    An initialized ``CIVector`` object of length @count@.
--
-- ObjC selector: @- initWithValues:count:@
initWithValues_count :: IsCIVector ciVector => ciVector -> Const (Ptr CDouble) -> CULong -> IO (Id CIVector)
initWithValues_count ciVector  values count =
  sendMsg ciVector (mkSelector "initWithValues:count:") (retPtr retVoid) [argPtr (unConst values), argCULong (fromIntegral count)] >>= ownedObject . castPtr

-- | Initialize a Core Image vector object with one value. - Parameters:   - x: The value for the first position in the vector. - Returns:    An initialized ``CIVector`` object of length 1.
--
-- ObjC selector: @- initWithX:@
initWithX :: IsCIVector ciVector => ciVector -> CDouble -> IO (Id CIVector)
initWithX ciVector  x =
  sendMsg ciVector (mkSelector "initWithX:") (retPtr retVoid) [argCDouble (fromIntegral x)] >>= ownedObject . castPtr

-- | Initialize a Core Image vector object with two values. - Parameters:   - x: The value for the first position in the vector.   - y: The value for the second position in the vector. - Returns:    An initialized ``CIVector`` object of length 2.
--
-- ObjC selector: @- initWithX:Y:@
initWithX_Y :: IsCIVector ciVector => ciVector -> CDouble -> CDouble -> IO (Id CIVector)
initWithX_Y ciVector  x y =
  sendMsg ciVector (mkSelector "initWithX:Y:") (retPtr retVoid) [argCDouble (fromIntegral x), argCDouble (fromIntegral y)] >>= ownedObject . castPtr

-- | Initialize a Core Image vector object with three values. - Parameters:   - x: The value for the first position in the vector.   - y: The value for the second position in the vector.   - z: The value for the third position in the vector. - Returns:    An initialized ``CIVector`` object of length 3.
--
-- ObjC selector: @- initWithX:Y:Z:@
initWithX_Y_Z :: IsCIVector ciVector => ciVector -> CDouble -> CDouble -> CDouble -> IO (Id CIVector)
initWithX_Y_Z ciVector  x y z =
  sendMsg ciVector (mkSelector "initWithX:Y:Z:") (retPtr retVoid) [argCDouble (fromIntegral x), argCDouble (fromIntegral y), argCDouble (fromIntegral z)] >>= ownedObject . castPtr

-- | Initialize a Core Image vector object with four values. - Parameters:   - x: The value for the first position in the vector.   - y: The value for the second position in the vector.   - z: The value for the third position in the vector.   - w: The value for the forth position in the vector. - Returns:    An initialized ``CIVector`` object of length 4.
--
-- ObjC selector: @- initWithX:Y:Z:W:@
initWithX_Y_Z_W :: IsCIVector ciVector => ciVector -> CDouble -> CDouble -> CDouble -> CDouble -> IO (Id CIVector)
initWithX_Y_Z_W ciVector  x y z w =
  sendMsg ciVector (mkSelector "initWithX:Y:Z:W:") (retPtr retVoid) [argCDouble (fromIntegral x), argCDouble (fromIntegral y), argCDouble (fromIntegral z), argCDouble (fromIntegral w)] >>= ownedObject . castPtr

-- | Initialize a Core Image vector object with values provided in a string representation. - Parameters:   - representation: A string that is in one of the formats returned by the @stringRepresentation@ method. - Returns:    An initialized ``CIVector`` object.
--
-- ObjC selector: @- initWithString:@
initWithString :: (IsCIVector ciVector, IsNSString representation) => ciVector -> representation -> IO (Id CIVector)
initWithString ciVector  representation =
withObjCPtr representation $ \raw_representation ->
    sendMsg ciVector (mkSelector "initWithString:") (retPtr retVoid) [argPtr (castPtr raw_representation :: Ptr ())] >>= ownedObject . castPtr

-- | Returns a value from a specific position in the vector.
--
-- The numbering of elements in a vector begins with zero. - Parameters:   - index: The position in the vector of the value that you want to retrieve. - Returns:    The value retrieved from the vector or @0@ if the position is undefined.
--
-- ObjC selector: @- valueAtIndex:@
valueAtIndex :: IsCIVector ciVector => ciVector -> CULong -> IO CDouble
valueAtIndex ciVector  index =
  sendMsg ciVector (mkSelector "valueAtIndex:") retCDouble [argCULong (fromIntegral index)]

-- | The number of items in the vector.
--
-- ObjC selector: @- count@
count :: IsCIVector ciVector => ciVector -> IO CULong
count ciVector  =
  sendMsg ciVector (mkSelector "count") retCULong []

-- | The value located in the first position in the vector.
--
-- ObjC selector: @- X@
x :: IsCIVector ciVector => ciVector -> IO CDouble
x ciVector  =
  sendMsg ciVector (mkSelector "X") retCDouble []

-- | The value located in the second position in the vector.
--
-- ObjC selector: @- Y@
y :: IsCIVector ciVector => ciVector -> IO CDouble
y ciVector  =
  sendMsg ciVector (mkSelector "Y") retCDouble []

-- | The value located in the third position in the vector.
--
-- ObjC selector: @- Z@
z :: IsCIVector ciVector => ciVector -> IO CDouble
z ciVector  =
  sendMsg ciVector (mkSelector "Z") retCDouble []

-- | The value located in the forth position in the vector.
--
-- ObjC selector: @- W@
w :: IsCIVector ciVector => ciVector -> IO CDouble
w ciVector  =
  sendMsg ciVector (mkSelector "W") retCDouble []

-- | Returns a formatted string with all the values of a @CIVector@.
--
-- Some example string representations of vectors:
--
-- @CIVector@                               | @stringRepresentation@ ---------------------------------------- | -------------- @[CIVector vectorWithX:1.0 Y:0.5 Z:0.3]@ | @"[1.0 0.5 0.3]"@ @[CIVector vectorWithX:10.0 Y:23.0]@     | @"[10.0 23.0]"@
--
-- To create a ``CIVector`` object from a string representation, use the ``vectorWithString:`` method.
--
-- ObjC selector: @- stringRepresentation@
stringRepresentation :: IsCIVector ciVector => ciVector -> IO (Id NSString)
stringRepresentation ciVector  =
  sendMsg ciVector (mkSelector "stringRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @vectorWithValues:count:@
vectorWithValues_countSelector :: Selector
vectorWithValues_countSelector = mkSelector "vectorWithValues:count:"

-- | @Selector@ for @vectorWithX:@
vectorWithXSelector :: Selector
vectorWithXSelector = mkSelector "vectorWithX:"

-- | @Selector@ for @vectorWithX:Y:@
vectorWithX_YSelector :: Selector
vectorWithX_YSelector = mkSelector "vectorWithX:Y:"

-- | @Selector@ for @vectorWithX:Y:Z:@
vectorWithX_Y_ZSelector :: Selector
vectorWithX_Y_ZSelector = mkSelector "vectorWithX:Y:Z:"

-- | @Selector@ for @vectorWithX:Y:Z:W:@
vectorWithX_Y_Z_WSelector :: Selector
vectorWithX_Y_Z_WSelector = mkSelector "vectorWithX:Y:Z:W:"

-- | @Selector@ for @vectorWithString:@
vectorWithStringSelector :: Selector
vectorWithStringSelector = mkSelector "vectorWithString:"

-- | @Selector@ for @initWithValues:count:@
initWithValues_countSelector :: Selector
initWithValues_countSelector = mkSelector "initWithValues:count:"

-- | @Selector@ for @initWithX:@
initWithXSelector :: Selector
initWithXSelector = mkSelector "initWithX:"

-- | @Selector@ for @initWithX:Y:@
initWithX_YSelector :: Selector
initWithX_YSelector = mkSelector "initWithX:Y:"

-- | @Selector@ for @initWithX:Y:Z:@
initWithX_Y_ZSelector :: Selector
initWithX_Y_ZSelector = mkSelector "initWithX:Y:Z:"

-- | @Selector@ for @initWithX:Y:Z:W:@
initWithX_Y_Z_WSelector :: Selector
initWithX_Y_Z_WSelector = mkSelector "initWithX:Y:Z:W:"

-- | @Selector@ for @initWithString:@
initWithStringSelector :: Selector
initWithStringSelector = mkSelector "initWithString:"

-- | @Selector@ for @valueAtIndex:@
valueAtIndexSelector :: Selector
valueAtIndexSelector = mkSelector "valueAtIndex:"

-- | @Selector@ for @count@
countSelector :: Selector
countSelector = mkSelector "count"

-- | @Selector@ for @X@
xSelector :: Selector
xSelector = mkSelector "X"

-- | @Selector@ for @Y@
ySelector :: Selector
ySelector = mkSelector "Y"

-- | @Selector@ for @Z@
zSelector :: Selector
zSelector = mkSelector "Z"

-- | @Selector@ for @W@
wSelector :: Selector
wSelector = mkSelector "W"

-- | @Selector@ for @stringRepresentation@
stringRepresentationSelector :: Selector
stringRepresentationSelector = mkSelector "stringRepresentation"

