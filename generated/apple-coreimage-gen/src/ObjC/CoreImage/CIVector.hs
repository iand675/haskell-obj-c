{-# LANGUAGE DataKinds #-}
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
  , countSelector
  , initWithStringSelector
  , initWithValues_countSelector
  , initWithXSelector
  , initWithX_YSelector
  , initWithX_Y_ZSelector
  , initWithX_Y_Z_WSelector
  , stringRepresentationSelector
  , valueAtIndexSelector
  , vectorWithStringSelector
  , vectorWithValues_countSelector
  , vectorWithXSelector
  , vectorWithX_YSelector
  , vectorWithX_Y_ZSelector
  , vectorWithX_Y_Z_WSelector
  , wSelector
  , xSelector
  , ySelector
  , zSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' vectorWithValues_countSelector values count

-- | Create a Core Image vector object that is initialized with one value. - Parameters:   - x: The value for the first position in the vector. - Returns:    An autoreleased ``CIVector`` object of length 1.
--
-- ObjC selector: @+ vectorWithX:@
vectorWithX :: CDouble -> IO (Id CIVector)
vectorWithX x =
  do
    cls' <- getRequiredClass "CIVector"
    sendClassMessage cls' vectorWithXSelector x

-- | Create a Core Image vector object that is initialized with two values. - Parameters:   - x: The value for the first position in the vector.   - y: The value for the second position in the vector. - Returns:    An autoreleased ``CIVector`` object of length 2.
--
-- ObjC selector: @+ vectorWithX:Y:@
vectorWithX_Y :: CDouble -> CDouble -> IO (Id CIVector)
vectorWithX_Y x y =
  do
    cls' <- getRequiredClass "CIVector"
    sendClassMessage cls' vectorWithX_YSelector x y

-- | Create a Core Image vector object that is initialized with three values. - Parameters:   - x: The value for the first position in the vector.   - y: The value for the second position in the vector.   - z: The value for the third position in the vector. - Returns:    An autoreleased ``CIVector`` object of length 3.
--
-- ObjC selector: @+ vectorWithX:Y:Z:@
vectorWithX_Y_Z :: CDouble -> CDouble -> CDouble -> IO (Id CIVector)
vectorWithX_Y_Z x y z =
  do
    cls' <- getRequiredClass "CIVector"
    sendClassMessage cls' vectorWithX_Y_ZSelector x y z

-- | Create a Core Image vector object that is initialized with four values. - Parameters:   - x: The value for the first position in the vector.   - y: The value for the second position in the vector.   - z: The value for the third position in the vector.   - w: The value for the forth position in the vector. - Returns:    An autoreleased ``CIVector`` object of length 4.
--
-- ObjC selector: @+ vectorWithX:Y:Z:W:@
vectorWithX_Y_Z_W :: CDouble -> CDouble -> CDouble -> CDouble -> IO (Id CIVector)
vectorWithX_Y_Z_W x y z w =
  do
    cls' <- getRequiredClass "CIVector"
    sendClassMessage cls' vectorWithX_Y_Z_WSelector x y z w

-- | Create a Core Image vector object with values provided in a string representation. - Parameters:   - representation: A string that is in one of the formats returned by the @stringRepresentation@ method. - Returns:    An autoreleased ``CIVector`` object.
--
-- ObjC selector: @+ vectorWithString:@
vectorWithString :: IsNSString representation => representation -> IO (Id CIVector)
vectorWithString representation =
  do
    cls' <- getRequiredClass "CIVector"
    sendClassMessage cls' vectorWithStringSelector (toNSString representation)

-- | Initialize a Core Image vector object with the specified the values. - Parameters:   - values: A pointer @CGFloat@ values for vector.   - count: The number of @CGFloats@ specified by the @values@ parameter. - Returns:    An initialized ``CIVector`` object of length @count@.
--
-- ObjC selector: @- initWithValues:count:@
initWithValues_count :: IsCIVector ciVector => ciVector -> Const (Ptr CDouble) -> CULong -> IO (Id CIVector)
initWithValues_count ciVector values count =
  sendOwnedMessage ciVector initWithValues_countSelector values count

-- | Initialize a Core Image vector object with one value. - Parameters:   - x: The value for the first position in the vector. - Returns:    An initialized ``CIVector`` object of length 1.
--
-- ObjC selector: @- initWithX:@
initWithX :: IsCIVector ciVector => ciVector -> CDouble -> IO (Id CIVector)
initWithX ciVector x =
  sendOwnedMessage ciVector initWithXSelector x

-- | Initialize a Core Image vector object with two values. - Parameters:   - x: The value for the first position in the vector.   - y: The value for the second position in the vector. - Returns:    An initialized ``CIVector`` object of length 2.
--
-- ObjC selector: @- initWithX:Y:@
initWithX_Y :: IsCIVector ciVector => ciVector -> CDouble -> CDouble -> IO (Id CIVector)
initWithX_Y ciVector x y =
  sendOwnedMessage ciVector initWithX_YSelector x y

-- | Initialize a Core Image vector object with three values. - Parameters:   - x: The value for the first position in the vector.   - y: The value for the second position in the vector.   - z: The value for the third position in the vector. - Returns:    An initialized ``CIVector`` object of length 3.
--
-- ObjC selector: @- initWithX:Y:Z:@
initWithX_Y_Z :: IsCIVector ciVector => ciVector -> CDouble -> CDouble -> CDouble -> IO (Id CIVector)
initWithX_Y_Z ciVector x y z =
  sendOwnedMessage ciVector initWithX_Y_ZSelector x y z

-- | Initialize a Core Image vector object with four values. - Parameters:   - x: The value for the first position in the vector.   - y: The value for the second position in the vector.   - z: The value for the third position in the vector.   - w: The value for the forth position in the vector. - Returns:    An initialized ``CIVector`` object of length 4.
--
-- ObjC selector: @- initWithX:Y:Z:W:@
initWithX_Y_Z_W :: IsCIVector ciVector => ciVector -> CDouble -> CDouble -> CDouble -> CDouble -> IO (Id CIVector)
initWithX_Y_Z_W ciVector x y z w =
  sendOwnedMessage ciVector initWithX_Y_Z_WSelector x y z w

-- | Initialize a Core Image vector object with values provided in a string representation. - Parameters:   - representation: A string that is in one of the formats returned by the @stringRepresentation@ method. - Returns:    An initialized ``CIVector`` object.
--
-- ObjC selector: @- initWithString:@
initWithString :: (IsCIVector ciVector, IsNSString representation) => ciVector -> representation -> IO (Id CIVector)
initWithString ciVector representation =
  sendOwnedMessage ciVector initWithStringSelector (toNSString representation)

-- | Returns a value from a specific position in the vector.
--
-- The numbering of elements in a vector begins with zero. - Parameters:   - index: The position in the vector of the value that you want to retrieve. - Returns:    The value retrieved from the vector or @0@ if the position is undefined.
--
-- ObjC selector: @- valueAtIndex:@
valueAtIndex :: IsCIVector ciVector => ciVector -> CULong -> IO CDouble
valueAtIndex ciVector index =
  sendMessage ciVector valueAtIndexSelector index

-- | The number of items in the vector.
--
-- ObjC selector: @- count@
count :: IsCIVector ciVector => ciVector -> IO CULong
count ciVector =
  sendMessage ciVector countSelector

-- | The value located in the first position in the vector.
--
-- ObjC selector: @- X@
x :: IsCIVector ciVector => ciVector -> IO CDouble
x ciVector =
  sendMessage ciVector xSelector

-- | The value located in the second position in the vector.
--
-- ObjC selector: @- Y@
y :: IsCIVector ciVector => ciVector -> IO CDouble
y ciVector =
  sendMessage ciVector ySelector

-- | The value located in the third position in the vector.
--
-- ObjC selector: @- Z@
z :: IsCIVector ciVector => ciVector -> IO CDouble
z ciVector =
  sendMessage ciVector zSelector

-- | The value located in the forth position in the vector.
--
-- ObjC selector: @- W@
w :: IsCIVector ciVector => ciVector -> IO CDouble
w ciVector =
  sendMessage ciVector wSelector

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
stringRepresentation ciVector =
  sendMessage ciVector stringRepresentationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @vectorWithValues:count:@
vectorWithValues_countSelector :: Selector '[Const (Ptr CDouble), CULong] (Id CIVector)
vectorWithValues_countSelector = mkSelector "vectorWithValues:count:"

-- | @Selector@ for @vectorWithX:@
vectorWithXSelector :: Selector '[CDouble] (Id CIVector)
vectorWithXSelector = mkSelector "vectorWithX:"

-- | @Selector@ for @vectorWithX:Y:@
vectorWithX_YSelector :: Selector '[CDouble, CDouble] (Id CIVector)
vectorWithX_YSelector = mkSelector "vectorWithX:Y:"

-- | @Selector@ for @vectorWithX:Y:Z:@
vectorWithX_Y_ZSelector :: Selector '[CDouble, CDouble, CDouble] (Id CIVector)
vectorWithX_Y_ZSelector = mkSelector "vectorWithX:Y:Z:"

-- | @Selector@ for @vectorWithX:Y:Z:W:@
vectorWithX_Y_Z_WSelector :: Selector '[CDouble, CDouble, CDouble, CDouble] (Id CIVector)
vectorWithX_Y_Z_WSelector = mkSelector "vectorWithX:Y:Z:W:"

-- | @Selector@ for @vectorWithString:@
vectorWithStringSelector :: Selector '[Id NSString] (Id CIVector)
vectorWithStringSelector = mkSelector "vectorWithString:"

-- | @Selector@ for @initWithValues:count:@
initWithValues_countSelector :: Selector '[Const (Ptr CDouble), CULong] (Id CIVector)
initWithValues_countSelector = mkSelector "initWithValues:count:"

-- | @Selector@ for @initWithX:@
initWithXSelector :: Selector '[CDouble] (Id CIVector)
initWithXSelector = mkSelector "initWithX:"

-- | @Selector@ for @initWithX:Y:@
initWithX_YSelector :: Selector '[CDouble, CDouble] (Id CIVector)
initWithX_YSelector = mkSelector "initWithX:Y:"

-- | @Selector@ for @initWithX:Y:Z:@
initWithX_Y_ZSelector :: Selector '[CDouble, CDouble, CDouble] (Id CIVector)
initWithX_Y_ZSelector = mkSelector "initWithX:Y:Z:"

-- | @Selector@ for @initWithX:Y:Z:W:@
initWithX_Y_Z_WSelector :: Selector '[CDouble, CDouble, CDouble, CDouble] (Id CIVector)
initWithX_Y_Z_WSelector = mkSelector "initWithX:Y:Z:W:"

-- | @Selector@ for @initWithString:@
initWithStringSelector :: Selector '[Id NSString] (Id CIVector)
initWithStringSelector = mkSelector "initWithString:"

-- | @Selector@ for @valueAtIndex:@
valueAtIndexSelector :: Selector '[CULong] CDouble
valueAtIndexSelector = mkSelector "valueAtIndex:"

-- | @Selector@ for @count@
countSelector :: Selector '[] CULong
countSelector = mkSelector "count"

-- | @Selector@ for @X@
xSelector :: Selector '[] CDouble
xSelector = mkSelector "X"

-- | @Selector@ for @Y@
ySelector :: Selector '[] CDouble
ySelector = mkSelector "Y"

-- | @Selector@ for @Z@
zSelector :: Selector '[] CDouble
zSelector = mkSelector "Z"

-- | @Selector@ for @W@
wSelector :: Selector '[] CDouble
wSelector = mkSelector "W"

-- | @Selector@ for @stringRepresentation@
stringRepresentationSelector :: Selector '[] (Id NSString)
stringRepresentationSelector = mkSelector "stringRepresentation"

