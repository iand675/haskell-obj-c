{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VNVector
--
-- VNVector is a two-dimensional vector represented its X and Y axis projections. Once created, VNVector objects are immutable.
--
-- Generated bindings for @VNVector@.
module ObjC.Vision.VNVector
  ( VNVector
  , IsVNVector(..)
  , unitVectorForVector
  , vectorByMultiplyingVector_byScalar
  , vectorByAddingVector_toVector
  , vectorBySubtractingVector_fromVector
  , dotProductOfVector_vector
  , initWithXComponent_yComponent
  , initWithR_theta
  , initWithVectorHead_tail
  , zeroVector
  , x
  , y
  , r
  , theta
  , length_
  , squaredLength
  , dotProductOfVector_vectorSelector
  , initWithR_thetaSelector
  , initWithVectorHead_tailSelector
  , initWithXComponent_yComponentSelector
  , lengthSelector
  , rSelector
  , squaredLengthSelector
  , thetaSelector
  , unitVectorForVectorSelector
  , vectorByAddingVector_toVectorSelector
  , vectorByMultiplyingVector_byScalarSelector
  , vectorBySubtractingVector_fromVectorSelector
  , xSelector
  , ySelector
  , zeroVectorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Returns a vector that is normalized by preserving direction, such as |v|, or vector length = 1.0.
--
-- ObjC selector: @+ unitVectorForVector:@
unitVectorForVector :: IsVNVector vector => vector -> IO (Id VNVector)
unitVectorForVector vector =
  do
    cls' <- getRequiredClass "VNVector"
    sendClassMessage cls' unitVectorForVectorSelector (toVNVector vector)

-- | Returns a vector that whose X and Y projections multiplied by a scalar value.
--
-- ObjC selector: @+ vectorByMultiplyingVector:byScalar:@
vectorByMultiplyingVector_byScalar :: IsVNVector vector => vector -> CDouble -> IO (Id VNVector)
vectorByMultiplyingVector_byScalar vector scalar =
  do
    cls' <- getRequiredClass "VNVector"
    sendClassMessage cls' vectorByMultiplyingVector_byScalarSelector (toVNVector vector) scalar

-- | Adds two vectors v1 and v2 and returns a resulting vector v, such as v = v1 + v2.
--
-- ObjC selector: @+ vectorByAddingVector:toVector:@
vectorByAddingVector_toVector :: (IsVNVector v1, IsVNVector v2) => v1 -> v2 -> IO (Id VNVector)
vectorByAddingVector_toVector v1 v2 =
  do
    cls' <- getRequiredClass "VNVector"
    sendClassMessage cls' vectorByAddingVector_toVectorSelector (toVNVector v1) (toVNVector v2)

-- | Substructs vector v1 from v2 and returns a resulting vector v, such as v = v2 - v1.
--
-- ObjC selector: @+ vectorBySubtractingVector:fromVector:@
vectorBySubtractingVector_fromVector :: (IsVNVector v1, IsVNVector v2) => v1 -> v2 -> IO (Id VNVector)
vectorBySubtractingVector_fromVector v1 v2 =
  do
    cls' <- getRequiredClass "VNVector"
    sendClassMessage cls' vectorBySubtractingVector_fromVectorSelector (toVNVector v1) (toVNVector v2)

-- | Caclulates a dot product (aka 'scalar product' or 'inner product') of two vectors v1 and v2 and returns dot product value.
--
-- ObjC selector: @+ dotProductOfVector:vector:@
dotProductOfVector_vector :: (IsVNVector v1, IsVNVector v2) => v1 -> v2 -> IO CDouble
dotProductOfVector_vector v1 v2 =
  do
    cls' <- getRequiredClass "VNVector"
    sendClassMessage cls' dotProductOfVector_vectorSelector (toVNVector v1) (toVNVector v2)

-- | Initializes a vector in Cartesian Coordinate space, using its X and Y axis projections.
--
-- ObjC selector: @- initWithXComponent:yComponent:@
initWithXComponent_yComponent :: IsVNVector vnVector => vnVector -> CDouble -> CDouble -> IO (Id VNVector)
initWithXComponent_yComponent vnVector x y =
  sendOwnedMessage vnVector initWithXComponent_yComponentSelector x y

-- | Initializes a vector in polar coordinate space, using R and Theta (radians), where R is the length of the vector and       Theta is the ange that the vector forms with the positive direction of X axis.
--
-- ObjC selector: @- initWithR:theta:@
initWithR_theta :: IsVNVector vnVector => vnVector -> CDouble -> CDouble -> IO (Id VNVector)
initWithR_theta vnVector r theta =
  sendOwnedMessage vnVector initWithR_thetaSelector r theta

-- | Initializes a vector in Cartesian Coordinate space, using two VNPoints - the head and the tail of the vector.
--
-- ObjC selector: @- initWithVectorHead:tail:@
initWithVectorHead_tail :: (IsVNVector vnVector, IsVNPoint head_, IsVNPoint tail_) => vnVector -> head_ -> tail_ -> IO (Id VNVector)
initWithVectorHead_tail vnVector head_ tail_ =
  sendOwnedMessage vnVector initWithVectorHead_tailSelector (toVNPoint head_) (toVNPoint tail_)

-- | Returns a VNVector object with zero length. The theta for zeroVector is not defined (NaN).
--
-- ObjC selector: @+ zeroVector@
zeroVector :: IO (Id VNVector)
zeroVector  =
  do
    cls' <- getRequiredClass "VNVector"
    sendClassMessage cls' zeroVectorSelector

-- | Signed projection on X-axis, or X component of the vector. Sign determines direction the vector is facing in X direction.
--
-- ObjC selector: @- x@
x :: IsVNVector vnVector => vnVector -> IO CDouble
x vnVector =
  sendMessage vnVector xSelector

-- | Signed projection on Y-axis, or Y component of the vector. Sign determines direction the vector is facing in Y direction.
--
-- ObjC selector: @- y@
y :: IsVNVector vnVector => vnVector -> IO CDouble
y vnVector =
  sendMessage vnVector ySelector

-- | Radius, or absolute value, or length of the vector.
--
-- ObjC selector: @- r@
r :: IsVNVector vnVector => vnVector -> IO CDouble
r vnVector =
  sendMessage vnVector rSelector

-- | Angle between the vector direction and positive direction of X axis.
--
-- ObjC selector: @- theta@
theta :: IsVNVector vnVector => vnVector -> IO CDouble
theta vnVector =
  sendMessage vnVector thetaSelector

-- | Returns a length, or absolute value, of the vector.
--
-- ObjC selector: @- length@
length_ :: IsVNVector vnVector => vnVector -> IO CDouble
length_ vnVector =
  sendMessage vnVector lengthSelector

-- | Returns a length ^ 2 of a vector.
--
-- ObjC selector: @- squaredLength@
squaredLength :: IsVNVector vnVector => vnVector -> IO CDouble
squaredLength vnVector =
  sendMessage vnVector squaredLengthSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @unitVectorForVector:@
unitVectorForVectorSelector :: Selector '[Id VNVector] (Id VNVector)
unitVectorForVectorSelector = mkSelector "unitVectorForVector:"

-- | @Selector@ for @vectorByMultiplyingVector:byScalar:@
vectorByMultiplyingVector_byScalarSelector :: Selector '[Id VNVector, CDouble] (Id VNVector)
vectorByMultiplyingVector_byScalarSelector = mkSelector "vectorByMultiplyingVector:byScalar:"

-- | @Selector@ for @vectorByAddingVector:toVector:@
vectorByAddingVector_toVectorSelector :: Selector '[Id VNVector, Id VNVector] (Id VNVector)
vectorByAddingVector_toVectorSelector = mkSelector "vectorByAddingVector:toVector:"

-- | @Selector@ for @vectorBySubtractingVector:fromVector:@
vectorBySubtractingVector_fromVectorSelector :: Selector '[Id VNVector, Id VNVector] (Id VNVector)
vectorBySubtractingVector_fromVectorSelector = mkSelector "vectorBySubtractingVector:fromVector:"

-- | @Selector@ for @dotProductOfVector:vector:@
dotProductOfVector_vectorSelector :: Selector '[Id VNVector, Id VNVector] CDouble
dotProductOfVector_vectorSelector = mkSelector "dotProductOfVector:vector:"

-- | @Selector@ for @initWithXComponent:yComponent:@
initWithXComponent_yComponentSelector :: Selector '[CDouble, CDouble] (Id VNVector)
initWithXComponent_yComponentSelector = mkSelector "initWithXComponent:yComponent:"

-- | @Selector@ for @initWithR:theta:@
initWithR_thetaSelector :: Selector '[CDouble, CDouble] (Id VNVector)
initWithR_thetaSelector = mkSelector "initWithR:theta:"

-- | @Selector@ for @initWithVectorHead:tail:@
initWithVectorHead_tailSelector :: Selector '[Id VNPoint, Id VNPoint] (Id VNVector)
initWithVectorHead_tailSelector = mkSelector "initWithVectorHead:tail:"

-- | @Selector@ for @zeroVector@
zeroVectorSelector :: Selector '[] (Id VNVector)
zeroVectorSelector = mkSelector "zeroVector"

-- | @Selector@ for @x@
xSelector :: Selector '[] CDouble
xSelector = mkSelector "x"

-- | @Selector@ for @y@
ySelector :: Selector '[] CDouble
ySelector = mkSelector "y"

-- | @Selector@ for @r@
rSelector :: Selector '[] CDouble
rSelector = mkSelector "r"

-- | @Selector@ for @theta@
thetaSelector :: Selector '[] CDouble
thetaSelector = mkSelector "theta"

-- | @Selector@ for @length@
lengthSelector :: Selector '[] CDouble
lengthSelector = mkSelector "length"

-- | @Selector@ for @squaredLength@
squaredLengthSelector :: Selector '[] CDouble
squaredLengthSelector = mkSelector "squaredLength"

