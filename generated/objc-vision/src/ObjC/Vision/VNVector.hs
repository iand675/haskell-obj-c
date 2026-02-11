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
  , unitVectorForVectorSelector
  , vectorByMultiplyingVector_byScalarSelector
  , vectorByAddingVector_toVectorSelector
  , vectorBySubtractingVector_fromVectorSelector
  , dotProductOfVector_vectorSelector
  , initWithXComponent_yComponentSelector
  , initWithR_thetaSelector
  , initWithVectorHead_tailSelector
  , zeroVectorSelector
  , xSelector
  , ySelector
  , rSelector
  , thetaSelector
  , lengthSelector
  , squaredLengthSelector


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

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Returns a vector that is normalized by preserving direction, such as |v|, or vector length = 1.0.
--
-- ObjC selector: @+ unitVectorForVector:@
unitVectorForVector :: IsVNVector vector => vector -> IO (Id VNVector)
unitVectorForVector vector =
  do
    cls' <- getRequiredClass "VNVector"
    withObjCPtr vector $ \raw_vector ->
      sendClassMsg cls' (mkSelector "unitVectorForVector:") (retPtr retVoid) [argPtr (castPtr raw_vector :: Ptr ())] >>= retainedObject . castPtr

-- | Returns a vector that whose X and Y projections multiplied by a scalar value.
--
-- ObjC selector: @+ vectorByMultiplyingVector:byScalar:@
vectorByMultiplyingVector_byScalar :: IsVNVector vector => vector -> CDouble -> IO (Id VNVector)
vectorByMultiplyingVector_byScalar vector scalar =
  do
    cls' <- getRequiredClass "VNVector"
    withObjCPtr vector $ \raw_vector ->
      sendClassMsg cls' (mkSelector "vectorByMultiplyingVector:byScalar:") (retPtr retVoid) [argPtr (castPtr raw_vector :: Ptr ()), argCDouble (fromIntegral scalar)] >>= retainedObject . castPtr

-- | Adds two vectors v1 and v2 and returns a resulting vector v, such as v = v1 + v2.
--
-- ObjC selector: @+ vectorByAddingVector:toVector:@
vectorByAddingVector_toVector :: (IsVNVector v1, IsVNVector v2) => v1 -> v2 -> IO (Id VNVector)
vectorByAddingVector_toVector v1 v2 =
  do
    cls' <- getRequiredClass "VNVector"
    withObjCPtr v1 $ \raw_v1 ->
      withObjCPtr v2 $ \raw_v2 ->
        sendClassMsg cls' (mkSelector "vectorByAddingVector:toVector:") (retPtr retVoid) [argPtr (castPtr raw_v1 :: Ptr ()), argPtr (castPtr raw_v2 :: Ptr ())] >>= retainedObject . castPtr

-- | Substructs vector v1 from v2 and returns a resulting vector v, such as v = v2 - v1.
--
-- ObjC selector: @+ vectorBySubtractingVector:fromVector:@
vectorBySubtractingVector_fromVector :: (IsVNVector v1, IsVNVector v2) => v1 -> v2 -> IO (Id VNVector)
vectorBySubtractingVector_fromVector v1 v2 =
  do
    cls' <- getRequiredClass "VNVector"
    withObjCPtr v1 $ \raw_v1 ->
      withObjCPtr v2 $ \raw_v2 ->
        sendClassMsg cls' (mkSelector "vectorBySubtractingVector:fromVector:") (retPtr retVoid) [argPtr (castPtr raw_v1 :: Ptr ()), argPtr (castPtr raw_v2 :: Ptr ())] >>= retainedObject . castPtr

-- | Caclulates a dot product (aka 'scalar product' or 'inner product') of two vectors v1 and v2 and returns dot product value.
--
-- ObjC selector: @+ dotProductOfVector:vector:@
dotProductOfVector_vector :: (IsVNVector v1, IsVNVector v2) => v1 -> v2 -> IO CDouble
dotProductOfVector_vector v1 v2 =
  do
    cls' <- getRequiredClass "VNVector"
    withObjCPtr v1 $ \raw_v1 ->
      withObjCPtr v2 $ \raw_v2 ->
        sendClassMsg cls' (mkSelector "dotProductOfVector:vector:") retCDouble [argPtr (castPtr raw_v1 :: Ptr ()), argPtr (castPtr raw_v2 :: Ptr ())]

-- | Initializes a vector in Cartesian Coordinate space, using its X and Y axis projections.
--
-- ObjC selector: @- initWithXComponent:yComponent:@
initWithXComponent_yComponent :: IsVNVector vnVector => vnVector -> CDouble -> CDouble -> IO (Id VNVector)
initWithXComponent_yComponent vnVector  x y =
  sendMsg vnVector (mkSelector "initWithXComponent:yComponent:") (retPtr retVoid) [argCDouble (fromIntegral x), argCDouble (fromIntegral y)] >>= ownedObject . castPtr

-- | Initializes a vector in polar coordinate space, using R and Theta (radians), where R is the length of the vector and       Theta is the ange that the vector forms with the positive direction of X axis.
--
-- ObjC selector: @- initWithR:theta:@
initWithR_theta :: IsVNVector vnVector => vnVector -> CDouble -> CDouble -> IO (Id VNVector)
initWithR_theta vnVector  r theta =
  sendMsg vnVector (mkSelector "initWithR:theta:") (retPtr retVoid) [argCDouble (fromIntegral r), argCDouble (fromIntegral theta)] >>= ownedObject . castPtr

-- | Initializes a vector in Cartesian Coordinate space, using two VNPoints - the head and the tail of the vector.
--
-- ObjC selector: @- initWithVectorHead:tail:@
initWithVectorHead_tail :: (IsVNVector vnVector, IsVNPoint head_, IsVNPoint tail_) => vnVector -> head_ -> tail_ -> IO (Id VNVector)
initWithVectorHead_tail vnVector  head_ tail_ =
withObjCPtr head_ $ \raw_head_ ->
  withObjCPtr tail_ $ \raw_tail_ ->
      sendMsg vnVector (mkSelector "initWithVectorHead:tail:") (retPtr retVoid) [argPtr (castPtr raw_head_ :: Ptr ()), argPtr (castPtr raw_tail_ :: Ptr ())] >>= ownedObject . castPtr

-- | Returns a VNVector object with zero length. The theta for zeroVector is not defined (NaN).
--
-- ObjC selector: @+ zeroVector@
zeroVector :: IO (Id VNVector)
zeroVector  =
  do
    cls' <- getRequiredClass "VNVector"
    sendClassMsg cls' (mkSelector "zeroVector") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Signed projection on X-axis, or X component of the vector. Sign determines direction the vector is facing in X direction.
--
-- ObjC selector: @- x@
x :: IsVNVector vnVector => vnVector -> IO CDouble
x vnVector  =
  sendMsg vnVector (mkSelector "x") retCDouble []

-- | Signed projection on Y-axis, or Y component of the vector. Sign determines direction the vector is facing in Y direction.
--
-- ObjC selector: @- y@
y :: IsVNVector vnVector => vnVector -> IO CDouble
y vnVector  =
  sendMsg vnVector (mkSelector "y") retCDouble []

-- | Radius, or absolute value, or length of the vector.
--
-- ObjC selector: @- r@
r :: IsVNVector vnVector => vnVector -> IO CDouble
r vnVector  =
  sendMsg vnVector (mkSelector "r") retCDouble []

-- | Angle between the vector direction and positive direction of X axis.
--
-- ObjC selector: @- theta@
theta :: IsVNVector vnVector => vnVector -> IO CDouble
theta vnVector  =
  sendMsg vnVector (mkSelector "theta") retCDouble []

-- | Returns a length, or absolute value, of the vector.
--
-- ObjC selector: @- length@
length_ :: IsVNVector vnVector => vnVector -> IO CDouble
length_ vnVector  =
  sendMsg vnVector (mkSelector "length") retCDouble []

-- | Returns a length ^ 2 of a vector.
--
-- ObjC selector: @- squaredLength@
squaredLength :: IsVNVector vnVector => vnVector -> IO CDouble
squaredLength vnVector  =
  sendMsg vnVector (mkSelector "squaredLength") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @unitVectorForVector:@
unitVectorForVectorSelector :: Selector
unitVectorForVectorSelector = mkSelector "unitVectorForVector:"

-- | @Selector@ for @vectorByMultiplyingVector:byScalar:@
vectorByMultiplyingVector_byScalarSelector :: Selector
vectorByMultiplyingVector_byScalarSelector = mkSelector "vectorByMultiplyingVector:byScalar:"

-- | @Selector@ for @vectorByAddingVector:toVector:@
vectorByAddingVector_toVectorSelector :: Selector
vectorByAddingVector_toVectorSelector = mkSelector "vectorByAddingVector:toVector:"

-- | @Selector@ for @vectorBySubtractingVector:fromVector:@
vectorBySubtractingVector_fromVectorSelector :: Selector
vectorBySubtractingVector_fromVectorSelector = mkSelector "vectorBySubtractingVector:fromVector:"

-- | @Selector@ for @dotProductOfVector:vector:@
dotProductOfVector_vectorSelector :: Selector
dotProductOfVector_vectorSelector = mkSelector "dotProductOfVector:vector:"

-- | @Selector@ for @initWithXComponent:yComponent:@
initWithXComponent_yComponentSelector :: Selector
initWithXComponent_yComponentSelector = mkSelector "initWithXComponent:yComponent:"

-- | @Selector@ for @initWithR:theta:@
initWithR_thetaSelector :: Selector
initWithR_thetaSelector = mkSelector "initWithR:theta:"

-- | @Selector@ for @initWithVectorHead:tail:@
initWithVectorHead_tailSelector :: Selector
initWithVectorHead_tailSelector = mkSelector "initWithVectorHead:tail:"

-- | @Selector@ for @zeroVector@
zeroVectorSelector :: Selector
zeroVectorSelector = mkSelector "zeroVector"

-- | @Selector@ for @x@
xSelector :: Selector
xSelector = mkSelector "x"

-- | @Selector@ for @y@
ySelector :: Selector
ySelector = mkSelector "y"

-- | @Selector@ for @r@
rSelector :: Selector
rSelector = mkSelector "r"

-- | @Selector@ for @theta@
thetaSelector :: Selector
thetaSelector = mkSelector "theta"

-- | @Selector@ for @length@
lengthSelector :: Selector
lengthSelector = mkSelector "length"

-- | @Selector@ for @squaredLength@
squaredLengthSelector :: Selector
squaredLengthSelector = mkSelector "squaredLength"

