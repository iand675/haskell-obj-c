{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNSliderConstraint
--
-- A SCNSliderConstraint constraint makes a node to collide and slide against a category of nodes
--
-- Generated bindings for @SCNSliderConstraint@.
module ObjC.SceneKit.SCNSliderConstraint
  ( SCNSliderConstraint
  , IsSCNSliderConstraint(..)
  , sliderConstraint
  , collisionCategoryBitMask
  , setCollisionCategoryBitMask
  , radius
  , setRadius
  , offset
  , setOffset
  , sliderConstraintSelector
  , collisionCategoryBitMaskSelector
  , setCollisionCategoryBitMaskSelector
  , radiusSelector
  , setRadiusSelector
  , offsetSelector
  , setOffsetSelector


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
import ObjC.SceneKit.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | accelerationConstraint
--
-- Creates and returns a SCNSliderConstraint object.
--
-- ObjC selector: @+ sliderConstraint@
sliderConstraint :: IO (Id SCNSliderConstraint)
sliderConstraint  =
  do
    cls' <- getRequiredClass "SCNSliderConstraint"
    sendClassMsg cls' (mkSelector "sliderConstraint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | collisionCategoryBitMask
--
-- Defines the category of node to collide against. Defaults to 0.
--
-- ObjC selector: @- collisionCategoryBitMask@
collisionCategoryBitMask :: IsSCNSliderConstraint scnSliderConstraint => scnSliderConstraint -> IO CULong
collisionCategoryBitMask scnSliderConstraint  =
  sendMsg scnSliderConstraint (mkSelector "collisionCategoryBitMask") retCULong []

-- | collisionCategoryBitMask
--
-- Defines the category of node to collide against. Defaults to 0.
--
-- ObjC selector: @- setCollisionCategoryBitMask:@
setCollisionCategoryBitMask :: IsSCNSliderConstraint scnSliderConstraint => scnSliderConstraint -> CULong -> IO ()
setCollisionCategoryBitMask scnSliderConstraint  value =
  sendMsg scnSliderConstraint (mkSelector "setCollisionCategoryBitMask:") retVoid [argCULong (fromIntegral value)]

-- | radius
--
-- Defines the radius of the slider. Defaults to 1.
--
-- ObjC selector: @- radius@
radius :: IsSCNSliderConstraint scnSliderConstraint => scnSliderConstraint -> IO CDouble
radius scnSliderConstraint  =
  sendMsg scnSliderConstraint (mkSelector "radius") retCDouble []

-- | radius
--
-- Defines the radius of the slider. Defaults to 1.
--
-- ObjC selector: @- setRadius:@
setRadius :: IsSCNSliderConstraint scnSliderConstraint => scnSliderConstraint -> CDouble -> IO ()
setRadius scnSliderConstraint  value =
  sendMsg scnSliderConstraint (mkSelector "setRadius:") retVoid [argCDouble (fromIntegral value)]

-- | offset
--
-- Defines the offset of the slider. Defaults to (0,0,0).
--
-- ObjC selector: @- offset@
offset :: IsSCNSliderConstraint scnSliderConstraint => scnSliderConstraint -> IO SCNVector3
offset scnSliderConstraint  =
  sendMsgStret scnSliderConstraint (mkSelector "offset") retSCNVector3 []

-- | offset
--
-- Defines the offset of the slider. Defaults to (0,0,0).
--
-- ObjC selector: @- setOffset:@
setOffset :: IsSCNSliderConstraint scnSliderConstraint => scnSliderConstraint -> SCNVector3 -> IO ()
setOffset scnSliderConstraint  value =
  sendMsg scnSliderConstraint (mkSelector "setOffset:") retVoid [argSCNVector3 value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sliderConstraint@
sliderConstraintSelector :: Selector
sliderConstraintSelector = mkSelector "sliderConstraint"

-- | @Selector@ for @collisionCategoryBitMask@
collisionCategoryBitMaskSelector :: Selector
collisionCategoryBitMaskSelector = mkSelector "collisionCategoryBitMask"

-- | @Selector@ for @setCollisionCategoryBitMask:@
setCollisionCategoryBitMaskSelector :: Selector
setCollisionCategoryBitMaskSelector = mkSelector "setCollisionCategoryBitMask:"

-- | @Selector@ for @radius@
radiusSelector :: Selector
radiusSelector = mkSelector "radius"

-- | @Selector@ for @setRadius:@
setRadiusSelector :: Selector
setRadiusSelector = mkSelector "setRadius:"

-- | @Selector@ for @offset@
offsetSelector :: Selector
offsetSelector = mkSelector "offset"

-- | @Selector@ for @setOffset:@
setOffsetSelector :: Selector
setOffsetSelector = mkSelector "setOffset:"

