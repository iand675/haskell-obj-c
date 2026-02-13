{-# LANGUAGE DataKinds #-}
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
  , collisionCategoryBitMaskSelector
  , offsetSelector
  , radiusSelector
  , setCollisionCategoryBitMaskSelector
  , setOffsetSelector
  , setRadiusSelector
  , sliderConstraintSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' sliderConstraintSelector

-- | collisionCategoryBitMask
--
-- Defines the category of node to collide against. Defaults to 0.
--
-- ObjC selector: @- collisionCategoryBitMask@
collisionCategoryBitMask :: IsSCNSliderConstraint scnSliderConstraint => scnSliderConstraint -> IO CULong
collisionCategoryBitMask scnSliderConstraint =
  sendMessage scnSliderConstraint collisionCategoryBitMaskSelector

-- | collisionCategoryBitMask
--
-- Defines the category of node to collide against. Defaults to 0.
--
-- ObjC selector: @- setCollisionCategoryBitMask:@
setCollisionCategoryBitMask :: IsSCNSliderConstraint scnSliderConstraint => scnSliderConstraint -> CULong -> IO ()
setCollisionCategoryBitMask scnSliderConstraint value =
  sendMessage scnSliderConstraint setCollisionCategoryBitMaskSelector value

-- | radius
--
-- Defines the radius of the slider. Defaults to 1.
--
-- ObjC selector: @- radius@
radius :: IsSCNSliderConstraint scnSliderConstraint => scnSliderConstraint -> IO CDouble
radius scnSliderConstraint =
  sendMessage scnSliderConstraint radiusSelector

-- | radius
--
-- Defines the radius of the slider. Defaults to 1.
--
-- ObjC selector: @- setRadius:@
setRadius :: IsSCNSliderConstraint scnSliderConstraint => scnSliderConstraint -> CDouble -> IO ()
setRadius scnSliderConstraint value =
  sendMessage scnSliderConstraint setRadiusSelector value

-- | offset
--
-- Defines the offset of the slider. Defaults to (0,0,0).
--
-- ObjC selector: @- offset@
offset :: IsSCNSliderConstraint scnSliderConstraint => scnSliderConstraint -> IO SCNVector3
offset scnSliderConstraint =
  sendMessage scnSliderConstraint offsetSelector

-- | offset
--
-- Defines the offset of the slider. Defaults to (0,0,0).
--
-- ObjC selector: @- setOffset:@
setOffset :: IsSCNSliderConstraint scnSliderConstraint => scnSliderConstraint -> SCNVector3 -> IO ()
setOffset scnSliderConstraint value =
  sendMessage scnSliderConstraint setOffsetSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sliderConstraint@
sliderConstraintSelector :: Selector '[] (Id SCNSliderConstraint)
sliderConstraintSelector = mkSelector "sliderConstraint"

-- | @Selector@ for @collisionCategoryBitMask@
collisionCategoryBitMaskSelector :: Selector '[] CULong
collisionCategoryBitMaskSelector = mkSelector "collisionCategoryBitMask"

-- | @Selector@ for @setCollisionCategoryBitMask:@
setCollisionCategoryBitMaskSelector :: Selector '[CULong] ()
setCollisionCategoryBitMaskSelector = mkSelector "setCollisionCategoryBitMask:"

-- | @Selector@ for @radius@
radiusSelector :: Selector '[] CDouble
radiusSelector = mkSelector "radius"

-- | @Selector@ for @setRadius:@
setRadiusSelector :: Selector '[CDouble] ()
setRadiusSelector = mkSelector "setRadius:"

-- | @Selector@ for @offset@
offsetSelector :: Selector '[] SCNVector3
offsetSelector = mkSelector "offset"

-- | @Selector@ for @setOffset:@
setOffsetSelector :: Selector '[SCNVector3] ()
setOffsetSelector = mkSelector "setOffset:"

