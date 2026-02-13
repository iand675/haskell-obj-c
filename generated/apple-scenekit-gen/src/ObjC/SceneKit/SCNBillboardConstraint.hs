{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SCNBillboardConstraint@.
module ObjC.SceneKit.SCNBillboardConstraint
  ( SCNBillboardConstraint
  , IsSCNBillboardConstraint(..)
  , billboardConstraint
  , freeAxes
  , setFreeAxes
  , billboardConstraintSelector
  , freeAxesSelector
  , setFreeAxesSelector

  -- * Enum types
  , SCNBillboardAxis(SCNBillboardAxis)
  , pattern SCNBillboardAxisX
  , pattern SCNBillboardAxisY
  , pattern SCNBillboardAxisZ
  , pattern SCNBillboardAxisAll

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.SceneKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | billboardConstraint:
--
-- Creates and returns a SCNBillboardConstraint constraint.
--
-- A billboard constraint forces the receiver to look into the direction of the current point of view.
--
-- ObjC selector: @+ billboardConstraint@
billboardConstraint :: IO (Id SCNBillboardConstraint)
billboardConstraint  =
  do
    cls' <- getRequiredClass "SCNBillboardConstraint"
    sendClassMessage cls' billboardConstraintSelector

-- | freeAxes
--
-- Specifies the axes on which the billboarding orientation operates. Defaults to SCNBillboardAxisAll.
--
-- ObjC selector: @- freeAxes@
freeAxes :: IsSCNBillboardConstraint scnBillboardConstraint => scnBillboardConstraint -> IO SCNBillboardAxis
freeAxes scnBillboardConstraint =
  sendMessage scnBillboardConstraint freeAxesSelector

-- | freeAxes
--
-- Specifies the axes on which the billboarding orientation operates. Defaults to SCNBillboardAxisAll.
--
-- ObjC selector: @- setFreeAxes:@
setFreeAxes :: IsSCNBillboardConstraint scnBillboardConstraint => scnBillboardConstraint -> SCNBillboardAxis -> IO ()
setFreeAxes scnBillboardConstraint value =
  sendMessage scnBillboardConstraint setFreeAxesSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @billboardConstraint@
billboardConstraintSelector :: Selector '[] (Id SCNBillboardConstraint)
billboardConstraintSelector = mkSelector "billboardConstraint"

-- | @Selector@ for @freeAxes@
freeAxesSelector :: Selector '[] SCNBillboardAxis
freeAxesSelector = mkSelector "freeAxes"

-- | @Selector@ for @setFreeAxes:@
setFreeAxesSelector :: Selector '[SCNBillboardAxis] ()
setFreeAxesSelector = mkSelector "setFreeAxes:"

