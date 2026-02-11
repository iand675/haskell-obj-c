{-# LANGUAGE PatternSynonyms #-}
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
    sendClassMsg cls' (mkSelector "billboardConstraint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | freeAxes
--
-- Specifies the axes on which the billboarding orientation operates. Defaults to SCNBillboardAxisAll.
--
-- ObjC selector: @- freeAxes@
freeAxes :: IsSCNBillboardConstraint scnBillboardConstraint => scnBillboardConstraint -> IO SCNBillboardAxis
freeAxes scnBillboardConstraint  =
  fmap (coerce :: CULong -> SCNBillboardAxis) $ sendMsg scnBillboardConstraint (mkSelector "freeAxes") retCULong []

-- | freeAxes
--
-- Specifies the axes on which the billboarding orientation operates. Defaults to SCNBillboardAxisAll.
--
-- ObjC selector: @- setFreeAxes:@
setFreeAxes :: IsSCNBillboardConstraint scnBillboardConstraint => scnBillboardConstraint -> SCNBillboardAxis -> IO ()
setFreeAxes scnBillboardConstraint  value =
  sendMsg scnBillboardConstraint (mkSelector "setFreeAxes:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @billboardConstraint@
billboardConstraintSelector :: Selector
billboardConstraintSelector = mkSelector "billboardConstraint"

-- | @Selector@ for @freeAxes@
freeAxesSelector :: Selector
freeAxesSelector = mkSelector "freeAxes"

-- | @Selector@ for @setFreeAxes:@
setFreeAxesSelector :: Selector
setFreeAxesSelector = mkSelector "setFreeAxes:"

