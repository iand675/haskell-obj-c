{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class representing the estimated cost of executing a layer/operation.
--
-- Generated bindings for @MLComputePlanCost@.
module ObjC.CoreML.MLComputePlanCost
  ( MLComputePlanCost
  , IsMLComputePlanCost(..)
  , init_
  , new
  , weight
  , initSelector
  , newSelector
  , weightSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMLComputePlanCost mlComputePlanCost => mlComputePlanCost -> IO (Id MLComputePlanCost)
init_ mlComputePlanCost  =
  sendMsg mlComputePlanCost (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MLComputePlanCost)
new  =
  do
    cls' <- getRequiredClass "MLComputePlanCost"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The estimated workload of executing the operation over the total model execution. The value is between [0.0, 1.0].
--
-- ObjC selector: @- weight@
weight :: IsMLComputePlanCost mlComputePlanCost => mlComputePlanCost -> IO CDouble
weight mlComputePlanCost  =
  sendMsg mlComputePlanCost (mkSelector "weight") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @weight@
weightSelector :: Selector
weightSelector = mkSelector "weight"

