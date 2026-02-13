{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMLComputePlanCost mlComputePlanCost => mlComputePlanCost -> IO (Id MLComputePlanCost)
init_ mlComputePlanCost =
  sendOwnedMessage mlComputePlanCost initSelector

-- | @+ new@
new :: IO (Id MLComputePlanCost)
new  =
  do
    cls' <- getRequiredClass "MLComputePlanCost"
    sendOwnedClassMessage cls' newSelector

-- | The estimated workload of executing the operation over the total model execution. The value is between [0.0, 1.0].
--
-- ObjC selector: @- weight@
weight :: IsMLComputePlanCost mlComputePlanCost => mlComputePlanCost -> IO CDouble
weight mlComputePlanCost =
  sendMessage mlComputePlanCost weightSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLComputePlanCost)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MLComputePlanCost)
newSelector = mkSelector "new"

-- | @Selector@ for @weight@
weightSelector :: Selector '[] CDouble
weightSelector = mkSelector "weight"

