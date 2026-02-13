{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MLCTensorOptimizerDeviceData@.
module ObjC.MLCompute.MLCTensorOptimizerDeviceData
  ( MLCTensorOptimizerDeviceData
  , IsMLCTensorOptimizerDeviceData(..)
  , new
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MLCompute.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MLCTensorOptimizerDeviceData)
new  =
  do
    cls' <- getRequiredClass "MLCTensorOptimizerDeviceData"
    sendOwnedClassMessage cls' newSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MLCTensorOptimizerDeviceData)
newSelector = mkSelector "new"

