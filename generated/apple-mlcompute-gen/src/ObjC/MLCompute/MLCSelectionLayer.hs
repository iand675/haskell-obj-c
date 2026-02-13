{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Selection layer is used to select elements from two tensors
--
-- The selection layer takes a condition tensor which acts as a mask that chooses whether the corresponding element / row              in the output should be taken from tensor x (if the element in condition is true) or tensor y (if it is false).              The order of source tensors of the layer must be condition tensor, tensor x, and tensor y.
--
-- Generated bindings for @MLCSelectionLayer@.
module ObjC.MLCompute.MLCSelectionLayer
  ( MLCSelectionLayer
  , IsMLCSelectionLayer(..)
  , layer
  , layerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MLCompute.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a select layer
--
-- Returns: A new layer for selecting elements between two tensors.
--
-- ObjC selector: @+ layer@
layer :: IO (Id MLCSelectionLayer)
layer  =
  do
    cls' <- getRequiredClass "MLCSelectionLayer"
    sendClassMessage cls' layerSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layer@
layerSelector :: Selector '[] (Id MLCSelectionLayer)
layerSelector = mkSelector "layer"

