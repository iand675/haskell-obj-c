{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCTensorParameter
--
-- A tensor parameter object.  This is used to describe input tensors that are updated by the optimizer during training.
--
-- Generated bindings for @MLCTensorParameter@.
module ObjC.MLCompute.MLCTensorParameter
  ( MLCTensorParameter
  , IsMLCTensorParameter(..)
  , new
  , init_
  , parameterWithTensor
  , parameterWithTensor_optimizerData
  , tensor
  , isUpdatable
  , setIsUpdatable
  , initSelector
  , isUpdatableSelector
  , newSelector
  , parameterWithTensorSelector
  , parameterWithTensor_optimizerDataSelector
  , setIsUpdatableSelector
  , tensorSelector


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
new :: IO (Id MLCTensorParameter)
new  =
  do
    cls' <- getRequiredClass "MLCTensorParameter"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMLCTensorParameter mlcTensorParameter => mlcTensorParameter -> IO (Id MLCTensorParameter)
init_ mlcTensorParameter =
  sendOwnedMessage mlcTensorParameter initSelector

-- | Create a tensor parameter
--
-- @tensor@ — The unedrlying tensor
--
-- Returns: A new tensor parameter object
--
-- ObjC selector: @+ parameterWithTensor:@
parameterWithTensor :: IsMLCTensor tensor => tensor -> IO (Id MLCTensorParameter)
parameterWithTensor tensor =
  do
    cls' <- getRequiredClass "MLCTensorParameter"
    sendClassMessage cls' parameterWithTensorSelector (toMLCTensor tensor)

-- | Create a tensor parameter
--
-- @tensor@ — The unedrlying tensor
--
-- @optimizerData@ — The optimizer data needed for this input tensor
--
-- Returns: A new tensor parameter object
--
-- ObjC selector: @+ parameterWithTensor:optimizerData:@
parameterWithTensor_optimizerData :: (IsMLCTensor tensor, IsNSArray optimizerData) => tensor -> optimizerData -> IO (Id MLCTensorParameter)
parameterWithTensor_optimizerData tensor optimizerData =
  do
    cls' <- getRequiredClass "MLCTensorParameter"
    sendClassMessage cls' parameterWithTensor_optimizerDataSelector (toMLCTensor tensor) (toNSArray optimizerData)

-- | tensor
--
-- The underlying tensor
--
-- ObjC selector: @- tensor@
tensor :: IsMLCTensorParameter mlcTensorParameter => mlcTensorParameter -> IO (Id MLCTensor)
tensor mlcTensorParameter =
  sendMessage mlcTensorParameter tensorSelector

-- | isUpdatable
--
-- Specifies whether this tensor parameter is updatable
--
-- ObjC selector: @- isUpdatable@
isUpdatable :: IsMLCTensorParameter mlcTensorParameter => mlcTensorParameter -> IO Bool
isUpdatable mlcTensorParameter =
  sendMessage mlcTensorParameter isUpdatableSelector

-- | isUpdatable
--
-- Specifies whether this tensor parameter is updatable
--
-- ObjC selector: @- setIsUpdatable:@
setIsUpdatable :: IsMLCTensorParameter mlcTensorParameter => mlcTensorParameter -> Bool -> IO ()
setIsUpdatable mlcTensorParameter value =
  sendMessage mlcTensorParameter setIsUpdatableSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MLCTensorParameter)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLCTensorParameter)
initSelector = mkSelector "init"

-- | @Selector@ for @parameterWithTensor:@
parameterWithTensorSelector :: Selector '[Id MLCTensor] (Id MLCTensorParameter)
parameterWithTensorSelector = mkSelector "parameterWithTensor:"

-- | @Selector@ for @parameterWithTensor:optimizerData:@
parameterWithTensor_optimizerDataSelector :: Selector '[Id MLCTensor, Id NSArray] (Id MLCTensorParameter)
parameterWithTensor_optimizerDataSelector = mkSelector "parameterWithTensor:optimizerData:"

-- | @Selector@ for @tensor@
tensorSelector :: Selector '[] (Id MLCTensor)
tensorSelector = mkSelector "tensor"

-- | @Selector@ for @isUpdatable@
isUpdatableSelector :: Selector '[] Bool
isUpdatableSelector = mkSelector "isUpdatable"

-- | @Selector@ for @setIsUpdatable:@
setIsUpdatableSelector :: Selector '[Bool] ()
setIsUpdatableSelector = mkSelector "setIsUpdatable:"

