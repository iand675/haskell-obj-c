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
  , newSelector
  , initSelector
  , parameterWithTensorSelector
  , parameterWithTensor_optimizerDataSelector
  , tensorSelector
  , isUpdatableSelector
  , setIsUpdatableSelector


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

import ObjC.MLCompute.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MLCTensorParameter)
new  =
  do
    cls' <- getRequiredClass "MLCTensorParameter"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMLCTensorParameter mlcTensorParameter => mlcTensorParameter -> IO (Id MLCTensorParameter)
init_ mlcTensorParameter  =
  sendMsg mlcTensorParameter (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
    withObjCPtr tensor $ \raw_tensor ->
      sendClassMsg cls' (mkSelector "parameterWithTensor:") (retPtr retVoid) [argPtr (castPtr raw_tensor :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr tensor $ \raw_tensor ->
      withObjCPtr optimizerData $ \raw_optimizerData ->
        sendClassMsg cls' (mkSelector "parameterWithTensor:optimizerData:") (retPtr retVoid) [argPtr (castPtr raw_tensor :: Ptr ()), argPtr (castPtr raw_optimizerData :: Ptr ())] >>= retainedObject . castPtr

-- | tensor
--
-- The underlying tensor
--
-- ObjC selector: @- tensor@
tensor :: IsMLCTensorParameter mlcTensorParameter => mlcTensorParameter -> IO (Id MLCTensor)
tensor mlcTensorParameter  =
  sendMsg mlcTensorParameter (mkSelector "tensor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | isUpdatable
--
-- Specifies whether this tensor parameter is updatable
--
-- ObjC selector: @- isUpdatable@
isUpdatable :: IsMLCTensorParameter mlcTensorParameter => mlcTensorParameter -> IO Bool
isUpdatable mlcTensorParameter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mlcTensorParameter (mkSelector "isUpdatable") retCULong []

-- | isUpdatable
--
-- Specifies whether this tensor parameter is updatable
--
-- ObjC selector: @- setIsUpdatable:@
setIsUpdatable :: IsMLCTensorParameter mlcTensorParameter => mlcTensorParameter -> Bool -> IO ()
setIsUpdatable mlcTensorParameter  value =
  sendMsg mlcTensorParameter (mkSelector "setIsUpdatable:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @parameterWithTensor:@
parameterWithTensorSelector :: Selector
parameterWithTensorSelector = mkSelector "parameterWithTensor:"

-- | @Selector@ for @parameterWithTensor:optimizerData:@
parameterWithTensor_optimizerDataSelector :: Selector
parameterWithTensor_optimizerDataSelector = mkSelector "parameterWithTensor:optimizerData:"

-- | @Selector@ for @tensor@
tensorSelector :: Selector
tensorSelector = mkSelector "tensor"

-- | @Selector@ for @isUpdatable@
isUpdatableSelector :: Selector
isUpdatableSelector = mkSelector "isUpdatable"

-- | @Selector@ for @setIsUpdatable:@
setIsUpdatableSelector :: Selector
setIsUpdatableSelector = mkSelector "setIsUpdatable:"

