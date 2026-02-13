{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCPaddingLayer
--
-- A padding layer
--
-- Generated bindings for @MLCPaddingLayer@.
module ObjC.MLCompute.MLCPaddingLayer
  ( MLCPaddingLayer
  , IsMLCPaddingLayer(..)
  , layerWithReflectionPadding
  , layerWithSymmetricPadding
  , layerWithZeroPadding
  , layerWithConstantPadding_constantValue
  , paddingType
  , paddingLeft
  , paddingRight
  , paddingTop
  , paddingBottom
  , constantValue
  , constantValueSelector
  , layerWithConstantPadding_constantValueSelector
  , layerWithReflectionPaddingSelector
  , layerWithSymmetricPaddingSelector
  , layerWithZeroPaddingSelector
  , paddingBottomSelector
  , paddingLeftSelector
  , paddingRightSelector
  , paddingTopSelector
  , paddingTypeSelector

  -- * Enum types
  , MLCPaddingType(MLCPaddingType)
  , pattern MLCPaddingTypeZero
  , pattern MLCPaddingTypeReflect
  , pattern MLCPaddingTypeSymmetric
  , pattern MLCPaddingTypeConstant

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MLCompute.Internal.Classes
import ObjC.MLCompute.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Create a padding layer with reflection padding
--
-- @padding@ — The padding sizes.
--
-- Returns: A new padding layer
--
-- ObjC selector: @+ layerWithReflectionPadding:@
layerWithReflectionPadding :: IsNSArray padding => padding -> IO (Id MLCPaddingLayer)
layerWithReflectionPadding padding =
  do
    cls' <- getRequiredClass "MLCPaddingLayer"
    sendClassMessage cls' layerWithReflectionPaddingSelector (toNSArray padding)

-- | Create a padding layer with symmetric padding
--
-- @padding@ — The padding sizes.
--
-- Returns: A new padding layer
--
-- ObjC selector: @+ layerWithSymmetricPadding:@
layerWithSymmetricPadding :: IsNSArray padding => padding -> IO (Id MLCPaddingLayer)
layerWithSymmetricPadding padding =
  do
    cls' <- getRequiredClass "MLCPaddingLayer"
    sendClassMessage cls' layerWithSymmetricPaddingSelector (toNSArray padding)

-- | Create a padding layer with zero padding
--
-- @padding@ — The padding sizes.
--
-- Returns: A new padding layer
--
-- ObjC selector: @+ layerWithZeroPadding:@
layerWithZeroPadding :: IsNSArray padding => padding -> IO (Id MLCPaddingLayer)
layerWithZeroPadding padding =
  do
    cls' <- getRequiredClass "MLCPaddingLayer"
    sendClassMessage cls' layerWithZeroPaddingSelector (toNSArray padding)

-- | Create a padding layer with constant padding
--
-- @padding@ — The padding sizes.
--
-- @constantValue@ — The constant value to pad the source tensor.
--
-- Returns: A new padding layer
--
-- ObjC selector: @+ layerWithConstantPadding:constantValue:@
layerWithConstantPadding_constantValue :: IsNSArray padding => padding -> CFloat -> IO (Id MLCPaddingLayer)
layerWithConstantPadding_constantValue padding constantValue =
  do
    cls' <- getRequiredClass "MLCPaddingLayer"
    sendClassMessage cls' layerWithConstantPadding_constantValueSelector (toNSArray padding) constantValue

-- | paddingType
--
-- The padding type i.e. constant, zero, reflect or symmetric
--
-- ObjC selector: @- paddingType@
paddingType :: IsMLCPaddingLayer mlcPaddingLayer => mlcPaddingLayer -> IO MLCPaddingType
paddingType mlcPaddingLayer =
  sendMessage mlcPaddingLayer paddingTypeSelector

-- | paddingLeft
--
-- The left padding size
--
-- ObjC selector: @- paddingLeft@
paddingLeft :: IsMLCPaddingLayer mlcPaddingLayer => mlcPaddingLayer -> IO CULong
paddingLeft mlcPaddingLayer =
  sendMessage mlcPaddingLayer paddingLeftSelector

-- | paddingRight
--
-- The right padding size
--
-- ObjC selector: @- paddingRight@
paddingRight :: IsMLCPaddingLayer mlcPaddingLayer => mlcPaddingLayer -> IO CULong
paddingRight mlcPaddingLayer =
  sendMessage mlcPaddingLayer paddingRightSelector

-- | paddingTop
--
-- The top padding size
--
-- ObjC selector: @- paddingTop@
paddingTop :: IsMLCPaddingLayer mlcPaddingLayer => mlcPaddingLayer -> IO CULong
paddingTop mlcPaddingLayer =
  sendMessage mlcPaddingLayer paddingTopSelector

-- | paddingBottom
--
-- The bottom padding size
--
-- ObjC selector: @- paddingBottom@
paddingBottom :: IsMLCPaddingLayer mlcPaddingLayer => mlcPaddingLayer -> IO CULong
paddingBottom mlcPaddingLayer =
  sendMessage mlcPaddingLayer paddingBottomSelector

-- | constantValue
--
-- The constant value to use if padding type is constant.
--
-- ObjC selector: @- constantValue@
constantValue :: IsMLCPaddingLayer mlcPaddingLayer => mlcPaddingLayer -> IO CFloat
constantValue mlcPaddingLayer =
  sendMessage mlcPaddingLayer constantValueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithReflectionPadding:@
layerWithReflectionPaddingSelector :: Selector '[Id NSArray] (Id MLCPaddingLayer)
layerWithReflectionPaddingSelector = mkSelector "layerWithReflectionPadding:"

-- | @Selector@ for @layerWithSymmetricPadding:@
layerWithSymmetricPaddingSelector :: Selector '[Id NSArray] (Id MLCPaddingLayer)
layerWithSymmetricPaddingSelector = mkSelector "layerWithSymmetricPadding:"

-- | @Selector@ for @layerWithZeroPadding:@
layerWithZeroPaddingSelector :: Selector '[Id NSArray] (Id MLCPaddingLayer)
layerWithZeroPaddingSelector = mkSelector "layerWithZeroPadding:"

-- | @Selector@ for @layerWithConstantPadding:constantValue:@
layerWithConstantPadding_constantValueSelector :: Selector '[Id NSArray, CFloat] (Id MLCPaddingLayer)
layerWithConstantPadding_constantValueSelector = mkSelector "layerWithConstantPadding:constantValue:"

-- | @Selector@ for @paddingType@
paddingTypeSelector :: Selector '[] MLCPaddingType
paddingTypeSelector = mkSelector "paddingType"

-- | @Selector@ for @paddingLeft@
paddingLeftSelector :: Selector '[] CULong
paddingLeftSelector = mkSelector "paddingLeft"

-- | @Selector@ for @paddingRight@
paddingRightSelector :: Selector '[] CULong
paddingRightSelector = mkSelector "paddingRight"

-- | @Selector@ for @paddingTop@
paddingTopSelector :: Selector '[] CULong
paddingTopSelector = mkSelector "paddingTop"

-- | @Selector@ for @paddingBottom@
paddingBottomSelector :: Selector '[] CULong
paddingBottomSelector = mkSelector "paddingBottom"

-- | @Selector@ for @constantValue@
constantValueSelector :: Selector '[] CFloat
constantValueSelector = mkSelector "constantValue"

