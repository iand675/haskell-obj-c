{-# LANGUAGE PatternSynonyms #-}
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
  , layerWithReflectionPaddingSelector
  , layerWithSymmetricPaddingSelector
  , layerWithZeroPaddingSelector
  , layerWithConstantPadding_constantValueSelector
  , paddingTypeSelector
  , paddingLeftSelector
  , paddingRightSelector
  , paddingTopSelector
  , paddingBottomSelector
  , constantValueSelector

  -- * Enum types
  , MLCPaddingType(MLCPaddingType)
  , pattern MLCPaddingTypeZero
  , pattern MLCPaddingTypeReflect
  , pattern MLCPaddingTypeSymmetric
  , pattern MLCPaddingTypeConstant

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
    withObjCPtr padding $ \raw_padding ->
      sendClassMsg cls' (mkSelector "layerWithReflectionPadding:") (retPtr retVoid) [argPtr (castPtr raw_padding :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr padding $ \raw_padding ->
      sendClassMsg cls' (mkSelector "layerWithSymmetricPadding:") (retPtr retVoid) [argPtr (castPtr raw_padding :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr padding $ \raw_padding ->
      sendClassMsg cls' (mkSelector "layerWithZeroPadding:") (retPtr retVoid) [argPtr (castPtr raw_padding :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr padding $ \raw_padding ->
      sendClassMsg cls' (mkSelector "layerWithConstantPadding:constantValue:") (retPtr retVoid) [argPtr (castPtr raw_padding :: Ptr ()), argCFloat (fromIntegral constantValue)] >>= retainedObject . castPtr

-- | paddingType
--
-- The padding type i.e. constant, zero, reflect or symmetric
--
-- ObjC selector: @- paddingType@
paddingType :: IsMLCPaddingLayer mlcPaddingLayer => mlcPaddingLayer -> IO MLCPaddingType
paddingType mlcPaddingLayer  =
  fmap (coerce :: CInt -> MLCPaddingType) $ sendMsg mlcPaddingLayer (mkSelector "paddingType") retCInt []

-- | paddingLeft
--
-- The left padding size
--
-- ObjC selector: @- paddingLeft@
paddingLeft :: IsMLCPaddingLayer mlcPaddingLayer => mlcPaddingLayer -> IO CULong
paddingLeft mlcPaddingLayer  =
  sendMsg mlcPaddingLayer (mkSelector "paddingLeft") retCULong []

-- | paddingRight
--
-- The right padding size
--
-- ObjC selector: @- paddingRight@
paddingRight :: IsMLCPaddingLayer mlcPaddingLayer => mlcPaddingLayer -> IO CULong
paddingRight mlcPaddingLayer  =
  sendMsg mlcPaddingLayer (mkSelector "paddingRight") retCULong []

-- | paddingTop
--
-- The top padding size
--
-- ObjC selector: @- paddingTop@
paddingTop :: IsMLCPaddingLayer mlcPaddingLayer => mlcPaddingLayer -> IO CULong
paddingTop mlcPaddingLayer  =
  sendMsg mlcPaddingLayer (mkSelector "paddingTop") retCULong []

-- | paddingBottom
--
-- The bottom padding size
--
-- ObjC selector: @- paddingBottom@
paddingBottom :: IsMLCPaddingLayer mlcPaddingLayer => mlcPaddingLayer -> IO CULong
paddingBottom mlcPaddingLayer  =
  sendMsg mlcPaddingLayer (mkSelector "paddingBottom") retCULong []

-- | constantValue
--
-- The constant value to use if padding type is constant.
--
-- ObjC selector: @- constantValue@
constantValue :: IsMLCPaddingLayer mlcPaddingLayer => mlcPaddingLayer -> IO CFloat
constantValue mlcPaddingLayer  =
  sendMsg mlcPaddingLayer (mkSelector "constantValue") retCFloat []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithReflectionPadding:@
layerWithReflectionPaddingSelector :: Selector
layerWithReflectionPaddingSelector = mkSelector "layerWithReflectionPadding:"

-- | @Selector@ for @layerWithSymmetricPadding:@
layerWithSymmetricPaddingSelector :: Selector
layerWithSymmetricPaddingSelector = mkSelector "layerWithSymmetricPadding:"

-- | @Selector@ for @layerWithZeroPadding:@
layerWithZeroPaddingSelector :: Selector
layerWithZeroPaddingSelector = mkSelector "layerWithZeroPadding:"

-- | @Selector@ for @layerWithConstantPadding:constantValue:@
layerWithConstantPadding_constantValueSelector :: Selector
layerWithConstantPadding_constantValueSelector = mkSelector "layerWithConstantPadding:constantValue:"

-- | @Selector@ for @paddingType@
paddingTypeSelector :: Selector
paddingTypeSelector = mkSelector "paddingType"

-- | @Selector@ for @paddingLeft@
paddingLeftSelector :: Selector
paddingLeftSelector = mkSelector "paddingLeft"

-- | @Selector@ for @paddingRight@
paddingRightSelector :: Selector
paddingRightSelector = mkSelector "paddingRight"

-- | @Selector@ for @paddingTop@
paddingTopSelector :: Selector
paddingTopSelector = mkSelector "paddingTop"

-- | @Selector@ for @paddingBottom@
paddingBottomSelector :: Selector
paddingBottomSelector = mkSelector "paddingBottom"

-- | @Selector@ for @constantValue@
constantValueSelector :: Selector
constantValueSelector = mkSelector "constantValue"

