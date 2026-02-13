{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUnitConverter@.
module ObjC.Foundation.NSUnitConverter
  ( NSUnitConverter
  , IsNSUnitConverter(..)
  , baseUnitValueFromValue
  , valueFromBaseUnitValue
  , baseUnitValueFromValueSelector
  , valueFromBaseUnitValueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- baseUnitValueFromValue:@
baseUnitValueFromValue :: IsNSUnitConverter nsUnitConverter => nsUnitConverter -> CDouble -> IO CDouble
baseUnitValueFromValue nsUnitConverter value =
  sendMessage nsUnitConverter baseUnitValueFromValueSelector value

-- | @- valueFromBaseUnitValue:@
valueFromBaseUnitValue :: IsNSUnitConverter nsUnitConverter => nsUnitConverter -> CDouble -> IO CDouble
valueFromBaseUnitValue nsUnitConverter baseUnitValue =
  sendMessage nsUnitConverter valueFromBaseUnitValueSelector baseUnitValue

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @baseUnitValueFromValue:@
baseUnitValueFromValueSelector :: Selector '[CDouble] CDouble
baseUnitValueFromValueSelector = mkSelector "baseUnitValueFromValue:"

-- | @Selector@ for @valueFromBaseUnitValue:@
valueFromBaseUnitValueSelector :: Selector '[CDouble] CDouble
valueFromBaseUnitValueSelector = mkSelector "valueFromBaseUnitValue:"

