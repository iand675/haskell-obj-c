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

import ObjC.Foundation.Internal.Classes

-- | @- baseUnitValueFromValue:@
baseUnitValueFromValue :: IsNSUnitConverter nsUnitConverter => nsUnitConverter -> CDouble -> IO CDouble
baseUnitValueFromValue nsUnitConverter  value =
  sendMsg nsUnitConverter (mkSelector "baseUnitValueFromValue:") retCDouble [argCDouble (fromIntegral value)]

-- | @- valueFromBaseUnitValue:@
valueFromBaseUnitValue :: IsNSUnitConverter nsUnitConverter => nsUnitConverter -> CDouble -> IO CDouble
valueFromBaseUnitValue nsUnitConverter  baseUnitValue =
  sendMsg nsUnitConverter (mkSelector "valueFromBaseUnitValue:") retCDouble [argCDouble (fromIntegral baseUnitValue)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @baseUnitValueFromValue:@
baseUnitValueFromValueSelector :: Selector
baseUnitValueFromValueSelector = mkSelector "baseUnitValueFromValue:"

-- | @Selector@ for @valueFromBaseUnitValue:@
valueFromBaseUnitValueSelector :: Selector
valueFromBaseUnitValueSelector = mkSelector "valueFromBaseUnitValue:"

