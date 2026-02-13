{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUnitConverterLinear@.
module ObjC.Foundation.NSUnitConverterLinear
  ( NSUnitConverterLinear
  , IsNSUnitConverterLinear(..)
  , initWithCoefficient
  , initWithCoefficient_constant
  , coefficient
  , constant
  , coefficientSelector
  , constantSelector
  , initWithCoefficientSelector
  , initWithCoefficient_constantSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- initWithCoefficient:@
initWithCoefficient :: IsNSUnitConverterLinear nsUnitConverterLinear => nsUnitConverterLinear -> CDouble -> IO (Id NSUnitConverterLinear)
initWithCoefficient nsUnitConverterLinear coefficient =
  sendOwnedMessage nsUnitConverterLinear initWithCoefficientSelector coefficient

-- | @- initWithCoefficient:constant:@
initWithCoefficient_constant :: IsNSUnitConverterLinear nsUnitConverterLinear => nsUnitConverterLinear -> CDouble -> CDouble -> IO (Id NSUnitConverterLinear)
initWithCoefficient_constant nsUnitConverterLinear coefficient constant =
  sendOwnedMessage nsUnitConverterLinear initWithCoefficient_constantSelector coefficient constant

-- | @- coefficient@
coefficient :: IsNSUnitConverterLinear nsUnitConverterLinear => nsUnitConverterLinear -> IO CDouble
coefficient nsUnitConverterLinear =
  sendMessage nsUnitConverterLinear coefficientSelector

-- | @- constant@
constant :: IsNSUnitConverterLinear nsUnitConverterLinear => nsUnitConverterLinear -> IO CDouble
constant nsUnitConverterLinear =
  sendMessage nsUnitConverterLinear constantSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCoefficient:@
initWithCoefficientSelector :: Selector '[CDouble] (Id NSUnitConverterLinear)
initWithCoefficientSelector = mkSelector "initWithCoefficient:"

-- | @Selector@ for @initWithCoefficient:constant:@
initWithCoefficient_constantSelector :: Selector '[CDouble, CDouble] (Id NSUnitConverterLinear)
initWithCoefficient_constantSelector = mkSelector "initWithCoefficient:constant:"

-- | @Selector@ for @coefficient@
coefficientSelector :: Selector '[] CDouble
coefficientSelector = mkSelector "coefficient"

-- | @Selector@ for @constant@
constantSelector :: Selector '[] CDouble
constantSelector = mkSelector "constant"

