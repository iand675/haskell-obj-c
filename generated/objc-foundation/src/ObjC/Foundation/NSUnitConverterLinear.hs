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
  , initWithCoefficientSelector
  , initWithCoefficient_constantSelector
  , coefficientSelector
  , constantSelector


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

-- | @- initWithCoefficient:@
initWithCoefficient :: IsNSUnitConverterLinear nsUnitConverterLinear => nsUnitConverterLinear -> CDouble -> IO (Id NSUnitConverterLinear)
initWithCoefficient nsUnitConverterLinear  coefficient =
  sendMsg nsUnitConverterLinear (mkSelector "initWithCoefficient:") (retPtr retVoid) [argCDouble (fromIntegral coefficient)] >>= ownedObject . castPtr

-- | @- initWithCoefficient:constant:@
initWithCoefficient_constant :: IsNSUnitConverterLinear nsUnitConverterLinear => nsUnitConverterLinear -> CDouble -> CDouble -> IO (Id NSUnitConverterLinear)
initWithCoefficient_constant nsUnitConverterLinear  coefficient constant =
  sendMsg nsUnitConverterLinear (mkSelector "initWithCoefficient:constant:") (retPtr retVoid) [argCDouble (fromIntegral coefficient), argCDouble (fromIntegral constant)] >>= ownedObject . castPtr

-- | @- coefficient@
coefficient :: IsNSUnitConverterLinear nsUnitConverterLinear => nsUnitConverterLinear -> IO CDouble
coefficient nsUnitConverterLinear  =
  sendMsg nsUnitConverterLinear (mkSelector "coefficient") retCDouble []

-- | @- constant@
constant :: IsNSUnitConverterLinear nsUnitConverterLinear => nsUnitConverterLinear -> IO CDouble
constant nsUnitConverterLinear  =
  sendMsg nsUnitConverterLinear (mkSelector "constant") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCoefficient:@
initWithCoefficientSelector :: Selector
initWithCoefficientSelector = mkSelector "initWithCoefficient:"

-- | @Selector@ for @initWithCoefficient:constant:@
initWithCoefficient_constantSelector :: Selector
initWithCoefficient_constantSelector = mkSelector "initWithCoefficient:constant:"

-- | @Selector@ for @coefficient@
coefficientSelector :: Selector
coefficientSelector = mkSelector "coefficient"

-- | @Selector@ for @constant@
constantSelector :: Selector
constantSelector = mkSelector "constant"

