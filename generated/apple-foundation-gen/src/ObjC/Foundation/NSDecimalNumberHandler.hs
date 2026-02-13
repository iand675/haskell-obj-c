{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *********	A class for defining common behaviors		******
--
-- Generated bindings for @NSDecimalNumberHandler@.
module ObjC.Foundation.NSDecimalNumberHandler
  ( NSDecimalNumberHandler
  , IsNSDecimalNumberHandler(..)
  , initWithRoundingMode_scale_raiseOnExactness_raiseOnOverflow_raiseOnUnderflow_raiseOnDivideByZero
  , decimalNumberHandlerWithRoundingMode_scale_raiseOnExactness_raiseOnOverflow_raiseOnUnderflow_raiseOnDivideByZero
  , defaultDecimalNumberHandler
  , decimalNumberHandlerWithRoundingMode_scale_raiseOnExactness_raiseOnOverflow_raiseOnUnderflow_raiseOnDivideByZeroSelector
  , defaultDecimalNumberHandlerSelector
  , initWithRoundingMode_scale_raiseOnExactness_raiseOnOverflow_raiseOnUnderflow_raiseOnDivideByZeroSelector

  -- * Enum types
  , NSRoundingMode(NSRoundingMode)
  , pattern NSRoundPlain
  , pattern NSRoundDown
  , pattern NSRoundUp
  , pattern NSRoundBankers

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- initWithRoundingMode:scale:raiseOnExactness:raiseOnOverflow:raiseOnUnderflow:raiseOnDivideByZero:@
initWithRoundingMode_scale_raiseOnExactness_raiseOnOverflow_raiseOnUnderflow_raiseOnDivideByZero :: IsNSDecimalNumberHandler nsDecimalNumberHandler => nsDecimalNumberHandler -> NSRoundingMode -> CShort -> Bool -> Bool -> Bool -> Bool -> IO (Id NSDecimalNumberHandler)
initWithRoundingMode_scale_raiseOnExactness_raiseOnOverflow_raiseOnUnderflow_raiseOnDivideByZero nsDecimalNumberHandler roundingMode scale exact overflow underflow divideByZero =
  sendOwnedMessage nsDecimalNumberHandler initWithRoundingMode_scale_raiseOnExactness_raiseOnOverflow_raiseOnUnderflow_raiseOnDivideByZeroSelector roundingMode scale exact overflow underflow divideByZero

-- | @+ decimalNumberHandlerWithRoundingMode:scale:raiseOnExactness:raiseOnOverflow:raiseOnUnderflow:raiseOnDivideByZero:@
decimalNumberHandlerWithRoundingMode_scale_raiseOnExactness_raiseOnOverflow_raiseOnUnderflow_raiseOnDivideByZero :: NSRoundingMode -> CShort -> Bool -> Bool -> Bool -> Bool -> IO (Id NSDecimalNumberHandler)
decimalNumberHandlerWithRoundingMode_scale_raiseOnExactness_raiseOnOverflow_raiseOnUnderflow_raiseOnDivideByZero roundingMode scale exact overflow underflow divideByZero =
  do
    cls' <- getRequiredClass "NSDecimalNumberHandler"
    sendClassMessage cls' decimalNumberHandlerWithRoundingMode_scale_raiseOnExactness_raiseOnOverflow_raiseOnUnderflow_raiseOnDivideByZeroSelector roundingMode scale exact overflow underflow divideByZero

-- | @+ defaultDecimalNumberHandler@
defaultDecimalNumberHandler :: IO (Id NSDecimalNumberHandler)
defaultDecimalNumberHandler  =
  do
    cls' <- getRequiredClass "NSDecimalNumberHandler"
    sendClassMessage cls' defaultDecimalNumberHandlerSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRoundingMode:scale:raiseOnExactness:raiseOnOverflow:raiseOnUnderflow:raiseOnDivideByZero:@
initWithRoundingMode_scale_raiseOnExactness_raiseOnOverflow_raiseOnUnderflow_raiseOnDivideByZeroSelector :: Selector '[NSRoundingMode, CShort, Bool, Bool, Bool, Bool] (Id NSDecimalNumberHandler)
initWithRoundingMode_scale_raiseOnExactness_raiseOnOverflow_raiseOnUnderflow_raiseOnDivideByZeroSelector = mkSelector "initWithRoundingMode:scale:raiseOnExactness:raiseOnOverflow:raiseOnUnderflow:raiseOnDivideByZero:"

-- | @Selector@ for @decimalNumberHandlerWithRoundingMode:scale:raiseOnExactness:raiseOnOverflow:raiseOnUnderflow:raiseOnDivideByZero:@
decimalNumberHandlerWithRoundingMode_scale_raiseOnExactness_raiseOnOverflow_raiseOnUnderflow_raiseOnDivideByZeroSelector :: Selector '[NSRoundingMode, CShort, Bool, Bool, Bool, Bool] (Id NSDecimalNumberHandler)
decimalNumberHandlerWithRoundingMode_scale_raiseOnExactness_raiseOnOverflow_raiseOnUnderflow_raiseOnDivideByZeroSelector = mkSelector "decimalNumberHandlerWithRoundingMode:scale:raiseOnExactness:raiseOnOverflow:raiseOnUnderflow:raiseOnDivideByZero:"

-- | @Selector@ for @defaultDecimalNumberHandler@
defaultDecimalNumberHandlerSelector :: Selector '[] (Id NSDecimalNumberHandler)
defaultDecimalNumberHandlerSelector = mkSelector "defaultDecimalNumberHandler"

