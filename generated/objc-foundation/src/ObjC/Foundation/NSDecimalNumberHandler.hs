{-# LANGUAGE PatternSynonyms #-}
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
  , initWithRoundingMode_scale_raiseOnExactness_raiseOnOverflow_raiseOnUnderflow_raiseOnDivideByZeroSelector
  , decimalNumberHandlerWithRoundingMode_scale_raiseOnExactness_raiseOnOverflow_raiseOnUnderflow_raiseOnDivideByZeroSelector
  , defaultDecimalNumberHandlerSelector

  -- * Enum types
  , NSRoundingMode(NSRoundingMode)
  , pattern NSRoundPlain
  , pattern NSRoundDown
  , pattern NSRoundUp
  , pattern NSRoundBankers

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
import ObjC.Foundation.Internal.Enums

-- | @- initWithRoundingMode:scale:raiseOnExactness:raiseOnOverflow:raiseOnUnderflow:raiseOnDivideByZero:@
initWithRoundingMode_scale_raiseOnExactness_raiseOnOverflow_raiseOnUnderflow_raiseOnDivideByZero :: IsNSDecimalNumberHandler nsDecimalNumberHandler => nsDecimalNumberHandler -> NSRoundingMode -> CShort -> Bool -> Bool -> Bool -> Bool -> IO (Id NSDecimalNumberHandler)
initWithRoundingMode_scale_raiseOnExactness_raiseOnOverflow_raiseOnUnderflow_raiseOnDivideByZero nsDecimalNumberHandler  roundingMode scale exact overflow underflow divideByZero =
  sendMsg nsDecimalNumberHandler (mkSelector "initWithRoundingMode:scale:raiseOnExactness:raiseOnOverflow:raiseOnUnderflow:raiseOnDivideByZero:") (retPtr retVoid) [argCULong (coerce roundingMode), argCInt (fromIntegral scale), argCULong (if exact then 1 else 0), argCULong (if overflow then 1 else 0), argCULong (if underflow then 1 else 0), argCULong (if divideByZero then 1 else 0)] >>= ownedObject . castPtr

-- | @+ decimalNumberHandlerWithRoundingMode:scale:raiseOnExactness:raiseOnOverflow:raiseOnUnderflow:raiseOnDivideByZero:@
decimalNumberHandlerWithRoundingMode_scale_raiseOnExactness_raiseOnOverflow_raiseOnUnderflow_raiseOnDivideByZero :: NSRoundingMode -> CShort -> Bool -> Bool -> Bool -> Bool -> IO (Id NSDecimalNumberHandler)
decimalNumberHandlerWithRoundingMode_scale_raiseOnExactness_raiseOnOverflow_raiseOnUnderflow_raiseOnDivideByZero roundingMode scale exact overflow underflow divideByZero =
  do
    cls' <- getRequiredClass "NSDecimalNumberHandler"
    sendClassMsg cls' (mkSelector "decimalNumberHandlerWithRoundingMode:scale:raiseOnExactness:raiseOnOverflow:raiseOnUnderflow:raiseOnDivideByZero:") (retPtr retVoid) [argCULong (coerce roundingMode), argCInt (fromIntegral scale), argCULong (if exact then 1 else 0), argCULong (if overflow then 1 else 0), argCULong (if underflow then 1 else 0), argCULong (if divideByZero then 1 else 0)] >>= retainedObject . castPtr

-- | @+ defaultDecimalNumberHandler@
defaultDecimalNumberHandler :: IO (Id NSDecimalNumberHandler)
defaultDecimalNumberHandler  =
  do
    cls' <- getRequiredClass "NSDecimalNumberHandler"
    sendClassMsg cls' (mkSelector "defaultDecimalNumberHandler") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRoundingMode:scale:raiseOnExactness:raiseOnOverflow:raiseOnUnderflow:raiseOnDivideByZero:@
initWithRoundingMode_scale_raiseOnExactness_raiseOnOverflow_raiseOnUnderflow_raiseOnDivideByZeroSelector :: Selector
initWithRoundingMode_scale_raiseOnExactness_raiseOnOverflow_raiseOnUnderflow_raiseOnDivideByZeroSelector = mkSelector "initWithRoundingMode:scale:raiseOnExactness:raiseOnOverflow:raiseOnUnderflow:raiseOnDivideByZero:"

-- | @Selector@ for @decimalNumberHandlerWithRoundingMode:scale:raiseOnExactness:raiseOnOverflow:raiseOnUnderflow:raiseOnDivideByZero:@
decimalNumberHandlerWithRoundingMode_scale_raiseOnExactness_raiseOnOverflow_raiseOnUnderflow_raiseOnDivideByZeroSelector :: Selector
decimalNumberHandlerWithRoundingMode_scale_raiseOnExactness_raiseOnOverflow_raiseOnUnderflow_raiseOnDivideByZeroSelector = mkSelector "decimalNumberHandlerWithRoundingMode:scale:raiseOnExactness:raiseOnOverflow:raiseOnUnderflow:raiseOnDivideByZero:"

-- | @Selector@ for @defaultDecimalNumberHandler@
defaultDecimalNumberHandlerSelector :: Selector
defaultDecimalNumberHandlerSelector = mkSelector "defaultDecimalNumberHandler"

