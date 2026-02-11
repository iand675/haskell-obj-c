{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUnitTemperature@.
module ObjC.Foundation.NSUnitTemperature
  ( NSUnitTemperature
  , IsNSUnitTemperature(..)
  , kelvin
  , celsius
  , fahrenheit
  , kelvinSelector
  , celsiusSelector
  , fahrenheitSelector


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

-- | @+ kelvin@
kelvin :: IO (Id NSUnitTemperature)
kelvin  =
  do
    cls' <- getRequiredClass "NSUnitTemperature"
    sendClassMsg cls' (mkSelector "kelvin") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ celsius@
celsius :: IO (Id NSUnitTemperature)
celsius  =
  do
    cls' <- getRequiredClass "NSUnitTemperature"
    sendClassMsg cls' (mkSelector "celsius") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ fahrenheit@
fahrenheit :: IO (Id NSUnitTemperature)
fahrenheit  =
  do
    cls' <- getRequiredClass "NSUnitTemperature"
    sendClassMsg cls' (mkSelector "fahrenheit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @kelvin@
kelvinSelector :: Selector
kelvinSelector = mkSelector "kelvin"

-- | @Selector@ for @celsius@
celsiusSelector :: Selector
celsiusSelector = mkSelector "celsius"

-- | @Selector@ for @fahrenheit@
fahrenheitSelector :: Selector
fahrenheitSelector = mkSelector "fahrenheit"

