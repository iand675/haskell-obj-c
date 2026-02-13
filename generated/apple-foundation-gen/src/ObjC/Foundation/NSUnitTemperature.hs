{-# LANGUAGE DataKinds #-}
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
  , celsiusSelector
  , fahrenheitSelector
  , kelvinSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ kelvin@
kelvin :: IO (Id NSUnitTemperature)
kelvin  =
  do
    cls' <- getRequiredClass "NSUnitTemperature"
    sendClassMessage cls' kelvinSelector

-- | @+ celsius@
celsius :: IO (Id NSUnitTemperature)
celsius  =
  do
    cls' <- getRequiredClass "NSUnitTemperature"
    sendClassMessage cls' celsiusSelector

-- | @+ fahrenheit@
fahrenheit :: IO (Id NSUnitTemperature)
fahrenheit  =
  do
    cls' <- getRequiredClass "NSUnitTemperature"
    sendClassMessage cls' fahrenheitSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @kelvin@
kelvinSelector :: Selector '[] (Id NSUnitTemperature)
kelvinSelector = mkSelector "kelvin"

-- | @Selector@ for @celsius@
celsiusSelector :: Selector '[] (Id NSUnitTemperature)
celsiusSelector = mkSelector "celsius"

-- | @Selector@ for @fahrenheit@
fahrenheitSelector :: Selector '[] (Id NSUnitTemperature)
fahrenheitSelector = mkSelector "fahrenheit"

