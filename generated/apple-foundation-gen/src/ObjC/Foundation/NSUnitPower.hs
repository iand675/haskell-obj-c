{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUnitPower@.
module ObjC.Foundation.NSUnitPower
  ( NSUnitPower
  , IsNSUnitPower(..)
  , terawatts
  , gigawatts
  , megawatts
  , kilowatts
  , watts
  , milliwatts
  , microwatts
  , nanowatts
  , picowatts
  , femtowatts
  , horsepower
  , femtowattsSelector
  , gigawattsSelector
  , horsepowerSelector
  , kilowattsSelector
  , megawattsSelector
  , microwattsSelector
  , milliwattsSelector
  , nanowattsSelector
  , picowattsSelector
  , terawattsSelector
  , wattsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ terawatts@
terawatts :: IO (Id NSUnitPower)
terawatts  =
  do
    cls' <- getRequiredClass "NSUnitPower"
    sendClassMessage cls' terawattsSelector

-- | @+ gigawatts@
gigawatts :: IO (Id NSUnitPower)
gigawatts  =
  do
    cls' <- getRequiredClass "NSUnitPower"
    sendClassMessage cls' gigawattsSelector

-- | @+ megawatts@
megawatts :: IO (Id NSUnitPower)
megawatts  =
  do
    cls' <- getRequiredClass "NSUnitPower"
    sendClassMessage cls' megawattsSelector

-- | @+ kilowatts@
kilowatts :: IO (Id NSUnitPower)
kilowatts  =
  do
    cls' <- getRequiredClass "NSUnitPower"
    sendClassMessage cls' kilowattsSelector

-- | @+ watts@
watts :: IO (Id NSUnitPower)
watts  =
  do
    cls' <- getRequiredClass "NSUnitPower"
    sendClassMessage cls' wattsSelector

-- | @+ milliwatts@
milliwatts :: IO (Id NSUnitPower)
milliwatts  =
  do
    cls' <- getRequiredClass "NSUnitPower"
    sendClassMessage cls' milliwattsSelector

-- | @+ microwatts@
microwatts :: IO (Id NSUnitPower)
microwatts  =
  do
    cls' <- getRequiredClass "NSUnitPower"
    sendClassMessage cls' microwattsSelector

-- | @+ nanowatts@
nanowatts :: IO (Id NSUnitPower)
nanowatts  =
  do
    cls' <- getRequiredClass "NSUnitPower"
    sendClassMessage cls' nanowattsSelector

-- | @+ picowatts@
picowatts :: IO (Id NSUnitPower)
picowatts  =
  do
    cls' <- getRequiredClass "NSUnitPower"
    sendClassMessage cls' picowattsSelector

-- | @+ femtowatts@
femtowatts :: IO (Id NSUnitPower)
femtowatts  =
  do
    cls' <- getRequiredClass "NSUnitPower"
    sendClassMessage cls' femtowattsSelector

-- | @+ horsepower@
horsepower :: IO (Id NSUnitPower)
horsepower  =
  do
    cls' <- getRequiredClass "NSUnitPower"
    sendClassMessage cls' horsepowerSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @terawatts@
terawattsSelector :: Selector '[] (Id NSUnitPower)
terawattsSelector = mkSelector "terawatts"

-- | @Selector@ for @gigawatts@
gigawattsSelector :: Selector '[] (Id NSUnitPower)
gigawattsSelector = mkSelector "gigawatts"

-- | @Selector@ for @megawatts@
megawattsSelector :: Selector '[] (Id NSUnitPower)
megawattsSelector = mkSelector "megawatts"

-- | @Selector@ for @kilowatts@
kilowattsSelector :: Selector '[] (Id NSUnitPower)
kilowattsSelector = mkSelector "kilowatts"

-- | @Selector@ for @watts@
wattsSelector :: Selector '[] (Id NSUnitPower)
wattsSelector = mkSelector "watts"

-- | @Selector@ for @milliwatts@
milliwattsSelector :: Selector '[] (Id NSUnitPower)
milliwattsSelector = mkSelector "milliwatts"

-- | @Selector@ for @microwatts@
microwattsSelector :: Selector '[] (Id NSUnitPower)
microwattsSelector = mkSelector "microwatts"

-- | @Selector@ for @nanowatts@
nanowattsSelector :: Selector '[] (Id NSUnitPower)
nanowattsSelector = mkSelector "nanowatts"

-- | @Selector@ for @picowatts@
picowattsSelector :: Selector '[] (Id NSUnitPower)
picowattsSelector = mkSelector "picowatts"

-- | @Selector@ for @femtowatts@
femtowattsSelector :: Selector '[] (Id NSUnitPower)
femtowattsSelector = mkSelector "femtowatts"

-- | @Selector@ for @horsepower@
horsepowerSelector :: Selector '[] (Id NSUnitPower)
horsepowerSelector = mkSelector "horsepower"

