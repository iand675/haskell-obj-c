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
  , terawattsSelector
  , gigawattsSelector
  , megawattsSelector
  , kilowattsSelector
  , wattsSelector
  , milliwattsSelector
  , microwattsSelector
  , nanowattsSelector
  , picowattsSelector
  , femtowattsSelector
  , horsepowerSelector


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

-- | @+ terawatts@
terawatts :: IO (Id NSUnitPower)
terawatts  =
  do
    cls' <- getRequiredClass "NSUnitPower"
    sendClassMsg cls' (mkSelector "terawatts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ gigawatts@
gigawatts :: IO (Id NSUnitPower)
gigawatts  =
  do
    cls' <- getRequiredClass "NSUnitPower"
    sendClassMsg cls' (mkSelector "gigawatts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ megawatts@
megawatts :: IO (Id NSUnitPower)
megawatts  =
  do
    cls' <- getRequiredClass "NSUnitPower"
    sendClassMsg cls' (mkSelector "megawatts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ kilowatts@
kilowatts :: IO (Id NSUnitPower)
kilowatts  =
  do
    cls' <- getRequiredClass "NSUnitPower"
    sendClassMsg cls' (mkSelector "kilowatts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ watts@
watts :: IO (Id NSUnitPower)
watts  =
  do
    cls' <- getRequiredClass "NSUnitPower"
    sendClassMsg cls' (mkSelector "watts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ milliwatts@
milliwatts :: IO (Id NSUnitPower)
milliwatts  =
  do
    cls' <- getRequiredClass "NSUnitPower"
    sendClassMsg cls' (mkSelector "milliwatts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ microwatts@
microwatts :: IO (Id NSUnitPower)
microwatts  =
  do
    cls' <- getRequiredClass "NSUnitPower"
    sendClassMsg cls' (mkSelector "microwatts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ nanowatts@
nanowatts :: IO (Id NSUnitPower)
nanowatts  =
  do
    cls' <- getRequiredClass "NSUnitPower"
    sendClassMsg cls' (mkSelector "nanowatts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ picowatts@
picowatts :: IO (Id NSUnitPower)
picowatts  =
  do
    cls' <- getRequiredClass "NSUnitPower"
    sendClassMsg cls' (mkSelector "picowatts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ femtowatts@
femtowatts :: IO (Id NSUnitPower)
femtowatts  =
  do
    cls' <- getRequiredClass "NSUnitPower"
    sendClassMsg cls' (mkSelector "femtowatts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ horsepower@
horsepower :: IO (Id NSUnitPower)
horsepower  =
  do
    cls' <- getRequiredClass "NSUnitPower"
    sendClassMsg cls' (mkSelector "horsepower") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @terawatts@
terawattsSelector :: Selector
terawattsSelector = mkSelector "terawatts"

-- | @Selector@ for @gigawatts@
gigawattsSelector :: Selector
gigawattsSelector = mkSelector "gigawatts"

-- | @Selector@ for @megawatts@
megawattsSelector :: Selector
megawattsSelector = mkSelector "megawatts"

-- | @Selector@ for @kilowatts@
kilowattsSelector :: Selector
kilowattsSelector = mkSelector "kilowatts"

-- | @Selector@ for @watts@
wattsSelector :: Selector
wattsSelector = mkSelector "watts"

-- | @Selector@ for @milliwatts@
milliwattsSelector :: Selector
milliwattsSelector = mkSelector "milliwatts"

-- | @Selector@ for @microwatts@
microwattsSelector :: Selector
microwattsSelector = mkSelector "microwatts"

-- | @Selector@ for @nanowatts@
nanowattsSelector :: Selector
nanowattsSelector = mkSelector "nanowatts"

-- | @Selector@ for @picowatts@
picowattsSelector :: Selector
picowattsSelector = mkSelector "picowatts"

-- | @Selector@ for @femtowatts@
femtowattsSelector :: Selector
femtowattsSelector = mkSelector "femtowatts"

-- | @Selector@ for @horsepower@
horsepowerSelector :: Selector
horsepowerSelector = mkSelector "horsepower"

