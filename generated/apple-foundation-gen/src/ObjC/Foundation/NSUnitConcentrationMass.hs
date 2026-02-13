{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUnitConcentrationMass@.
module ObjC.Foundation.NSUnitConcentrationMass
  ( NSUnitConcentrationMass
  , IsNSUnitConcentrationMass(..)
  , millimolesPerLiterWithGramsPerMole
  , gramsPerLiter
  , milligramsPerDeciliter
  , gramsPerLiterSelector
  , milligramsPerDeciliterSelector
  , millimolesPerLiterWithGramsPerMoleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ millimolesPerLiterWithGramsPerMole:@
millimolesPerLiterWithGramsPerMole :: CDouble -> IO (Id NSUnitConcentrationMass)
millimolesPerLiterWithGramsPerMole gramsPerMole =
  do
    cls' <- getRequiredClass "NSUnitConcentrationMass"
    sendClassMessage cls' millimolesPerLiterWithGramsPerMoleSelector gramsPerMole

-- | @+ gramsPerLiter@
gramsPerLiter :: IO (Id NSUnitConcentrationMass)
gramsPerLiter  =
  do
    cls' <- getRequiredClass "NSUnitConcentrationMass"
    sendClassMessage cls' gramsPerLiterSelector

-- | @+ milligramsPerDeciliter@
milligramsPerDeciliter :: IO (Id NSUnitConcentrationMass)
milligramsPerDeciliter  =
  do
    cls' <- getRequiredClass "NSUnitConcentrationMass"
    sendClassMessage cls' milligramsPerDeciliterSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @millimolesPerLiterWithGramsPerMole:@
millimolesPerLiterWithGramsPerMoleSelector :: Selector '[CDouble] (Id NSUnitConcentrationMass)
millimolesPerLiterWithGramsPerMoleSelector = mkSelector "millimolesPerLiterWithGramsPerMole:"

-- | @Selector@ for @gramsPerLiter@
gramsPerLiterSelector :: Selector '[] (Id NSUnitConcentrationMass)
gramsPerLiterSelector = mkSelector "gramsPerLiter"

-- | @Selector@ for @milligramsPerDeciliter@
milligramsPerDeciliterSelector :: Selector '[] (Id NSUnitConcentrationMass)
milligramsPerDeciliterSelector = mkSelector "milligramsPerDeciliter"

