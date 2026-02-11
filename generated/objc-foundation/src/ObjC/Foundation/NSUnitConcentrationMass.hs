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
  , millimolesPerLiterWithGramsPerMoleSelector
  , gramsPerLiterSelector
  , milligramsPerDeciliterSelector


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

-- | @+ millimolesPerLiterWithGramsPerMole:@
millimolesPerLiterWithGramsPerMole :: CDouble -> IO (Id NSUnitConcentrationMass)
millimolesPerLiterWithGramsPerMole gramsPerMole =
  do
    cls' <- getRequiredClass "NSUnitConcentrationMass"
    sendClassMsg cls' (mkSelector "millimolesPerLiterWithGramsPerMole:") (retPtr retVoid) [argCDouble (fromIntegral gramsPerMole)] >>= retainedObject . castPtr

-- | @+ gramsPerLiter@
gramsPerLiter :: IO (Id NSUnitConcentrationMass)
gramsPerLiter  =
  do
    cls' <- getRequiredClass "NSUnitConcentrationMass"
    sendClassMsg cls' (mkSelector "gramsPerLiter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ milligramsPerDeciliter@
milligramsPerDeciliter :: IO (Id NSUnitConcentrationMass)
milligramsPerDeciliter  =
  do
    cls' <- getRequiredClass "NSUnitConcentrationMass"
    sendClassMsg cls' (mkSelector "milligramsPerDeciliter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @millimolesPerLiterWithGramsPerMole:@
millimolesPerLiterWithGramsPerMoleSelector :: Selector
millimolesPerLiterWithGramsPerMoleSelector = mkSelector "millimolesPerLiterWithGramsPerMole:"

-- | @Selector@ for @gramsPerLiter@
gramsPerLiterSelector :: Selector
gramsPerLiterSelector = mkSelector "gramsPerLiter"

-- | @Selector@ for @milligramsPerDeciliter@
milligramsPerDeciliterSelector :: Selector
milligramsPerDeciliterSelector = mkSelector "milligramsPerDeciliter"

