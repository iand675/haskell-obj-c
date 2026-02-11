{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUnitElectricCurrent@.
module ObjC.Foundation.NSUnitElectricCurrent
  ( NSUnitElectricCurrent
  , IsNSUnitElectricCurrent(..)
  , megaamperes
  , kiloamperes
  , amperes
  , milliamperes
  , microamperes
  , megaamperesSelector
  , kiloamperesSelector
  , amperesSelector
  , milliamperesSelector
  , microamperesSelector


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

-- | @+ megaamperes@
megaamperes :: IO (Id NSUnitElectricCurrent)
megaamperes  =
  do
    cls' <- getRequiredClass "NSUnitElectricCurrent"
    sendClassMsg cls' (mkSelector "megaamperes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ kiloamperes@
kiloamperes :: IO (Id NSUnitElectricCurrent)
kiloamperes  =
  do
    cls' <- getRequiredClass "NSUnitElectricCurrent"
    sendClassMsg cls' (mkSelector "kiloamperes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ amperes@
amperes :: IO (Id NSUnitElectricCurrent)
amperes  =
  do
    cls' <- getRequiredClass "NSUnitElectricCurrent"
    sendClassMsg cls' (mkSelector "amperes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ milliamperes@
milliamperes :: IO (Id NSUnitElectricCurrent)
milliamperes  =
  do
    cls' <- getRequiredClass "NSUnitElectricCurrent"
    sendClassMsg cls' (mkSelector "milliamperes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ microamperes@
microamperes :: IO (Id NSUnitElectricCurrent)
microamperes  =
  do
    cls' <- getRequiredClass "NSUnitElectricCurrent"
    sendClassMsg cls' (mkSelector "microamperes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @megaamperes@
megaamperesSelector :: Selector
megaamperesSelector = mkSelector "megaamperes"

-- | @Selector@ for @kiloamperes@
kiloamperesSelector :: Selector
kiloamperesSelector = mkSelector "kiloamperes"

-- | @Selector@ for @amperes@
amperesSelector :: Selector
amperesSelector = mkSelector "amperes"

-- | @Selector@ for @milliamperes@
milliamperesSelector :: Selector
milliamperesSelector = mkSelector "milliamperes"

-- | @Selector@ for @microamperes@
microamperesSelector :: Selector
microamperesSelector = mkSelector "microamperes"

