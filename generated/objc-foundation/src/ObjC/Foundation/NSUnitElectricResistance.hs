{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUnitElectricResistance@.
module ObjC.Foundation.NSUnitElectricResistance
  ( NSUnitElectricResistance
  , IsNSUnitElectricResistance(..)
  , megaohms
  , kiloohms
  , ohms
  , milliohms
  , microohms
  , megaohmsSelector
  , kiloohmsSelector
  , ohmsSelector
  , milliohmsSelector
  , microohmsSelector


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

-- | @+ megaohms@
megaohms :: IO (Id NSUnitElectricResistance)
megaohms  =
  do
    cls' <- getRequiredClass "NSUnitElectricResistance"
    sendClassMsg cls' (mkSelector "megaohms") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ kiloohms@
kiloohms :: IO (Id NSUnitElectricResistance)
kiloohms  =
  do
    cls' <- getRequiredClass "NSUnitElectricResistance"
    sendClassMsg cls' (mkSelector "kiloohms") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ ohms@
ohms :: IO (Id NSUnitElectricResistance)
ohms  =
  do
    cls' <- getRequiredClass "NSUnitElectricResistance"
    sendClassMsg cls' (mkSelector "ohms") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ milliohms@
milliohms :: IO (Id NSUnitElectricResistance)
milliohms  =
  do
    cls' <- getRequiredClass "NSUnitElectricResistance"
    sendClassMsg cls' (mkSelector "milliohms") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ microohms@
microohms :: IO (Id NSUnitElectricResistance)
microohms  =
  do
    cls' <- getRequiredClass "NSUnitElectricResistance"
    sendClassMsg cls' (mkSelector "microohms") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @megaohms@
megaohmsSelector :: Selector
megaohmsSelector = mkSelector "megaohms"

-- | @Selector@ for @kiloohms@
kiloohmsSelector :: Selector
kiloohmsSelector = mkSelector "kiloohms"

-- | @Selector@ for @ohms@
ohmsSelector :: Selector
ohmsSelector = mkSelector "ohms"

-- | @Selector@ for @milliohms@
milliohmsSelector :: Selector
milliohmsSelector = mkSelector "milliohms"

-- | @Selector@ for @microohms@
microohmsSelector :: Selector
microohmsSelector = mkSelector "microohms"

