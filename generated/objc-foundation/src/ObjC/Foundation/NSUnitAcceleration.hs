{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUnitAcceleration@.
module ObjC.Foundation.NSUnitAcceleration
  ( NSUnitAcceleration
  , IsNSUnitAcceleration(..)
  , metersPerSecondSquared
  , gravity
  , metersPerSecondSquaredSelector
  , gravitySelector


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

-- | @+ metersPerSecondSquared@
metersPerSecondSquared :: IO (Id NSUnitAcceleration)
metersPerSecondSquared  =
  do
    cls' <- getRequiredClass "NSUnitAcceleration"
    sendClassMsg cls' (mkSelector "metersPerSecondSquared") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ gravity@
gravity :: IO (Id NSUnitAcceleration)
gravity  =
  do
    cls' <- getRequiredClass "NSUnitAcceleration"
    sendClassMsg cls' (mkSelector "gravity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @metersPerSecondSquared@
metersPerSecondSquaredSelector :: Selector
metersPerSecondSquaredSelector = mkSelector "metersPerSecondSquared"

-- | @Selector@ for @gravity@
gravitySelector :: Selector
gravitySelector = mkSelector "gravity"

