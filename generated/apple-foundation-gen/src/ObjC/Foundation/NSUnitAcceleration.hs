{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUnitAcceleration@.
module ObjC.Foundation.NSUnitAcceleration
  ( NSUnitAcceleration
  , IsNSUnitAcceleration(..)
  , metersPerSecondSquared
  , gravity
  , gravitySelector
  , metersPerSecondSquaredSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ metersPerSecondSquared@
metersPerSecondSquared :: IO (Id NSUnitAcceleration)
metersPerSecondSquared  =
  do
    cls' <- getRequiredClass "NSUnitAcceleration"
    sendClassMessage cls' metersPerSecondSquaredSelector

-- | @+ gravity@
gravity :: IO (Id NSUnitAcceleration)
gravity  =
  do
    cls' <- getRequiredClass "NSUnitAcceleration"
    sendClassMessage cls' gravitySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @metersPerSecondSquared@
metersPerSecondSquaredSelector :: Selector '[] (Id NSUnitAcceleration)
metersPerSecondSquaredSelector = mkSelector "metersPerSecondSquared"

-- | @Selector@ for @gravity@
gravitySelector :: Selector '[] (Id NSUnitAcceleration)
gravitySelector = mkSelector "gravity"

