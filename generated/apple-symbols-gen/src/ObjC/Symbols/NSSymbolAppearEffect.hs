{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A symbol effect that applies the Appear animation to symbol images.
--
-- The Appear animation makes the symbol visible either as a whole, or one motion group at a time.
--
-- Generated bindings for @NSSymbolAppearEffect@.
module ObjC.Symbols.NSSymbolAppearEffect
  ( NSSymbolAppearEffect
  , IsNSSymbolAppearEffect(..)
  , effect
  , appearUpEffect
  , appearDownEffect
  , effectWithByLayer
  , effectWithWholeSymbol
  , appearDownEffectSelector
  , appearUpEffectSelector
  , effectSelector
  , effectWithByLayerSelector
  , effectWithWholeSymbolSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Symbols.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The default appear effect, determined by the system.
--
-- ObjC selector: @+ effect@
effect :: IO (Id NSSymbolAppearEffect)
effect  =
  do
    cls' <- getRequiredClass "NSSymbolAppearEffect"
    sendClassMessage cls' effectSelector

-- | Convenience initializer for an appear effect that appears scaling up.
--
-- ObjC selector: @+ appearUpEffect@
appearUpEffect :: IO (Id NSSymbolAppearEffect)
appearUpEffect  =
  do
    cls' <- getRequiredClass "NSSymbolAppearEffect"
    sendClassMessage cls' appearUpEffectSelector

-- | Convenience initializer for an appear effect that appears scaling down.
--
-- ObjC selector: @+ appearDownEffect@
appearDownEffect :: IO (Id NSSymbolAppearEffect)
appearDownEffect  =
  do
    cls' <- getRequiredClass "NSSymbolAppearEffect"
    sendClassMessage cls' appearDownEffectSelector

-- | Returns a copy of the effect that animates incrementally, by layer.
--
-- ObjC selector: @- effectWithByLayer@
effectWithByLayer :: IsNSSymbolAppearEffect nsSymbolAppearEffect => nsSymbolAppearEffect -> IO (Id NSSymbolAppearEffect)
effectWithByLayer nsSymbolAppearEffect =
  sendMessage nsSymbolAppearEffect effectWithByLayerSelector

-- | Returns a copy of the effect that animates all layers of the symbol simultaneously.
--
-- ObjC selector: @- effectWithWholeSymbol@
effectWithWholeSymbol :: IsNSSymbolAppearEffect nsSymbolAppearEffect => nsSymbolAppearEffect -> IO (Id NSSymbolAppearEffect)
effectWithWholeSymbol nsSymbolAppearEffect =
  sendMessage nsSymbolAppearEffect effectWithWholeSymbolSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @effect@
effectSelector :: Selector '[] (Id NSSymbolAppearEffect)
effectSelector = mkSelector "effect"

-- | @Selector@ for @appearUpEffect@
appearUpEffectSelector :: Selector '[] (Id NSSymbolAppearEffect)
appearUpEffectSelector = mkSelector "appearUpEffect"

-- | @Selector@ for @appearDownEffect@
appearDownEffectSelector :: Selector '[] (Id NSSymbolAppearEffect)
appearDownEffectSelector = mkSelector "appearDownEffect"

-- | @Selector@ for @effectWithByLayer@
effectWithByLayerSelector :: Selector '[] (Id NSSymbolAppearEffect)
effectWithByLayerSelector = mkSelector "effectWithByLayer"

-- | @Selector@ for @effectWithWholeSymbol@
effectWithWholeSymbolSelector :: Selector '[] (Id NSSymbolAppearEffect)
effectWithWholeSymbolSelector = mkSelector "effectWithWholeSymbol"

