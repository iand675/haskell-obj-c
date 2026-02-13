{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A symbol effect that applies the Disappear animation to symbol images.
--
-- The Disappear animation makes the symbol visible either as a whole, or one motion group at a time.
--
-- Generated bindings for @NSSymbolDisappearEffect@.
module ObjC.Symbols.NSSymbolDisappearEffect
  ( NSSymbolDisappearEffect
  , IsNSSymbolDisappearEffect(..)
  , effect
  , disappearUpEffect
  , disappearDownEffect
  , effectWithByLayer
  , effectWithWholeSymbol
  , disappearDownEffectSelector
  , disappearUpEffectSelector
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

-- | The default disappear effect, determined by the system.
--
-- ObjC selector: @+ effect@
effect :: IO (Id NSSymbolDisappearEffect)
effect  =
  do
    cls' <- getRequiredClass "NSSymbolDisappearEffect"
    sendClassMessage cls' effectSelector

-- | Convenience initializer for a disappear effect that disappears scaling up.
--
-- ObjC selector: @+ disappearUpEffect@
disappearUpEffect :: IO (Id NSSymbolDisappearEffect)
disappearUpEffect  =
  do
    cls' <- getRequiredClass "NSSymbolDisappearEffect"
    sendClassMessage cls' disappearUpEffectSelector

-- | Convenience initializer for a disappear effect that disappears scaling down.
--
-- ObjC selector: @+ disappearDownEffect@
disappearDownEffect :: IO (Id NSSymbolDisappearEffect)
disappearDownEffect  =
  do
    cls' <- getRequiredClass "NSSymbolDisappearEffect"
    sendClassMessage cls' disappearDownEffectSelector

-- | Returns a copy of the effect that animates incrementally, by layer.
--
-- ObjC selector: @- effectWithByLayer@
effectWithByLayer :: IsNSSymbolDisappearEffect nsSymbolDisappearEffect => nsSymbolDisappearEffect -> IO (Id NSSymbolDisappearEffect)
effectWithByLayer nsSymbolDisappearEffect =
  sendMessage nsSymbolDisappearEffect effectWithByLayerSelector

-- | Returns a copy of the effect that animates all layers of the symbol simultaneously.
--
-- ObjC selector: @- effectWithWholeSymbol@
effectWithWholeSymbol :: IsNSSymbolDisappearEffect nsSymbolDisappearEffect => nsSymbolDisappearEffect -> IO (Id NSSymbolDisappearEffect)
effectWithWholeSymbol nsSymbolDisappearEffect =
  sendMessage nsSymbolDisappearEffect effectWithWholeSymbolSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @effect@
effectSelector :: Selector '[] (Id NSSymbolDisappearEffect)
effectSelector = mkSelector "effect"

-- | @Selector@ for @disappearUpEffect@
disappearUpEffectSelector :: Selector '[] (Id NSSymbolDisappearEffect)
disappearUpEffectSelector = mkSelector "disappearUpEffect"

-- | @Selector@ for @disappearDownEffect@
disappearDownEffectSelector :: Selector '[] (Id NSSymbolDisappearEffect)
disappearDownEffectSelector = mkSelector "disappearDownEffect"

-- | @Selector@ for @effectWithByLayer@
effectWithByLayerSelector :: Selector '[] (Id NSSymbolDisappearEffect)
effectWithByLayerSelector = mkSelector "effectWithByLayer"

-- | @Selector@ for @effectWithWholeSymbol@
effectWithWholeSymbolSelector :: Selector '[] (Id NSSymbolDisappearEffect)
effectWithWholeSymbolSelector = mkSelector "effectWithWholeSymbol"

