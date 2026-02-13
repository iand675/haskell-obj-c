{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A symbol effect that animates the replacement of one symbol image with another.
--
-- Generated bindings for @NSSymbolReplaceContentTransition@.
module ObjC.Symbols.NSSymbolReplaceContentTransition
  ( NSSymbolReplaceContentTransition
  , IsNSSymbolReplaceContentTransition(..)
  , transition
  , replaceDownUpTransition
  , replaceUpUpTransition
  , replaceOffUpTransition
  , transitionWithByLayer
  , transitionWithWholeSymbol
  , magicTransitionWithFallback
  , magicTransitionWithFallbackSelector
  , replaceDownUpTransitionSelector
  , replaceOffUpTransitionSelector
  , replaceUpUpTransitionSelector
  , transitionSelector
  , transitionWithByLayerSelector
  , transitionWithWholeSymbolSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Symbols.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The default replace transition, determined by the system.
--
-- ObjC selector: @+ transition@
transition :: IO (Id NSSymbolReplaceContentTransition)
transition  =
  do
    cls' <- getRequiredClass "NSSymbolReplaceContentTransition"
    sendClassMessage cls' transitionSelector

-- | Convenience initializer for a replace content transition where the initial symbol scales down as it is removed, and the new symbol scales up as it is added.
--
-- ObjC selector: @+ replaceDownUpTransition@
replaceDownUpTransition :: IO (Id NSSymbolReplaceContentTransition)
replaceDownUpTransition  =
  do
    cls' <- getRequiredClass "NSSymbolReplaceContentTransition"
    sendClassMessage cls' replaceDownUpTransitionSelector

-- | Convenience initializer for a replace content transition where the initial symbol scales up as it is removed, and the new symbol scales up as it is added.
--
-- ObjC selector: @+ replaceUpUpTransition@
replaceUpUpTransition :: IO (Id NSSymbolReplaceContentTransition)
replaceUpUpTransition  =
  do
    cls' <- getRequiredClass "NSSymbolReplaceContentTransition"
    sendClassMessage cls' replaceUpUpTransitionSelector

-- | Convenience initializer for a replace content transition where the initial symbol is removed with no animation, and the new symbol scales up as it is added.
--
-- ObjC selector: @+ replaceOffUpTransition@
replaceOffUpTransition :: IO (Id NSSymbolReplaceContentTransition)
replaceOffUpTransition  =
  do
    cls' <- getRequiredClass "NSSymbolReplaceContentTransition"
    sendClassMessage cls' replaceOffUpTransitionSelector

-- | Returns a copy of the content transition that animates incrementally, by layer.
--
-- ObjC selector: @- transitionWithByLayer@
transitionWithByLayer :: IsNSSymbolReplaceContentTransition nsSymbolReplaceContentTransition => nsSymbolReplaceContentTransition -> IO (Id NSSymbolReplaceContentTransition)
transitionWithByLayer nsSymbolReplaceContentTransition =
  sendMessage nsSymbolReplaceContentTransition transitionWithByLayerSelector

-- | Returns a copy of the content transition that animates all layers of the symbol simultaneously.
--
-- ObjC selector: @- transitionWithWholeSymbol@
transitionWithWholeSymbol :: IsNSSymbolReplaceContentTransition nsSymbolReplaceContentTransition => nsSymbolReplaceContentTransition -> IO (Id NSSymbolReplaceContentTransition)
transitionWithWholeSymbol nsSymbolReplaceContentTransition =
  sendMessage nsSymbolReplaceContentTransition transitionWithWholeSymbolSelector

-- | Convenience initializer for a MagicReplace content transition with a configured Replace fallback.
--
-- ObjC selector: @+ magicTransitionWithFallback:@
magicTransitionWithFallback :: IsNSSymbolReplaceContentTransition fallback => fallback -> IO (Id NSSymbolMagicReplaceContentTransition)
magicTransitionWithFallback fallback =
  do
    cls' <- getRequiredClass "NSSymbolReplaceContentTransition"
    sendClassMessage cls' magicTransitionWithFallbackSelector (toNSSymbolReplaceContentTransition fallback)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @transition@
transitionSelector :: Selector '[] (Id NSSymbolReplaceContentTransition)
transitionSelector = mkSelector "transition"

-- | @Selector@ for @replaceDownUpTransition@
replaceDownUpTransitionSelector :: Selector '[] (Id NSSymbolReplaceContentTransition)
replaceDownUpTransitionSelector = mkSelector "replaceDownUpTransition"

-- | @Selector@ for @replaceUpUpTransition@
replaceUpUpTransitionSelector :: Selector '[] (Id NSSymbolReplaceContentTransition)
replaceUpUpTransitionSelector = mkSelector "replaceUpUpTransition"

-- | @Selector@ for @replaceOffUpTransition@
replaceOffUpTransitionSelector :: Selector '[] (Id NSSymbolReplaceContentTransition)
replaceOffUpTransitionSelector = mkSelector "replaceOffUpTransition"

-- | @Selector@ for @transitionWithByLayer@
transitionWithByLayerSelector :: Selector '[] (Id NSSymbolReplaceContentTransition)
transitionWithByLayerSelector = mkSelector "transitionWithByLayer"

-- | @Selector@ for @transitionWithWholeSymbol@
transitionWithWholeSymbolSelector :: Selector '[] (Id NSSymbolReplaceContentTransition)
transitionWithWholeSymbolSelector = mkSelector "transitionWithWholeSymbol"

-- | @Selector@ for @magicTransitionWithFallback:@
magicTransitionWithFallbackSelector :: Selector '[Id NSSymbolReplaceContentTransition] (Id NSSymbolMagicReplaceContentTransition)
magicTransitionWithFallbackSelector = mkSelector "magicTransitionWithFallback:"

