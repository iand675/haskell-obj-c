{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A symbol effect that animates the replacement of one symbol image with another.
--
-- Generated bindings for @NSSymbolReplaceContentTransition@.
module ObjC.AppKit.NSSymbolReplaceContentTransition
  ( NSSymbolReplaceContentTransition
  , IsNSSymbolReplaceContentTransition(..)
  , transition
  , replaceDownUpTransition
  , replaceUpUpTransition
  , replaceOffUpTransition
  , transitionWithByLayer
  , transitionWithWholeSymbol
  , magicTransitionWithFallback
  , transitionSelector
  , replaceDownUpTransitionSelector
  , replaceUpUpTransitionSelector
  , replaceOffUpTransitionSelector
  , transitionWithByLayerSelector
  , transitionWithWholeSymbolSelector
  , magicTransitionWithFallbackSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The default replace transition, determined by the system.
--
-- ObjC selector: @+ transition@
transition :: IO (Id NSSymbolReplaceContentTransition)
transition  =
  do
    cls' <- getRequiredClass "NSSymbolReplaceContentTransition"
    sendClassMsg cls' (mkSelector "transition") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Convenience initializer for a replace content transition where the initial symbol scales down as it is removed, and the new symbol scales up as it is added.
--
-- ObjC selector: @+ replaceDownUpTransition@
replaceDownUpTransition :: IO (Id NSSymbolReplaceContentTransition)
replaceDownUpTransition  =
  do
    cls' <- getRequiredClass "NSSymbolReplaceContentTransition"
    sendClassMsg cls' (mkSelector "replaceDownUpTransition") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Convenience initializer for a replace content transition where the initial symbol scales up as it is removed, and the new symbol scales up as it is added.
--
-- ObjC selector: @+ replaceUpUpTransition@
replaceUpUpTransition :: IO (Id NSSymbolReplaceContentTransition)
replaceUpUpTransition  =
  do
    cls' <- getRequiredClass "NSSymbolReplaceContentTransition"
    sendClassMsg cls' (mkSelector "replaceUpUpTransition") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Convenience initializer for a replace content transition where the initial symbol is removed with no animation, and the new symbol scales up as it is added.
--
-- ObjC selector: @+ replaceOffUpTransition@
replaceOffUpTransition :: IO (Id NSSymbolReplaceContentTransition)
replaceOffUpTransition  =
  do
    cls' <- getRequiredClass "NSSymbolReplaceContentTransition"
    sendClassMsg cls' (mkSelector "replaceOffUpTransition") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a copy of the content transition that animates incrementally, by layer.
--
-- ObjC selector: @- transitionWithByLayer@
transitionWithByLayer :: IsNSSymbolReplaceContentTransition nsSymbolReplaceContentTransition => nsSymbolReplaceContentTransition -> IO (Id NSSymbolReplaceContentTransition)
transitionWithByLayer nsSymbolReplaceContentTransition  =
  sendMsg nsSymbolReplaceContentTransition (mkSelector "transitionWithByLayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a copy of the content transition that animates all layers of the symbol simultaneously.
--
-- ObjC selector: @- transitionWithWholeSymbol@
transitionWithWholeSymbol :: IsNSSymbolReplaceContentTransition nsSymbolReplaceContentTransition => nsSymbolReplaceContentTransition -> IO (Id NSSymbolReplaceContentTransition)
transitionWithWholeSymbol nsSymbolReplaceContentTransition  =
  sendMsg nsSymbolReplaceContentTransition (mkSelector "transitionWithWholeSymbol") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Convenience initializer for a MagicReplace content transition with a configured Replace fallback.
--
-- ObjC selector: @+ magicTransitionWithFallback:@
magicTransitionWithFallback :: IsNSSymbolReplaceContentTransition fallback => fallback -> IO (Id NSSymbolMagicReplaceContentTransition)
magicTransitionWithFallback fallback =
  do
    cls' <- getRequiredClass "NSSymbolReplaceContentTransition"
    withObjCPtr fallback $ \raw_fallback ->
      sendClassMsg cls' (mkSelector "magicTransitionWithFallback:") (retPtr retVoid) [argPtr (castPtr raw_fallback :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @transition@
transitionSelector :: Selector
transitionSelector = mkSelector "transition"

-- | @Selector@ for @replaceDownUpTransition@
replaceDownUpTransitionSelector :: Selector
replaceDownUpTransitionSelector = mkSelector "replaceDownUpTransition"

-- | @Selector@ for @replaceUpUpTransition@
replaceUpUpTransitionSelector :: Selector
replaceUpUpTransitionSelector = mkSelector "replaceUpUpTransition"

-- | @Selector@ for @replaceOffUpTransition@
replaceOffUpTransitionSelector :: Selector
replaceOffUpTransitionSelector = mkSelector "replaceOffUpTransition"

-- | @Selector@ for @transitionWithByLayer@
transitionWithByLayerSelector :: Selector
transitionWithByLayerSelector = mkSelector "transitionWithByLayer"

-- | @Selector@ for @transitionWithWholeSymbol@
transitionWithWholeSymbolSelector :: Selector
transitionWithWholeSymbolSelector = mkSelector "transitionWithWholeSymbol"

-- | @Selector@ for @magicTransitionWithFallback:@
magicTransitionWithFallbackSelector :: Selector
magicTransitionWithFallbackSelector = mkSelector "magicTransitionWithFallback:"

