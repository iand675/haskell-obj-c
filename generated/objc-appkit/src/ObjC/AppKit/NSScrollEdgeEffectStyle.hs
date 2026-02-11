{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Styles for a scroll view’s edge effect.
--
-- Generated bindings for @NSScrollEdgeEffectStyle@.
module ObjC.AppKit.NSScrollEdgeEffectStyle
  ( NSScrollEdgeEffectStyle
  , IsNSScrollEdgeEffectStyle(..)
  , init_
  , new
  , automaticStyle
  , softStyle
  , hardStyle
  , initSelector
  , newSelector
  , automaticStyleSelector
  , softStyleSelector
  , hardStyleSelector


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

-- | @- init@
init_ :: IsNSScrollEdgeEffectStyle nsScrollEdgeEffectStyle => nsScrollEdgeEffectStyle -> IO (Id NSScrollEdgeEffectStyle)
init_ nsScrollEdgeEffectStyle  =
  sendMsg nsScrollEdgeEffectStyle (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NSScrollEdgeEffectStyle)
new  =
  do
    cls' <- getRequiredClass "NSScrollEdgeEffectStyle"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The automatic scroll edge effect style.
--
-- ObjC selector: @+ automaticStyle@
automaticStyle :: IO (Id NSScrollEdgeEffectStyle)
automaticStyle  =
  do
    cls' <- getRequiredClass "NSScrollEdgeEffectStyle"
    sendClassMsg cls' (mkSelector "automaticStyle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A scroll edge effect with a soft edge.
--
-- ObjC selector: @+ softStyle@
softStyle :: IO (Id NSScrollEdgeEffectStyle)
softStyle  =
  do
    cls' <- getRequiredClass "NSScrollEdgeEffectStyle"
    sendClassMsg cls' (mkSelector "softStyle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A scroll edge effect with a hard cutoff.
--
-- ObjC selector: @+ hardStyle@
hardStyle :: IO (Id NSScrollEdgeEffectStyle)
hardStyle  =
  do
    cls' <- getRequiredClass "NSScrollEdgeEffectStyle"
    sendClassMsg cls' (mkSelector "hardStyle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @automaticStyle@
automaticStyleSelector :: Selector
automaticStyleSelector = mkSelector "automaticStyle"

-- | @Selector@ for @softStyle@
softStyleSelector :: Selector
softStyleSelector = mkSelector "softStyle"

-- | @Selector@ for @hardStyle@
hardStyleSelector :: Selector
hardStyleSelector = mkSelector "hardStyle"

