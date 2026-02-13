{-# LANGUAGE DataKinds #-}
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
  , automaticStyleSelector
  , hardStyleSelector
  , initSelector
  , newSelector
  , softStyleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSScrollEdgeEffectStyle nsScrollEdgeEffectStyle => nsScrollEdgeEffectStyle -> IO (Id NSScrollEdgeEffectStyle)
init_ nsScrollEdgeEffectStyle =
  sendOwnedMessage nsScrollEdgeEffectStyle initSelector

-- | @+ new@
new :: IO (Id NSScrollEdgeEffectStyle)
new  =
  do
    cls' <- getRequiredClass "NSScrollEdgeEffectStyle"
    sendOwnedClassMessage cls' newSelector

-- | The automatic scroll edge effect style.
--
-- ObjC selector: @+ automaticStyle@
automaticStyle :: IO (Id NSScrollEdgeEffectStyle)
automaticStyle  =
  do
    cls' <- getRequiredClass "NSScrollEdgeEffectStyle"
    sendClassMessage cls' automaticStyleSelector

-- | A scroll edge effect with a soft edge.
--
-- ObjC selector: @+ softStyle@
softStyle :: IO (Id NSScrollEdgeEffectStyle)
softStyle  =
  do
    cls' <- getRequiredClass "NSScrollEdgeEffectStyle"
    sendClassMessage cls' softStyleSelector

-- | A scroll edge effect with a hard cutoff.
--
-- ObjC selector: @+ hardStyle@
hardStyle :: IO (Id NSScrollEdgeEffectStyle)
hardStyle  =
  do
    cls' <- getRequiredClass "NSScrollEdgeEffectStyle"
    sendClassMessage cls' hardStyleSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSScrollEdgeEffectStyle)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSScrollEdgeEffectStyle)
newSelector = mkSelector "new"

-- | @Selector@ for @automaticStyle@
automaticStyleSelector :: Selector '[] (Id NSScrollEdgeEffectStyle)
automaticStyleSelector = mkSelector "automaticStyle"

-- | @Selector@ for @softStyle@
softStyleSelector :: Selector '[] (Id NSScrollEdgeEffectStyle)
softStyleSelector = mkSelector "softStyle"

-- | @Selector@ for @hardStyle@
hardStyleSelector :: Selector '[] (Id NSScrollEdgeEffectStyle)
hardStyleSelector = mkSelector "hardStyle"

