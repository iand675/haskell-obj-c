{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NSScrubberSelectionStyle
--
-- @NSScrubberSelectionStyle@ is an abstract class that provides decorative accessory views for selected and highlighted items within a NSScrubber control. Class properties provide convenient access to built-in styles. For a completely custom style, subclassers can override @-makeSelectionView@ to create and configure arbitrary @NSScrubberSelectionView@ subclasses.
--
-- Generated bindings for @NSScrubberSelectionStyle@.
module ObjC.AppKit.NSScrubberSelectionStyle
  ( NSScrubberSelectionStyle
  , IsNSScrubberSelectionStyle(..)
  , init_
  , initWithCoder
  , makeSelectionView
  , outlineOverlayStyle
  , roundedBackgroundStyle
  , initSelector
  , initWithCoderSelector
  , makeSelectionViewSelector
  , outlineOverlayStyleSelector
  , roundedBackgroundStyleSelector


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
init_ :: IsNSScrubberSelectionStyle nsScrubberSelectionStyle => nsScrubberSelectionStyle -> IO (Id NSScrubberSelectionStyle)
init_ nsScrubberSelectionStyle =
  sendOwnedMessage nsScrubberSelectionStyle initSelector

-- | @- initWithCoder:@
initWithCoder :: (IsNSScrubberSelectionStyle nsScrubberSelectionStyle, IsNSCoder coder) => nsScrubberSelectionStyle -> coder -> IO (Id NSScrubberSelectionStyle)
initWithCoder nsScrubberSelectionStyle coder =
  sendOwnedMessage nsScrubberSelectionStyle initWithCoderSelector (toNSCoder coder)

-- | @- makeSelectionView@
makeSelectionView :: IsNSScrubberSelectionStyle nsScrubberSelectionStyle => nsScrubberSelectionStyle -> IO (Id NSScrubberSelectionView)
makeSelectionView nsScrubberSelectionStyle =
  sendMessage nsScrubberSelectionStyle makeSelectionViewSelector

-- | @+ outlineOverlayStyle@
outlineOverlayStyle :: IO (Id NSScrubberSelectionStyle)
outlineOverlayStyle  =
  do
    cls' <- getRequiredClass "NSScrubberSelectionStyle"
    sendClassMessage cls' outlineOverlayStyleSelector

-- | @+ roundedBackgroundStyle@
roundedBackgroundStyle :: IO (Id NSScrubberSelectionStyle)
roundedBackgroundStyle  =
  do
    cls' <- getRequiredClass "NSScrubberSelectionStyle"
    sendClassMessage cls' roundedBackgroundStyleSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSScrubberSelectionStyle)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSScrubberSelectionStyle)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @makeSelectionView@
makeSelectionViewSelector :: Selector '[] (Id NSScrubberSelectionView)
makeSelectionViewSelector = mkSelector "makeSelectionView"

-- | @Selector@ for @outlineOverlayStyle@
outlineOverlayStyleSelector :: Selector '[] (Id NSScrubberSelectionStyle)
outlineOverlayStyleSelector = mkSelector "outlineOverlayStyle"

-- | @Selector@ for @roundedBackgroundStyle@
roundedBackgroundStyleSelector :: Selector '[] (Id NSScrubberSelectionStyle)
roundedBackgroundStyleSelector = mkSelector "roundedBackgroundStyle"

