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
init_ :: IsNSScrubberSelectionStyle nsScrubberSelectionStyle => nsScrubberSelectionStyle -> IO (Id NSScrubberSelectionStyle)
init_ nsScrubberSelectionStyle  =
  sendMsg nsScrubberSelectionStyle (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSScrubberSelectionStyle nsScrubberSelectionStyle, IsNSCoder coder) => nsScrubberSelectionStyle -> coder -> IO (Id NSScrubberSelectionStyle)
initWithCoder nsScrubberSelectionStyle  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsScrubberSelectionStyle (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- makeSelectionView@
makeSelectionView :: IsNSScrubberSelectionStyle nsScrubberSelectionStyle => nsScrubberSelectionStyle -> IO (Id NSScrubberSelectionView)
makeSelectionView nsScrubberSelectionStyle  =
  sendMsg nsScrubberSelectionStyle (mkSelector "makeSelectionView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ outlineOverlayStyle@
outlineOverlayStyle :: IO (Id NSScrubberSelectionStyle)
outlineOverlayStyle  =
  do
    cls' <- getRequiredClass "NSScrubberSelectionStyle"
    sendClassMsg cls' (mkSelector "outlineOverlayStyle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ roundedBackgroundStyle@
roundedBackgroundStyle :: IO (Id NSScrubberSelectionStyle)
roundedBackgroundStyle  =
  do
    cls' <- getRequiredClass "NSScrubberSelectionStyle"
    sendClassMsg cls' (mkSelector "roundedBackgroundStyle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @makeSelectionView@
makeSelectionViewSelector :: Selector
makeSelectionViewSelector = mkSelector "makeSelectionView"

-- | @Selector@ for @outlineOverlayStyle@
outlineOverlayStyleSelector :: Selector
outlineOverlayStyleSelector = mkSelector "outlineOverlayStyle"

-- | @Selector@ for @roundedBackgroundStyle@
roundedBackgroundStyleSelector :: Selector
roundedBackgroundStyleSelector = mkSelector "roundedBackgroundStyle"

