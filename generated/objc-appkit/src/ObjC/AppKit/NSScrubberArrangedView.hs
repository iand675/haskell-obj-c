{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSScrubberArrangedView@.
module ObjC.AppKit.NSScrubberArrangedView
  ( NSScrubberArrangedView
  , IsNSScrubberArrangedView(..)
  , applyLayoutAttributes
  , selected
  , setSelected
  , highlighted
  , setHighlighted
  , applyLayoutAttributesSelector
  , selectedSelector
  , setSelectedSelector
  , highlightedSelector
  , setHighlightedSelector


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

-- | @- applyLayoutAttributes:@
applyLayoutAttributes :: (IsNSScrubberArrangedView nsScrubberArrangedView, IsNSScrubberLayoutAttributes layoutAttributes) => nsScrubberArrangedView -> layoutAttributes -> IO ()
applyLayoutAttributes nsScrubberArrangedView  layoutAttributes =
withObjCPtr layoutAttributes $ \raw_layoutAttributes ->
    sendMsg nsScrubberArrangedView (mkSelector "applyLayoutAttributes:") retVoid [argPtr (castPtr raw_layoutAttributes :: Ptr ())]

-- | @- selected@
selected :: IsNSScrubberArrangedView nsScrubberArrangedView => nsScrubberArrangedView -> IO Bool
selected nsScrubberArrangedView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScrubberArrangedView (mkSelector "selected") retCULong []

-- | @- setSelected:@
setSelected :: IsNSScrubberArrangedView nsScrubberArrangedView => nsScrubberArrangedView -> Bool -> IO ()
setSelected nsScrubberArrangedView  value =
  sendMsg nsScrubberArrangedView (mkSelector "setSelected:") retVoid [argCULong (if value then 1 else 0)]

-- | @- highlighted@
highlighted :: IsNSScrubberArrangedView nsScrubberArrangedView => nsScrubberArrangedView -> IO Bool
highlighted nsScrubberArrangedView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScrubberArrangedView (mkSelector "highlighted") retCULong []

-- | @- setHighlighted:@
setHighlighted :: IsNSScrubberArrangedView nsScrubberArrangedView => nsScrubberArrangedView -> Bool -> IO ()
setHighlighted nsScrubberArrangedView  value =
  sendMsg nsScrubberArrangedView (mkSelector "setHighlighted:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @applyLayoutAttributes:@
applyLayoutAttributesSelector :: Selector
applyLayoutAttributesSelector = mkSelector "applyLayoutAttributes:"

-- | @Selector@ for @selected@
selectedSelector :: Selector
selectedSelector = mkSelector "selected"

-- | @Selector@ for @setSelected:@
setSelectedSelector :: Selector
setSelectedSelector = mkSelector "setSelected:"

-- | @Selector@ for @highlighted@
highlightedSelector :: Selector
highlightedSelector = mkSelector "highlighted"

-- | @Selector@ for @setHighlighted:@
setHighlightedSelector :: Selector
setHighlightedSelector = mkSelector "setHighlighted:"

