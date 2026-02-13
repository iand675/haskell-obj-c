{-# LANGUAGE DataKinds #-}
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
  , highlightedSelector
  , selectedSelector
  , setHighlightedSelector
  , setSelectedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- applyLayoutAttributes:@
applyLayoutAttributes :: (IsNSScrubberArrangedView nsScrubberArrangedView, IsNSScrubberLayoutAttributes layoutAttributes) => nsScrubberArrangedView -> layoutAttributes -> IO ()
applyLayoutAttributes nsScrubberArrangedView layoutAttributes =
  sendMessage nsScrubberArrangedView applyLayoutAttributesSelector (toNSScrubberLayoutAttributes layoutAttributes)

-- | @- selected@
selected :: IsNSScrubberArrangedView nsScrubberArrangedView => nsScrubberArrangedView -> IO Bool
selected nsScrubberArrangedView =
  sendMessage nsScrubberArrangedView selectedSelector

-- | @- setSelected:@
setSelected :: IsNSScrubberArrangedView nsScrubberArrangedView => nsScrubberArrangedView -> Bool -> IO ()
setSelected nsScrubberArrangedView value =
  sendMessage nsScrubberArrangedView setSelectedSelector value

-- | @- highlighted@
highlighted :: IsNSScrubberArrangedView nsScrubberArrangedView => nsScrubberArrangedView -> IO Bool
highlighted nsScrubberArrangedView =
  sendMessage nsScrubberArrangedView highlightedSelector

-- | @- setHighlighted:@
setHighlighted :: IsNSScrubberArrangedView nsScrubberArrangedView => nsScrubberArrangedView -> Bool -> IO ()
setHighlighted nsScrubberArrangedView value =
  sendMessage nsScrubberArrangedView setHighlightedSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @applyLayoutAttributes:@
applyLayoutAttributesSelector :: Selector '[Id NSScrubberLayoutAttributes] ()
applyLayoutAttributesSelector = mkSelector "applyLayoutAttributes:"

-- | @Selector@ for @selected@
selectedSelector :: Selector '[] Bool
selectedSelector = mkSelector "selected"

-- | @Selector@ for @setSelected:@
setSelectedSelector :: Selector '[Bool] ()
setSelectedSelector = mkSelector "setSelected:"

-- | @Selector@ for @highlighted@
highlightedSelector :: Selector '[] Bool
highlightedSelector = mkSelector "highlighted"

-- | @Selector@ for @setHighlighted:@
setHighlightedSelector :: Selector '[Bool] ()
setHighlightedSelector = mkSelector "setHighlighted:"

