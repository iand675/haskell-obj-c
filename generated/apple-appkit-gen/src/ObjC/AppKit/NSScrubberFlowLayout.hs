{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NSScrubberFlowLayout
--
-- @NSScrubberFlowLayout@ is a concrete layout object that arranges items end-to-end in a linear strip. It supports a fixed inter-item spacing and both fixed- and variable-sized items.
--
-- If the associated scrubber's @delegate@ conforms to @NSScrubberFlowLayoutDelegate,@ and it implements the @scrubber:layout:sizeForItemAtIndex:@ method, @NSScrubberFlowLayout@ will obtain the item size from the delegate. If the delegate does not implement that method, or if the method returns @NSZeroSize,@ it will fall back to using the layout's @itemSize@ property. By default, NSScrubberFlowLayout does not invalidate its layout on selection change, highlight change, or visible rectangle change.
--
-- Generated bindings for @NSScrubberFlowLayout@.
module ObjC.AppKit.NSScrubberFlowLayout
  ( NSScrubberFlowLayout
  , IsNSScrubberFlowLayout(..)
  , invalidateLayoutForItemsAtIndexes
  , itemSpacing
  , setItemSpacing
  , itemSize
  , setItemSize
  , invalidateLayoutForItemsAtIndexesSelector
  , itemSizeSelector
  , itemSpacingSelector
  , setItemSizeSelector
  , setItemSpacingSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- invalidateLayoutForItemsAtIndexes:@
invalidateLayoutForItemsAtIndexes :: (IsNSScrubberFlowLayout nsScrubberFlowLayout, IsNSIndexSet invalidItemIndexes) => nsScrubberFlowLayout -> invalidItemIndexes -> IO ()
invalidateLayoutForItemsAtIndexes nsScrubberFlowLayout invalidItemIndexes =
  sendMessage nsScrubberFlowLayout invalidateLayoutForItemsAtIndexesSelector (toNSIndexSet invalidItemIndexes)

-- | The amount of horizontal spacing between items in points. The default value is 0.0.
--
-- ObjC selector: @- itemSpacing@
itemSpacing :: IsNSScrubberFlowLayout nsScrubberFlowLayout => nsScrubberFlowLayout -> IO CDouble
itemSpacing nsScrubberFlowLayout =
  sendMessage nsScrubberFlowLayout itemSpacingSelector

-- | The amount of horizontal spacing between items in points. The default value is 0.0.
--
-- ObjC selector: @- setItemSpacing:@
setItemSpacing :: IsNSScrubberFlowLayout nsScrubberFlowLayout => nsScrubberFlowLayout -> CDouble -> IO ()
setItemSpacing nsScrubberFlowLayout value =
  sendMessage nsScrubberFlowLayout setItemSpacingSelector value

-- | The frame size for each item, if not provided by the scrubber's delegate. The default value is { 50.0, 30.0 }.
--
-- ObjC selector: @- itemSize@
itemSize :: IsNSScrubberFlowLayout nsScrubberFlowLayout => nsScrubberFlowLayout -> IO NSSize
itemSize nsScrubberFlowLayout =
  sendMessage nsScrubberFlowLayout itemSizeSelector

-- | The frame size for each item, if not provided by the scrubber's delegate. The default value is { 50.0, 30.0 }.
--
-- ObjC selector: @- setItemSize:@
setItemSize :: IsNSScrubberFlowLayout nsScrubberFlowLayout => nsScrubberFlowLayout -> NSSize -> IO ()
setItemSize nsScrubberFlowLayout value =
  sendMessage nsScrubberFlowLayout setItemSizeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @invalidateLayoutForItemsAtIndexes:@
invalidateLayoutForItemsAtIndexesSelector :: Selector '[Id NSIndexSet] ()
invalidateLayoutForItemsAtIndexesSelector = mkSelector "invalidateLayoutForItemsAtIndexes:"

-- | @Selector@ for @itemSpacing@
itemSpacingSelector :: Selector '[] CDouble
itemSpacingSelector = mkSelector "itemSpacing"

-- | @Selector@ for @setItemSpacing:@
setItemSpacingSelector :: Selector '[CDouble] ()
setItemSpacingSelector = mkSelector "setItemSpacing:"

-- | @Selector@ for @itemSize@
itemSizeSelector :: Selector '[] NSSize
itemSizeSelector = mkSelector "itemSize"

-- | @Selector@ for @setItemSize:@
setItemSizeSelector :: Selector '[NSSize] ()
setItemSizeSelector = mkSelector "setItemSize:"

