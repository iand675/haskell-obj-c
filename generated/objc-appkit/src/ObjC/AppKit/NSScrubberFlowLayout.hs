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
  , itemSpacingSelector
  , setItemSpacingSelector
  , itemSizeSelector
  , setItemSizeSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- invalidateLayoutForItemsAtIndexes:@
invalidateLayoutForItemsAtIndexes :: (IsNSScrubberFlowLayout nsScrubberFlowLayout, IsNSIndexSet invalidItemIndexes) => nsScrubberFlowLayout -> invalidItemIndexes -> IO ()
invalidateLayoutForItemsAtIndexes nsScrubberFlowLayout  invalidItemIndexes =
withObjCPtr invalidItemIndexes $ \raw_invalidItemIndexes ->
    sendMsg nsScrubberFlowLayout (mkSelector "invalidateLayoutForItemsAtIndexes:") retVoid [argPtr (castPtr raw_invalidItemIndexes :: Ptr ())]

-- | The amount of horizontal spacing between items in points. The default value is 0.0.
--
-- ObjC selector: @- itemSpacing@
itemSpacing :: IsNSScrubberFlowLayout nsScrubberFlowLayout => nsScrubberFlowLayout -> IO CDouble
itemSpacing nsScrubberFlowLayout  =
  sendMsg nsScrubberFlowLayout (mkSelector "itemSpacing") retCDouble []

-- | The amount of horizontal spacing between items in points. The default value is 0.0.
--
-- ObjC selector: @- setItemSpacing:@
setItemSpacing :: IsNSScrubberFlowLayout nsScrubberFlowLayout => nsScrubberFlowLayout -> CDouble -> IO ()
setItemSpacing nsScrubberFlowLayout  value =
  sendMsg nsScrubberFlowLayout (mkSelector "setItemSpacing:") retVoid [argCDouble (fromIntegral value)]

-- | The frame size for each item, if not provided by the scrubber's delegate. The default value is { 50.0, 30.0 }.
--
-- ObjC selector: @- itemSize@
itemSize :: IsNSScrubberFlowLayout nsScrubberFlowLayout => nsScrubberFlowLayout -> IO NSSize
itemSize nsScrubberFlowLayout  =
  sendMsgStret nsScrubberFlowLayout (mkSelector "itemSize") retNSSize []

-- | The frame size for each item, if not provided by the scrubber's delegate. The default value is { 50.0, 30.0 }.
--
-- ObjC selector: @- setItemSize:@
setItemSize :: IsNSScrubberFlowLayout nsScrubberFlowLayout => nsScrubberFlowLayout -> NSSize -> IO ()
setItemSize nsScrubberFlowLayout  value =
  sendMsg nsScrubberFlowLayout (mkSelector "setItemSize:") retVoid [argNSSize value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @invalidateLayoutForItemsAtIndexes:@
invalidateLayoutForItemsAtIndexesSelector :: Selector
invalidateLayoutForItemsAtIndexesSelector = mkSelector "invalidateLayoutForItemsAtIndexes:"

-- | @Selector@ for @itemSpacing@
itemSpacingSelector :: Selector
itemSpacingSelector = mkSelector "itemSpacing"

-- | @Selector@ for @setItemSpacing:@
setItemSpacingSelector :: Selector
setItemSpacingSelector = mkSelector "setItemSpacing:"

-- | @Selector@ for @itemSize@
itemSizeSelector :: Selector
itemSizeSelector = mkSelector "itemSize"

-- | @Selector@ for @setItemSize:@
setItemSizeSelector :: Selector
setItemSizeSelector = mkSelector "setItemSize:"

