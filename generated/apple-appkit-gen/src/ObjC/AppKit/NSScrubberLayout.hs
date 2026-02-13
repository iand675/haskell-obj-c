{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NSScrubberLayout
--
-- @NSScrubberLayout@ is an abstract class that describes the layout of items within a @NSScrubber@ control.
--
-- Generated bindings for @NSScrubberLayout@.
module ObjC.AppKit.NSScrubberLayout
  ( NSScrubberLayout
  , IsNSScrubberLayout(..)
  , init_
  , initWithCoder
  , invalidateLayout
  , prepareLayout
  , layoutAttributesForItemAtIndex
  , layoutAttributesForItemsInRect
  , shouldInvalidateLayoutForChangeFromVisibleRect_toVisibleRect
  , layoutAttributesClass
  , scrubber
  , visibleRect
  , scrubberContentSize
  , shouldInvalidateLayoutForSelectionChange
  , shouldInvalidateLayoutForHighlightChange
  , automaticallyMirrorsInRightToLeftLayout
  , automaticallyMirrorsInRightToLeftLayoutSelector
  , initSelector
  , initWithCoderSelector
  , invalidateLayoutSelector
  , layoutAttributesClassSelector
  , layoutAttributesForItemAtIndexSelector
  , layoutAttributesForItemsInRectSelector
  , prepareLayoutSelector
  , scrubberContentSizeSelector
  , scrubberSelector
  , shouldInvalidateLayoutForChangeFromVisibleRect_toVisibleRectSelector
  , shouldInvalidateLayoutForHighlightChangeSelector
  , shouldInvalidateLayoutForSelectionChangeSelector
  , visibleRectSelector


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

-- | @- init@
init_ :: IsNSScrubberLayout nsScrubberLayout => nsScrubberLayout -> IO (Id NSScrubberLayout)
init_ nsScrubberLayout =
  sendOwnedMessage nsScrubberLayout initSelector

-- | @- initWithCoder:@
initWithCoder :: (IsNSScrubberLayout nsScrubberLayout, IsNSCoder coder) => nsScrubberLayout -> coder -> IO (Id NSScrubberLayout)
initWithCoder nsScrubberLayout coder =
  sendOwnedMessage nsScrubberLayout initWithCoderSelector (toNSCoder coder)

-- | Signals that layout has been invalidated and the NSScrubber should run a fresh layout pass. Subclasses may define more granular invalidation methods suitable for their own data structures, but those methods should always call up to -invalidateLayout.
--
-- ObjC selector: @- invalidateLayout@
invalidateLayout :: IsNSScrubberLayout nsScrubberLayout => nsScrubberLayout -> IO ()
invalidateLayout nsScrubberLayout =
  sendMessage nsScrubberLayout invalidateLayoutSelector

-- | Following any invalidation in layout, @NSScrubber@ will call @prepareLayout@ on its layout object prior to requesting any other layout information. Subclasses should use this method to perform upfront calculations and caching. The base implementation of this method does nothing.
--
-- ObjC selector: @- prepareLayout@
prepareLayout :: IsNSScrubberLayout nsScrubberLayout => nsScrubberLayout -> IO ()
prepareLayout nsScrubberLayout =
  sendMessage nsScrubberLayout prepareLayoutSelector

-- | Returns the layout attributes for a single item within the scrubber. The base implementation returns @nil.@
--
-- ObjC selector: @- layoutAttributesForItemAtIndex:@
layoutAttributesForItemAtIndex :: IsNSScrubberLayout nsScrubberLayout => nsScrubberLayout -> CLong -> IO (Id NSScrubberLayoutAttributes)
layoutAttributesForItemAtIndex nsScrubberLayout index =
  sendMessage nsScrubberLayout layoutAttributesForItemAtIndexSelector index

-- | Returns the set of layout attributes for all items within the provided rectangle. The base implementation returns an empty set.
--
-- ObjC selector: @- layoutAttributesForItemsInRect:@
layoutAttributesForItemsInRect :: IsNSScrubberLayout nsScrubberLayout => nsScrubberLayout -> NSRect -> IO (Id NSSet)
layoutAttributesForItemsInRect nsScrubberLayout rect =
  sendMessage nsScrubberLayout layoutAttributesForItemsInRectSelector rect

-- | If @YES,@ the scrubber will invalidate its layout in response to a change in the visible region. The default value is @NO.@ Subclasses which rely on the size or origin of the visible region should return @YES.@
--
-- ObjC selector: @- shouldInvalidateLayoutForChangeFromVisibleRect:toVisibleRect:@
shouldInvalidateLayoutForChangeFromVisibleRect_toVisibleRect :: IsNSScrubberLayout nsScrubberLayout => nsScrubberLayout -> NSRect -> NSRect -> IO Bool
shouldInvalidateLayoutForChangeFromVisibleRect_toVisibleRect nsScrubberLayout fromVisibleRect toVisibleRect =
  sendMessage nsScrubberLayout shouldInvalidateLayoutForChangeFromVisibleRect_toVisibleRectSelector fromVisibleRect toVisibleRect

-- | Specifies a class for describing layout attributes. By default, this is @NSScrubberLayoutAttributes,@ but subclasses may override this method to use a custom subclass of @NSScrubberLayoutAttributes.@
--
-- ObjC selector: @+ layoutAttributesClass@
layoutAttributesClass :: IO Class
layoutAttributesClass  =
  do
    cls' <- getRequiredClass "NSScrubberLayout"
    sendClassMessage cls' layoutAttributesClassSelector

-- | The NSScrubber control that this layout is assigned to, or @nil@ if the receiver is not assigned to a scrubber.
--
-- ObjC selector: @- scrubber@
scrubber :: IsNSScrubberLayout nsScrubberLayout => nsScrubberLayout -> IO (Id NSScrubber)
scrubber nsScrubberLayout =
  sendMessage nsScrubberLayout scrubberSelector

-- | The currently visible rectangle, in the coordinate space of the scrubber content. Returns @NSZeroRect@ if the receiver is not assigned to a scrubber.
--
-- ObjC selector: @- visibleRect@
visibleRect :: IsNSScrubberLayout nsScrubberLayout => nsScrubberLayout -> IO NSRect
visibleRect nsScrubberLayout =
  sendMessage nsScrubberLayout visibleRectSelector

-- | Returns the content size for all elements within the scrubber. The base implementation returns @NSZeroSize.@
--
-- ObjC selector: @- scrubberContentSize@
scrubberContentSize :: IsNSScrubberLayout nsScrubberLayout => nsScrubberLayout -> IO NSSize
scrubberContentSize nsScrubberLayout =
  sendMessage nsScrubberLayout scrubberContentSizeSelector

-- | If @YES,@ the scrubber will invalidate its layout when the selection changes. The default value is @NO.@ Subclasses should return @YES@ if the selection index affects the item layout.
--
-- ObjC selector: @- shouldInvalidateLayoutForSelectionChange@
shouldInvalidateLayoutForSelectionChange :: IsNSScrubberLayout nsScrubberLayout => nsScrubberLayout -> IO Bool
shouldInvalidateLayoutForSelectionChange nsScrubberLayout =
  sendMessage nsScrubberLayout shouldInvalidateLayoutForSelectionChangeSelector

-- | If @YES,@ the scrubber will invalidate its layout when an item is highlighted. The default value is @NO.@ Subclasses should return @YES@ if the highlight state affects the item layout.
--
-- ObjC selector: @- shouldInvalidateLayoutForHighlightChange@
shouldInvalidateLayoutForHighlightChange :: IsNSScrubberLayout nsScrubberLayout => nsScrubberLayout -> IO Bool
shouldInvalidateLayoutForHighlightChange nsScrubberLayout =
  sendMessage nsScrubberLayout shouldInvalidateLayoutForHighlightChangeSelector

-- | If @YES,@ the layout object will automatically have its inputs and outputs mirrored in right-to-left interfaces. The default value is @YES.@ Subclasses that wish to handle RTL layout manually should return @NO.@
--
-- ObjC selector: @- automaticallyMirrorsInRightToLeftLayout@
automaticallyMirrorsInRightToLeftLayout :: IsNSScrubberLayout nsScrubberLayout => nsScrubberLayout -> IO Bool
automaticallyMirrorsInRightToLeftLayout nsScrubberLayout =
  sendMessage nsScrubberLayout automaticallyMirrorsInRightToLeftLayoutSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSScrubberLayout)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSScrubberLayout)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @invalidateLayout@
invalidateLayoutSelector :: Selector '[] ()
invalidateLayoutSelector = mkSelector "invalidateLayout"

-- | @Selector@ for @prepareLayout@
prepareLayoutSelector :: Selector '[] ()
prepareLayoutSelector = mkSelector "prepareLayout"

-- | @Selector@ for @layoutAttributesForItemAtIndex:@
layoutAttributesForItemAtIndexSelector :: Selector '[CLong] (Id NSScrubberLayoutAttributes)
layoutAttributesForItemAtIndexSelector = mkSelector "layoutAttributesForItemAtIndex:"

-- | @Selector@ for @layoutAttributesForItemsInRect:@
layoutAttributesForItemsInRectSelector :: Selector '[NSRect] (Id NSSet)
layoutAttributesForItemsInRectSelector = mkSelector "layoutAttributesForItemsInRect:"

-- | @Selector@ for @shouldInvalidateLayoutForChangeFromVisibleRect:toVisibleRect:@
shouldInvalidateLayoutForChangeFromVisibleRect_toVisibleRectSelector :: Selector '[NSRect, NSRect] Bool
shouldInvalidateLayoutForChangeFromVisibleRect_toVisibleRectSelector = mkSelector "shouldInvalidateLayoutForChangeFromVisibleRect:toVisibleRect:"

-- | @Selector@ for @layoutAttributesClass@
layoutAttributesClassSelector :: Selector '[] Class
layoutAttributesClassSelector = mkSelector "layoutAttributesClass"

-- | @Selector@ for @scrubber@
scrubberSelector :: Selector '[] (Id NSScrubber)
scrubberSelector = mkSelector "scrubber"

-- | @Selector@ for @visibleRect@
visibleRectSelector :: Selector '[] NSRect
visibleRectSelector = mkSelector "visibleRect"

-- | @Selector@ for @scrubberContentSize@
scrubberContentSizeSelector :: Selector '[] NSSize
scrubberContentSizeSelector = mkSelector "scrubberContentSize"

-- | @Selector@ for @shouldInvalidateLayoutForSelectionChange@
shouldInvalidateLayoutForSelectionChangeSelector :: Selector '[] Bool
shouldInvalidateLayoutForSelectionChangeSelector = mkSelector "shouldInvalidateLayoutForSelectionChange"

-- | @Selector@ for @shouldInvalidateLayoutForHighlightChange@
shouldInvalidateLayoutForHighlightChangeSelector :: Selector '[] Bool
shouldInvalidateLayoutForHighlightChangeSelector = mkSelector "shouldInvalidateLayoutForHighlightChange"

-- | @Selector@ for @automaticallyMirrorsInRightToLeftLayout@
automaticallyMirrorsInRightToLeftLayoutSelector :: Selector '[] Bool
automaticallyMirrorsInRightToLeftLayoutSelector = mkSelector "automaticallyMirrorsInRightToLeftLayout"

