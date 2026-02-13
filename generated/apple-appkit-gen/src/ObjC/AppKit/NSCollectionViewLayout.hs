{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCollectionViewLayout@.
module ObjC.AppKit.NSCollectionViewLayout
  ( NSCollectionViewLayout
  , IsNSCollectionViewLayout(..)
  , invalidateLayout
  , invalidateLayoutWithContext
  , registerClass_forDecorationViewOfKind
  , registerNib_forDecorationViewOfKind
  , prepareForCollectionViewUpdates
  , finalizeCollectionViewUpdates
  , prepareForAnimatedBoundsChange
  , finalizeAnimatedBoundsChange
  , prepareForTransitionToLayout
  , prepareForTransitionFromLayout
  , finalizeLayoutTransition
  , initialLayoutAttributesForAppearingItemAtIndexPath
  , finalLayoutAttributesForDisappearingItemAtIndexPath
  , initialLayoutAttributesForAppearingSupplementaryElementOfKind_atIndexPath
  , finalLayoutAttributesForDisappearingSupplementaryElementOfKind_atIndexPath
  , initialLayoutAttributesForAppearingDecorationElementOfKind_atIndexPath
  , finalLayoutAttributesForDisappearingDecorationElementOfKind_atIndexPath
  , indexPathsToDeleteForSupplementaryViewOfKind
  , indexPathsToDeleteForDecorationViewOfKind
  , indexPathsToInsertForSupplementaryViewOfKind
  , indexPathsToInsertForDecorationViewOfKind
  , prepareLayout
  , layoutAttributesForElementsInRect
  , layoutAttributesForItemAtIndexPath
  , layoutAttributesForSupplementaryViewOfKind_atIndexPath
  , layoutAttributesForDecorationViewOfKind_atIndexPath
  , layoutAttributesForDropTargetAtPoint
  , layoutAttributesForInterItemGapBeforeIndexPath
  , shouldInvalidateLayoutForBoundsChange
  , invalidationContextForBoundsChange
  , shouldInvalidateLayoutForPreferredLayoutAttributes_withOriginalAttributes
  , invalidationContextForPreferredLayoutAttributes_withOriginalAttributes
  , targetContentOffsetForProposedContentOffset_withScrollingVelocity
  , targetContentOffsetForProposedContentOffset
  , collectionView
  , layoutAttributesClass
  , invalidationContextClass
  , collectionViewContentSize
  , collectionViewContentSizeSelector
  , collectionViewSelector
  , finalLayoutAttributesForDisappearingDecorationElementOfKind_atIndexPathSelector
  , finalLayoutAttributesForDisappearingItemAtIndexPathSelector
  , finalLayoutAttributesForDisappearingSupplementaryElementOfKind_atIndexPathSelector
  , finalizeAnimatedBoundsChangeSelector
  , finalizeCollectionViewUpdatesSelector
  , finalizeLayoutTransitionSelector
  , indexPathsToDeleteForDecorationViewOfKindSelector
  , indexPathsToDeleteForSupplementaryViewOfKindSelector
  , indexPathsToInsertForDecorationViewOfKindSelector
  , indexPathsToInsertForSupplementaryViewOfKindSelector
  , initialLayoutAttributesForAppearingDecorationElementOfKind_atIndexPathSelector
  , initialLayoutAttributesForAppearingItemAtIndexPathSelector
  , initialLayoutAttributesForAppearingSupplementaryElementOfKind_atIndexPathSelector
  , invalidateLayoutSelector
  , invalidateLayoutWithContextSelector
  , invalidationContextClassSelector
  , invalidationContextForBoundsChangeSelector
  , invalidationContextForPreferredLayoutAttributes_withOriginalAttributesSelector
  , layoutAttributesClassSelector
  , layoutAttributesForDecorationViewOfKind_atIndexPathSelector
  , layoutAttributesForDropTargetAtPointSelector
  , layoutAttributesForElementsInRectSelector
  , layoutAttributesForInterItemGapBeforeIndexPathSelector
  , layoutAttributesForItemAtIndexPathSelector
  , layoutAttributesForSupplementaryViewOfKind_atIndexPathSelector
  , prepareForAnimatedBoundsChangeSelector
  , prepareForCollectionViewUpdatesSelector
  , prepareForTransitionFromLayoutSelector
  , prepareForTransitionToLayoutSelector
  , prepareLayoutSelector
  , registerClass_forDecorationViewOfKindSelector
  , registerNib_forDecorationViewOfKindSelector
  , shouldInvalidateLayoutForBoundsChangeSelector
  , shouldInvalidateLayoutForPreferredLayoutAttributes_withOriginalAttributesSelector
  , targetContentOffsetForProposedContentOffsetSelector
  , targetContentOffsetForProposedContentOffset_withScrollingVelocitySelector


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

-- | @- invalidateLayout@
invalidateLayout :: IsNSCollectionViewLayout nsCollectionViewLayout => nsCollectionViewLayout -> IO ()
invalidateLayout nsCollectionViewLayout =
  sendMessage nsCollectionViewLayout invalidateLayoutSelector

-- | @- invalidateLayoutWithContext:@
invalidateLayoutWithContext :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSCollectionViewLayoutInvalidationContext context) => nsCollectionViewLayout -> context -> IO ()
invalidateLayoutWithContext nsCollectionViewLayout context =
  sendMessage nsCollectionViewLayout invalidateLayoutWithContextSelector (toNSCollectionViewLayoutInvalidationContext context)

-- | @- registerClass:forDecorationViewOfKind:@
registerClass_forDecorationViewOfKind :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSString elementKind) => nsCollectionViewLayout -> Class -> elementKind -> IO ()
registerClass_forDecorationViewOfKind nsCollectionViewLayout viewClass elementKind =
  sendMessage nsCollectionViewLayout registerClass_forDecorationViewOfKindSelector viewClass (toNSString elementKind)

-- | @- registerNib:forDecorationViewOfKind:@
registerNib_forDecorationViewOfKind :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSNib nib, IsNSString elementKind) => nsCollectionViewLayout -> nib -> elementKind -> IO ()
registerNib_forDecorationViewOfKind nsCollectionViewLayout nib elementKind =
  sendMessage nsCollectionViewLayout registerNib_forDecorationViewOfKindSelector (toNSNib nib) (toNSString elementKind)

-- | @- prepareForCollectionViewUpdates:@
prepareForCollectionViewUpdates :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSArray updateItems) => nsCollectionViewLayout -> updateItems -> IO ()
prepareForCollectionViewUpdates nsCollectionViewLayout updateItems =
  sendMessage nsCollectionViewLayout prepareForCollectionViewUpdatesSelector (toNSArray updateItems)

-- | @- finalizeCollectionViewUpdates@
finalizeCollectionViewUpdates :: IsNSCollectionViewLayout nsCollectionViewLayout => nsCollectionViewLayout -> IO ()
finalizeCollectionViewUpdates nsCollectionViewLayout =
  sendMessage nsCollectionViewLayout finalizeCollectionViewUpdatesSelector

-- | @- prepareForAnimatedBoundsChange:@
prepareForAnimatedBoundsChange :: IsNSCollectionViewLayout nsCollectionViewLayout => nsCollectionViewLayout -> NSRect -> IO ()
prepareForAnimatedBoundsChange nsCollectionViewLayout oldBounds =
  sendMessage nsCollectionViewLayout prepareForAnimatedBoundsChangeSelector oldBounds

-- | @- finalizeAnimatedBoundsChange@
finalizeAnimatedBoundsChange :: IsNSCollectionViewLayout nsCollectionViewLayout => nsCollectionViewLayout -> IO ()
finalizeAnimatedBoundsChange nsCollectionViewLayout =
  sendMessage nsCollectionViewLayout finalizeAnimatedBoundsChangeSelector

-- | @- prepareForTransitionToLayout:@
prepareForTransitionToLayout :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSCollectionViewLayout newLayout) => nsCollectionViewLayout -> newLayout -> IO ()
prepareForTransitionToLayout nsCollectionViewLayout newLayout =
  sendMessage nsCollectionViewLayout prepareForTransitionToLayoutSelector (toNSCollectionViewLayout newLayout)

-- | @- prepareForTransitionFromLayout:@
prepareForTransitionFromLayout :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSCollectionViewLayout oldLayout) => nsCollectionViewLayout -> oldLayout -> IO ()
prepareForTransitionFromLayout nsCollectionViewLayout oldLayout =
  sendMessage nsCollectionViewLayout prepareForTransitionFromLayoutSelector (toNSCollectionViewLayout oldLayout)

-- | @- finalizeLayoutTransition@
finalizeLayoutTransition :: IsNSCollectionViewLayout nsCollectionViewLayout => nsCollectionViewLayout -> IO ()
finalizeLayoutTransition nsCollectionViewLayout =
  sendMessage nsCollectionViewLayout finalizeLayoutTransitionSelector

-- | @- initialLayoutAttributesForAppearingItemAtIndexPath:@
initialLayoutAttributesForAppearingItemAtIndexPath :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSIndexPath itemIndexPath) => nsCollectionViewLayout -> itemIndexPath -> IO (Id NSCollectionViewLayoutAttributes)
initialLayoutAttributesForAppearingItemAtIndexPath nsCollectionViewLayout itemIndexPath =
  sendOwnedMessage nsCollectionViewLayout initialLayoutAttributesForAppearingItemAtIndexPathSelector (toNSIndexPath itemIndexPath)

-- | @- finalLayoutAttributesForDisappearingItemAtIndexPath:@
finalLayoutAttributesForDisappearingItemAtIndexPath :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSIndexPath itemIndexPath) => nsCollectionViewLayout -> itemIndexPath -> IO (Id NSCollectionViewLayoutAttributes)
finalLayoutAttributesForDisappearingItemAtIndexPath nsCollectionViewLayout itemIndexPath =
  sendMessage nsCollectionViewLayout finalLayoutAttributesForDisappearingItemAtIndexPathSelector (toNSIndexPath itemIndexPath)

-- | @- initialLayoutAttributesForAppearingSupplementaryElementOfKind:atIndexPath:@
initialLayoutAttributesForAppearingSupplementaryElementOfKind_atIndexPath :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSString elementKind, IsNSIndexPath elementIndexPath) => nsCollectionViewLayout -> elementKind -> elementIndexPath -> IO (Id NSCollectionViewLayoutAttributes)
initialLayoutAttributesForAppearingSupplementaryElementOfKind_atIndexPath nsCollectionViewLayout elementKind elementIndexPath =
  sendOwnedMessage nsCollectionViewLayout initialLayoutAttributesForAppearingSupplementaryElementOfKind_atIndexPathSelector (toNSString elementKind) (toNSIndexPath elementIndexPath)

-- | @- finalLayoutAttributesForDisappearingSupplementaryElementOfKind:atIndexPath:@
finalLayoutAttributesForDisappearingSupplementaryElementOfKind_atIndexPath :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSString elementKind, IsNSIndexPath elementIndexPath) => nsCollectionViewLayout -> elementKind -> elementIndexPath -> IO (Id NSCollectionViewLayoutAttributes)
finalLayoutAttributesForDisappearingSupplementaryElementOfKind_atIndexPath nsCollectionViewLayout elementKind elementIndexPath =
  sendMessage nsCollectionViewLayout finalLayoutAttributesForDisappearingSupplementaryElementOfKind_atIndexPathSelector (toNSString elementKind) (toNSIndexPath elementIndexPath)

-- | @- initialLayoutAttributesForAppearingDecorationElementOfKind:atIndexPath:@
initialLayoutAttributesForAppearingDecorationElementOfKind_atIndexPath :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSString elementKind, IsNSIndexPath decorationIndexPath) => nsCollectionViewLayout -> elementKind -> decorationIndexPath -> IO (Id NSCollectionViewLayoutAttributes)
initialLayoutAttributesForAppearingDecorationElementOfKind_atIndexPath nsCollectionViewLayout elementKind decorationIndexPath =
  sendOwnedMessage nsCollectionViewLayout initialLayoutAttributesForAppearingDecorationElementOfKind_atIndexPathSelector (toNSString elementKind) (toNSIndexPath decorationIndexPath)

-- | @- finalLayoutAttributesForDisappearingDecorationElementOfKind:atIndexPath:@
finalLayoutAttributesForDisappearingDecorationElementOfKind_atIndexPath :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSString elementKind, IsNSIndexPath decorationIndexPath) => nsCollectionViewLayout -> elementKind -> decorationIndexPath -> IO (Id NSCollectionViewLayoutAttributes)
finalLayoutAttributesForDisappearingDecorationElementOfKind_atIndexPath nsCollectionViewLayout elementKind decorationIndexPath =
  sendMessage nsCollectionViewLayout finalLayoutAttributesForDisappearingDecorationElementOfKind_atIndexPathSelector (toNSString elementKind) (toNSIndexPath decorationIndexPath)

-- | @- indexPathsToDeleteForSupplementaryViewOfKind:@
indexPathsToDeleteForSupplementaryViewOfKind :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSString elementKind) => nsCollectionViewLayout -> elementKind -> IO (Id NSSet)
indexPathsToDeleteForSupplementaryViewOfKind nsCollectionViewLayout elementKind =
  sendMessage nsCollectionViewLayout indexPathsToDeleteForSupplementaryViewOfKindSelector (toNSString elementKind)

-- | @- indexPathsToDeleteForDecorationViewOfKind:@
indexPathsToDeleteForDecorationViewOfKind :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSString elementKind) => nsCollectionViewLayout -> elementKind -> IO (Id NSSet)
indexPathsToDeleteForDecorationViewOfKind nsCollectionViewLayout elementKind =
  sendMessage nsCollectionViewLayout indexPathsToDeleteForDecorationViewOfKindSelector (toNSString elementKind)

-- | @- indexPathsToInsertForSupplementaryViewOfKind:@
indexPathsToInsertForSupplementaryViewOfKind :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSString elementKind) => nsCollectionViewLayout -> elementKind -> IO (Id NSSet)
indexPathsToInsertForSupplementaryViewOfKind nsCollectionViewLayout elementKind =
  sendMessage nsCollectionViewLayout indexPathsToInsertForSupplementaryViewOfKindSelector (toNSString elementKind)

-- | @- indexPathsToInsertForDecorationViewOfKind:@
indexPathsToInsertForDecorationViewOfKind :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSString elementKind) => nsCollectionViewLayout -> elementKind -> IO (Id NSSet)
indexPathsToInsertForDecorationViewOfKind nsCollectionViewLayout elementKind =
  sendMessage nsCollectionViewLayout indexPathsToInsertForDecorationViewOfKindSelector (toNSString elementKind)

-- | @- prepareLayout@
prepareLayout :: IsNSCollectionViewLayout nsCollectionViewLayout => nsCollectionViewLayout -> IO ()
prepareLayout nsCollectionViewLayout =
  sendMessage nsCollectionViewLayout prepareLayoutSelector

-- | @- layoutAttributesForElementsInRect:@
layoutAttributesForElementsInRect :: IsNSCollectionViewLayout nsCollectionViewLayout => nsCollectionViewLayout -> NSRect -> IO (Id NSArray)
layoutAttributesForElementsInRect nsCollectionViewLayout rect =
  sendMessage nsCollectionViewLayout layoutAttributesForElementsInRectSelector rect

-- | @- layoutAttributesForItemAtIndexPath:@
layoutAttributesForItemAtIndexPath :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSIndexPath indexPath) => nsCollectionViewLayout -> indexPath -> IO (Id NSCollectionViewLayoutAttributes)
layoutAttributesForItemAtIndexPath nsCollectionViewLayout indexPath =
  sendMessage nsCollectionViewLayout layoutAttributesForItemAtIndexPathSelector (toNSIndexPath indexPath)

-- | @- layoutAttributesForSupplementaryViewOfKind:atIndexPath:@
layoutAttributesForSupplementaryViewOfKind_atIndexPath :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSString elementKind, IsNSIndexPath indexPath) => nsCollectionViewLayout -> elementKind -> indexPath -> IO (Id NSCollectionViewLayoutAttributes)
layoutAttributesForSupplementaryViewOfKind_atIndexPath nsCollectionViewLayout elementKind indexPath =
  sendMessage nsCollectionViewLayout layoutAttributesForSupplementaryViewOfKind_atIndexPathSelector (toNSString elementKind) (toNSIndexPath indexPath)

-- | @- layoutAttributesForDecorationViewOfKind:atIndexPath:@
layoutAttributesForDecorationViewOfKind_atIndexPath :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSString elementKind, IsNSIndexPath indexPath) => nsCollectionViewLayout -> elementKind -> indexPath -> IO (Id NSCollectionViewLayoutAttributes)
layoutAttributesForDecorationViewOfKind_atIndexPath nsCollectionViewLayout elementKind indexPath =
  sendMessage nsCollectionViewLayout layoutAttributesForDecorationViewOfKind_atIndexPathSelector (toNSString elementKind) (toNSIndexPath indexPath)

-- | @- layoutAttributesForDropTargetAtPoint:@
layoutAttributesForDropTargetAtPoint :: IsNSCollectionViewLayout nsCollectionViewLayout => nsCollectionViewLayout -> NSPoint -> IO (Id NSCollectionViewLayoutAttributes)
layoutAttributesForDropTargetAtPoint nsCollectionViewLayout pointInCollectionView =
  sendMessage nsCollectionViewLayout layoutAttributesForDropTargetAtPointSelector pointInCollectionView

-- | @- layoutAttributesForInterItemGapBeforeIndexPath:@
layoutAttributesForInterItemGapBeforeIndexPath :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSIndexPath indexPath) => nsCollectionViewLayout -> indexPath -> IO (Id NSCollectionViewLayoutAttributes)
layoutAttributesForInterItemGapBeforeIndexPath nsCollectionViewLayout indexPath =
  sendMessage nsCollectionViewLayout layoutAttributesForInterItemGapBeforeIndexPathSelector (toNSIndexPath indexPath)

-- | @- shouldInvalidateLayoutForBoundsChange:@
shouldInvalidateLayoutForBoundsChange :: IsNSCollectionViewLayout nsCollectionViewLayout => nsCollectionViewLayout -> NSRect -> IO Bool
shouldInvalidateLayoutForBoundsChange nsCollectionViewLayout newBounds =
  sendMessage nsCollectionViewLayout shouldInvalidateLayoutForBoundsChangeSelector newBounds

-- | @- invalidationContextForBoundsChange:@
invalidationContextForBoundsChange :: IsNSCollectionViewLayout nsCollectionViewLayout => nsCollectionViewLayout -> NSRect -> IO (Id NSCollectionViewLayoutInvalidationContext)
invalidationContextForBoundsChange nsCollectionViewLayout newBounds =
  sendMessage nsCollectionViewLayout invalidationContextForBoundsChangeSelector newBounds

-- | @- shouldInvalidateLayoutForPreferredLayoutAttributes:withOriginalAttributes:@
shouldInvalidateLayoutForPreferredLayoutAttributes_withOriginalAttributes :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSCollectionViewLayoutAttributes preferredAttributes, IsNSCollectionViewLayoutAttributes originalAttributes) => nsCollectionViewLayout -> preferredAttributes -> originalAttributes -> IO Bool
shouldInvalidateLayoutForPreferredLayoutAttributes_withOriginalAttributes nsCollectionViewLayout preferredAttributes originalAttributes =
  sendMessage nsCollectionViewLayout shouldInvalidateLayoutForPreferredLayoutAttributes_withOriginalAttributesSelector (toNSCollectionViewLayoutAttributes preferredAttributes) (toNSCollectionViewLayoutAttributes originalAttributes)

-- | @- invalidationContextForPreferredLayoutAttributes:withOriginalAttributes:@
invalidationContextForPreferredLayoutAttributes_withOriginalAttributes :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSCollectionViewLayoutAttributes preferredAttributes, IsNSCollectionViewLayoutAttributes originalAttributes) => nsCollectionViewLayout -> preferredAttributes -> originalAttributes -> IO (Id NSCollectionViewLayoutInvalidationContext)
invalidationContextForPreferredLayoutAttributes_withOriginalAttributes nsCollectionViewLayout preferredAttributes originalAttributes =
  sendMessage nsCollectionViewLayout invalidationContextForPreferredLayoutAttributes_withOriginalAttributesSelector (toNSCollectionViewLayoutAttributes preferredAttributes) (toNSCollectionViewLayoutAttributes originalAttributes)

-- | @- targetContentOffsetForProposedContentOffset:withScrollingVelocity:@
targetContentOffsetForProposedContentOffset_withScrollingVelocity :: IsNSCollectionViewLayout nsCollectionViewLayout => nsCollectionViewLayout -> NSPoint -> NSPoint -> IO NSPoint
targetContentOffsetForProposedContentOffset_withScrollingVelocity nsCollectionViewLayout proposedContentOffset velocity =
  sendMessage nsCollectionViewLayout targetContentOffsetForProposedContentOffset_withScrollingVelocitySelector proposedContentOffset velocity

-- | @- targetContentOffsetForProposedContentOffset:@
targetContentOffsetForProposedContentOffset :: IsNSCollectionViewLayout nsCollectionViewLayout => nsCollectionViewLayout -> NSPoint -> IO NSPoint
targetContentOffsetForProposedContentOffset nsCollectionViewLayout proposedContentOffset =
  sendMessage nsCollectionViewLayout targetContentOffsetForProposedContentOffsetSelector proposedContentOffset

-- | @- collectionView@
collectionView :: IsNSCollectionViewLayout nsCollectionViewLayout => nsCollectionViewLayout -> IO (Id NSCollectionView)
collectionView nsCollectionViewLayout =
  sendMessage nsCollectionViewLayout collectionViewSelector

-- | @+ layoutAttributesClass@
layoutAttributesClass :: IO Class
layoutAttributesClass  =
  do
    cls' <- getRequiredClass "NSCollectionViewLayout"
    sendClassMessage cls' layoutAttributesClassSelector

-- | @+ invalidationContextClass@
invalidationContextClass :: IO Class
invalidationContextClass  =
  do
    cls' <- getRequiredClass "NSCollectionViewLayout"
    sendClassMessage cls' invalidationContextClassSelector

-- | @- collectionViewContentSize@
collectionViewContentSize :: IsNSCollectionViewLayout nsCollectionViewLayout => nsCollectionViewLayout -> IO NSSize
collectionViewContentSize nsCollectionViewLayout =
  sendMessage nsCollectionViewLayout collectionViewContentSizeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @invalidateLayout@
invalidateLayoutSelector :: Selector '[] ()
invalidateLayoutSelector = mkSelector "invalidateLayout"

-- | @Selector@ for @invalidateLayoutWithContext:@
invalidateLayoutWithContextSelector :: Selector '[Id NSCollectionViewLayoutInvalidationContext] ()
invalidateLayoutWithContextSelector = mkSelector "invalidateLayoutWithContext:"

-- | @Selector@ for @registerClass:forDecorationViewOfKind:@
registerClass_forDecorationViewOfKindSelector :: Selector '[Class, Id NSString] ()
registerClass_forDecorationViewOfKindSelector = mkSelector "registerClass:forDecorationViewOfKind:"

-- | @Selector@ for @registerNib:forDecorationViewOfKind:@
registerNib_forDecorationViewOfKindSelector :: Selector '[Id NSNib, Id NSString] ()
registerNib_forDecorationViewOfKindSelector = mkSelector "registerNib:forDecorationViewOfKind:"

-- | @Selector@ for @prepareForCollectionViewUpdates:@
prepareForCollectionViewUpdatesSelector :: Selector '[Id NSArray] ()
prepareForCollectionViewUpdatesSelector = mkSelector "prepareForCollectionViewUpdates:"

-- | @Selector@ for @finalizeCollectionViewUpdates@
finalizeCollectionViewUpdatesSelector :: Selector '[] ()
finalizeCollectionViewUpdatesSelector = mkSelector "finalizeCollectionViewUpdates"

-- | @Selector@ for @prepareForAnimatedBoundsChange:@
prepareForAnimatedBoundsChangeSelector :: Selector '[NSRect] ()
prepareForAnimatedBoundsChangeSelector = mkSelector "prepareForAnimatedBoundsChange:"

-- | @Selector@ for @finalizeAnimatedBoundsChange@
finalizeAnimatedBoundsChangeSelector :: Selector '[] ()
finalizeAnimatedBoundsChangeSelector = mkSelector "finalizeAnimatedBoundsChange"

-- | @Selector@ for @prepareForTransitionToLayout:@
prepareForTransitionToLayoutSelector :: Selector '[Id NSCollectionViewLayout] ()
prepareForTransitionToLayoutSelector = mkSelector "prepareForTransitionToLayout:"

-- | @Selector@ for @prepareForTransitionFromLayout:@
prepareForTransitionFromLayoutSelector :: Selector '[Id NSCollectionViewLayout] ()
prepareForTransitionFromLayoutSelector = mkSelector "prepareForTransitionFromLayout:"

-- | @Selector@ for @finalizeLayoutTransition@
finalizeLayoutTransitionSelector :: Selector '[] ()
finalizeLayoutTransitionSelector = mkSelector "finalizeLayoutTransition"

-- | @Selector@ for @initialLayoutAttributesForAppearingItemAtIndexPath:@
initialLayoutAttributesForAppearingItemAtIndexPathSelector :: Selector '[Id NSIndexPath] (Id NSCollectionViewLayoutAttributes)
initialLayoutAttributesForAppearingItemAtIndexPathSelector = mkSelector "initialLayoutAttributesForAppearingItemAtIndexPath:"

-- | @Selector@ for @finalLayoutAttributesForDisappearingItemAtIndexPath:@
finalLayoutAttributesForDisappearingItemAtIndexPathSelector :: Selector '[Id NSIndexPath] (Id NSCollectionViewLayoutAttributes)
finalLayoutAttributesForDisappearingItemAtIndexPathSelector = mkSelector "finalLayoutAttributesForDisappearingItemAtIndexPath:"

-- | @Selector@ for @initialLayoutAttributesForAppearingSupplementaryElementOfKind:atIndexPath:@
initialLayoutAttributesForAppearingSupplementaryElementOfKind_atIndexPathSelector :: Selector '[Id NSString, Id NSIndexPath] (Id NSCollectionViewLayoutAttributes)
initialLayoutAttributesForAppearingSupplementaryElementOfKind_atIndexPathSelector = mkSelector "initialLayoutAttributesForAppearingSupplementaryElementOfKind:atIndexPath:"

-- | @Selector@ for @finalLayoutAttributesForDisappearingSupplementaryElementOfKind:atIndexPath:@
finalLayoutAttributesForDisappearingSupplementaryElementOfKind_atIndexPathSelector :: Selector '[Id NSString, Id NSIndexPath] (Id NSCollectionViewLayoutAttributes)
finalLayoutAttributesForDisappearingSupplementaryElementOfKind_atIndexPathSelector = mkSelector "finalLayoutAttributesForDisappearingSupplementaryElementOfKind:atIndexPath:"

-- | @Selector@ for @initialLayoutAttributesForAppearingDecorationElementOfKind:atIndexPath:@
initialLayoutAttributesForAppearingDecorationElementOfKind_atIndexPathSelector :: Selector '[Id NSString, Id NSIndexPath] (Id NSCollectionViewLayoutAttributes)
initialLayoutAttributesForAppearingDecorationElementOfKind_atIndexPathSelector = mkSelector "initialLayoutAttributesForAppearingDecorationElementOfKind:atIndexPath:"

-- | @Selector@ for @finalLayoutAttributesForDisappearingDecorationElementOfKind:atIndexPath:@
finalLayoutAttributesForDisappearingDecorationElementOfKind_atIndexPathSelector :: Selector '[Id NSString, Id NSIndexPath] (Id NSCollectionViewLayoutAttributes)
finalLayoutAttributesForDisappearingDecorationElementOfKind_atIndexPathSelector = mkSelector "finalLayoutAttributesForDisappearingDecorationElementOfKind:atIndexPath:"

-- | @Selector@ for @indexPathsToDeleteForSupplementaryViewOfKind:@
indexPathsToDeleteForSupplementaryViewOfKindSelector :: Selector '[Id NSString] (Id NSSet)
indexPathsToDeleteForSupplementaryViewOfKindSelector = mkSelector "indexPathsToDeleteForSupplementaryViewOfKind:"

-- | @Selector@ for @indexPathsToDeleteForDecorationViewOfKind:@
indexPathsToDeleteForDecorationViewOfKindSelector :: Selector '[Id NSString] (Id NSSet)
indexPathsToDeleteForDecorationViewOfKindSelector = mkSelector "indexPathsToDeleteForDecorationViewOfKind:"

-- | @Selector@ for @indexPathsToInsertForSupplementaryViewOfKind:@
indexPathsToInsertForSupplementaryViewOfKindSelector :: Selector '[Id NSString] (Id NSSet)
indexPathsToInsertForSupplementaryViewOfKindSelector = mkSelector "indexPathsToInsertForSupplementaryViewOfKind:"

-- | @Selector@ for @indexPathsToInsertForDecorationViewOfKind:@
indexPathsToInsertForDecorationViewOfKindSelector :: Selector '[Id NSString] (Id NSSet)
indexPathsToInsertForDecorationViewOfKindSelector = mkSelector "indexPathsToInsertForDecorationViewOfKind:"

-- | @Selector@ for @prepareLayout@
prepareLayoutSelector :: Selector '[] ()
prepareLayoutSelector = mkSelector "prepareLayout"

-- | @Selector@ for @layoutAttributesForElementsInRect:@
layoutAttributesForElementsInRectSelector :: Selector '[NSRect] (Id NSArray)
layoutAttributesForElementsInRectSelector = mkSelector "layoutAttributesForElementsInRect:"

-- | @Selector@ for @layoutAttributesForItemAtIndexPath:@
layoutAttributesForItemAtIndexPathSelector :: Selector '[Id NSIndexPath] (Id NSCollectionViewLayoutAttributes)
layoutAttributesForItemAtIndexPathSelector = mkSelector "layoutAttributesForItemAtIndexPath:"

-- | @Selector@ for @layoutAttributesForSupplementaryViewOfKind:atIndexPath:@
layoutAttributesForSupplementaryViewOfKind_atIndexPathSelector :: Selector '[Id NSString, Id NSIndexPath] (Id NSCollectionViewLayoutAttributes)
layoutAttributesForSupplementaryViewOfKind_atIndexPathSelector = mkSelector "layoutAttributesForSupplementaryViewOfKind:atIndexPath:"

-- | @Selector@ for @layoutAttributesForDecorationViewOfKind:atIndexPath:@
layoutAttributesForDecorationViewOfKind_atIndexPathSelector :: Selector '[Id NSString, Id NSIndexPath] (Id NSCollectionViewLayoutAttributes)
layoutAttributesForDecorationViewOfKind_atIndexPathSelector = mkSelector "layoutAttributesForDecorationViewOfKind:atIndexPath:"

-- | @Selector@ for @layoutAttributesForDropTargetAtPoint:@
layoutAttributesForDropTargetAtPointSelector :: Selector '[NSPoint] (Id NSCollectionViewLayoutAttributes)
layoutAttributesForDropTargetAtPointSelector = mkSelector "layoutAttributesForDropTargetAtPoint:"

-- | @Selector@ for @layoutAttributesForInterItemGapBeforeIndexPath:@
layoutAttributesForInterItemGapBeforeIndexPathSelector :: Selector '[Id NSIndexPath] (Id NSCollectionViewLayoutAttributes)
layoutAttributesForInterItemGapBeforeIndexPathSelector = mkSelector "layoutAttributesForInterItemGapBeforeIndexPath:"

-- | @Selector@ for @shouldInvalidateLayoutForBoundsChange:@
shouldInvalidateLayoutForBoundsChangeSelector :: Selector '[NSRect] Bool
shouldInvalidateLayoutForBoundsChangeSelector = mkSelector "shouldInvalidateLayoutForBoundsChange:"

-- | @Selector@ for @invalidationContextForBoundsChange:@
invalidationContextForBoundsChangeSelector :: Selector '[NSRect] (Id NSCollectionViewLayoutInvalidationContext)
invalidationContextForBoundsChangeSelector = mkSelector "invalidationContextForBoundsChange:"

-- | @Selector@ for @shouldInvalidateLayoutForPreferredLayoutAttributes:withOriginalAttributes:@
shouldInvalidateLayoutForPreferredLayoutAttributes_withOriginalAttributesSelector :: Selector '[Id NSCollectionViewLayoutAttributes, Id NSCollectionViewLayoutAttributes] Bool
shouldInvalidateLayoutForPreferredLayoutAttributes_withOriginalAttributesSelector = mkSelector "shouldInvalidateLayoutForPreferredLayoutAttributes:withOriginalAttributes:"

-- | @Selector@ for @invalidationContextForPreferredLayoutAttributes:withOriginalAttributes:@
invalidationContextForPreferredLayoutAttributes_withOriginalAttributesSelector :: Selector '[Id NSCollectionViewLayoutAttributes, Id NSCollectionViewLayoutAttributes] (Id NSCollectionViewLayoutInvalidationContext)
invalidationContextForPreferredLayoutAttributes_withOriginalAttributesSelector = mkSelector "invalidationContextForPreferredLayoutAttributes:withOriginalAttributes:"

-- | @Selector@ for @targetContentOffsetForProposedContentOffset:withScrollingVelocity:@
targetContentOffsetForProposedContentOffset_withScrollingVelocitySelector :: Selector '[NSPoint, NSPoint] NSPoint
targetContentOffsetForProposedContentOffset_withScrollingVelocitySelector = mkSelector "targetContentOffsetForProposedContentOffset:withScrollingVelocity:"

-- | @Selector@ for @targetContentOffsetForProposedContentOffset:@
targetContentOffsetForProposedContentOffsetSelector :: Selector '[NSPoint] NSPoint
targetContentOffsetForProposedContentOffsetSelector = mkSelector "targetContentOffsetForProposedContentOffset:"

-- | @Selector@ for @collectionView@
collectionViewSelector :: Selector '[] (Id NSCollectionView)
collectionViewSelector = mkSelector "collectionView"

-- | @Selector@ for @layoutAttributesClass@
layoutAttributesClassSelector :: Selector '[] Class
layoutAttributesClassSelector = mkSelector "layoutAttributesClass"

-- | @Selector@ for @invalidationContextClass@
invalidationContextClassSelector :: Selector '[] Class
invalidationContextClassSelector = mkSelector "invalidationContextClass"

-- | @Selector@ for @collectionViewContentSize@
collectionViewContentSizeSelector :: Selector '[] NSSize
collectionViewContentSizeSelector = mkSelector "collectionViewContentSize"

