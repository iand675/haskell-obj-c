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
  , invalidateLayoutSelector
  , invalidateLayoutWithContextSelector
  , registerClass_forDecorationViewOfKindSelector
  , registerNib_forDecorationViewOfKindSelector
  , prepareForCollectionViewUpdatesSelector
  , finalizeCollectionViewUpdatesSelector
  , prepareForAnimatedBoundsChangeSelector
  , finalizeAnimatedBoundsChangeSelector
  , prepareForTransitionToLayoutSelector
  , prepareForTransitionFromLayoutSelector
  , finalizeLayoutTransitionSelector
  , initialLayoutAttributesForAppearingItemAtIndexPathSelector
  , finalLayoutAttributesForDisappearingItemAtIndexPathSelector
  , initialLayoutAttributesForAppearingSupplementaryElementOfKind_atIndexPathSelector
  , finalLayoutAttributesForDisappearingSupplementaryElementOfKind_atIndexPathSelector
  , initialLayoutAttributesForAppearingDecorationElementOfKind_atIndexPathSelector
  , finalLayoutAttributesForDisappearingDecorationElementOfKind_atIndexPathSelector
  , indexPathsToDeleteForSupplementaryViewOfKindSelector
  , indexPathsToDeleteForDecorationViewOfKindSelector
  , indexPathsToInsertForSupplementaryViewOfKindSelector
  , indexPathsToInsertForDecorationViewOfKindSelector
  , prepareLayoutSelector
  , layoutAttributesForElementsInRectSelector
  , layoutAttributesForItemAtIndexPathSelector
  , layoutAttributesForSupplementaryViewOfKind_atIndexPathSelector
  , layoutAttributesForDecorationViewOfKind_atIndexPathSelector
  , layoutAttributesForDropTargetAtPointSelector
  , layoutAttributesForInterItemGapBeforeIndexPathSelector
  , shouldInvalidateLayoutForBoundsChangeSelector
  , invalidationContextForBoundsChangeSelector
  , shouldInvalidateLayoutForPreferredLayoutAttributes_withOriginalAttributesSelector
  , invalidationContextForPreferredLayoutAttributes_withOriginalAttributesSelector
  , targetContentOffsetForProposedContentOffset_withScrollingVelocitySelector
  , targetContentOffsetForProposedContentOffsetSelector
  , collectionViewSelector
  , layoutAttributesClassSelector
  , invalidationContextClassSelector
  , collectionViewContentSizeSelector


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

-- | @- invalidateLayout@
invalidateLayout :: IsNSCollectionViewLayout nsCollectionViewLayout => nsCollectionViewLayout -> IO ()
invalidateLayout nsCollectionViewLayout  =
  sendMsg nsCollectionViewLayout (mkSelector "invalidateLayout") retVoid []

-- | @- invalidateLayoutWithContext:@
invalidateLayoutWithContext :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSCollectionViewLayoutInvalidationContext context) => nsCollectionViewLayout -> context -> IO ()
invalidateLayoutWithContext nsCollectionViewLayout  context =
withObjCPtr context $ \raw_context ->
    sendMsg nsCollectionViewLayout (mkSelector "invalidateLayoutWithContext:") retVoid [argPtr (castPtr raw_context :: Ptr ())]

-- | @- registerClass:forDecorationViewOfKind:@
registerClass_forDecorationViewOfKind :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSString elementKind) => nsCollectionViewLayout -> Class -> elementKind -> IO ()
registerClass_forDecorationViewOfKind nsCollectionViewLayout  viewClass elementKind =
withObjCPtr elementKind $ \raw_elementKind ->
    sendMsg nsCollectionViewLayout (mkSelector "registerClass:forDecorationViewOfKind:") retVoid [argPtr (unClass viewClass), argPtr (castPtr raw_elementKind :: Ptr ())]

-- | @- registerNib:forDecorationViewOfKind:@
registerNib_forDecorationViewOfKind :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSNib nib, IsNSString elementKind) => nsCollectionViewLayout -> nib -> elementKind -> IO ()
registerNib_forDecorationViewOfKind nsCollectionViewLayout  nib elementKind =
withObjCPtr nib $ \raw_nib ->
  withObjCPtr elementKind $ \raw_elementKind ->
      sendMsg nsCollectionViewLayout (mkSelector "registerNib:forDecorationViewOfKind:") retVoid [argPtr (castPtr raw_nib :: Ptr ()), argPtr (castPtr raw_elementKind :: Ptr ())]

-- | @- prepareForCollectionViewUpdates:@
prepareForCollectionViewUpdates :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSArray updateItems) => nsCollectionViewLayout -> updateItems -> IO ()
prepareForCollectionViewUpdates nsCollectionViewLayout  updateItems =
withObjCPtr updateItems $ \raw_updateItems ->
    sendMsg nsCollectionViewLayout (mkSelector "prepareForCollectionViewUpdates:") retVoid [argPtr (castPtr raw_updateItems :: Ptr ())]

-- | @- finalizeCollectionViewUpdates@
finalizeCollectionViewUpdates :: IsNSCollectionViewLayout nsCollectionViewLayout => nsCollectionViewLayout -> IO ()
finalizeCollectionViewUpdates nsCollectionViewLayout  =
  sendMsg nsCollectionViewLayout (mkSelector "finalizeCollectionViewUpdates") retVoid []

-- | @- prepareForAnimatedBoundsChange:@
prepareForAnimatedBoundsChange :: IsNSCollectionViewLayout nsCollectionViewLayout => nsCollectionViewLayout -> NSRect -> IO ()
prepareForAnimatedBoundsChange nsCollectionViewLayout  oldBounds =
  sendMsg nsCollectionViewLayout (mkSelector "prepareForAnimatedBoundsChange:") retVoid [argNSRect oldBounds]

-- | @- finalizeAnimatedBoundsChange@
finalizeAnimatedBoundsChange :: IsNSCollectionViewLayout nsCollectionViewLayout => nsCollectionViewLayout -> IO ()
finalizeAnimatedBoundsChange nsCollectionViewLayout  =
  sendMsg nsCollectionViewLayout (mkSelector "finalizeAnimatedBoundsChange") retVoid []

-- | @- prepareForTransitionToLayout:@
prepareForTransitionToLayout :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSCollectionViewLayout newLayout) => nsCollectionViewLayout -> newLayout -> IO ()
prepareForTransitionToLayout nsCollectionViewLayout  newLayout =
withObjCPtr newLayout $ \raw_newLayout ->
    sendMsg nsCollectionViewLayout (mkSelector "prepareForTransitionToLayout:") retVoid [argPtr (castPtr raw_newLayout :: Ptr ())]

-- | @- prepareForTransitionFromLayout:@
prepareForTransitionFromLayout :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSCollectionViewLayout oldLayout) => nsCollectionViewLayout -> oldLayout -> IO ()
prepareForTransitionFromLayout nsCollectionViewLayout  oldLayout =
withObjCPtr oldLayout $ \raw_oldLayout ->
    sendMsg nsCollectionViewLayout (mkSelector "prepareForTransitionFromLayout:") retVoid [argPtr (castPtr raw_oldLayout :: Ptr ())]

-- | @- finalizeLayoutTransition@
finalizeLayoutTransition :: IsNSCollectionViewLayout nsCollectionViewLayout => nsCollectionViewLayout -> IO ()
finalizeLayoutTransition nsCollectionViewLayout  =
  sendMsg nsCollectionViewLayout (mkSelector "finalizeLayoutTransition") retVoid []

-- | @- initialLayoutAttributesForAppearingItemAtIndexPath:@
initialLayoutAttributesForAppearingItemAtIndexPath :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSIndexPath itemIndexPath) => nsCollectionViewLayout -> itemIndexPath -> IO (Id NSCollectionViewLayoutAttributes)
initialLayoutAttributesForAppearingItemAtIndexPath nsCollectionViewLayout  itemIndexPath =
withObjCPtr itemIndexPath $ \raw_itemIndexPath ->
    sendMsg nsCollectionViewLayout (mkSelector "initialLayoutAttributesForAppearingItemAtIndexPath:") (retPtr retVoid) [argPtr (castPtr raw_itemIndexPath :: Ptr ())] >>= ownedObject . castPtr

-- | @- finalLayoutAttributesForDisappearingItemAtIndexPath:@
finalLayoutAttributesForDisappearingItemAtIndexPath :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSIndexPath itemIndexPath) => nsCollectionViewLayout -> itemIndexPath -> IO (Id NSCollectionViewLayoutAttributes)
finalLayoutAttributesForDisappearingItemAtIndexPath nsCollectionViewLayout  itemIndexPath =
withObjCPtr itemIndexPath $ \raw_itemIndexPath ->
    sendMsg nsCollectionViewLayout (mkSelector "finalLayoutAttributesForDisappearingItemAtIndexPath:") (retPtr retVoid) [argPtr (castPtr raw_itemIndexPath :: Ptr ())] >>= retainedObject . castPtr

-- | @- initialLayoutAttributesForAppearingSupplementaryElementOfKind:atIndexPath:@
initialLayoutAttributesForAppearingSupplementaryElementOfKind_atIndexPath :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSString elementKind, IsNSIndexPath elementIndexPath) => nsCollectionViewLayout -> elementKind -> elementIndexPath -> IO (Id NSCollectionViewLayoutAttributes)
initialLayoutAttributesForAppearingSupplementaryElementOfKind_atIndexPath nsCollectionViewLayout  elementKind elementIndexPath =
withObjCPtr elementKind $ \raw_elementKind ->
  withObjCPtr elementIndexPath $ \raw_elementIndexPath ->
      sendMsg nsCollectionViewLayout (mkSelector "initialLayoutAttributesForAppearingSupplementaryElementOfKind:atIndexPath:") (retPtr retVoid) [argPtr (castPtr raw_elementKind :: Ptr ()), argPtr (castPtr raw_elementIndexPath :: Ptr ())] >>= ownedObject . castPtr

-- | @- finalLayoutAttributesForDisappearingSupplementaryElementOfKind:atIndexPath:@
finalLayoutAttributesForDisappearingSupplementaryElementOfKind_atIndexPath :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSString elementKind, IsNSIndexPath elementIndexPath) => nsCollectionViewLayout -> elementKind -> elementIndexPath -> IO (Id NSCollectionViewLayoutAttributes)
finalLayoutAttributesForDisappearingSupplementaryElementOfKind_atIndexPath nsCollectionViewLayout  elementKind elementIndexPath =
withObjCPtr elementKind $ \raw_elementKind ->
  withObjCPtr elementIndexPath $ \raw_elementIndexPath ->
      sendMsg nsCollectionViewLayout (mkSelector "finalLayoutAttributesForDisappearingSupplementaryElementOfKind:atIndexPath:") (retPtr retVoid) [argPtr (castPtr raw_elementKind :: Ptr ()), argPtr (castPtr raw_elementIndexPath :: Ptr ())] >>= retainedObject . castPtr

-- | @- initialLayoutAttributesForAppearingDecorationElementOfKind:atIndexPath:@
initialLayoutAttributesForAppearingDecorationElementOfKind_atIndexPath :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSString elementKind, IsNSIndexPath decorationIndexPath) => nsCollectionViewLayout -> elementKind -> decorationIndexPath -> IO (Id NSCollectionViewLayoutAttributes)
initialLayoutAttributesForAppearingDecorationElementOfKind_atIndexPath nsCollectionViewLayout  elementKind decorationIndexPath =
withObjCPtr elementKind $ \raw_elementKind ->
  withObjCPtr decorationIndexPath $ \raw_decorationIndexPath ->
      sendMsg nsCollectionViewLayout (mkSelector "initialLayoutAttributesForAppearingDecorationElementOfKind:atIndexPath:") (retPtr retVoid) [argPtr (castPtr raw_elementKind :: Ptr ()), argPtr (castPtr raw_decorationIndexPath :: Ptr ())] >>= ownedObject . castPtr

-- | @- finalLayoutAttributesForDisappearingDecorationElementOfKind:atIndexPath:@
finalLayoutAttributesForDisappearingDecorationElementOfKind_atIndexPath :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSString elementKind, IsNSIndexPath decorationIndexPath) => nsCollectionViewLayout -> elementKind -> decorationIndexPath -> IO (Id NSCollectionViewLayoutAttributes)
finalLayoutAttributesForDisappearingDecorationElementOfKind_atIndexPath nsCollectionViewLayout  elementKind decorationIndexPath =
withObjCPtr elementKind $ \raw_elementKind ->
  withObjCPtr decorationIndexPath $ \raw_decorationIndexPath ->
      sendMsg nsCollectionViewLayout (mkSelector "finalLayoutAttributesForDisappearingDecorationElementOfKind:atIndexPath:") (retPtr retVoid) [argPtr (castPtr raw_elementKind :: Ptr ()), argPtr (castPtr raw_decorationIndexPath :: Ptr ())] >>= retainedObject . castPtr

-- | @- indexPathsToDeleteForSupplementaryViewOfKind:@
indexPathsToDeleteForSupplementaryViewOfKind :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSString elementKind) => nsCollectionViewLayout -> elementKind -> IO (Id NSSet)
indexPathsToDeleteForSupplementaryViewOfKind nsCollectionViewLayout  elementKind =
withObjCPtr elementKind $ \raw_elementKind ->
    sendMsg nsCollectionViewLayout (mkSelector "indexPathsToDeleteForSupplementaryViewOfKind:") (retPtr retVoid) [argPtr (castPtr raw_elementKind :: Ptr ())] >>= retainedObject . castPtr

-- | @- indexPathsToDeleteForDecorationViewOfKind:@
indexPathsToDeleteForDecorationViewOfKind :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSString elementKind) => nsCollectionViewLayout -> elementKind -> IO (Id NSSet)
indexPathsToDeleteForDecorationViewOfKind nsCollectionViewLayout  elementKind =
withObjCPtr elementKind $ \raw_elementKind ->
    sendMsg nsCollectionViewLayout (mkSelector "indexPathsToDeleteForDecorationViewOfKind:") (retPtr retVoid) [argPtr (castPtr raw_elementKind :: Ptr ())] >>= retainedObject . castPtr

-- | @- indexPathsToInsertForSupplementaryViewOfKind:@
indexPathsToInsertForSupplementaryViewOfKind :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSString elementKind) => nsCollectionViewLayout -> elementKind -> IO (Id NSSet)
indexPathsToInsertForSupplementaryViewOfKind nsCollectionViewLayout  elementKind =
withObjCPtr elementKind $ \raw_elementKind ->
    sendMsg nsCollectionViewLayout (mkSelector "indexPathsToInsertForSupplementaryViewOfKind:") (retPtr retVoid) [argPtr (castPtr raw_elementKind :: Ptr ())] >>= retainedObject . castPtr

-- | @- indexPathsToInsertForDecorationViewOfKind:@
indexPathsToInsertForDecorationViewOfKind :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSString elementKind) => nsCollectionViewLayout -> elementKind -> IO (Id NSSet)
indexPathsToInsertForDecorationViewOfKind nsCollectionViewLayout  elementKind =
withObjCPtr elementKind $ \raw_elementKind ->
    sendMsg nsCollectionViewLayout (mkSelector "indexPathsToInsertForDecorationViewOfKind:") (retPtr retVoid) [argPtr (castPtr raw_elementKind :: Ptr ())] >>= retainedObject . castPtr

-- | @- prepareLayout@
prepareLayout :: IsNSCollectionViewLayout nsCollectionViewLayout => nsCollectionViewLayout -> IO ()
prepareLayout nsCollectionViewLayout  =
  sendMsg nsCollectionViewLayout (mkSelector "prepareLayout") retVoid []

-- | @- layoutAttributesForElementsInRect:@
layoutAttributesForElementsInRect :: IsNSCollectionViewLayout nsCollectionViewLayout => nsCollectionViewLayout -> NSRect -> IO (Id NSArray)
layoutAttributesForElementsInRect nsCollectionViewLayout  rect =
  sendMsg nsCollectionViewLayout (mkSelector "layoutAttributesForElementsInRect:") (retPtr retVoid) [argNSRect rect] >>= retainedObject . castPtr

-- | @- layoutAttributesForItemAtIndexPath:@
layoutAttributesForItemAtIndexPath :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSIndexPath indexPath) => nsCollectionViewLayout -> indexPath -> IO (Id NSCollectionViewLayoutAttributes)
layoutAttributesForItemAtIndexPath nsCollectionViewLayout  indexPath =
withObjCPtr indexPath $ \raw_indexPath ->
    sendMsg nsCollectionViewLayout (mkSelector "layoutAttributesForItemAtIndexPath:") (retPtr retVoid) [argPtr (castPtr raw_indexPath :: Ptr ())] >>= retainedObject . castPtr

-- | @- layoutAttributesForSupplementaryViewOfKind:atIndexPath:@
layoutAttributesForSupplementaryViewOfKind_atIndexPath :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSString elementKind, IsNSIndexPath indexPath) => nsCollectionViewLayout -> elementKind -> indexPath -> IO (Id NSCollectionViewLayoutAttributes)
layoutAttributesForSupplementaryViewOfKind_atIndexPath nsCollectionViewLayout  elementKind indexPath =
withObjCPtr elementKind $ \raw_elementKind ->
  withObjCPtr indexPath $ \raw_indexPath ->
      sendMsg nsCollectionViewLayout (mkSelector "layoutAttributesForSupplementaryViewOfKind:atIndexPath:") (retPtr retVoid) [argPtr (castPtr raw_elementKind :: Ptr ()), argPtr (castPtr raw_indexPath :: Ptr ())] >>= retainedObject . castPtr

-- | @- layoutAttributesForDecorationViewOfKind:atIndexPath:@
layoutAttributesForDecorationViewOfKind_atIndexPath :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSString elementKind, IsNSIndexPath indexPath) => nsCollectionViewLayout -> elementKind -> indexPath -> IO (Id NSCollectionViewLayoutAttributes)
layoutAttributesForDecorationViewOfKind_atIndexPath nsCollectionViewLayout  elementKind indexPath =
withObjCPtr elementKind $ \raw_elementKind ->
  withObjCPtr indexPath $ \raw_indexPath ->
      sendMsg nsCollectionViewLayout (mkSelector "layoutAttributesForDecorationViewOfKind:atIndexPath:") (retPtr retVoid) [argPtr (castPtr raw_elementKind :: Ptr ()), argPtr (castPtr raw_indexPath :: Ptr ())] >>= retainedObject . castPtr

-- | @- layoutAttributesForDropTargetAtPoint:@
layoutAttributesForDropTargetAtPoint :: IsNSCollectionViewLayout nsCollectionViewLayout => nsCollectionViewLayout -> NSPoint -> IO (Id NSCollectionViewLayoutAttributes)
layoutAttributesForDropTargetAtPoint nsCollectionViewLayout  pointInCollectionView =
  sendMsg nsCollectionViewLayout (mkSelector "layoutAttributesForDropTargetAtPoint:") (retPtr retVoid) [argNSPoint pointInCollectionView] >>= retainedObject . castPtr

-- | @- layoutAttributesForInterItemGapBeforeIndexPath:@
layoutAttributesForInterItemGapBeforeIndexPath :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSIndexPath indexPath) => nsCollectionViewLayout -> indexPath -> IO (Id NSCollectionViewLayoutAttributes)
layoutAttributesForInterItemGapBeforeIndexPath nsCollectionViewLayout  indexPath =
withObjCPtr indexPath $ \raw_indexPath ->
    sendMsg nsCollectionViewLayout (mkSelector "layoutAttributesForInterItemGapBeforeIndexPath:") (retPtr retVoid) [argPtr (castPtr raw_indexPath :: Ptr ())] >>= retainedObject . castPtr

-- | @- shouldInvalidateLayoutForBoundsChange:@
shouldInvalidateLayoutForBoundsChange :: IsNSCollectionViewLayout nsCollectionViewLayout => nsCollectionViewLayout -> NSRect -> IO Bool
shouldInvalidateLayoutForBoundsChange nsCollectionViewLayout  newBounds =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCollectionViewLayout (mkSelector "shouldInvalidateLayoutForBoundsChange:") retCULong [argNSRect newBounds]

-- | @- invalidationContextForBoundsChange:@
invalidationContextForBoundsChange :: IsNSCollectionViewLayout nsCollectionViewLayout => nsCollectionViewLayout -> NSRect -> IO (Id NSCollectionViewLayoutInvalidationContext)
invalidationContextForBoundsChange nsCollectionViewLayout  newBounds =
  sendMsg nsCollectionViewLayout (mkSelector "invalidationContextForBoundsChange:") (retPtr retVoid) [argNSRect newBounds] >>= retainedObject . castPtr

-- | @- shouldInvalidateLayoutForPreferredLayoutAttributes:withOriginalAttributes:@
shouldInvalidateLayoutForPreferredLayoutAttributes_withOriginalAttributes :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSCollectionViewLayoutAttributes preferredAttributes, IsNSCollectionViewLayoutAttributes originalAttributes) => nsCollectionViewLayout -> preferredAttributes -> originalAttributes -> IO Bool
shouldInvalidateLayoutForPreferredLayoutAttributes_withOriginalAttributes nsCollectionViewLayout  preferredAttributes originalAttributes =
withObjCPtr preferredAttributes $ \raw_preferredAttributes ->
  withObjCPtr originalAttributes $ \raw_originalAttributes ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCollectionViewLayout (mkSelector "shouldInvalidateLayoutForPreferredLayoutAttributes:withOriginalAttributes:") retCULong [argPtr (castPtr raw_preferredAttributes :: Ptr ()), argPtr (castPtr raw_originalAttributes :: Ptr ())]

-- | @- invalidationContextForPreferredLayoutAttributes:withOriginalAttributes:@
invalidationContextForPreferredLayoutAttributes_withOriginalAttributes :: (IsNSCollectionViewLayout nsCollectionViewLayout, IsNSCollectionViewLayoutAttributes preferredAttributes, IsNSCollectionViewLayoutAttributes originalAttributes) => nsCollectionViewLayout -> preferredAttributes -> originalAttributes -> IO (Id NSCollectionViewLayoutInvalidationContext)
invalidationContextForPreferredLayoutAttributes_withOriginalAttributes nsCollectionViewLayout  preferredAttributes originalAttributes =
withObjCPtr preferredAttributes $ \raw_preferredAttributes ->
  withObjCPtr originalAttributes $ \raw_originalAttributes ->
      sendMsg nsCollectionViewLayout (mkSelector "invalidationContextForPreferredLayoutAttributes:withOriginalAttributes:") (retPtr retVoid) [argPtr (castPtr raw_preferredAttributes :: Ptr ()), argPtr (castPtr raw_originalAttributes :: Ptr ())] >>= retainedObject . castPtr

-- | @- targetContentOffsetForProposedContentOffset:withScrollingVelocity:@
targetContentOffsetForProposedContentOffset_withScrollingVelocity :: IsNSCollectionViewLayout nsCollectionViewLayout => nsCollectionViewLayout -> NSPoint -> NSPoint -> IO NSPoint
targetContentOffsetForProposedContentOffset_withScrollingVelocity nsCollectionViewLayout  proposedContentOffset velocity =
  sendMsgStret nsCollectionViewLayout (mkSelector "targetContentOffsetForProposedContentOffset:withScrollingVelocity:") retNSPoint [argNSPoint proposedContentOffset, argNSPoint velocity]

-- | @- targetContentOffsetForProposedContentOffset:@
targetContentOffsetForProposedContentOffset :: IsNSCollectionViewLayout nsCollectionViewLayout => nsCollectionViewLayout -> NSPoint -> IO NSPoint
targetContentOffsetForProposedContentOffset nsCollectionViewLayout  proposedContentOffset =
  sendMsgStret nsCollectionViewLayout (mkSelector "targetContentOffsetForProposedContentOffset:") retNSPoint [argNSPoint proposedContentOffset]

-- | @- collectionView@
collectionView :: IsNSCollectionViewLayout nsCollectionViewLayout => nsCollectionViewLayout -> IO (Id NSCollectionView)
collectionView nsCollectionViewLayout  =
  sendMsg nsCollectionViewLayout (mkSelector "collectionView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ layoutAttributesClass@
layoutAttributesClass :: IO Class
layoutAttributesClass  =
  do
    cls' <- getRequiredClass "NSCollectionViewLayout"
    fmap (Class . castPtr) $ sendClassMsg cls' (mkSelector "layoutAttributesClass") (retPtr retVoid) []

-- | @+ invalidationContextClass@
invalidationContextClass :: IO Class
invalidationContextClass  =
  do
    cls' <- getRequiredClass "NSCollectionViewLayout"
    fmap (Class . castPtr) $ sendClassMsg cls' (mkSelector "invalidationContextClass") (retPtr retVoid) []

-- | @- collectionViewContentSize@
collectionViewContentSize :: IsNSCollectionViewLayout nsCollectionViewLayout => nsCollectionViewLayout -> IO NSSize
collectionViewContentSize nsCollectionViewLayout  =
  sendMsgStret nsCollectionViewLayout (mkSelector "collectionViewContentSize") retNSSize []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @invalidateLayout@
invalidateLayoutSelector :: Selector
invalidateLayoutSelector = mkSelector "invalidateLayout"

-- | @Selector@ for @invalidateLayoutWithContext:@
invalidateLayoutWithContextSelector :: Selector
invalidateLayoutWithContextSelector = mkSelector "invalidateLayoutWithContext:"

-- | @Selector@ for @registerClass:forDecorationViewOfKind:@
registerClass_forDecorationViewOfKindSelector :: Selector
registerClass_forDecorationViewOfKindSelector = mkSelector "registerClass:forDecorationViewOfKind:"

-- | @Selector@ for @registerNib:forDecorationViewOfKind:@
registerNib_forDecorationViewOfKindSelector :: Selector
registerNib_forDecorationViewOfKindSelector = mkSelector "registerNib:forDecorationViewOfKind:"

-- | @Selector@ for @prepareForCollectionViewUpdates:@
prepareForCollectionViewUpdatesSelector :: Selector
prepareForCollectionViewUpdatesSelector = mkSelector "prepareForCollectionViewUpdates:"

-- | @Selector@ for @finalizeCollectionViewUpdates@
finalizeCollectionViewUpdatesSelector :: Selector
finalizeCollectionViewUpdatesSelector = mkSelector "finalizeCollectionViewUpdates"

-- | @Selector@ for @prepareForAnimatedBoundsChange:@
prepareForAnimatedBoundsChangeSelector :: Selector
prepareForAnimatedBoundsChangeSelector = mkSelector "prepareForAnimatedBoundsChange:"

-- | @Selector@ for @finalizeAnimatedBoundsChange@
finalizeAnimatedBoundsChangeSelector :: Selector
finalizeAnimatedBoundsChangeSelector = mkSelector "finalizeAnimatedBoundsChange"

-- | @Selector@ for @prepareForTransitionToLayout:@
prepareForTransitionToLayoutSelector :: Selector
prepareForTransitionToLayoutSelector = mkSelector "prepareForTransitionToLayout:"

-- | @Selector@ for @prepareForTransitionFromLayout:@
prepareForTransitionFromLayoutSelector :: Selector
prepareForTransitionFromLayoutSelector = mkSelector "prepareForTransitionFromLayout:"

-- | @Selector@ for @finalizeLayoutTransition@
finalizeLayoutTransitionSelector :: Selector
finalizeLayoutTransitionSelector = mkSelector "finalizeLayoutTransition"

-- | @Selector@ for @initialLayoutAttributesForAppearingItemAtIndexPath:@
initialLayoutAttributesForAppearingItemAtIndexPathSelector :: Selector
initialLayoutAttributesForAppearingItemAtIndexPathSelector = mkSelector "initialLayoutAttributesForAppearingItemAtIndexPath:"

-- | @Selector@ for @finalLayoutAttributesForDisappearingItemAtIndexPath:@
finalLayoutAttributesForDisappearingItemAtIndexPathSelector :: Selector
finalLayoutAttributesForDisappearingItemAtIndexPathSelector = mkSelector "finalLayoutAttributesForDisappearingItemAtIndexPath:"

-- | @Selector@ for @initialLayoutAttributesForAppearingSupplementaryElementOfKind:atIndexPath:@
initialLayoutAttributesForAppearingSupplementaryElementOfKind_atIndexPathSelector :: Selector
initialLayoutAttributesForAppearingSupplementaryElementOfKind_atIndexPathSelector = mkSelector "initialLayoutAttributesForAppearingSupplementaryElementOfKind:atIndexPath:"

-- | @Selector@ for @finalLayoutAttributesForDisappearingSupplementaryElementOfKind:atIndexPath:@
finalLayoutAttributesForDisappearingSupplementaryElementOfKind_atIndexPathSelector :: Selector
finalLayoutAttributesForDisappearingSupplementaryElementOfKind_atIndexPathSelector = mkSelector "finalLayoutAttributesForDisappearingSupplementaryElementOfKind:atIndexPath:"

-- | @Selector@ for @initialLayoutAttributesForAppearingDecorationElementOfKind:atIndexPath:@
initialLayoutAttributesForAppearingDecorationElementOfKind_atIndexPathSelector :: Selector
initialLayoutAttributesForAppearingDecorationElementOfKind_atIndexPathSelector = mkSelector "initialLayoutAttributesForAppearingDecorationElementOfKind:atIndexPath:"

-- | @Selector@ for @finalLayoutAttributesForDisappearingDecorationElementOfKind:atIndexPath:@
finalLayoutAttributesForDisappearingDecorationElementOfKind_atIndexPathSelector :: Selector
finalLayoutAttributesForDisappearingDecorationElementOfKind_atIndexPathSelector = mkSelector "finalLayoutAttributesForDisappearingDecorationElementOfKind:atIndexPath:"

-- | @Selector@ for @indexPathsToDeleteForSupplementaryViewOfKind:@
indexPathsToDeleteForSupplementaryViewOfKindSelector :: Selector
indexPathsToDeleteForSupplementaryViewOfKindSelector = mkSelector "indexPathsToDeleteForSupplementaryViewOfKind:"

-- | @Selector@ for @indexPathsToDeleteForDecorationViewOfKind:@
indexPathsToDeleteForDecorationViewOfKindSelector :: Selector
indexPathsToDeleteForDecorationViewOfKindSelector = mkSelector "indexPathsToDeleteForDecorationViewOfKind:"

-- | @Selector@ for @indexPathsToInsertForSupplementaryViewOfKind:@
indexPathsToInsertForSupplementaryViewOfKindSelector :: Selector
indexPathsToInsertForSupplementaryViewOfKindSelector = mkSelector "indexPathsToInsertForSupplementaryViewOfKind:"

-- | @Selector@ for @indexPathsToInsertForDecorationViewOfKind:@
indexPathsToInsertForDecorationViewOfKindSelector :: Selector
indexPathsToInsertForDecorationViewOfKindSelector = mkSelector "indexPathsToInsertForDecorationViewOfKind:"

-- | @Selector@ for @prepareLayout@
prepareLayoutSelector :: Selector
prepareLayoutSelector = mkSelector "prepareLayout"

-- | @Selector@ for @layoutAttributesForElementsInRect:@
layoutAttributesForElementsInRectSelector :: Selector
layoutAttributesForElementsInRectSelector = mkSelector "layoutAttributesForElementsInRect:"

-- | @Selector@ for @layoutAttributesForItemAtIndexPath:@
layoutAttributesForItemAtIndexPathSelector :: Selector
layoutAttributesForItemAtIndexPathSelector = mkSelector "layoutAttributesForItemAtIndexPath:"

-- | @Selector@ for @layoutAttributesForSupplementaryViewOfKind:atIndexPath:@
layoutAttributesForSupplementaryViewOfKind_atIndexPathSelector :: Selector
layoutAttributesForSupplementaryViewOfKind_atIndexPathSelector = mkSelector "layoutAttributesForSupplementaryViewOfKind:atIndexPath:"

-- | @Selector@ for @layoutAttributesForDecorationViewOfKind:atIndexPath:@
layoutAttributesForDecorationViewOfKind_atIndexPathSelector :: Selector
layoutAttributesForDecorationViewOfKind_atIndexPathSelector = mkSelector "layoutAttributesForDecorationViewOfKind:atIndexPath:"

-- | @Selector@ for @layoutAttributesForDropTargetAtPoint:@
layoutAttributesForDropTargetAtPointSelector :: Selector
layoutAttributesForDropTargetAtPointSelector = mkSelector "layoutAttributesForDropTargetAtPoint:"

-- | @Selector@ for @layoutAttributesForInterItemGapBeforeIndexPath:@
layoutAttributesForInterItemGapBeforeIndexPathSelector :: Selector
layoutAttributesForInterItemGapBeforeIndexPathSelector = mkSelector "layoutAttributesForInterItemGapBeforeIndexPath:"

-- | @Selector@ for @shouldInvalidateLayoutForBoundsChange:@
shouldInvalidateLayoutForBoundsChangeSelector :: Selector
shouldInvalidateLayoutForBoundsChangeSelector = mkSelector "shouldInvalidateLayoutForBoundsChange:"

-- | @Selector@ for @invalidationContextForBoundsChange:@
invalidationContextForBoundsChangeSelector :: Selector
invalidationContextForBoundsChangeSelector = mkSelector "invalidationContextForBoundsChange:"

-- | @Selector@ for @shouldInvalidateLayoutForPreferredLayoutAttributes:withOriginalAttributes:@
shouldInvalidateLayoutForPreferredLayoutAttributes_withOriginalAttributesSelector :: Selector
shouldInvalidateLayoutForPreferredLayoutAttributes_withOriginalAttributesSelector = mkSelector "shouldInvalidateLayoutForPreferredLayoutAttributes:withOriginalAttributes:"

-- | @Selector@ for @invalidationContextForPreferredLayoutAttributes:withOriginalAttributes:@
invalidationContextForPreferredLayoutAttributes_withOriginalAttributesSelector :: Selector
invalidationContextForPreferredLayoutAttributes_withOriginalAttributesSelector = mkSelector "invalidationContextForPreferredLayoutAttributes:withOriginalAttributes:"

-- | @Selector@ for @targetContentOffsetForProposedContentOffset:withScrollingVelocity:@
targetContentOffsetForProposedContentOffset_withScrollingVelocitySelector :: Selector
targetContentOffsetForProposedContentOffset_withScrollingVelocitySelector = mkSelector "targetContentOffsetForProposedContentOffset:withScrollingVelocity:"

-- | @Selector@ for @targetContentOffsetForProposedContentOffset:@
targetContentOffsetForProposedContentOffsetSelector :: Selector
targetContentOffsetForProposedContentOffsetSelector = mkSelector "targetContentOffsetForProposedContentOffset:"

-- | @Selector@ for @collectionView@
collectionViewSelector :: Selector
collectionViewSelector = mkSelector "collectionView"

-- | @Selector@ for @layoutAttributesClass@
layoutAttributesClassSelector :: Selector
layoutAttributesClassSelector = mkSelector "layoutAttributesClass"

-- | @Selector@ for @invalidationContextClass@
invalidationContextClassSelector :: Selector
invalidationContextClassSelector = mkSelector "invalidationContextClass"

-- | @Selector@ for @collectionViewContentSize@
collectionViewContentSizeSelector :: Selector
collectionViewContentSizeSelector = mkSelector "collectionViewContentSize"

