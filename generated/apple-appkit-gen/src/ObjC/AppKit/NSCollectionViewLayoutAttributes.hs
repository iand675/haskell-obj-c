{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCollectionViewLayoutAttributes@.
module ObjC.AppKit.NSCollectionViewLayoutAttributes
  ( NSCollectionViewLayoutAttributes
  , IsNSCollectionViewLayoutAttributes(..)
  , layoutAttributesForItemWithIndexPath
  , layoutAttributesForInterItemGapBeforeIndexPath
  , layoutAttributesForSupplementaryViewOfKind_withIndexPath
  , layoutAttributesForDecorationViewOfKind_withIndexPath
  , frame
  , setFrame
  , size
  , setSize
  , alpha
  , setAlpha
  , zIndex
  , setZIndex
  , hidden
  , setHidden
  , indexPath
  , setIndexPath
  , representedElementCategory
  , representedElementKind
  , alphaSelector
  , frameSelector
  , hiddenSelector
  , indexPathSelector
  , layoutAttributesForDecorationViewOfKind_withIndexPathSelector
  , layoutAttributesForInterItemGapBeforeIndexPathSelector
  , layoutAttributesForItemWithIndexPathSelector
  , layoutAttributesForSupplementaryViewOfKind_withIndexPathSelector
  , representedElementCategorySelector
  , representedElementKindSelector
  , setAlphaSelector
  , setFrameSelector
  , setHiddenSelector
  , setIndexPathSelector
  , setSizeSelector
  , setZIndexSelector
  , sizeSelector
  , zIndexSelector

  -- * Enum types
  , NSCollectionElementCategory(NSCollectionElementCategory)
  , pattern NSCollectionElementCategoryItem
  , pattern NSCollectionElementCategorySupplementaryView
  , pattern NSCollectionElementCategoryDecorationView
  , pattern NSCollectionElementCategoryInterItemGap

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ layoutAttributesForItemWithIndexPath:@
layoutAttributesForItemWithIndexPath :: IsNSIndexPath indexPath => indexPath -> IO (Id NSCollectionViewLayoutAttributes)
layoutAttributesForItemWithIndexPath indexPath =
  do
    cls' <- getRequiredClass "NSCollectionViewLayoutAttributes"
    sendClassMessage cls' layoutAttributesForItemWithIndexPathSelector (toNSIndexPath indexPath)

-- | @+ layoutAttributesForInterItemGapBeforeIndexPath:@
layoutAttributesForInterItemGapBeforeIndexPath :: IsNSIndexPath indexPath => indexPath -> IO (Id NSCollectionViewLayoutAttributes)
layoutAttributesForInterItemGapBeforeIndexPath indexPath =
  do
    cls' <- getRequiredClass "NSCollectionViewLayoutAttributes"
    sendClassMessage cls' layoutAttributesForInterItemGapBeforeIndexPathSelector (toNSIndexPath indexPath)

-- | @+ layoutAttributesForSupplementaryViewOfKind:withIndexPath:@
layoutAttributesForSupplementaryViewOfKind_withIndexPath :: (IsNSString elementKind, IsNSIndexPath indexPath) => elementKind -> indexPath -> IO (Id NSCollectionViewLayoutAttributes)
layoutAttributesForSupplementaryViewOfKind_withIndexPath elementKind indexPath =
  do
    cls' <- getRequiredClass "NSCollectionViewLayoutAttributes"
    sendClassMessage cls' layoutAttributesForSupplementaryViewOfKind_withIndexPathSelector (toNSString elementKind) (toNSIndexPath indexPath)

-- | @+ layoutAttributesForDecorationViewOfKind:withIndexPath:@
layoutAttributesForDecorationViewOfKind_withIndexPath :: (IsNSString decorationViewKind, IsNSIndexPath indexPath) => decorationViewKind -> indexPath -> IO (Id NSCollectionViewLayoutAttributes)
layoutAttributesForDecorationViewOfKind_withIndexPath decorationViewKind indexPath =
  do
    cls' <- getRequiredClass "NSCollectionViewLayoutAttributes"
    sendClassMessage cls' layoutAttributesForDecorationViewOfKind_withIndexPathSelector (toNSString decorationViewKind) (toNSIndexPath indexPath)

-- | @- frame@
frame :: IsNSCollectionViewLayoutAttributes nsCollectionViewLayoutAttributes => nsCollectionViewLayoutAttributes -> IO NSRect
frame nsCollectionViewLayoutAttributes =
  sendMessage nsCollectionViewLayoutAttributes frameSelector

-- | @- setFrame:@
setFrame :: IsNSCollectionViewLayoutAttributes nsCollectionViewLayoutAttributes => nsCollectionViewLayoutAttributes -> NSRect -> IO ()
setFrame nsCollectionViewLayoutAttributes value =
  sendMessage nsCollectionViewLayoutAttributes setFrameSelector value

-- | @- size@
size :: IsNSCollectionViewLayoutAttributes nsCollectionViewLayoutAttributes => nsCollectionViewLayoutAttributes -> IO NSSize
size nsCollectionViewLayoutAttributes =
  sendMessage nsCollectionViewLayoutAttributes sizeSelector

-- | @- setSize:@
setSize :: IsNSCollectionViewLayoutAttributes nsCollectionViewLayoutAttributes => nsCollectionViewLayoutAttributes -> NSSize -> IO ()
setSize nsCollectionViewLayoutAttributes value =
  sendMessage nsCollectionViewLayoutAttributes setSizeSelector value

-- | @- alpha@
alpha :: IsNSCollectionViewLayoutAttributes nsCollectionViewLayoutAttributes => nsCollectionViewLayoutAttributes -> IO CDouble
alpha nsCollectionViewLayoutAttributes =
  sendMessage nsCollectionViewLayoutAttributes alphaSelector

-- | @- setAlpha:@
setAlpha :: IsNSCollectionViewLayoutAttributes nsCollectionViewLayoutAttributes => nsCollectionViewLayoutAttributes -> CDouble -> IO ()
setAlpha nsCollectionViewLayoutAttributes value =
  sendMessage nsCollectionViewLayoutAttributes setAlphaSelector value

-- | @- zIndex@
zIndex :: IsNSCollectionViewLayoutAttributes nsCollectionViewLayoutAttributes => nsCollectionViewLayoutAttributes -> IO CLong
zIndex nsCollectionViewLayoutAttributes =
  sendMessage nsCollectionViewLayoutAttributes zIndexSelector

-- | @- setZIndex:@
setZIndex :: IsNSCollectionViewLayoutAttributes nsCollectionViewLayoutAttributes => nsCollectionViewLayoutAttributes -> CLong -> IO ()
setZIndex nsCollectionViewLayoutAttributes value =
  sendMessage nsCollectionViewLayoutAttributes setZIndexSelector value

-- | @- hidden@
hidden :: IsNSCollectionViewLayoutAttributes nsCollectionViewLayoutAttributes => nsCollectionViewLayoutAttributes -> IO Bool
hidden nsCollectionViewLayoutAttributes =
  sendMessage nsCollectionViewLayoutAttributes hiddenSelector

-- | @- setHidden:@
setHidden :: IsNSCollectionViewLayoutAttributes nsCollectionViewLayoutAttributes => nsCollectionViewLayoutAttributes -> Bool -> IO ()
setHidden nsCollectionViewLayoutAttributes value =
  sendMessage nsCollectionViewLayoutAttributes setHiddenSelector value

-- | @- indexPath@
indexPath :: IsNSCollectionViewLayoutAttributes nsCollectionViewLayoutAttributes => nsCollectionViewLayoutAttributes -> IO (Id NSIndexPath)
indexPath nsCollectionViewLayoutAttributes =
  sendMessage nsCollectionViewLayoutAttributes indexPathSelector

-- | @- setIndexPath:@
setIndexPath :: (IsNSCollectionViewLayoutAttributes nsCollectionViewLayoutAttributes, IsNSIndexPath value) => nsCollectionViewLayoutAttributes -> value -> IO ()
setIndexPath nsCollectionViewLayoutAttributes value =
  sendMessage nsCollectionViewLayoutAttributes setIndexPathSelector (toNSIndexPath value)

-- | @- representedElementCategory@
representedElementCategory :: IsNSCollectionViewLayoutAttributes nsCollectionViewLayoutAttributes => nsCollectionViewLayoutAttributes -> IO NSCollectionElementCategory
representedElementCategory nsCollectionViewLayoutAttributes =
  sendMessage nsCollectionViewLayoutAttributes representedElementCategorySelector

-- | @- representedElementKind@
representedElementKind :: IsNSCollectionViewLayoutAttributes nsCollectionViewLayoutAttributes => nsCollectionViewLayoutAttributes -> IO (Id NSString)
representedElementKind nsCollectionViewLayoutAttributes =
  sendMessage nsCollectionViewLayoutAttributes representedElementKindSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layoutAttributesForItemWithIndexPath:@
layoutAttributesForItemWithIndexPathSelector :: Selector '[Id NSIndexPath] (Id NSCollectionViewLayoutAttributes)
layoutAttributesForItemWithIndexPathSelector = mkSelector "layoutAttributesForItemWithIndexPath:"

-- | @Selector@ for @layoutAttributesForInterItemGapBeforeIndexPath:@
layoutAttributesForInterItemGapBeforeIndexPathSelector :: Selector '[Id NSIndexPath] (Id NSCollectionViewLayoutAttributes)
layoutAttributesForInterItemGapBeforeIndexPathSelector = mkSelector "layoutAttributesForInterItemGapBeforeIndexPath:"

-- | @Selector@ for @layoutAttributesForSupplementaryViewOfKind:withIndexPath:@
layoutAttributesForSupplementaryViewOfKind_withIndexPathSelector :: Selector '[Id NSString, Id NSIndexPath] (Id NSCollectionViewLayoutAttributes)
layoutAttributesForSupplementaryViewOfKind_withIndexPathSelector = mkSelector "layoutAttributesForSupplementaryViewOfKind:withIndexPath:"

-- | @Selector@ for @layoutAttributesForDecorationViewOfKind:withIndexPath:@
layoutAttributesForDecorationViewOfKind_withIndexPathSelector :: Selector '[Id NSString, Id NSIndexPath] (Id NSCollectionViewLayoutAttributes)
layoutAttributesForDecorationViewOfKind_withIndexPathSelector = mkSelector "layoutAttributesForDecorationViewOfKind:withIndexPath:"

-- | @Selector@ for @frame@
frameSelector :: Selector '[] NSRect
frameSelector = mkSelector "frame"

-- | @Selector@ for @setFrame:@
setFrameSelector :: Selector '[NSRect] ()
setFrameSelector = mkSelector "setFrame:"

-- | @Selector@ for @size@
sizeSelector :: Selector '[] NSSize
sizeSelector = mkSelector "size"

-- | @Selector@ for @setSize:@
setSizeSelector :: Selector '[NSSize] ()
setSizeSelector = mkSelector "setSize:"

-- | @Selector@ for @alpha@
alphaSelector :: Selector '[] CDouble
alphaSelector = mkSelector "alpha"

-- | @Selector@ for @setAlpha:@
setAlphaSelector :: Selector '[CDouble] ()
setAlphaSelector = mkSelector "setAlpha:"

-- | @Selector@ for @zIndex@
zIndexSelector :: Selector '[] CLong
zIndexSelector = mkSelector "zIndex"

-- | @Selector@ for @setZIndex:@
setZIndexSelector :: Selector '[CLong] ()
setZIndexSelector = mkSelector "setZIndex:"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector '[] Bool
hiddenSelector = mkSelector "hidden"

-- | @Selector@ for @setHidden:@
setHiddenSelector :: Selector '[Bool] ()
setHiddenSelector = mkSelector "setHidden:"

-- | @Selector@ for @indexPath@
indexPathSelector :: Selector '[] (Id NSIndexPath)
indexPathSelector = mkSelector "indexPath"

-- | @Selector@ for @setIndexPath:@
setIndexPathSelector :: Selector '[Id NSIndexPath] ()
setIndexPathSelector = mkSelector "setIndexPath:"

-- | @Selector@ for @representedElementCategory@
representedElementCategorySelector :: Selector '[] NSCollectionElementCategory
representedElementCategorySelector = mkSelector "representedElementCategory"

-- | @Selector@ for @representedElementKind@
representedElementKindSelector :: Selector '[] (Id NSString)
representedElementKindSelector = mkSelector "representedElementKind"

