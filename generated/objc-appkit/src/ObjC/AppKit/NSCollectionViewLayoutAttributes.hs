{-# LANGUAGE PatternSynonyms #-}
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
  , layoutAttributesForItemWithIndexPathSelector
  , layoutAttributesForInterItemGapBeforeIndexPathSelector
  , layoutAttributesForSupplementaryViewOfKind_withIndexPathSelector
  , layoutAttributesForDecorationViewOfKind_withIndexPathSelector
  , frameSelector
  , setFrameSelector
  , sizeSelector
  , setSizeSelector
  , alphaSelector
  , setAlphaSelector
  , zIndexSelector
  , setZIndexSelector
  , hiddenSelector
  , setHiddenSelector
  , indexPathSelector
  , setIndexPathSelector
  , representedElementCategorySelector
  , representedElementKindSelector

  -- * Enum types
  , NSCollectionElementCategory(NSCollectionElementCategory)
  , pattern NSCollectionElementCategoryItem
  , pattern NSCollectionElementCategorySupplementaryView
  , pattern NSCollectionElementCategoryDecorationView
  , pattern NSCollectionElementCategoryInterItemGap

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
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ layoutAttributesForItemWithIndexPath:@
layoutAttributesForItemWithIndexPath :: IsNSIndexPath indexPath => indexPath -> IO (Id NSCollectionViewLayoutAttributes)
layoutAttributesForItemWithIndexPath indexPath =
  do
    cls' <- getRequiredClass "NSCollectionViewLayoutAttributes"
    withObjCPtr indexPath $ \raw_indexPath ->
      sendClassMsg cls' (mkSelector "layoutAttributesForItemWithIndexPath:") (retPtr retVoid) [argPtr (castPtr raw_indexPath :: Ptr ())] >>= retainedObject . castPtr

-- | @+ layoutAttributesForInterItemGapBeforeIndexPath:@
layoutAttributesForInterItemGapBeforeIndexPath :: IsNSIndexPath indexPath => indexPath -> IO (Id NSCollectionViewLayoutAttributes)
layoutAttributesForInterItemGapBeforeIndexPath indexPath =
  do
    cls' <- getRequiredClass "NSCollectionViewLayoutAttributes"
    withObjCPtr indexPath $ \raw_indexPath ->
      sendClassMsg cls' (mkSelector "layoutAttributesForInterItemGapBeforeIndexPath:") (retPtr retVoid) [argPtr (castPtr raw_indexPath :: Ptr ())] >>= retainedObject . castPtr

-- | @+ layoutAttributesForSupplementaryViewOfKind:withIndexPath:@
layoutAttributesForSupplementaryViewOfKind_withIndexPath :: (IsNSString elementKind, IsNSIndexPath indexPath) => elementKind -> indexPath -> IO (Id NSCollectionViewLayoutAttributes)
layoutAttributesForSupplementaryViewOfKind_withIndexPath elementKind indexPath =
  do
    cls' <- getRequiredClass "NSCollectionViewLayoutAttributes"
    withObjCPtr elementKind $ \raw_elementKind ->
      withObjCPtr indexPath $ \raw_indexPath ->
        sendClassMsg cls' (mkSelector "layoutAttributesForSupplementaryViewOfKind:withIndexPath:") (retPtr retVoid) [argPtr (castPtr raw_elementKind :: Ptr ()), argPtr (castPtr raw_indexPath :: Ptr ())] >>= retainedObject . castPtr

-- | @+ layoutAttributesForDecorationViewOfKind:withIndexPath:@
layoutAttributesForDecorationViewOfKind_withIndexPath :: (IsNSString decorationViewKind, IsNSIndexPath indexPath) => decorationViewKind -> indexPath -> IO (Id NSCollectionViewLayoutAttributes)
layoutAttributesForDecorationViewOfKind_withIndexPath decorationViewKind indexPath =
  do
    cls' <- getRequiredClass "NSCollectionViewLayoutAttributes"
    withObjCPtr decorationViewKind $ \raw_decorationViewKind ->
      withObjCPtr indexPath $ \raw_indexPath ->
        sendClassMsg cls' (mkSelector "layoutAttributesForDecorationViewOfKind:withIndexPath:") (retPtr retVoid) [argPtr (castPtr raw_decorationViewKind :: Ptr ()), argPtr (castPtr raw_indexPath :: Ptr ())] >>= retainedObject . castPtr

-- | @- frame@
frame :: IsNSCollectionViewLayoutAttributes nsCollectionViewLayoutAttributes => nsCollectionViewLayoutAttributes -> IO NSRect
frame nsCollectionViewLayoutAttributes  =
  sendMsgStret nsCollectionViewLayoutAttributes (mkSelector "frame") retNSRect []

-- | @- setFrame:@
setFrame :: IsNSCollectionViewLayoutAttributes nsCollectionViewLayoutAttributes => nsCollectionViewLayoutAttributes -> NSRect -> IO ()
setFrame nsCollectionViewLayoutAttributes  value =
  sendMsg nsCollectionViewLayoutAttributes (mkSelector "setFrame:") retVoid [argNSRect value]

-- | @- size@
size :: IsNSCollectionViewLayoutAttributes nsCollectionViewLayoutAttributes => nsCollectionViewLayoutAttributes -> IO NSSize
size nsCollectionViewLayoutAttributes  =
  sendMsgStret nsCollectionViewLayoutAttributes (mkSelector "size") retNSSize []

-- | @- setSize:@
setSize :: IsNSCollectionViewLayoutAttributes nsCollectionViewLayoutAttributes => nsCollectionViewLayoutAttributes -> NSSize -> IO ()
setSize nsCollectionViewLayoutAttributes  value =
  sendMsg nsCollectionViewLayoutAttributes (mkSelector "setSize:") retVoid [argNSSize value]

-- | @- alpha@
alpha :: IsNSCollectionViewLayoutAttributes nsCollectionViewLayoutAttributes => nsCollectionViewLayoutAttributes -> IO CDouble
alpha nsCollectionViewLayoutAttributes  =
  sendMsg nsCollectionViewLayoutAttributes (mkSelector "alpha") retCDouble []

-- | @- setAlpha:@
setAlpha :: IsNSCollectionViewLayoutAttributes nsCollectionViewLayoutAttributes => nsCollectionViewLayoutAttributes -> CDouble -> IO ()
setAlpha nsCollectionViewLayoutAttributes  value =
  sendMsg nsCollectionViewLayoutAttributes (mkSelector "setAlpha:") retVoid [argCDouble (fromIntegral value)]

-- | @- zIndex@
zIndex :: IsNSCollectionViewLayoutAttributes nsCollectionViewLayoutAttributes => nsCollectionViewLayoutAttributes -> IO CLong
zIndex nsCollectionViewLayoutAttributes  =
  sendMsg nsCollectionViewLayoutAttributes (mkSelector "zIndex") retCLong []

-- | @- setZIndex:@
setZIndex :: IsNSCollectionViewLayoutAttributes nsCollectionViewLayoutAttributes => nsCollectionViewLayoutAttributes -> CLong -> IO ()
setZIndex nsCollectionViewLayoutAttributes  value =
  sendMsg nsCollectionViewLayoutAttributes (mkSelector "setZIndex:") retVoid [argCLong (fromIntegral value)]

-- | @- hidden@
hidden :: IsNSCollectionViewLayoutAttributes nsCollectionViewLayoutAttributes => nsCollectionViewLayoutAttributes -> IO Bool
hidden nsCollectionViewLayoutAttributes  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCollectionViewLayoutAttributes (mkSelector "hidden") retCULong []

-- | @- setHidden:@
setHidden :: IsNSCollectionViewLayoutAttributes nsCollectionViewLayoutAttributes => nsCollectionViewLayoutAttributes -> Bool -> IO ()
setHidden nsCollectionViewLayoutAttributes  value =
  sendMsg nsCollectionViewLayoutAttributes (mkSelector "setHidden:") retVoid [argCULong (if value then 1 else 0)]

-- | @- indexPath@
indexPath :: IsNSCollectionViewLayoutAttributes nsCollectionViewLayoutAttributes => nsCollectionViewLayoutAttributes -> IO (Id NSIndexPath)
indexPath nsCollectionViewLayoutAttributes  =
  sendMsg nsCollectionViewLayoutAttributes (mkSelector "indexPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIndexPath:@
setIndexPath :: (IsNSCollectionViewLayoutAttributes nsCollectionViewLayoutAttributes, IsNSIndexPath value) => nsCollectionViewLayoutAttributes -> value -> IO ()
setIndexPath nsCollectionViewLayoutAttributes  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsCollectionViewLayoutAttributes (mkSelector "setIndexPath:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- representedElementCategory@
representedElementCategory :: IsNSCollectionViewLayoutAttributes nsCollectionViewLayoutAttributes => nsCollectionViewLayoutAttributes -> IO NSCollectionElementCategory
representedElementCategory nsCollectionViewLayoutAttributes  =
  fmap (coerce :: CLong -> NSCollectionElementCategory) $ sendMsg nsCollectionViewLayoutAttributes (mkSelector "representedElementCategory") retCLong []

-- | @- representedElementKind@
representedElementKind :: IsNSCollectionViewLayoutAttributes nsCollectionViewLayoutAttributes => nsCollectionViewLayoutAttributes -> IO (Id NSString)
representedElementKind nsCollectionViewLayoutAttributes  =
  sendMsg nsCollectionViewLayoutAttributes (mkSelector "representedElementKind") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layoutAttributesForItemWithIndexPath:@
layoutAttributesForItemWithIndexPathSelector :: Selector
layoutAttributesForItemWithIndexPathSelector = mkSelector "layoutAttributesForItemWithIndexPath:"

-- | @Selector@ for @layoutAttributesForInterItemGapBeforeIndexPath:@
layoutAttributesForInterItemGapBeforeIndexPathSelector :: Selector
layoutAttributesForInterItemGapBeforeIndexPathSelector = mkSelector "layoutAttributesForInterItemGapBeforeIndexPath:"

-- | @Selector@ for @layoutAttributesForSupplementaryViewOfKind:withIndexPath:@
layoutAttributesForSupplementaryViewOfKind_withIndexPathSelector :: Selector
layoutAttributesForSupplementaryViewOfKind_withIndexPathSelector = mkSelector "layoutAttributesForSupplementaryViewOfKind:withIndexPath:"

-- | @Selector@ for @layoutAttributesForDecorationViewOfKind:withIndexPath:@
layoutAttributesForDecorationViewOfKind_withIndexPathSelector :: Selector
layoutAttributesForDecorationViewOfKind_withIndexPathSelector = mkSelector "layoutAttributesForDecorationViewOfKind:withIndexPath:"

-- | @Selector@ for @frame@
frameSelector :: Selector
frameSelector = mkSelector "frame"

-- | @Selector@ for @setFrame:@
setFrameSelector :: Selector
setFrameSelector = mkSelector "setFrame:"

-- | @Selector@ for @size@
sizeSelector :: Selector
sizeSelector = mkSelector "size"

-- | @Selector@ for @setSize:@
setSizeSelector :: Selector
setSizeSelector = mkSelector "setSize:"

-- | @Selector@ for @alpha@
alphaSelector :: Selector
alphaSelector = mkSelector "alpha"

-- | @Selector@ for @setAlpha:@
setAlphaSelector :: Selector
setAlphaSelector = mkSelector "setAlpha:"

-- | @Selector@ for @zIndex@
zIndexSelector :: Selector
zIndexSelector = mkSelector "zIndex"

-- | @Selector@ for @setZIndex:@
setZIndexSelector :: Selector
setZIndexSelector = mkSelector "setZIndex:"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector
hiddenSelector = mkSelector "hidden"

-- | @Selector@ for @setHidden:@
setHiddenSelector :: Selector
setHiddenSelector = mkSelector "setHidden:"

-- | @Selector@ for @indexPath@
indexPathSelector :: Selector
indexPathSelector = mkSelector "indexPath"

-- | @Selector@ for @setIndexPath:@
setIndexPathSelector :: Selector
setIndexPathSelector = mkSelector "setIndexPath:"

-- | @Selector@ for @representedElementCategory@
representedElementCategorySelector :: Selector
representedElementCategorySelector = mkSelector "representedElementCategory"

-- | @Selector@ for @representedElementKind@
representedElementKindSelector :: Selector
representedElementKindSelector = mkSelector "representedElementKind"

