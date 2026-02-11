{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCollectionViewFlowLayout@.
module ObjC.AppKit.NSCollectionViewFlowLayout
  ( NSCollectionViewFlowLayout
  , IsNSCollectionViewFlowLayout(..)
  , sectionAtIndexIsCollapsed
  , collapseSectionAtIndex
  , expandSectionAtIndex
  , minimumLineSpacing
  , setMinimumLineSpacing
  , minimumInteritemSpacing
  , setMinimumInteritemSpacing
  , itemSize
  , setItemSize
  , estimatedItemSize
  , setEstimatedItemSize
  , scrollDirection
  , setScrollDirection
  , headerReferenceSize
  , setHeaderReferenceSize
  , footerReferenceSize
  , setFooterReferenceSize
  , sectionInset
  , setSectionInset
  , sectionHeadersPinToVisibleBounds
  , setSectionHeadersPinToVisibleBounds
  , sectionFootersPinToVisibleBounds
  , setSectionFootersPinToVisibleBounds
  , sectionAtIndexIsCollapsedSelector
  , collapseSectionAtIndexSelector
  , expandSectionAtIndexSelector
  , minimumLineSpacingSelector
  , setMinimumLineSpacingSelector
  , minimumInteritemSpacingSelector
  , setMinimumInteritemSpacingSelector
  , itemSizeSelector
  , setItemSizeSelector
  , estimatedItemSizeSelector
  , setEstimatedItemSizeSelector
  , scrollDirectionSelector
  , setScrollDirectionSelector
  , headerReferenceSizeSelector
  , setHeaderReferenceSizeSelector
  , footerReferenceSizeSelector
  , setFooterReferenceSizeSelector
  , sectionInsetSelector
  , setSectionInsetSelector
  , sectionHeadersPinToVisibleBoundsSelector
  , setSectionHeadersPinToVisibleBoundsSelector
  , sectionFootersPinToVisibleBoundsSelector
  , setSectionFootersPinToVisibleBoundsSelector

  -- * Enum types
  , NSCollectionViewScrollDirection(NSCollectionViewScrollDirection)
  , pattern NSCollectionViewScrollDirectionVertical
  , pattern NSCollectionViewScrollDirectionHorizontal

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

-- | @- sectionAtIndexIsCollapsed:@
sectionAtIndexIsCollapsed :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> CULong -> IO Bool
sectionAtIndexIsCollapsed nsCollectionViewFlowLayout  sectionIndex =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCollectionViewFlowLayout (mkSelector "sectionAtIndexIsCollapsed:") retCULong [argCULong (fromIntegral sectionIndex)]

-- | @- collapseSectionAtIndex:@
collapseSectionAtIndex :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> CULong -> IO ()
collapseSectionAtIndex nsCollectionViewFlowLayout  sectionIndex =
  sendMsg nsCollectionViewFlowLayout (mkSelector "collapseSectionAtIndex:") retVoid [argCULong (fromIntegral sectionIndex)]

-- | @- expandSectionAtIndex:@
expandSectionAtIndex :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> CULong -> IO ()
expandSectionAtIndex nsCollectionViewFlowLayout  sectionIndex =
  sendMsg nsCollectionViewFlowLayout (mkSelector "expandSectionAtIndex:") retVoid [argCULong (fromIntegral sectionIndex)]

-- | @- minimumLineSpacing@
minimumLineSpacing :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> IO CDouble
minimumLineSpacing nsCollectionViewFlowLayout  =
  sendMsg nsCollectionViewFlowLayout (mkSelector "minimumLineSpacing") retCDouble []

-- | @- setMinimumLineSpacing:@
setMinimumLineSpacing :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> CDouble -> IO ()
setMinimumLineSpacing nsCollectionViewFlowLayout  value =
  sendMsg nsCollectionViewFlowLayout (mkSelector "setMinimumLineSpacing:") retVoid [argCDouble (fromIntegral value)]

-- | @- minimumInteritemSpacing@
minimumInteritemSpacing :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> IO CDouble
minimumInteritemSpacing nsCollectionViewFlowLayout  =
  sendMsg nsCollectionViewFlowLayout (mkSelector "minimumInteritemSpacing") retCDouble []

-- | @- setMinimumInteritemSpacing:@
setMinimumInteritemSpacing :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> CDouble -> IO ()
setMinimumInteritemSpacing nsCollectionViewFlowLayout  value =
  sendMsg nsCollectionViewFlowLayout (mkSelector "setMinimumInteritemSpacing:") retVoid [argCDouble (fromIntegral value)]

-- | @- itemSize@
itemSize :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> IO NSSize
itemSize nsCollectionViewFlowLayout  =
  sendMsgStret nsCollectionViewFlowLayout (mkSelector "itemSize") retNSSize []

-- | @- setItemSize:@
setItemSize :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> NSSize -> IO ()
setItemSize nsCollectionViewFlowLayout  value =
  sendMsg nsCollectionViewFlowLayout (mkSelector "setItemSize:") retVoid [argNSSize value]

-- | @- estimatedItemSize@
estimatedItemSize :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> IO NSSize
estimatedItemSize nsCollectionViewFlowLayout  =
  sendMsgStret nsCollectionViewFlowLayout (mkSelector "estimatedItemSize") retNSSize []

-- | @- setEstimatedItemSize:@
setEstimatedItemSize :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> NSSize -> IO ()
setEstimatedItemSize nsCollectionViewFlowLayout  value =
  sendMsg nsCollectionViewFlowLayout (mkSelector "setEstimatedItemSize:") retVoid [argNSSize value]

-- | @- scrollDirection@
scrollDirection :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> IO NSCollectionViewScrollDirection
scrollDirection nsCollectionViewFlowLayout  =
  fmap (coerce :: CLong -> NSCollectionViewScrollDirection) $ sendMsg nsCollectionViewFlowLayout (mkSelector "scrollDirection") retCLong []

-- | @- setScrollDirection:@
setScrollDirection :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> NSCollectionViewScrollDirection -> IO ()
setScrollDirection nsCollectionViewFlowLayout  value =
  sendMsg nsCollectionViewFlowLayout (mkSelector "setScrollDirection:") retVoid [argCLong (coerce value)]

-- | @- headerReferenceSize@
headerReferenceSize :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> IO NSSize
headerReferenceSize nsCollectionViewFlowLayout  =
  sendMsgStret nsCollectionViewFlowLayout (mkSelector "headerReferenceSize") retNSSize []

-- | @- setHeaderReferenceSize:@
setHeaderReferenceSize :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> NSSize -> IO ()
setHeaderReferenceSize nsCollectionViewFlowLayout  value =
  sendMsg nsCollectionViewFlowLayout (mkSelector "setHeaderReferenceSize:") retVoid [argNSSize value]

-- | @- footerReferenceSize@
footerReferenceSize :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> IO NSSize
footerReferenceSize nsCollectionViewFlowLayout  =
  sendMsgStret nsCollectionViewFlowLayout (mkSelector "footerReferenceSize") retNSSize []

-- | @- setFooterReferenceSize:@
setFooterReferenceSize :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> NSSize -> IO ()
setFooterReferenceSize nsCollectionViewFlowLayout  value =
  sendMsg nsCollectionViewFlowLayout (mkSelector "setFooterReferenceSize:") retVoid [argNSSize value]

-- | @- sectionInset@
sectionInset :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> IO NSEdgeInsets
sectionInset nsCollectionViewFlowLayout  =
  sendMsgStret nsCollectionViewFlowLayout (mkSelector "sectionInset") retNSEdgeInsets []

-- | @- setSectionInset:@
setSectionInset :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> NSEdgeInsets -> IO ()
setSectionInset nsCollectionViewFlowLayout  value =
  sendMsg nsCollectionViewFlowLayout (mkSelector "setSectionInset:") retVoid [argNSEdgeInsets value]

-- | @- sectionHeadersPinToVisibleBounds@
sectionHeadersPinToVisibleBounds :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> IO Bool
sectionHeadersPinToVisibleBounds nsCollectionViewFlowLayout  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCollectionViewFlowLayout (mkSelector "sectionHeadersPinToVisibleBounds") retCULong []

-- | @- setSectionHeadersPinToVisibleBounds:@
setSectionHeadersPinToVisibleBounds :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> Bool -> IO ()
setSectionHeadersPinToVisibleBounds nsCollectionViewFlowLayout  value =
  sendMsg nsCollectionViewFlowLayout (mkSelector "setSectionHeadersPinToVisibleBounds:") retVoid [argCULong (if value then 1 else 0)]

-- | @- sectionFootersPinToVisibleBounds@
sectionFootersPinToVisibleBounds :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> IO Bool
sectionFootersPinToVisibleBounds nsCollectionViewFlowLayout  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCollectionViewFlowLayout (mkSelector "sectionFootersPinToVisibleBounds") retCULong []

-- | @- setSectionFootersPinToVisibleBounds:@
setSectionFootersPinToVisibleBounds :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> Bool -> IO ()
setSectionFootersPinToVisibleBounds nsCollectionViewFlowLayout  value =
  sendMsg nsCollectionViewFlowLayout (mkSelector "setSectionFootersPinToVisibleBounds:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sectionAtIndexIsCollapsed:@
sectionAtIndexIsCollapsedSelector :: Selector
sectionAtIndexIsCollapsedSelector = mkSelector "sectionAtIndexIsCollapsed:"

-- | @Selector@ for @collapseSectionAtIndex:@
collapseSectionAtIndexSelector :: Selector
collapseSectionAtIndexSelector = mkSelector "collapseSectionAtIndex:"

-- | @Selector@ for @expandSectionAtIndex:@
expandSectionAtIndexSelector :: Selector
expandSectionAtIndexSelector = mkSelector "expandSectionAtIndex:"

-- | @Selector@ for @minimumLineSpacing@
minimumLineSpacingSelector :: Selector
minimumLineSpacingSelector = mkSelector "minimumLineSpacing"

-- | @Selector@ for @setMinimumLineSpacing:@
setMinimumLineSpacingSelector :: Selector
setMinimumLineSpacingSelector = mkSelector "setMinimumLineSpacing:"

-- | @Selector@ for @minimumInteritemSpacing@
minimumInteritemSpacingSelector :: Selector
minimumInteritemSpacingSelector = mkSelector "minimumInteritemSpacing"

-- | @Selector@ for @setMinimumInteritemSpacing:@
setMinimumInteritemSpacingSelector :: Selector
setMinimumInteritemSpacingSelector = mkSelector "setMinimumInteritemSpacing:"

-- | @Selector@ for @itemSize@
itemSizeSelector :: Selector
itemSizeSelector = mkSelector "itemSize"

-- | @Selector@ for @setItemSize:@
setItemSizeSelector :: Selector
setItemSizeSelector = mkSelector "setItemSize:"

-- | @Selector@ for @estimatedItemSize@
estimatedItemSizeSelector :: Selector
estimatedItemSizeSelector = mkSelector "estimatedItemSize"

-- | @Selector@ for @setEstimatedItemSize:@
setEstimatedItemSizeSelector :: Selector
setEstimatedItemSizeSelector = mkSelector "setEstimatedItemSize:"

-- | @Selector@ for @scrollDirection@
scrollDirectionSelector :: Selector
scrollDirectionSelector = mkSelector "scrollDirection"

-- | @Selector@ for @setScrollDirection:@
setScrollDirectionSelector :: Selector
setScrollDirectionSelector = mkSelector "setScrollDirection:"

-- | @Selector@ for @headerReferenceSize@
headerReferenceSizeSelector :: Selector
headerReferenceSizeSelector = mkSelector "headerReferenceSize"

-- | @Selector@ for @setHeaderReferenceSize:@
setHeaderReferenceSizeSelector :: Selector
setHeaderReferenceSizeSelector = mkSelector "setHeaderReferenceSize:"

-- | @Selector@ for @footerReferenceSize@
footerReferenceSizeSelector :: Selector
footerReferenceSizeSelector = mkSelector "footerReferenceSize"

-- | @Selector@ for @setFooterReferenceSize:@
setFooterReferenceSizeSelector :: Selector
setFooterReferenceSizeSelector = mkSelector "setFooterReferenceSize:"

-- | @Selector@ for @sectionInset@
sectionInsetSelector :: Selector
sectionInsetSelector = mkSelector "sectionInset"

-- | @Selector@ for @setSectionInset:@
setSectionInsetSelector :: Selector
setSectionInsetSelector = mkSelector "setSectionInset:"

-- | @Selector@ for @sectionHeadersPinToVisibleBounds@
sectionHeadersPinToVisibleBoundsSelector :: Selector
sectionHeadersPinToVisibleBoundsSelector = mkSelector "sectionHeadersPinToVisibleBounds"

-- | @Selector@ for @setSectionHeadersPinToVisibleBounds:@
setSectionHeadersPinToVisibleBoundsSelector :: Selector
setSectionHeadersPinToVisibleBoundsSelector = mkSelector "setSectionHeadersPinToVisibleBounds:"

-- | @Selector@ for @sectionFootersPinToVisibleBounds@
sectionFootersPinToVisibleBoundsSelector :: Selector
sectionFootersPinToVisibleBoundsSelector = mkSelector "sectionFootersPinToVisibleBounds"

-- | @Selector@ for @setSectionFootersPinToVisibleBounds:@
setSectionFootersPinToVisibleBoundsSelector :: Selector
setSectionFootersPinToVisibleBoundsSelector = mkSelector "setSectionFootersPinToVisibleBounds:"

