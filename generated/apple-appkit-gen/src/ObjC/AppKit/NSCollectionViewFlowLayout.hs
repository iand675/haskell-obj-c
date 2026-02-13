{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , collapseSectionAtIndexSelector
  , estimatedItemSizeSelector
  , expandSectionAtIndexSelector
  , footerReferenceSizeSelector
  , headerReferenceSizeSelector
  , itemSizeSelector
  , minimumInteritemSpacingSelector
  , minimumLineSpacingSelector
  , scrollDirectionSelector
  , sectionAtIndexIsCollapsedSelector
  , sectionFootersPinToVisibleBoundsSelector
  , sectionHeadersPinToVisibleBoundsSelector
  , sectionInsetSelector
  , setEstimatedItemSizeSelector
  , setFooterReferenceSizeSelector
  , setHeaderReferenceSizeSelector
  , setItemSizeSelector
  , setMinimumInteritemSpacingSelector
  , setMinimumLineSpacingSelector
  , setScrollDirectionSelector
  , setSectionFootersPinToVisibleBoundsSelector
  , setSectionHeadersPinToVisibleBoundsSelector
  , setSectionInsetSelector

  -- * Enum types
  , NSCollectionViewScrollDirection(NSCollectionViewScrollDirection)
  , pattern NSCollectionViewScrollDirectionVertical
  , pattern NSCollectionViewScrollDirectionHorizontal

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

-- | @- sectionAtIndexIsCollapsed:@
sectionAtIndexIsCollapsed :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> CULong -> IO Bool
sectionAtIndexIsCollapsed nsCollectionViewFlowLayout sectionIndex =
  sendMessage nsCollectionViewFlowLayout sectionAtIndexIsCollapsedSelector sectionIndex

-- | @- collapseSectionAtIndex:@
collapseSectionAtIndex :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> CULong -> IO ()
collapseSectionAtIndex nsCollectionViewFlowLayout sectionIndex =
  sendMessage nsCollectionViewFlowLayout collapseSectionAtIndexSelector sectionIndex

-- | @- expandSectionAtIndex:@
expandSectionAtIndex :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> CULong -> IO ()
expandSectionAtIndex nsCollectionViewFlowLayout sectionIndex =
  sendMessage nsCollectionViewFlowLayout expandSectionAtIndexSelector sectionIndex

-- | @- minimumLineSpacing@
minimumLineSpacing :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> IO CDouble
minimumLineSpacing nsCollectionViewFlowLayout =
  sendMessage nsCollectionViewFlowLayout minimumLineSpacingSelector

-- | @- setMinimumLineSpacing:@
setMinimumLineSpacing :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> CDouble -> IO ()
setMinimumLineSpacing nsCollectionViewFlowLayout value =
  sendMessage nsCollectionViewFlowLayout setMinimumLineSpacingSelector value

-- | @- minimumInteritemSpacing@
minimumInteritemSpacing :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> IO CDouble
minimumInteritemSpacing nsCollectionViewFlowLayout =
  sendMessage nsCollectionViewFlowLayout minimumInteritemSpacingSelector

-- | @- setMinimumInteritemSpacing:@
setMinimumInteritemSpacing :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> CDouble -> IO ()
setMinimumInteritemSpacing nsCollectionViewFlowLayout value =
  sendMessage nsCollectionViewFlowLayout setMinimumInteritemSpacingSelector value

-- | @- itemSize@
itemSize :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> IO NSSize
itemSize nsCollectionViewFlowLayout =
  sendMessage nsCollectionViewFlowLayout itemSizeSelector

-- | @- setItemSize:@
setItemSize :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> NSSize -> IO ()
setItemSize nsCollectionViewFlowLayout value =
  sendMessage nsCollectionViewFlowLayout setItemSizeSelector value

-- | @- estimatedItemSize@
estimatedItemSize :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> IO NSSize
estimatedItemSize nsCollectionViewFlowLayout =
  sendMessage nsCollectionViewFlowLayout estimatedItemSizeSelector

-- | @- setEstimatedItemSize:@
setEstimatedItemSize :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> NSSize -> IO ()
setEstimatedItemSize nsCollectionViewFlowLayout value =
  sendMessage nsCollectionViewFlowLayout setEstimatedItemSizeSelector value

-- | @- scrollDirection@
scrollDirection :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> IO NSCollectionViewScrollDirection
scrollDirection nsCollectionViewFlowLayout =
  sendMessage nsCollectionViewFlowLayout scrollDirectionSelector

-- | @- setScrollDirection:@
setScrollDirection :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> NSCollectionViewScrollDirection -> IO ()
setScrollDirection nsCollectionViewFlowLayout value =
  sendMessage nsCollectionViewFlowLayout setScrollDirectionSelector value

-- | @- headerReferenceSize@
headerReferenceSize :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> IO NSSize
headerReferenceSize nsCollectionViewFlowLayout =
  sendMessage nsCollectionViewFlowLayout headerReferenceSizeSelector

-- | @- setHeaderReferenceSize:@
setHeaderReferenceSize :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> NSSize -> IO ()
setHeaderReferenceSize nsCollectionViewFlowLayout value =
  sendMessage nsCollectionViewFlowLayout setHeaderReferenceSizeSelector value

-- | @- footerReferenceSize@
footerReferenceSize :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> IO NSSize
footerReferenceSize nsCollectionViewFlowLayout =
  sendMessage nsCollectionViewFlowLayout footerReferenceSizeSelector

-- | @- setFooterReferenceSize:@
setFooterReferenceSize :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> NSSize -> IO ()
setFooterReferenceSize nsCollectionViewFlowLayout value =
  sendMessage nsCollectionViewFlowLayout setFooterReferenceSizeSelector value

-- | @- sectionInset@
sectionInset :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> IO NSEdgeInsets
sectionInset nsCollectionViewFlowLayout =
  sendMessage nsCollectionViewFlowLayout sectionInsetSelector

-- | @- setSectionInset:@
setSectionInset :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> NSEdgeInsets -> IO ()
setSectionInset nsCollectionViewFlowLayout value =
  sendMessage nsCollectionViewFlowLayout setSectionInsetSelector value

-- | @- sectionHeadersPinToVisibleBounds@
sectionHeadersPinToVisibleBounds :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> IO Bool
sectionHeadersPinToVisibleBounds nsCollectionViewFlowLayout =
  sendMessage nsCollectionViewFlowLayout sectionHeadersPinToVisibleBoundsSelector

-- | @- setSectionHeadersPinToVisibleBounds:@
setSectionHeadersPinToVisibleBounds :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> Bool -> IO ()
setSectionHeadersPinToVisibleBounds nsCollectionViewFlowLayout value =
  sendMessage nsCollectionViewFlowLayout setSectionHeadersPinToVisibleBoundsSelector value

-- | @- sectionFootersPinToVisibleBounds@
sectionFootersPinToVisibleBounds :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> IO Bool
sectionFootersPinToVisibleBounds nsCollectionViewFlowLayout =
  sendMessage nsCollectionViewFlowLayout sectionFootersPinToVisibleBoundsSelector

-- | @- setSectionFootersPinToVisibleBounds:@
setSectionFootersPinToVisibleBounds :: IsNSCollectionViewFlowLayout nsCollectionViewFlowLayout => nsCollectionViewFlowLayout -> Bool -> IO ()
setSectionFootersPinToVisibleBounds nsCollectionViewFlowLayout value =
  sendMessage nsCollectionViewFlowLayout setSectionFootersPinToVisibleBoundsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sectionAtIndexIsCollapsed:@
sectionAtIndexIsCollapsedSelector :: Selector '[CULong] Bool
sectionAtIndexIsCollapsedSelector = mkSelector "sectionAtIndexIsCollapsed:"

-- | @Selector@ for @collapseSectionAtIndex:@
collapseSectionAtIndexSelector :: Selector '[CULong] ()
collapseSectionAtIndexSelector = mkSelector "collapseSectionAtIndex:"

-- | @Selector@ for @expandSectionAtIndex:@
expandSectionAtIndexSelector :: Selector '[CULong] ()
expandSectionAtIndexSelector = mkSelector "expandSectionAtIndex:"

-- | @Selector@ for @minimumLineSpacing@
minimumLineSpacingSelector :: Selector '[] CDouble
minimumLineSpacingSelector = mkSelector "minimumLineSpacing"

-- | @Selector@ for @setMinimumLineSpacing:@
setMinimumLineSpacingSelector :: Selector '[CDouble] ()
setMinimumLineSpacingSelector = mkSelector "setMinimumLineSpacing:"

-- | @Selector@ for @minimumInteritemSpacing@
minimumInteritemSpacingSelector :: Selector '[] CDouble
minimumInteritemSpacingSelector = mkSelector "minimumInteritemSpacing"

-- | @Selector@ for @setMinimumInteritemSpacing:@
setMinimumInteritemSpacingSelector :: Selector '[CDouble] ()
setMinimumInteritemSpacingSelector = mkSelector "setMinimumInteritemSpacing:"

-- | @Selector@ for @itemSize@
itemSizeSelector :: Selector '[] NSSize
itemSizeSelector = mkSelector "itemSize"

-- | @Selector@ for @setItemSize:@
setItemSizeSelector :: Selector '[NSSize] ()
setItemSizeSelector = mkSelector "setItemSize:"

-- | @Selector@ for @estimatedItemSize@
estimatedItemSizeSelector :: Selector '[] NSSize
estimatedItemSizeSelector = mkSelector "estimatedItemSize"

-- | @Selector@ for @setEstimatedItemSize:@
setEstimatedItemSizeSelector :: Selector '[NSSize] ()
setEstimatedItemSizeSelector = mkSelector "setEstimatedItemSize:"

-- | @Selector@ for @scrollDirection@
scrollDirectionSelector :: Selector '[] NSCollectionViewScrollDirection
scrollDirectionSelector = mkSelector "scrollDirection"

-- | @Selector@ for @setScrollDirection:@
setScrollDirectionSelector :: Selector '[NSCollectionViewScrollDirection] ()
setScrollDirectionSelector = mkSelector "setScrollDirection:"

-- | @Selector@ for @headerReferenceSize@
headerReferenceSizeSelector :: Selector '[] NSSize
headerReferenceSizeSelector = mkSelector "headerReferenceSize"

-- | @Selector@ for @setHeaderReferenceSize:@
setHeaderReferenceSizeSelector :: Selector '[NSSize] ()
setHeaderReferenceSizeSelector = mkSelector "setHeaderReferenceSize:"

-- | @Selector@ for @footerReferenceSize@
footerReferenceSizeSelector :: Selector '[] NSSize
footerReferenceSizeSelector = mkSelector "footerReferenceSize"

-- | @Selector@ for @setFooterReferenceSize:@
setFooterReferenceSizeSelector :: Selector '[NSSize] ()
setFooterReferenceSizeSelector = mkSelector "setFooterReferenceSize:"

-- | @Selector@ for @sectionInset@
sectionInsetSelector :: Selector '[] NSEdgeInsets
sectionInsetSelector = mkSelector "sectionInset"

-- | @Selector@ for @setSectionInset:@
setSectionInsetSelector :: Selector '[NSEdgeInsets] ()
setSectionInsetSelector = mkSelector "setSectionInset:"

-- | @Selector@ for @sectionHeadersPinToVisibleBounds@
sectionHeadersPinToVisibleBoundsSelector :: Selector '[] Bool
sectionHeadersPinToVisibleBoundsSelector = mkSelector "sectionHeadersPinToVisibleBounds"

-- | @Selector@ for @setSectionHeadersPinToVisibleBounds:@
setSectionHeadersPinToVisibleBoundsSelector :: Selector '[Bool] ()
setSectionHeadersPinToVisibleBoundsSelector = mkSelector "setSectionHeadersPinToVisibleBounds:"

-- | @Selector@ for @sectionFootersPinToVisibleBounds@
sectionFootersPinToVisibleBoundsSelector :: Selector '[] Bool
sectionFootersPinToVisibleBoundsSelector = mkSelector "sectionFootersPinToVisibleBounds"

-- | @Selector@ for @setSectionFootersPinToVisibleBounds:@
setSectionFootersPinToVisibleBoundsSelector :: Selector '[Bool] ()
setSectionFootersPinToVisibleBoundsSelector = mkSelector "setSectionFootersPinToVisibleBounds:"

