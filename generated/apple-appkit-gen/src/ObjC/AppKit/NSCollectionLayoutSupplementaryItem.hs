{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCollectionLayoutSupplementaryItem@.
module ObjC.AppKit.NSCollectionLayoutSupplementaryItem
  ( NSCollectionLayoutSupplementaryItem
  , IsNSCollectionLayoutSupplementaryItem(..)
  , supplementaryItemWithLayoutSize_elementKind_containerAnchor
  , supplementaryItemWithLayoutSize_elementKind_containerAnchor_itemAnchor
  , init_
  , new
  , zIndex
  , setZIndex
  , elementKind
  , containerAnchor
  , itemAnchor
  , containerAnchorSelector
  , elementKindSelector
  , initSelector
  , itemAnchorSelector
  , newSelector
  , setZIndexSelector
  , supplementaryItemWithLayoutSize_elementKind_containerAnchorSelector
  , supplementaryItemWithLayoutSize_elementKind_containerAnchor_itemAnchorSelector
  , zIndexSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ supplementaryItemWithLayoutSize:elementKind:containerAnchor:@
supplementaryItemWithLayoutSize_elementKind_containerAnchor :: (IsNSCollectionLayoutSize layoutSize, IsNSString elementKind, IsNSCollectionLayoutAnchor containerAnchor) => layoutSize -> elementKind -> containerAnchor -> IO (Id NSCollectionLayoutSupplementaryItem)
supplementaryItemWithLayoutSize_elementKind_containerAnchor layoutSize elementKind containerAnchor =
  do
    cls' <- getRequiredClass "NSCollectionLayoutSupplementaryItem"
    sendClassMessage cls' supplementaryItemWithLayoutSize_elementKind_containerAnchorSelector (toNSCollectionLayoutSize layoutSize) (toNSString elementKind) (toNSCollectionLayoutAnchor containerAnchor)

-- | @+ supplementaryItemWithLayoutSize:elementKind:containerAnchor:itemAnchor:@
supplementaryItemWithLayoutSize_elementKind_containerAnchor_itemAnchor :: (IsNSCollectionLayoutSize layoutSize, IsNSString elementKind, IsNSCollectionLayoutAnchor containerAnchor, IsNSCollectionLayoutAnchor itemAnchor) => layoutSize -> elementKind -> containerAnchor -> itemAnchor -> IO (Id NSCollectionLayoutSupplementaryItem)
supplementaryItemWithLayoutSize_elementKind_containerAnchor_itemAnchor layoutSize elementKind containerAnchor itemAnchor =
  do
    cls' <- getRequiredClass "NSCollectionLayoutSupplementaryItem"
    sendClassMessage cls' supplementaryItemWithLayoutSize_elementKind_containerAnchor_itemAnchorSelector (toNSCollectionLayoutSize layoutSize) (toNSString elementKind) (toNSCollectionLayoutAnchor containerAnchor) (toNSCollectionLayoutAnchor itemAnchor)

-- | @- init@
init_ :: IsNSCollectionLayoutSupplementaryItem nsCollectionLayoutSupplementaryItem => nsCollectionLayoutSupplementaryItem -> IO (Id NSCollectionLayoutSupplementaryItem)
init_ nsCollectionLayoutSupplementaryItem =
  sendOwnedMessage nsCollectionLayoutSupplementaryItem initSelector

-- | @+ new@
new :: IO (Id NSCollectionLayoutSupplementaryItem)
new  =
  do
    cls' <- getRequiredClass "NSCollectionLayoutSupplementaryItem"
    sendOwnedClassMessage cls' newSelector

-- | @- zIndex@
zIndex :: IsNSCollectionLayoutSupplementaryItem nsCollectionLayoutSupplementaryItem => nsCollectionLayoutSupplementaryItem -> IO CLong
zIndex nsCollectionLayoutSupplementaryItem =
  sendMessage nsCollectionLayoutSupplementaryItem zIndexSelector

-- | @- setZIndex:@
setZIndex :: IsNSCollectionLayoutSupplementaryItem nsCollectionLayoutSupplementaryItem => nsCollectionLayoutSupplementaryItem -> CLong -> IO ()
setZIndex nsCollectionLayoutSupplementaryItem value =
  sendMessage nsCollectionLayoutSupplementaryItem setZIndexSelector value

-- | @- elementKind@
elementKind :: IsNSCollectionLayoutSupplementaryItem nsCollectionLayoutSupplementaryItem => nsCollectionLayoutSupplementaryItem -> IO (Id NSString)
elementKind nsCollectionLayoutSupplementaryItem =
  sendMessage nsCollectionLayoutSupplementaryItem elementKindSelector

-- | @- containerAnchor@
containerAnchor :: IsNSCollectionLayoutSupplementaryItem nsCollectionLayoutSupplementaryItem => nsCollectionLayoutSupplementaryItem -> IO (Id NSCollectionLayoutAnchor)
containerAnchor nsCollectionLayoutSupplementaryItem =
  sendMessage nsCollectionLayoutSupplementaryItem containerAnchorSelector

-- | @- itemAnchor@
itemAnchor :: IsNSCollectionLayoutSupplementaryItem nsCollectionLayoutSupplementaryItem => nsCollectionLayoutSupplementaryItem -> IO (Id NSCollectionLayoutAnchor)
itemAnchor nsCollectionLayoutSupplementaryItem =
  sendMessage nsCollectionLayoutSupplementaryItem itemAnchorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @supplementaryItemWithLayoutSize:elementKind:containerAnchor:@
supplementaryItemWithLayoutSize_elementKind_containerAnchorSelector :: Selector '[Id NSCollectionLayoutSize, Id NSString, Id NSCollectionLayoutAnchor] (Id NSCollectionLayoutSupplementaryItem)
supplementaryItemWithLayoutSize_elementKind_containerAnchorSelector = mkSelector "supplementaryItemWithLayoutSize:elementKind:containerAnchor:"

-- | @Selector@ for @supplementaryItemWithLayoutSize:elementKind:containerAnchor:itemAnchor:@
supplementaryItemWithLayoutSize_elementKind_containerAnchor_itemAnchorSelector :: Selector '[Id NSCollectionLayoutSize, Id NSString, Id NSCollectionLayoutAnchor, Id NSCollectionLayoutAnchor] (Id NSCollectionLayoutSupplementaryItem)
supplementaryItemWithLayoutSize_elementKind_containerAnchor_itemAnchorSelector = mkSelector "supplementaryItemWithLayoutSize:elementKind:containerAnchor:itemAnchor:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSCollectionLayoutSupplementaryItem)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSCollectionLayoutSupplementaryItem)
newSelector = mkSelector "new"

-- | @Selector@ for @zIndex@
zIndexSelector :: Selector '[] CLong
zIndexSelector = mkSelector "zIndex"

-- | @Selector@ for @setZIndex:@
setZIndexSelector :: Selector '[CLong] ()
setZIndexSelector = mkSelector "setZIndex:"

-- | @Selector@ for @elementKind@
elementKindSelector :: Selector '[] (Id NSString)
elementKindSelector = mkSelector "elementKind"

-- | @Selector@ for @containerAnchor@
containerAnchorSelector :: Selector '[] (Id NSCollectionLayoutAnchor)
containerAnchorSelector = mkSelector "containerAnchor"

-- | @Selector@ for @itemAnchor@
itemAnchorSelector :: Selector '[] (Id NSCollectionLayoutAnchor)
itemAnchorSelector = mkSelector "itemAnchor"

