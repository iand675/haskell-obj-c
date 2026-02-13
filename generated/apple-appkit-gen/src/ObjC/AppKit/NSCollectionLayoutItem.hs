{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCollectionLayoutItem@.
module ObjC.AppKit.NSCollectionLayoutItem
  ( NSCollectionLayoutItem
  , IsNSCollectionLayoutItem(..)
  , itemWithLayoutSize
  , itemWithLayoutSize_supplementaryItems
  , init_
  , new
  , contentInsets
  , setContentInsets
  , edgeSpacing
  , setEdgeSpacing
  , layoutSize
  , supplementaryItems
  , contentInsetsSelector
  , edgeSpacingSelector
  , initSelector
  , itemWithLayoutSizeSelector
  , itemWithLayoutSize_supplementaryItemsSelector
  , layoutSizeSelector
  , newSelector
  , setContentInsetsSelector
  , setEdgeSpacingSelector
  , supplementaryItemsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @+ itemWithLayoutSize:@
itemWithLayoutSize :: IsNSCollectionLayoutSize layoutSize => layoutSize -> IO (Id NSCollectionLayoutItem)
itemWithLayoutSize layoutSize =
  do
    cls' <- getRequiredClass "NSCollectionLayoutItem"
    sendClassMessage cls' itemWithLayoutSizeSelector (toNSCollectionLayoutSize layoutSize)

-- | @+ itemWithLayoutSize:supplementaryItems:@
itemWithLayoutSize_supplementaryItems :: (IsNSCollectionLayoutSize layoutSize, IsNSArray supplementaryItems) => layoutSize -> supplementaryItems -> IO (Id NSCollectionLayoutItem)
itemWithLayoutSize_supplementaryItems layoutSize supplementaryItems =
  do
    cls' <- getRequiredClass "NSCollectionLayoutItem"
    sendClassMessage cls' itemWithLayoutSize_supplementaryItemsSelector (toNSCollectionLayoutSize layoutSize) (toNSArray supplementaryItems)

-- | @- init@
init_ :: IsNSCollectionLayoutItem nsCollectionLayoutItem => nsCollectionLayoutItem -> IO (Id NSCollectionLayoutItem)
init_ nsCollectionLayoutItem =
  sendOwnedMessage nsCollectionLayoutItem initSelector

-- | @+ new@
new :: IO (Id NSCollectionLayoutItem)
new  =
  do
    cls' <- getRequiredClass "NSCollectionLayoutItem"
    sendOwnedClassMessage cls' newSelector

-- | @- contentInsets@
contentInsets :: IsNSCollectionLayoutItem nsCollectionLayoutItem => nsCollectionLayoutItem -> IO NSDirectionalEdgeInsets
contentInsets nsCollectionLayoutItem =
  sendMessage nsCollectionLayoutItem contentInsetsSelector

-- | @- setContentInsets:@
setContentInsets :: IsNSCollectionLayoutItem nsCollectionLayoutItem => nsCollectionLayoutItem -> NSDirectionalEdgeInsets -> IO ()
setContentInsets nsCollectionLayoutItem value =
  sendMessage nsCollectionLayoutItem setContentInsetsSelector value

-- | @- edgeSpacing@
edgeSpacing :: IsNSCollectionLayoutItem nsCollectionLayoutItem => nsCollectionLayoutItem -> IO (Id NSCollectionLayoutEdgeSpacing)
edgeSpacing nsCollectionLayoutItem =
  sendMessage nsCollectionLayoutItem edgeSpacingSelector

-- | @- setEdgeSpacing:@
setEdgeSpacing :: (IsNSCollectionLayoutItem nsCollectionLayoutItem, IsNSCollectionLayoutEdgeSpacing value) => nsCollectionLayoutItem -> value -> IO ()
setEdgeSpacing nsCollectionLayoutItem value =
  sendMessage nsCollectionLayoutItem setEdgeSpacingSelector (toNSCollectionLayoutEdgeSpacing value)

-- | @- layoutSize@
layoutSize :: IsNSCollectionLayoutItem nsCollectionLayoutItem => nsCollectionLayoutItem -> IO (Id NSCollectionLayoutSize)
layoutSize nsCollectionLayoutItem =
  sendMessage nsCollectionLayoutItem layoutSizeSelector

-- | @- supplementaryItems@
supplementaryItems :: IsNSCollectionLayoutItem nsCollectionLayoutItem => nsCollectionLayoutItem -> IO (Id NSArray)
supplementaryItems nsCollectionLayoutItem =
  sendMessage nsCollectionLayoutItem supplementaryItemsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @itemWithLayoutSize:@
itemWithLayoutSizeSelector :: Selector '[Id NSCollectionLayoutSize] (Id NSCollectionLayoutItem)
itemWithLayoutSizeSelector = mkSelector "itemWithLayoutSize:"

-- | @Selector@ for @itemWithLayoutSize:supplementaryItems:@
itemWithLayoutSize_supplementaryItemsSelector :: Selector '[Id NSCollectionLayoutSize, Id NSArray] (Id NSCollectionLayoutItem)
itemWithLayoutSize_supplementaryItemsSelector = mkSelector "itemWithLayoutSize:supplementaryItems:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSCollectionLayoutItem)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSCollectionLayoutItem)
newSelector = mkSelector "new"

-- | @Selector@ for @contentInsets@
contentInsetsSelector :: Selector '[] NSDirectionalEdgeInsets
contentInsetsSelector = mkSelector "contentInsets"

-- | @Selector@ for @setContentInsets:@
setContentInsetsSelector :: Selector '[NSDirectionalEdgeInsets] ()
setContentInsetsSelector = mkSelector "setContentInsets:"

-- | @Selector@ for @edgeSpacing@
edgeSpacingSelector :: Selector '[] (Id NSCollectionLayoutEdgeSpacing)
edgeSpacingSelector = mkSelector "edgeSpacing"

-- | @Selector@ for @setEdgeSpacing:@
setEdgeSpacingSelector :: Selector '[Id NSCollectionLayoutEdgeSpacing] ()
setEdgeSpacingSelector = mkSelector "setEdgeSpacing:"

-- | @Selector@ for @layoutSize@
layoutSizeSelector :: Selector '[] (Id NSCollectionLayoutSize)
layoutSizeSelector = mkSelector "layoutSize"

-- | @Selector@ for @supplementaryItems@
supplementaryItemsSelector :: Selector '[] (Id NSArray)
supplementaryItemsSelector = mkSelector "supplementaryItems"

