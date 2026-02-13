{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCollectionLayoutGroup@.
module ObjC.AppKit.NSCollectionLayoutGroup
  ( NSCollectionLayoutGroup
  , IsNSCollectionLayoutGroup(..)
  , horizontalGroupWithLayoutSize_subitem_count
  , horizontalGroupWithLayoutSize_subitems
  , verticalGroupWithLayoutSize_subitem_count
  , verticalGroupWithLayoutSize_subitems
  , customGroupWithLayoutSize_itemProvider
  , init_
  , new
  , visualDescription
  , supplementaryItems
  , setSupplementaryItems
  , interItemSpacing
  , setInterItemSpacing
  , subitems
  , customGroupWithLayoutSize_itemProviderSelector
  , horizontalGroupWithLayoutSize_subitem_countSelector
  , horizontalGroupWithLayoutSize_subitemsSelector
  , initSelector
  , interItemSpacingSelector
  , newSelector
  , setInterItemSpacingSelector
  , setSupplementaryItemsSelector
  , subitemsSelector
  , supplementaryItemsSelector
  , verticalGroupWithLayoutSize_subitem_countSelector
  , verticalGroupWithLayoutSize_subitemsSelector
  , visualDescriptionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ horizontalGroupWithLayoutSize:subitem:count:@
horizontalGroupWithLayoutSize_subitem_count :: (IsNSCollectionLayoutSize layoutSize, IsNSCollectionLayoutItem subitem) => layoutSize -> subitem -> CLong -> IO (Id NSCollectionLayoutGroup)
horizontalGroupWithLayoutSize_subitem_count layoutSize subitem count =
  do
    cls' <- getRequiredClass "NSCollectionLayoutGroup"
    sendClassMessage cls' horizontalGroupWithLayoutSize_subitem_countSelector (toNSCollectionLayoutSize layoutSize) (toNSCollectionLayoutItem subitem) count

-- | @+ horizontalGroupWithLayoutSize:subitems:@
horizontalGroupWithLayoutSize_subitems :: (IsNSCollectionLayoutSize layoutSize, IsNSArray subitems) => layoutSize -> subitems -> IO (Id NSCollectionLayoutGroup)
horizontalGroupWithLayoutSize_subitems layoutSize subitems =
  do
    cls' <- getRequiredClass "NSCollectionLayoutGroup"
    sendClassMessage cls' horizontalGroupWithLayoutSize_subitemsSelector (toNSCollectionLayoutSize layoutSize) (toNSArray subitems)

-- | @+ verticalGroupWithLayoutSize:subitem:count:@
verticalGroupWithLayoutSize_subitem_count :: (IsNSCollectionLayoutSize layoutSize, IsNSCollectionLayoutItem subitem) => layoutSize -> subitem -> CLong -> IO (Id NSCollectionLayoutGroup)
verticalGroupWithLayoutSize_subitem_count layoutSize subitem count =
  do
    cls' <- getRequiredClass "NSCollectionLayoutGroup"
    sendClassMessage cls' verticalGroupWithLayoutSize_subitem_countSelector (toNSCollectionLayoutSize layoutSize) (toNSCollectionLayoutItem subitem) count

-- | @+ verticalGroupWithLayoutSize:subitems:@
verticalGroupWithLayoutSize_subitems :: (IsNSCollectionLayoutSize layoutSize, IsNSArray subitems) => layoutSize -> subitems -> IO (Id NSCollectionLayoutGroup)
verticalGroupWithLayoutSize_subitems layoutSize subitems =
  do
    cls' <- getRequiredClass "NSCollectionLayoutGroup"
    sendClassMessage cls' verticalGroupWithLayoutSize_subitemsSelector (toNSCollectionLayoutSize layoutSize) (toNSArray subitems)

-- | @+ customGroupWithLayoutSize:itemProvider:@
customGroupWithLayoutSize_itemProvider :: IsNSCollectionLayoutSize layoutSize => layoutSize -> Ptr () -> IO (Id NSCollectionLayoutGroup)
customGroupWithLayoutSize_itemProvider layoutSize itemProvider =
  do
    cls' <- getRequiredClass "NSCollectionLayoutGroup"
    sendClassMessage cls' customGroupWithLayoutSize_itemProviderSelector (toNSCollectionLayoutSize layoutSize) itemProvider

-- | @- init@
init_ :: IsNSCollectionLayoutGroup nsCollectionLayoutGroup => nsCollectionLayoutGroup -> IO (Id NSCollectionLayoutGroup)
init_ nsCollectionLayoutGroup =
  sendOwnedMessage nsCollectionLayoutGroup initSelector

-- | @+ new@
new :: IO (Id NSCollectionLayoutGroup)
new  =
  do
    cls' <- getRequiredClass "NSCollectionLayoutGroup"
    sendOwnedClassMessage cls' newSelector

-- | @- visualDescription@
visualDescription :: IsNSCollectionLayoutGroup nsCollectionLayoutGroup => nsCollectionLayoutGroup -> IO (Id NSString)
visualDescription nsCollectionLayoutGroup =
  sendMessage nsCollectionLayoutGroup visualDescriptionSelector

-- | @- supplementaryItems@
supplementaryItems :: IsNSCollectionLayoutGroup nsCollectionLayoutGroup => nsCollectionLayoutGroup -> IO (Id NSArray)
supplementaryItems nsCollectionLayoutGroup =
  sendMessage nsCollectionLayoutGroup supplementaryItemsSelector

-- | @- setSupplementaryItems:@
setSupplementaryItems :: (IsNSCollectionLayoutGroup nsCollectionLayoutGroup, IsNSArray value) => nsCollectionLayoutGroup -> value -> IO ()
setSupplementaryItems nsCollectionLayoutGroup value =
  sendMessage nsCollectionLayoutGroup setSupplementaryItemsSelector (toNSArray value)

-- | @- interItemSpacing@
interItemSpacing :: IsNSCollectionLayoutGroup nsCollectionLayoutGroup => nsCollectionLayoutGroup -> IO (Id NSCollectionLayoutSpacing)
interItemSpacing nsCollectionLayoutGroup =
  sendMessage nsCollectionLayoutGroup interItemSpacingSelector

-- | @- setInterItemSpacing:@
setInterItemSpacing :: (IsNSCollectionLayoutGroup nsCollectionLayoutGroup, IsNSCollectionLayoutSpacing value) => nsCollectionLayoutGroup -> value -> IO ()
setInterItemSpacing nsCollectionLayoutGroup value =
  sendMessage nsCollectionLayoutGroup setInterItemSpacingSelector (toNSCollectionLayoutSpacing value)

-- | @- subitems@
subitems :: IsNSCollectionLayoutGroup nsCollectionLayoutGroup => nsCollectionLayoutGroup -> IO (Id NSArray)
subitems nsCollectionLayoutGroup =
  sendMessage nsCollectionLayoutGroup subitemsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @horizontalGroupWithLayoutSize:subitem:count:@
horizontalGroupWithLayoutSize_subitem_countSelector :: Selector '[Id NSCollectionLayoutSize, Id NSCollectionLayoutItem, CLong] (Id NSCollectionLayoutGroup)
horizontalGroupWithLayoutSize_subitem_countSelector = mkSelector "horizontalGroupWithLayoutSize:subitem:count:"

-- | @Selector@ for @horizontalGroupWithLayoutSize:subitems:@
horizontalGroupWithLayoutSize_subitemsSelector :: Selector '[Id NSCollectionLayoutSize, Id NSArray] (Id NSCollectionLayoutGroup)
horizontalGroupWithLayoutSize_subitemsSelector = mkSelector "horizontalGroupWithLayoutSize:subitems:"

-- | @Selector@ for @verticalGroupWithLayoutSize:subitem:count:@
verticalGroupWithLayoutSize_subitem_countSelector :: Selector '[Id NSCollectionLayoutSize, Id NSCollectionLayoutItem, CLong] (Id NSCollectionLayoutGroup)
verticalGroupWithLayoutSize_subitem_countSelector = mkSelector "verticalGroupWithLayoutSize:subitem:count:"

-- | @Selector@ for @verticalGroupWithLayoutSize:subitems:@
verticalGroupWithLayoutSize_subitemsSelector :: Selector '[Id NSCollectionLayoutSize, Id NSArray] (Id NSCollectionLayoutGroup)
verticalGroupWithLayoutSize_subitemsSelector = mkSelector "verticalGroupWithLayoutSize:subitems:"

-- | @Selector@ for @customGroupWithLayoutSize:itemProvider:@
customGroupWithLayoutSize_itemProviderSelector :: Selector '[Id NSCollectionLayoutSize, Ptr ()] (Id NSCollectionLayoutGroup)
customGroupWithLayoutSize_itemProviderSelector = mkSelector "customGroupWithLayoutSize:itemProvider:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSCollectionLayoutGroup)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSCollectionLayoutGroup)
newSelector = mkSelector "new"

-- | @Selector@ for @visualDescription@
visualDescriptionSelector :: Selector '[] (Id NSString)
visualDescriptionSelector = mkSelector "visualDescription"

-- | @Selector@ for @supplementaryItems@
supplementaryItemsSelector :: Selector '[] (Id NSArray)
supplementaryItemsSelector = mkSelector "supplementaryItems"

-- | @Selector@ for @setSupplementaryItems:@
setSupplementaryItemsSelector :: Selector '[Id NSArray] ()
setSupplementaryItemsSelector = mkSelector "setSupplementaryItems:"

-- | @Selector@ for @interItemSpacing@
interItemSpacingSelector :: Selector '[] (Id NSCollectionLayoutSpacing)
interItemSpacingSelector = mkSelector "interItemSpacing"

-- | @Selector@ for @setInterItemSpacing:@
setInterItemSpacingSelector :: Selector '[Id NSCollectionLayoutSpacing] ()
setInterItemSpacingSelector = mkSelector "setInterItemSpacing:"

-- | @Selector@ for @subitems@
subitemsSelector :: Selector '[] (Id NSArray)
subitemsSelector = mkSelector "subitems"

