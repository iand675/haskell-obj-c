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
  , horizontalGroupWithLayoutSize_subitem_countSelector
  , horizontalGroupWithLayoutSize_subitemsSelector
  , verticalGroupWithLayoutSize_subitem_countSelector
  , verticalGroupWithLayoutSize_subitemsSelector
  , customGroupWithLayoutSize_itemProviderSelector
  , initSelector
  , newSelector
  , visualDescriptionSelector
  , supplementaryItemsSelector
  , setSupplementaryItemsSelector
  , interItemSpacingSelector
  , setInterItemSpacingSelector
  , subitemsSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ horizontalGroupWithLayoutSize:subitem:count:@
horizontalGroupWithLayoutSize_subitem_count :: (IsNSCollectionLayoutSize layoutSize, IsNSCollectionLayoutItem subitem) => layoutSize -> subitem -> CLong -> IO (Id NSCollectionLayoutGroup)
horizontalGroupWithLayoutSize_subitem_count layoutSize subitem count =
  do
    cls' <- getRequiredClass "NSCollectionLayoutGroup"
    withObjCPtr layoutSize $ \raw_layoutSize ->
      withObjCPtr subitem $ \raw_subitem ->
        sendClassMsg cls' (mkSelector "horizontalGroupWithLayoutSize:subitem:count:") (retPtr retVoid) [argPtr (castPtr raw_layoutSize :: Ptr ()), argPtr (castPtr raw_subitem :: Ptr ()), argCLong (fromIntegral count)] >>= retainedObject . castPtr

-- | @+ horizontalGroupWithLayoutSize:subitems:@
horizontalGroupWithLayoutSize_subitems :: (IsNSCollectionLayoutSize layoutSize, IsNSArray subitems) => layoutSize -> subitems -> IO (Id NSCollectionLayoutGroup)
horizontalGroupWithLayoutSize_subitems layoutSize subitems =
  do
    cls' <- getRequiredClass "NSCollectionLayoutGroup"
    withObjCPtr layoutSize $ \raw_layoutSize ->
      withObjCPtr subitems $ \raw_subitems ->
        sendClassMsg cls' (mkSelector "horizontalGroupWithLayoutSize:subitems:") (retPtr retVoid) [argPtr (castPtr raw_layoutSize :: Ptr ()), argPtr (castPtr raw_subitems :: Ptr ())] >>= retainedObject . castPtr

-- | @+ verticalGroupWithLayoutSize:subitem:count:@
verticalGroupWithLayoutSize_subitem_count :: (IsNSCollectionLayoutSize layoutSize, IsNSCollectionLayoutItem subitem) => layoutSize -> subitem -> CLong -> IO (Id NSCollectionLayoutGroup)
verticalGroupWithLayoutSize_subitem_count layoutSize subitem count =
  do
    cls' <- getRequiredClass "NSCollectionLayoutGroup"
    withObjCPtr layoutSize $ \raw_layoutSize ->
      withObjCPtr subitem $ \raw_subitem ->
        sendClassMsg cls' (mkSelector "verticalGroupWithLayoutSize:subitem:count:") (retPtr retVoid) [argPtr (castPtr raw_layoutSize :: Ptr ()), argPtr (castPtr raw_subitem :: Ptr ()), argCLong (fromIntegral count)] >>= retainedObject . castPtr

-- | @+ verticalGroupWithLayoutSize:subitems:@
verticalGroupWithLayoutSize_subitems :: (IsNSCollectionLayoutSize layoutSize, IsNSArray subitems) => layoutSize -> subitems -> IO (Id NSCollectionLayoutGroup)
verticalGroupWithLayoutSize_subitems layoutSize subitems =
  do
    cls' <- getRequiredClass "NSCollectionLayoutGroup"
    withObjCPtr layoutSize $ \raw_layoutSize ->
      withObjCPtr subitems $ \raw_subitems ->
        sendClassMsg cls' (mkSelector "verticalGroupWithLayoutSize:subitems:") (retPtr retVoid) [argPtr (castPtr raw_layoutSize :: Ptr ()), argPtr (castPtr raw_subitems :: Ptr ())] >>= retainedObject . castPtr

-- | @+ customGroupWithLayoutSize:itemProvider:@
customGroupWithLayoutSize_itemProvider :: IsNSCollectionLayoutSize layoutSize => layoutSize -> Ptr () -> IO (Id NSCollectionLayoutGroup)
customGroupWithLayoutSize_itemProvider layoutSize itemProvider =
  do
    cls' <- getRequiredClass "NSCollectionLayoutGroup"
    withObjCPtr layoutSize $ \raw_layoutSize ->
      sendClassMsg cls' (mkSelector "customGroupWithLayoutSize:itemProvider:") (retPtr retVoid) [argPtr (castPtr raw_layoutSize :: Ptr ()), argPtr (castPtr itemProvider :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsNSCollectionLayoutGroup nsCollectionLayoutGroup => nsCollectionLayoutGroup -> IO (Id NSCollectionLayoutGroup)
init_ nsCollectionLayoutGroup  =
  sendMsg nsCollectionLayoutGroup (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NSCollectionLayoutGroup)
new  =
  do
    cls' <- getRequiredClass "NSCollectionLayoutGroup"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- visualDescription@
visualDescription :: IsNSCollectionLayoutGroup nsCollectionLayoutGroup => nsCollectionLayoutGroup -> IO (Id NSString)
visualDescription nsCollectionLayoutGroup  =
  sendMsg nsCollectionLayoutGroup (mkSelector "visualDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- supplementaryItems@
supplementaryItems :: IsNSCollectionLayoutGroup nsCollectionLayoutGroup => nsCollectionLayoutGroup -> IO (Id NSArray)
supplementaryItems nsCollectionLayoutGroup  =
  sendMsg nsCollectionLayoutGroup (mkSelector "supplementaryItems") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSupplementaryItems:@
setSupplementaryItems :: (IsNSCollectionLayoutGroup nsCollectionLayoutGroup, IsNSArray value) => nsCollectionLayoutGroup -> value -> IO ()
setSupplementaryItems nsCollectionLayoutGroup  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsCollectionLayoutGroup (mkSelector "setSupplementaryItems:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- interItemSpacing@
interItemSpacing :: IsNSCollectionLayoutGroup nsCollectionLayoutGroup => nsCollectionLayoutGroup -> IO (Id NSCollectionLayoutSpacing)
interItemSpacing nsCollectionLayoutGroup  =
  sendMsg nsCollectionLayoutGroup (mkSelector "interItemSpacing") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setInterItemSpacing:@
setInterItemSpacing :: (IsNSCollectionLayoutGroup nsCollectionLayoutGroup, IsNSCollectionLayoutSpacing value) => nsCollectionLayoutGroup -> value -> IO ()
setInterItemSpacing nsCollectionLayoutGroup  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsCollectionLayoutGroup (mkSelector "setInterItemSpacing:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- subitems@
subitems :: IsNSCollectionLayoutGroup nsCollectionLayoutGroup => nsCollectionLayoutGroup -> IO (Id NSArray)
subitems nsCollectionLayoutGroup  =
  sendMsg nsCollectionLayoutGroup (mkSelector "subitems") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @horizontalGroupWithLayoutSize:subitem:count:@
horizontalGroupWithLayoutSize_subitem_countSelector :: Selector
horizontalGroupWithLayoutSize_subitem_countSelector = mkSelector "horizontalGroupWithLayoutSize:subitem:count:"

-- | @Selector@ for @horizontalGroupWithLayoutSize:subitems:@
horizontalGroupWithLayoutSize_subitemsSelector :: Selector
horizontalGroupWithLayoutSize_subitemsSelector = mkSelector "horizontalGroupWithLayoutSize:subitems:"

-- | @Selector@ for @verticalGroupWithLayoutSize:subitem:count:@
verticalGroupWithLayoutSize_subitem_countSelector :: Selector
verticalGroupWithLayoutSize_subitem_countSelector = mkSelector "verticalGroupWithLayoutSize:subitem:count:"

-- | @Selector@ for @verticalGroupWithLayoutSize:subitems:@
verticalGroupWithLayoutSize_subitemsSelector :: Selector
verticalGroupWithLayoutSize_subitemsSelector = mkSelector "verticalGroupWithLayoutSize:subitems:"

-- | @Selector@ for @customGroupWithLayoutSize:itemProvider:@
customGroupWithLayoutSize_itemProviderSelector :: Selector
customGroupWithLayoutSize_itemProviderSelector = mkSelector "customGroupWithLayoutSize:itemProvider:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @visualDescription@
visualDescriptionSelector :: Selector
visualDescriptionSelector = mkSelector "visualDescription"

-- | @Selector@ for @supplementaryItems@
supplementaryItemsSelector :: Selector
supplementaryItemsSelector = mkSelector "supplementaryItems"

-- | @Selector@ for @setSupplementaryItems:@
setSupplementaryItemsSelector :: Selector
setSupplementaryItemsSelector = mkSelector "setSupplementaryItems:"

-- | @Selector@ for @interItemSpacing@
interItemSpacingSelector :: Selector
interItemSpacingSelector = mkSelector "interItemSpacing"

-- | @Selector@ for @setInterItemSpacing:@
setInterItemSpacingSelector :: Selector
setInterItemSpacingSelector = mkSelector "setInterItemSpacing:"

-- | @Selector@ for @subitems@
subitemsSelector :: Selector
subitemsSelector = mkSelector "subitems"

