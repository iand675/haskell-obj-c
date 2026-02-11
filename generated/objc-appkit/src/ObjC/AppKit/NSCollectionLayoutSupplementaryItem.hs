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
  , supplementaryItemWithLayoutSize_elementKind_containerAnchorSelector
  , supplementaryItemWithLayoutSize_elementKind_containerAnchor_itemAnchorSelector
  , initSelector
  , newSelector
  , zIndexSelector
  , setZIndexSelector
  , elementKindSelector
  , containerAnchorSelector
  , itemAnchorSelector


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

-- | @+ supplementaryItemWithLayoutSize:elementKind:containerAnchor:@
supplementaryItemWithLayoutSize_elementKind_containerAnchor :: (IsNSCollectionLayoutSize layoutSize, IsNSString elementKind, IsNSCollectionLayoutAnchor containerAnchor) => layoutSize -> elementKind -> containerAnchor -> IO (Id NSCollectionLayoutSupplementaryItem)
supplementaryItemWithLayoutSize_elementKind_containerAnchor layoutSize elementKind containerAnchor =
  do
    cls' <- getRequiredClass "NSCollectionLayoutSupplementaryItem"
    withObjCPtr layoutSize $ \raw_layoutSize ->
      withObjCPtr elementKind $ \raw_elementKind ->
        withObjCPtr containerAnchor $ \raw_containerAnchor ->
          sendClassMsg cls' (mkSelector "supplementaryItemWithLayoutSize:elementKind:containerAnchor:") (retPtr retVoid) [argPtr (castPtr raw_layoutSize :: Ptr ()), argPtr (castPtr raw_elementKind :: Ptr ()), argPtr (castPtr raw_containerAnchor :: Ptr ())] >>= retainedObject . castPtr

-- | @+ supplementaryItemWithLayoutSize:elementKind:containerAnchor:itemAnchor:@
supplementaryItemWithLayoutSize_elementKind_containerAnchor_itemAnchor :: (IsNSCollectionLayoutSize layoutSize, IsNSString elementKind, IsNSCollectionLayoutAnchor containerAnchor, IsNSCollectionLayoutAnchor itemAnchor) => layoutSize -> elementKind -> containerAnchor -> itemAnchor -> IO (Id NSCollectionLayoutSupplementaryItem)
supplementaryItemWithLayoutSize_elementKind_containerAnchor_itemAnchor layoutSize elementKind containerAnchor itemAnchor =
  do
    cls' <- getRequiredClass "NSCollectionLayoutSupplementaryItem"
    withObjCPtr layoutSize $ \raw_layoutSize ->
      withObjCPtr elementKind $ \raw_elementKind ->
        withObjCPtr containerAnchor $ \raw_containerAnchor ->
          withObjCPtr itemAnchor $ \raw_itemAnchor ->
            sendClassMsg cls' (mkSelector "supplementaryItemWithLayoutSize:elementKind:containerAnchor:itemAnchor:") (retPtr retVoid) [argPtr (castPtr raw_layoutSize :: Ptr ()), argPtr (castPtr raw_elementKind :: Ptr ()), argPtr (castPtr raw_containerAnchor :: Ptr ()), argPtr (castPtr raw_itemAnchor :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsNSCollectionLayoutSupplementaryItem nsCollectionLayoutSupplementaryItem => nsCollectionLayoutSupplementaryItem -> IO (Id NSCollectionLayoutSupplementaryItem)
init_ nsCollectionLayoutSupplementaryItem  =
  sendMsg nsCollectionLayoutSupplementaryItem (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NSCollectionLayoutSupplementaryItem)
new  =
  do
    cls' <- getRequiredClass "NSCollectionLayoutSupplementaryItem"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- zIndex@
zIndex :: IsNSCollectionLayoutSupplementaryItem nsCollectionLayoutSupplementaryItem => nsCollectionLayoutSupplementaryItem -> IO CLong
zIndex nsCollectionLayoutSupplementaryItem  =
  sendMsg nsCollectionLayoutSupplementaryItem (mkSelector "zIndex") retCLong []

-- | @- setZIndex:@
setZIndex :: IsNSCollectionLayoutSupplementaryItem nsCollectionLayoutSupplementaryItem => nsCollectionLayoutSupplementaryItem -> CLong -> IO ()
setZIndex nsCollectionLayoutSupplementaryItem  value =
  sendMsg nsCollectionLayoutSupplementaryItem (mkSelector "setZIndex:") retVoid [argCLong (fromIntegral value)]

-- | @- elementKind@
elementKind :: IsNSCollectionLayoutSupplementaryItem nsCollectionLayoutSupplementaryItem => nsCollectionLayoutSupplementaryItem -> IO (Id NSString)
elementKind nsCollectionLayoutSupplementaryItem  =
  sendMsg nsCollectionLayoutSupplementaryItem (mkSelector "elementKind") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- containerAnchor@
containerAnchor :: IsNSCollectionLayoutSupplementaryItem nsCollectionLayoutSupplementaryItem => nsCollectionLayoutSupplementaryItem -> IO (Id NSCollectionLayoutAnchor)
containerAnchor nsCollectionLayoutSupplementaryItem  =
  sendMsg nsCollectionLayoutSupplementaryItem (mkSelector "containerAnchor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- itemAnchor@
itemAnchor :: IsNSCollectionLayoutSupplementaryItem nsCollectionLayoutSupplementaryItem => nsCollectionLayoutSupplementaryItem -> IO (Id NSCollectionLayoutAnchor)
itemAnchor nsCollectionLayoutSupplementaryItem  =
  sendMsg nsCollectionLayoutSupplementaryItem (mkSelector "itemAnchor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @supplementaryItemWithLayoutSize:elementKind:containerAnchor:@
supplementaryItemWithLayoutSize_elementKind_containerAnchorSelector :: Selector
supplementaryItemWithLayoutSize_elementKind_containerAnchorSelector = mkSelector "supplementaryItemWithLayoutSize:elementKind:containerAnchor:"

-- | @Selector@ for @supplementaryItemWithLayoutSize:elementKind:containerAnchor:itemAnchor:@
supplementaryItemWithLayoutSize_elementKind_containerAnchor_itemAnchorSelector :: Selector
supplementaryItemWithLayoutSize_elementKind_containerAnchor_itemAnchorSelector = mkSelector "supplementaryItemWithLayoutSize:elementKind:containerAnchor:itemAnchor:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @zIndex@
zIndexSelector :: Selector
zIndexSelector = mkSelector "zIndex"

-- | @Selector@ for @setZIndex:@
setZIndexSelector :: Selector
setZIndexSelector = mkSelector "setZIndex:"

-- | @Selector@ for @elementKind@
elementKindSelector :: Selector
elementKindSelector = mkSelector "elementKind"

-- | @Selector@ for @containerAnchor@
containerAnchorSelector :: Selector
containerAnchorSelector = mkSelector "containerAnchor"

-- | @Selector@ for @itemAnchor@
itemAnchorSelector :: Selector
itemAnchorSelector = mkSelector "itemAnchor"

