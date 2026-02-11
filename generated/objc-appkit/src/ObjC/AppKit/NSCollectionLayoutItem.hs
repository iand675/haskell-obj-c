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
  , itemWithLayoutSizeSelector
  , itemWithLayoutSize_supplementaryItemsSelector
  , initSelector
  , newSelector
  , contentInsetsSelector
  , setContentInsetsSelector
  , edgeSpacingSelector
  , setEdgeSpacingSelector
  , layoutSizeSelector
  , supplementaryItemsSelector


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
import ObjC.AppKit.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @+ itemWithLayoutSize:@
itemWithLayoutSize :: IsNSCollectionLayoutSize layoutSize => layoutSize -> IO (Id NSCollectionLayoutItem)
itemWithLayoutSize layoutSize =
  do
    cls' <- getRequiredClass "NSCollectionLayoutItem"
    withObjCPtr layoutSize $ \raw_layoutSize ->
      sendClassMsg cls' (mkSelector "itemWithLayoutSize:") (retPtr retVoid) [argPtr (castPtr raw_layoutSize :: Ptr ())] >>= retainedObject . castPtr

-- | @+ itemWithLayoutSize:supplementaryItems:@
itemWithLayoutSize_supplementaryItems :: (IsNSCollectionLayoutSize layoutSize, IsNSArray supplementaryItems) => layoutSize -> supplementaryItems -> IO (Id NSCollectionLayoutItem)
itemWithLayoutSize_supplementaryItems layoutSize supplementaryItems =
  do
    cls' <- getRequiredClass "NSCollectionLayoutItem"
    withObjCPtr layoutSize $ \raw_layoutSize ->
      withObjCPtr supplementaryItems $ \raw_supplementaryItems ->
        sendClassMsg cls' (mkSelector "itemWithLayoutSize:supplementaryItems:") (retPtr retVoid) [argPtr (castPtr raw_layoutSize :: Ptr ()), argPtr (castPtr raw_supplementaryItems :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsNSCollectionLayoutItem nsCollectionLayoutItem => nsCollectionLayoutItem -> IO (Id NSCollectionLayoutItem)
init_ nsCollectionLayoutItem  =
  sendMsg nsCollectionLayoutItem (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NSCollectionLayoutItem)
new  =
  do
    cls' <- getRequiredClass "NSCollectionLayoutItem"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- contentInsets@
contentInsets :: IsNSCollectionLayoutItem nsCollectionLayoutItem => nsCollectionLayoutItem -> IO NSDirectionalEdgeInsets
contentInsets nsCollectionLayoutItem  =
  sendMsgStret nsCollectionLayoutItem (mkSelector "contentInsets") retNSDirectionalEdgeInsets []

-- | @- setContentInsets:@
setContentInsets :: IsNSCollectionLayoutItem nsCollectionLayoutItem => nsCollectionLayoutItem -> NSDirectionalEdgeInsets -> IO ()
setContentInsets nsCollectionLayoutItem  value =
  sendMsg nsCollectionLayoutItem (mkSelector "setContentInsets:") retVoid [argNSDirectionalEdgeInsets value]

-- | @- edgeSpacing@
edgeSpacing :: IsNSCollectionLayoutItem nsCollectionLayoutItem => nsCollectionLayoutItem -> IO (Id NSCollectionLayoutEdgeSpacing)
edgeSpacing nsCollectionLayoutItem  =
  sendMsg nsCollectionLayoutItem (mkSelector "edgeSpacing") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEdgeSpacing:@
setEdgeSpacing :: (IsNSCollectionLayoutItem nsCollectionLayoutItem, IsNSCollectionLayoutEdgeSpacing value) => nsCollectionLayoutItem -> value -> IO ()
setEdgeSpacing nsCollectionLayoutItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsCollectionLayoutItem (mkSelector "setEdgeSpacing:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- layoutSize@
layoutSize :: IsNSCollectionLayoutItem nsCollectionLayoutItem => nsCollectionLayoutItem -> IO (Id NSCollectionLayoutSize)
layoutSize nsCollectionLayoutItem  =
  sendMsg nsCollectionLayoutItem (mkSelector "layoutSize") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- supplementaryItems@
supplementaryItems :: IsNSCollectionLayoutItem nsCollectionLayoutItem => nsCollectionLayoutItem -> IO (Id NSArray)
supplementaryItems nsCollectionLayoutItem  =
  sendMsg nsCollectionLayoutItem (mkSelector "supplementaryItems") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @itemWithLayoutSize:@
itemWithLayoutSizeSelector :: Selector
itemWithLayoutSizeSelector = mkSelector "itemWithLayoutSize:"

-- | @Selector@ for @itemWithLayoutSize:supplementaryItems:@
itemWithLayoutSize_supplementaryItemsSelector :: Selector
itemWithLayoutSize_supplementaryItemsSelector = mkSelector "itemWithLayoutSize:supplementaryItems:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @contentInsets@
contentInsetsSelector :: Selector
contentInsetsSelector = mkSelector "contentInsets"

-- | @Selector@ for @setContentInsets:@
setContentInsetsSelector :: Selector
setContentInsetsSelector = mkSelector "setContentInsets:"

-- | @Selector@ for @edgeSpacing@
edgeSpacingSelector :: Selector
edgeSpacingSelector = mkSelector "edgeSpacing"

-- | @Selector@ for @setEdgeSpacing:@
setEdgeSpacingSelector :: Selector
setEdgeSpacingSelector = mkSelector "setEdgeSpacing:"

-- | @Selector@ for @layoutSize@
layoutSizeSelector :: Selector
layoutSizeSelector = mkSelector "layoutSize"

-- | @Selector@ for @supplementaryItems@
supplementaryItemsSelector :: Selector
supplementaryItemsSelector = mkSelector "supplementaryItems"

