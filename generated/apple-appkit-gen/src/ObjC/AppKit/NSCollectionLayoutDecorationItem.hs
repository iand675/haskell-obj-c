{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCollectionLayoutDecorationItem@.
module ObjC.AppKit.NSCollectionLayoutDecorationItem
  ( NSCollectionLayoutDecorationItem
  , IsNSCollectionLayoutDecorationItem(..)
  , backgroundDecorationItemWithElementKind
  , init_
  , new
  , zIndex
  , setZIndex
  , elementKind
  , backgroundDecorationItemWithElementKindSelector
  , elementKindSelector
  , initSelector
  , newSelector
  , setZIndexSelector
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

-- | @+ backgroundDecorationItemWithElementKind:@
backgroundDecorationItemWithElementKind :: IsNSString elementKind => elementKind -> IO (Id NSCollectionLayoutDecorationItem)
backgroundDecorationItemWithElementKind elementKind =
  do
    cls' <- getRequiredClass "NSCollectionLayoutDecorationItem"
    sendClassMessage cls' backgroundDecorationItemWithElementKindSelector (toNSString elementKind)

-- | @- init@
init_ :: IsNSCollectionLayoutDecorationItem nsCollectionLayoutDecorationItem => nsCollectionLayoutDecorationItem -> IO (Id NSCollectionLayoutDecorationItem)
init_ nsCollectionLayoutDecorationItem =
  sendOwnedMessage nsCollectionLayoutDecorationItem initSelector

-- | @+ new@
new :: IO (Id NSCollectionLayoutDecorationItem)
new  =
  do
    cls' <- getRequiredClass "NSCollectionLayoutDecorationItem"
    sendOwnedClassMessage cls' newSelector

-- | @- zIndex@
zIndex :: IsNSCollectionLayoutDecorationItem nsCollectionLayoutDecorationItem => nsCollectionLayoutDecorationItem -> IO CLong
zIndex nsCollectionLayoutDecorationItem =
  sendMessage nsCollectionLayoutDecorationItem zIndexSelector

-- | @- setZIndex:@
setZIndex :: IsNSCollectionLayoutDecorationItem nsCollectionLayoutDecorationItem => nsCollectionLayoutDecorationItem -> CLong -> IO ()
setZIndex nsCollectionLayoutDecorationItem value =
  sendMessage nsCollectionLayoutDecorationItem setZIndexSelector value

-- | @- elementKind@
elementKind :: IsNSCollectionLayoutDecorationItem nsCollectionLayoutDecorationItem => nsCollectionLayoutDecorationItem -> IO (Id NSString)
elementKind nsCollectionLayoutDecorationItem =
  sendMessage nsCollectionLayoutDecorationItem elementKindSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @backgroundDecorationItemWithElementKind:@
backgroundDecorationItemWithElementKindSelector :: Selector '[Id NSString] (Id NSCollectionLayoutDecorationItem)
backgroundDecorationItemWithElementKindSelector = mkSelector "backgroundDecorationItemWithElementKind:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSCollectionLayoutDecorationItem)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSCollectionLayoutDecorationItem)
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

