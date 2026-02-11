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
  , initSelector
  , newSelector
  , zIndexSelector
  , setZIndexSelector
  , elementKindSelector


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

-- | @+ backgroundDecorationItemWithElementKind:@
backgroundDecorationItemWithElementKind :: IsNSString elementKind => elementKind -> IO (Id NSCollectionLayoutDecorationItem)
backgroundDecorationItemWithElementKind elementKind =
  do
    cls' <- getRequiredClass "NSCollectionLayoutDecorationItem"
    withObjCPtr elementKind $ \raw_elementKind ->
      sendClassMsg cls' (mkSelector "backgroundDecorationItemWithElementKind:") (retPtr retVoid) [argPtr (castPtr raw_elementKind :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsNSCollectionLayoutDecorationItem nsCollectionLayoutDecorationItem => nsCollectionLayoutDecorationItem -> IO (Id NSCollectionLayoutDecorationItem)
init_ nsCollectionLayoutDecorationItem  =
  sendMsg nsCollectionLayoutDecorationItem (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NSCollectionLayoutDecorationItem)
new  =
  do
    cls' <- getRequiredClass "NSCollectionLayoutDecorationItem"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- zIndex@
zIndex :: IsNSCollectionLayoutDecorationItem nsCollectionLayoutDecorationItem => nsCollectionLayoutDecorationItem -> IO CLong
zIndex nsCollectionLayoutDecorationItem  =
  sendMsg nsCollectionLayoutDecorationItem (mkSelector "zIndex") retCLong []

-- | @- setZIndex:@
setZIndex :: IsNSCollectionLayoutDecorationItem nsCollectionLayoutDecorationItem => nsCollectionLayoutDecorationItem -> CLong -> IO ()
setZIndex nsCollectionLayoutDecorationItem  value =
  sendMsg nsCollectionLayoutDecorationItem (mkSelector "setZIndex:") retVoid [argCLong (fromIntegral value)]

-- | @- elementKind@
elementKind :: IsNSCollectionLayoutDecorationItem nsCollectionLayoutDecorationItem => nsCollectionLayoutDecorationItem -> IO (Id NSString)
elementKind nsCollectionLayoutDecorationItem  =
  sendMsg nsCollectionLayoutDecorationItem (mkSelector "elementKind") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @backgroundDecorationItemWithElementKind:@
backgroundDecorationItemWithElementKindSelector :: Selector
backgroundDecorationItemWithElementKindSelector = mkSelector "backgroundDecorationItemWithElementKind:"

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

