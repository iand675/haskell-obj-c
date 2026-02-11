{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCollectionViewDiffableDataSource@.
module ObjC.AppKit.NSCollectionViewDiffableDataSource
  ( NSCollectionViewDiffableDataSource
  , IsNSCollectionViewDiffableDataSource(..)
  , initWithCollectionView_itemProvider
  , init_
  , new
  , snapshot
  , applySnapshot_animatingDifferences
  , itemIdentifierForIndexPath
  , indexPathForItemIdentifier
  , supplementaryViewProvider
  , setSupplementaryViewProvider
  , initWithCollectionView_itemProviderSelector
  , initSelector
  , newSelector
  , snapshotSelector
  , applySnapshot_animatingDifferencesSelector
  , itemIdentifierForIndexPathSelector
  , indexPathForItemIdentifierSelector
  , supplementaryViewProviderSelector
  , setSupplementaryViewProviderSelector


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

-- | @- initWithCollectionView:itemProvider:@
initWithCollectionView_itemProvider :: (IsNSCollectionViewDiffableDataSource nsCollectionViewDiffableDataSource, IsNSCollectionView collectionView) => nsCollectionViewDiffableDataSource -> collectionView -> Ptr () -> IO (Id NSCollectionViewDiffableDataSource)
initWithCollectionView_itemProvider nsCollectionViewDiffableDataSource  collectionView itemProvider =
withObjCPtr collectionView $ \raw_collectionView ->
    sendMsg nsCollectionViewDiffableDataSource (mkSelector "initWithCollectionView:itemProvider:") (retPtr retVoid) [argPtr (castPtr raw_collectionView :: Ptr ()), argPtr (castPtr itemProvider :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsNSCollectionViewDiffableDataSource nsCollectionViewDiffableDataSource => nsCollectionViewDiffableDataSource -> IO (Id NSCollectionViewDiffableDataSource)
init_ nsCollectionViewDiffableDataSource  =
  sendMsg nsCollectionViewDiffableDataSource (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NSCollectionViewDiffableDataSource)
new  =
  do
    cls' <- getRequiredClass "NSCollectionViewDiffableDataSource"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- snapshot@
snapshot :: IsNSCollectionViewDiffableDataSource nsCollectionViewDiffableDataSource => nsCollectionViewDiffableDataSource -> IO (Id NSDiffableDataSourceSnapshot)
snapshot nsCollectionViewDiffableDataSource  =
  sendMsg nsCollectionViewDiffableDataSource (mkSelector "snapshot") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- applySnapshot:animatingDifferences:@
applySnapshot_animatingDifferences :: (IsNSCollectionViewDiffableDataSource nsCollectionViewDiffableDataSource, IsNSDiffableDataSourceSnapshot snapshot) => nsCollectionViewDiffableDataSource -> snapshot -> Bool -> IO ()
applySnapshot_animatingDifferences nsCollectionViewDiffableDataSource  snapshot animatingDifferences =
withObjCPtr snapshot $ \raw_snapshot ->
    sendMsg nsCollectionViewDiffableDataSource (mkSelector "applySnapshot:animatingDifferences:") retVoid [argPtr (castPtr raw_snapshot :: Ptr ()), argCULong (if animatingDifferences then 1 else 0)]

-- | @- itemIdentifierForIndexPath:@
itemIdentifierForIndexPath :: (IsNSCollectionViewDiffableDataSource nsCollectionViewDiffableDataSource, IsNSIndexPath indexPath) => nsCollectionViewDiffableDataSource -> indexPath -> IO RawId
itemIdentifierForIndexPath nsCollectionViewDiffableDataSource  indexPath =
withObjCPtr indexPath $ \raw_indexPath ->
    fmap (RawId . castPtr) $ sendMsg nsCollectionViewDiffableDataSource (mkSelector "itemIdentifierForIndexPath:") (retPtr retVoid) [argPtr (castPtr raw_indexPath :: Ptr ())]

-- | @- indexPathForItemIdentifier:@
indexPathForItemIdentifier :: IsNSCollectionViewDiffableDataSource nsCollectionViewDiffableDataSource => nsCollectionViewDiffableDataSource -> RawId -> IO (Id NSIndexPath)
indexPathForItemIdentifier nsCollectionViewDiffableDataSource  identifier =
  sendMsg nsCollectionViewDiffableDataSource (mkSelector "indexPathForItemIdentifier:") (retPtr retVoid) [argPtr (castPtr (unRawId identifier) :: Ptr ())] >>= retainedObject . castPtr

-- | @- supplementaryViewProvider@
supplementaryViewProvider :: IsNSCollectionViewDiffableDataSource nsCollectionViewDiffableDataSource => nsCollectionViewDiffableDataSource -> IO (Ptr ())
supplementaryViewProvider nsCollectionViewDiffableDataSource  =
  fmap castPtr $ sendMsg nsCollectionViewDiffableDataSource (mkSelector "supplementaryViewProvider") (retPtr retVoid) []

-- | @- setSupplementaryViewProvider:@
setSupplementaryViewProvider :: IsNSCollectionViewDiffableDataSource nsCollectionViewDiffableDataSource => nsCollectionViewDiffableDataSource -> Ptr () -> IO ()
setSupplementaryViewProvider nsCollectionViewDiffableDataSource  value =
  sendMsg nsCollectionViewDiffableDataSource (mkSelector "setSupplementaryViewProvider:") retVoid [argPtr (castPtr value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCollectionView:itemProvider:@
initWithCollectionView_itemProviderSelector :: Selector
initWithCollectionView_itemProviderSelector = mkSelector "initWithCollectionView:itemProvider:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @snapshot@
snapshotSelector :: Selector
snapshotSelector = mkSelector "snapshot"

-- | @Selector@ for @applySnapshot:animatingDifferences:@
applySnapshot_animatingDifferencesSelector :: Selector
applySnapshot_animatingDifferencesSelector = mkSelector "applySnapshot:animatingDifferences:"

-- | @Selector@ for @itemIdentifierForIndexPath:@
itemIdentifierForIndexPathSelector :: Selector
itemIdentifierForIndexPathSelector = mkSelector "itemIdentifierForIndexPath:"

-- | @Selector@ for @indexPathForItemIdentifier:@
indexPathForItemIdentifierSelector :: Selector
indexPathForItemIdentifierSelector = mkSelector "indexPathForItemIdentifier:"

-- | @Selector@ for @supplementaryViewProvider@
supplementaryViewProviderSelector :: Selector
supplementaryViewProviderSelector = mkSelector "supplementaryViewProvider"

-- | @Selector@ for @setSupplementaryViewProvider:@
setSupplementaryViewProviderSelector :: Selector
setSupplementaryViewProviderSelector = mkSelector "setSupplementaryViewProvider:"

