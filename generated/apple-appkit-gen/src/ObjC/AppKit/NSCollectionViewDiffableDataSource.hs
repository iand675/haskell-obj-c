{-# LANGUAGE DataKinds #-}
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
  , applySnapshot_animatingDifferencesSelector
  , indexPathForItemIdentifierSelector
  , initSelector
  , initWithCollectionView_itemProviderSelector
  , itemIdentifierForIndexPathSelector
  , newSelector
  , setSupplementaryViewProviderSelector
  , snapshotSelector
  , supplementaryViewProviderSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithCollectionView:itemProvider:@
initWithCollectionView_itemProvider :: (IsNSCollectionViewDiffableDataSource nsCollectionViewDiffableDataSource, IsNSCollectionView collectionView) => nsCollectionViewDiffableDataSource -> collectionView -> Ptr () -> IO (Id NSCollectionViewDiffableDataSource)
initWithCollectionView_itemProvider nsCollectionViewDiffableDataSource collectionView itemProvider =
  sendOwnedMessage nsCollectionViewDiffableDataSource initWithCollectionView_itemProviderSelector (toNSCollectionView collectionView) itemProvider

-- | @- init@
init_ :: IsNSCollectionViewDiffableDataSource nsCollectionViewDiffableDataSource => nsCollectionViewDiffableDataSource -> IO (Id NSCollectionViewDiffableDataSource)
init_ nsCollectionViewDiffableDataSource =
  sendOwnedMessage nsCollectionViewDiffableDataSource initSelector

-- | @+ new@
new :: IO (Id NSCollectionViewDiffableDataSource)
new  =
  do
    cls' <- getRequiredClass "NSCollectionViewDiffableDataSource"
    sendOwnedClassMessage cls' newSelector

-- | @- snapshot@
snapshot :: IsNSCollectionViewDiffableDataSource nsCollectionViewDiffableDataSource => nsCollectionViewDiffableDataSource -> IO (Id NSDiffableDataSourceSnapshot)
snapshot nsCollectionViewDiffableDataSource =
  sendMessage nsCollectionViewDiffableDataSource snapshotSelector

-- | @- applySnapshot:animatingDifferences:@
applySnapshot_animatingDifferences :: (IsNSCollectionViewDiffableDataSource nsCollectionViewDiffableDataSource, IsNSDiffableDataSourceSnapshot snapshot) => nsCollectionViewDiffableDataSource -> snapshot -> Bool -> IO ()
applySnapshot_animatingDifferences nsCollectionViewDiffableDataSource snapshot animatingDifferences =
  sendMessage nsCollectionViewDiffableDataSource applySnapshot_animatingDifferencesSelector (toNSDiffableDataSourceSnapshot snapshot) animatingDifferences

-- | @- itemIdentifierForIndexPath:@
itemIdentifierForIndexPath :: (IsNSCollectionViewDiffableDataSource nsCollectionViewDiffableDataSource, IsNSIndexPath indexPath) => nsCollectionViewDiffableDataSource -> indexPath -> IO RawId
itemIdentifierForIndexPath nsCollectionViewDiffableDataSource indexPath =
  sendMessage nsCollectionViewDiffableDataSource itemIdentifierForIndexPathSelector (toNSIndexPath indexPath)

-- | @- indexPathForItemIdentifier:@
indexPathForItemIdentifier :: IsNSCollectionViewDiffableDataSource nsCollectionViewDiffableDataSource => nsCollectionViewDiffableDataSource -> RawId -> IO (Id NSIndexPath)
indexPathForItemIdentifier nsCollectionViewDiffableDataSource identifier =
  sendMessage nsCollectionViewDiffableDataSource indexPathForItemIdentifierSelector identifier

-- | @- supplementaryViewProvider@
supplementaryViewProvider :: IsNSCollectionViewDiffableDataSource nsCollectionViewDiffableDataSource => nsCollectionViewDiffableDataSource -> IO (Ptr ())
supplementaryViewProvider nsCollectionViewDiffableDataSource =
  sendMessage nsCollectionViewDiffableDataSource supplementaryViewProviderSelector

-- | @- setSupplementaryViewProvider:@
setSupplementaryViewProvider :: IsNSCollectionViewDiffableDataSource nsCollectionViewDiffableDataSource => nsCollectionViewDiffableDataSource -> Ptr () -> IO ()
setSupplementaryViewProvider nsCollectionViewDiffableDataSource value =
  sendMessage nsCollectionViewDiffableDataSource setSupplementaryViewProviderSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCollectionView:itemProvider:@
initWithCollectionView_itemProviderSelector :: Selector '[Id NSCollectionView, Ptr ()] (Id NSCollectionViewDiffableDataSource)
initWithCollectionView_itemProviderSelector = mkSelector "initWithCollectionView:itemProvider:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSCollectionViewDiffableDataSource)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSCollectionViewDiffableDataSource)
newSelector = mkSelector "new"

-- | @Selector@ for @snapshot@
snapshotSelector :: Selector '[] (Id NSDiffableDataSourceSnapshot)
snapshotSelector = mkSelector "snapshot"

-- | @Selector@ for @applySnapshot:animatingDifferences:@
applySnapshot_animatingDifferencesSelector :: Selector '[Id NSDiffableDataSourceSnapshot, Bool] ()
applySnapshot_animatingDifferencesSelector = mkSelector "applySnapshot:animatingDifferences:"

-- | @Selector@ for @itemIdentifierForIndexPath:@
itemIdentifierForIndexPathSelector :: Selector '[Id NSIndexPath] RawId
itemIdentifierForIndexPathSelector = mkSelector "itemIdentifierForIndexPath:"

-- | @Selector@ for @indexPathForItemIdentifier:@
indexPathForItemIdentifierSelector :: Selector '[RawId] (Id NSIndexPath)
indexPathForItemIdentifierSelector = mkSelector "indexPathForItemIdentifier:"

-- | @Selector@ for @supplementaryViewProvider@
supplementaryViewProviderSelector :: Selector '[] (Ptr ())
supplementaryViewProviderSelector = mkSelector "supplementaryViewProvider"

-- | @Selector@ for @setSupplementaryViewProvider:@
setSupplementaryViewProviderSelector :: Selector '[Ptr ()] ()
setSupplementaryViewProviderSelector = mkSelector "setSupplementaryViewProvider:"

