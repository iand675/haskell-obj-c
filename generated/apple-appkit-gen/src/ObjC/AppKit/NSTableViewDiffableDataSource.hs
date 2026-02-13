{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTableViewDiffableDataSource@.
module ObjC.AppKit.NSTableViewDiffableDataSource
  ( NSTableViewDiffableDataSource
  , IsNSTableViewDiffableDataSource(..)
  , initWithTableView_cellProvider
  , init_
  , new
  , snapshot
  , applySnapshot_animatingDifferences
  , applySnapshot_animatingDifferences_completion
  , itemIdentifierForRow
  , rowForItemIdentifier
  , sectionIdentifierForRow
  , rowForSectionIdentifier
  , rowViewProvider
  , setRowViewProvider
  , sectionHeaderViewProvider
  , setSectionHeaderViewProvider
  , defaultRowAnimation
  , setDefaultRowAnimation
  , applySnapshot_animatingDifferencesSelector
  , applySnapshot_animatingDifferences_completionSelector
  , defaultRowAnimationSelector
  , initSelector
  , initWithTableView_cellProviderSelector
  , itemIdentifierForRowSelector
  , newSelector
  , rowForItemIdentifierSelector
  , rowForSectionIdentifierSelector
  , rowViewProviderSelector
  , sectionHeaderViewProviderSelector
  , sectionIdentifierForRowSelector
  , setDefaultRowAnimationSelector
  , setRowViewProviderSelector
  , setSectionHeaderViewProviderSelector
  , snapshotSelector

  -- * Enum types
  , NSTableViewAnimationOptions(NSTableViewAnimationOptions)
  , pattern NSTableViewAnimationEffectNone
  , pattern NSTableViewAnimationEffectFade
  , pattern NSTableViewAnimationEffectGap
  , pattern NSTableViewAnimationSlideUp
  , pattern NSTableViewAnimationSlideDown
  , pattern NSTableViewAnimationSlideLeft
  , pattern NSTableViewAnimationSlideRight

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithTableView:cellProvider:@
initWithTableView_cellProvider :: (IsNSTableViewDiffableDataSource nsTableViewDiffableDataSource, IsNSTableView tableView) => nsTableViewDiffableDataSource -> tableView -> Ptr () -> IO (Id NSTableViewDiffableDataSource)
initWithTableView_cellProvider nsTableViewDiffableDataSource tableView cellProvider =
  sendOwnedMessage nsTableViewDiffableDataSource initWithTableView_cellProviderSelector (toNSTableView tableView) cellProvider

-- | @- init@
init_ :: IsNSTableViewDiffableDataSource nsTableViewDiffableDataSource => nsTableViewDiffableDataSource -> IO (Id NSTableViewDiffableDataSource)
init_ nsTableViewDiffableDataSource =
  sendOwnedMessage nsTableViewDiffableDataSource initSelector

-- | @+ new@
new :: IO (Id NSTableViewDiffableDataSource)
new  =
  do
    cls' <- getRequiredClass "NSTableViewDiffableDataSource"
    sendOwnedClassMessage cls' newSelector

-- | @- snapshot@
snapshot :: IsNSTableViewDiffableDataSource nsTableViewDiffableDataSource => nsTableViewDiffableDataSource -> IO (Id NSDiffableDataSourceSnapshot)
snapshot nsTableViewDiffableDataSource =
  sendMessage nsTableViewDiffableDataSource snapshotSelector

-- | @- applySnapshot:animatingDifferences:@
applySnapshot_animatingDifferences :: (IsNSTableViewDiffableDataSource nsTableViewDiffableDataSource, IsNSDiffableDataSourceSnapshot snapshot) => nsTableViewDiffableDataSource -> snapshot -> Bool -> IO ()
applySnapshot_animatingDifferences nsTableViewDiffableDataSource snapshot animatingDifferences =
  sendMessage nsTableViewDiffableDataSource applySnapshot_animatingDifferencesSelector (toNSDiffableDataSourceSnapshot snapshot) animatingDifferences

-- | @- applySnapshot:animatingDifferences:completion:@
applySnapshot_animatingDifferences_completion :: (IsNSTableViewDiffableDataSource nsTableViewDiffableDataSource, IsNSDiffableDataSourceSnapshot snapshot) => nsTableViewDiffableDataSource -> snapshot -> Bool -> Ptr () -> IO ()
applySnapshot_animatingDifferences_completion nsTableViewDiffableDataSource snapshot animatingDifferences completion =
  sendMessage nsTableViewDiffableDataSource applySnapshot_animatingDifferences_completionSelector (toNSDiffableDataSourceSnapshot snapshot) animatingDifferences completion

-- | @- itemIdentifierForRow:@
itemIdentifierForRow :: IsNSTableViewDiffableDataSource nsTableViewDiffableDataSource => nsTableViewDiffableDataSource -> CLong -> IO RawId
itemIdentifierForRow nsTableViewDiffableDataSource row =
  sendMessage nsTableViewDiffableDataSource itemIdentifierForRowSelector row

-- | @- rowForItemIdentifier:@
rowForItemIdentifier :: IsNSTableViewDiffableDataSource nsTableViewDiffableDataSource => nsTableViewDiffableDataSource -> RawId -> IO CLong
rowForItemIdentifier nsTableViewDiffableDataSource identifier =
  sendMessage nsTableViewDiffableDataSource rowForItemIdentifierSelector identifier

-- | @- sectionIdentifierForRow:@
sectionIdentifierForRow :: IsNSTableViewDiffableDataSource nsTableViewDiffableDataSource => nsTableViewDiffableDataSource -> CLong -> IO RawId
sectionIdentifierForRow nsTableViewDiffableDataSource row =
  sendMessage nsTableViewDiffableDataSource sectionIdentifierForRowSelector row

-- | @- rowForSectionIdentifier:@
rowForSectionIdentifier :: IsNSTableViewDiffableDataSource nsTableViewDiffableDataSource => nsTableViewDiffableDataSource -> RawId -> IO CLong
rowForSectionIdentifier nsTableViewDiffableDataSource identifier =
  sendMessage nsTableViewDiffableDataSource rowForSectionIdentifierSelector identifier

-- | @- rowViewProvider@
rowViewProvider :: IsNSTableViewDiffableDataSource nsTableViewDiffableDataSource => nsTableViewDiffableDataSource -> IO (Ptr ())
rowViewProvider nsTableViewDiffableDataSource =
  sendMessage nsTableViewDiffableDataSource rowViewProviderSelector

-- | @- setRowViewProvider:@
setRowViewProvider :: IsNSTableViewDiffableDataSource nsTableViewDiffableDataSource => nsTableViewDiffableDataSource -> Ptr () -> IO ()
setRowViewProvider nsTableViewDiffableDataSource value =
  sendMessage nsTableViewDiffableDataSource setRowViewProviderSelector value

-- | @- sectionHeaderViewProvider@
sectionHeaderViewProvider :: IsNSTableViewDiffableDataSource nsTableViewDiffableDataSource => nsTableViewDiffableDataSource -> IO (Ptr ())
sectionHeaderViewProvider nsTableViewDiffableDataSource =
  sendMessage nsTableViewDiffableDataSource sectionHeaderViewProviderSelector

-- | @- setSectionHeaderViewProvider:@
setSectionHeaderViewProvider :: IsNSTableViewDiffableDataSource nsTableViewDiffableDataSource => nsTableViewDiffableDataSource -> Ptr () -> IO ()
setSectionHeaderViewProvider nsTableViewDiffableDataSource value =
  sendMessage nsTableViewDiffableDataSource setSectionHeaderViewProviderSelector value

-- | @- defaultRowAnimation@
defaultRowAnimation :: IsNSTableViewDiffableDataSource nsTableViewDiffableDataSource => nsTableViewDiffableDataSource -> IO NSTableViewAnimationOptions
defaultRowAnimation nsTableViewDiffableDataSource =
  sendMessage nsTableViewDiffableDataSource defaultRowAnimationSelector

-- | @- setDefaultRowAnimation:@
setDefaultRowAnimation :: IsNSTableViewDiffableDataSource nsTableViewDiffableDataSource => nsTableViewDiffableDataSource -> NSTableViewAnimationOptions -> IO ()
setDefaultRowAnimation nsTableViewDiffableDataSource value =
  sendMessage nsTableViewDiffableDataSource setDefaultRowAnimationSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTableView:cellProvider:@
initWithTableView_cellProviderSelector :: Selector '[Id NSTableView, Ptr ()] (Id NSTableViewDiffableDataSource)
initWithTableView_cellProviderSelector = mkSelector "initWithTableView:cellProvider:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSTableViewDiffableDataSource)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSTableViewDiffableDataSource)
newSelector = mkSelector "new"

-- | @Selector@ for @snapshot@
snapshotSelector :: Selector '[] (Id NSDiffableDataSourceSnapshot)
snapshotSelector = mkSelector "snapshot"

-- | @Selector@ for @applySnapshot:animatingDifferences:@
applySnapshot_animatingDifferencesSelector :: Selector '[Id NSDiffableDataSourceSnapshot, Bool] ()
applySnapshot_animatingDifferencesSelector = mkSelector "applySnapshot:animatingDifferences:"

-- | @Selector@ for @applySnapshot:animatingDifferences:completion:@
applySnapshot_animatingDifferences_completionSelector :: Selector '[Id NSDiffableDataSourceSnapshot, Bool, Ptr ()] ()
applySnapshot_animatingDifferences_completionSelector = mkSelector "applySnapshot:animatingDifferences:completion:"

-- | @Selector@ for @itemIdentifierForRow:@
itemIdentifierForRowSelector :: Selector '[CLong] RawId
itemIdentifierForRowSelector = mkSelector "itemIdentifierForRow:"

-- | @Selector@ for @rowForItemIdentifier:@
rowForItemIdentifierSelector :: Selector '[RawId] CLong
rowForItemIdentifierSelector = mkSelector "rowForItemIdentifier:"

-- | @Selector@ for @sectionIdentifierForRow:@
sectionIdentifierForRowSelector :: Selector '[CLong] RawId
sectionIdentifierForRowSelector = mkSelector "sectionIdentifierForRow:"

-- | @Selector@ for @rowForSectionIdentifier:@
rowForSectionIdentifierSelector :: Selector '[RawId] CLong
rowForSectionIdentifierSelector = mkSelector "rowForSectionIdentifier:"

-- | @Selector@ for @rowViewProvider@
rowViewProviderSelector :: Selector '[] (Ptr ())
rowViewProviderSelector = mkSelector "rowViewProvider"

-- | @Selector@ for @setRowViewProvider:@
setRowViewProviderSelector :: Selector '[Ptr ()] ()
setRowViewProviderSelector = mkSelector "setRowViewProvider:"

-- | @Selector@ for @sectionHeaderViewProvider@
sectionHeaderViewProviderSelector :: Selector '[] (Ptr ())
sectionHeaderViewProviderSelector = mkSelector "sectionHeaderViewProvider"

-- | @Selector@ for @setSectionHeaderViewProvider:@
setSectionHeaderViewProviderSelector :: Selector '[Ptr ()] ()
setSectionHeaderViewProviderSelector = mkSelector "setSectionHeaderViewProvider:"

-- | @Selector@ for @defaultRowAnimation@
defaultRowAnimationSelector :: Selector '[] NSTableViewAnimationOptions
defaultRowAnimationSelector = mkSelector "defaultRowAnimation"

-- | @Selector@ for @setDefaultRowAnimation:@
setDefaultRowAnimationSelector :: Selector '[NSTableViewAnimationOptions] ()
setDefaultRowAnimationSelector = mkSelector "setDefaultRowAnimation:"

