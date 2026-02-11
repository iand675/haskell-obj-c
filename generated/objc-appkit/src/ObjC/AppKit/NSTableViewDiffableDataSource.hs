{-# LANGUAGE PatternSynonyms #-}
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
  , initWithTableView_cellProviderSelector
  , initSelector
  , newSelector
  , snapshotSelector
  , applySnapshot_animatingDifferencesSelector
  , applySnapshot_animatingDifferences_completionSelector
  , itemIdentifierForRowSelector
  , rowForItemIdentifierSelector
  , sectionIdentifierForRowSelector
  , rowForSectionIdentifierSelector
  , rowViewProviderSelector
  , setRowViewProviderSelector
  , sectionHeaderViewProviderSelector
  , setSectionHeaderViewProviderSelector
  , defaultRowAnimationSelector
  , setDefaultRowAnimationSelector

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
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithTableView:cellProvider:@
initWithTableView_cellProvider :: (IsNSTableViewDiffableDataSource nsTableViewDiffableDataSource, IsNSTableView tableView) => nsTableViewDiffableDataSource -> tableView -> Ptr () -> IO (Id NSTableViewDiffableDataSource)
initWithTableView_cellProvider nsTableViewDiffableDataSource  tableView cellProvider =
withObjCPtr tableView $ \raw_tableView ->
    sendMsg nsTableViewDiffableDataSource (mkSelector "initWithTableView:cellProvider:") (retPtr retVoid) [argPtr (castPtr raw_tableView :: Ptr ()), argPtr (castPtr cellProvider :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsNSTableViewDiffableDataSource nsTableViewDiffableDataSource => nsTableViewDiffableDataSource -> IO (Id NSTableViewDiffableDataSource)
init_ nsTableViewDiffableDataSource  =
  sendMsg nsTableViewDiffableDataSource (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id NSTableViewDiffableDataSource)
new  =
  do
    cls' <- getRequiredClass "NSTableViewDiffableDataSource"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- snapshot@
snapshot :: IsNSTableViewDiffableDataSource nsTableViewDiffableDataSource => nsTableViewDiffableDataSource -> IO (Id NSDiffableDataSourceSnapshot)
snapshot nsTableViewDiffableDataSource  =
  sendMsg nsTableViewDiffableDataSource (mkSelector "snapshot") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- applySnapshot:animatingDifferences:@
applySnapshot_animatingDifferences :: (IsNSTableViewDiffableDataSource nsTableViewDiffableDataSource, IsNSDiffableDataSourceSnapshot snapshot) => nsTableViewDiffableDataSource -> snapshot -> Bool -> IO ()
applySnapshot_animatingDifferences nsTableViewDiffableDataSource  snapshot animatingDifferences =
withObjCPtr snapshot $ \raw_snapshot ->
    sendMsg nsTableViewDiffableDataSource (mkSelector "applySnapshot:animatingDifferences:") retVoid [argPtr (castPtr raw_snapshot :: Ptr ()), argCULong (if animatingDifferences then 1 else 0)]

-- | @- applySnapshot:animatingDifferences:completion:@
applySnapshot_animatingDifferences_completion :: (IsNSTableViewDiffableDataSource nsTableViewDiffableDataSource, IsNSDiffableDataSourceSnapshot snapshot) => nsTableViewDiffableDataSource -> snapshot -> Bool -> Ptr () -> IO ()
applySnapshot_animatingDifferences_completion nsTableViewDiffableDataSource  snapshot animatingDifferences completion =
withObjCPtr snapshot $ \raw_snapshot ->
    sendMsg nsTableViewDiffableDataSource (mkSelector "applySnapshot:animatingDifferences:completion:") retVoid [argPtr (castPtr raw_snapshot :: Ptr ()), argCULong (if animatingDifferences then 1 else 0), argPtr (castPtr completion :: Ptr ())]

-- | @- itemIdentifierForRow:@
itemIdentifierForRow :: IsNSTableViewDiffableDataSource nsTableViewDiffableDataSource => nsTableViewDiffableDataSource -> CLong -> IO RawId
itemIdentifierForRow nsTableViewDiffableDataSource  row =
  fmap (RawId . castPtr) $ sendMsg nsTableViewDiffableDataSource (mkSelector "itemIdentifierForRow:") (retPtr retVoid) [argCLong (fromIntegral row)]

-- | @- rowForItemIdentifier:@
rowForItemIdentifier :: IsNSTableViewDiffableDataSource nsTableViewDiffableDataSource => nsTableViewDiffableDataSource -> RawId -> IO CLong
rowForItemIdentifier nsTableViewDiffableDataSource  identifier =
  sendMsg nsTableViewDiffableDataSource (mkSelector "rowForItemIdentifier:") retCLong [argPtr (castPtr (unRawId identifier) :: Ptr ())]

-- | @- sectionIdentifierForRow:@
sectionIdentifierForRow :: IsNSTableViewDiffableDataSource nsTableViewDiffableDataSource => nsTableViewDiffableDataSource -> CLong -> IO RawId
sectionIdentifierForRow nsTableViewDiffableDataSource  row =
  fmap (RawId . castPtr) $ sendMsg nsTableViewDiffableDataSource (mkSelector "sectionIdentifierForRow:") (retPtr retVoid) [argCLong (fromIntegral row)]

-- | @- rowForSectionIdentifier:@
rowForSectionIdentifier :: IsNSTableViewDiffableDataSource nsTableViewDiffableDataSource => nsTableViewDiffableDataSource -> RawId -> IO CLong
rowForSectionIdentifier nsTableViewDiffableDataSource  identifier =
  sendMsg nsTableViewDiffableDataSource (mkSelector "rowForSectionIdentifier:") retCLong [argPtr (castPtr (unRawId identifier) :: Ptr ())]

-- | @- rowViewProvider@
rowViewProvider :: IsNSTableViewDiffableDataSource nsTableViewDiffableDataSource => nsTableViewDiffableDataSource -> IO (Ptr ())
rowViewProvider nsTableViewDiffableDataSource  =
  fmap castPtr $ sendMsg nsTableViewDiffableDataSource (mkSelector "rowViewProvider") (retPtr retVoid) []

-- | @- setRowViewProvider:@
setRowViewProvider :: IsNSTableViewDiffableDataSource nsTableViewDiffableDataSource => nsTableViewDiffableDataSource -> Ptr () -> IO ()
setRowViewProvider nsTableViewDiffableDataSource  value =
  sendMsg nsTableViewDiffableDataSource (mkSelector "setRowViewProvider:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | @- sectionHeaderViewProvider@
sectionHeaderViewProvider :: IsNSTableViewDiffableDataSource nsTableViewDiffableDataSource => nsTableViewDiffableDataSource -> IO (Ptr ())
sectionHeaderViewProvider nsTableViewDiffableDataSource  =
  fmap castPtr $ sendMsg nsTableViewDiffableDataSource (mkSelector "sectionHeaderViewProvider") (retPtr retVoid) []

-- | @- setSectionHeaderViewProvider:@
setSectionHeaderViewProvider :: IsNSTableViewDiffableDataSource nsTableViewDiffableDataSource => nsTableViewDiffableDataSource -> Ptr () -> IO ()
setSectionHeaderViewProvider nsTableViewDiffableDataSource  value =
  sendMsg nsTableViewDiffableDataSource (mkSelector "setSectionHeaderViewProvider:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | @- defaultRowAnimation@
defaultRowAnimation :: IsNSTableViewDiffableDataSource nsTableViewDiffableDataSource => nsTableViewDiffableDataSource -> IO NSTableViewAnimationOptions
defaultRowAnimation nsTableViewDiffableDataSource  =
  fmap (coerce :: CULong -> NSTableViewAnimationOptions) $ sendMsg nsTableViewDiffableDataSource (mkSelector "defaultRowAnimation") retCULong []

-- | @- setDefaultRowAnimation:@
setDefaultRowAnimation :: IsNSTableViewDiffableDataSource nsTableViewDiffableDataSource => nsTableViewDiffableDataSource -> NSTableViewAnimationOptions -> IO ()
setDefaultRowAnimation nsTableViewDiffableDataSource  value =
  sendMsg nsTableViewDiffableDataSource (mkSelector "setDefaultRowAnimation:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTableView:cellProvider:@
initWithTableView_cellProviderSelector :: Selector
initWithTableView_cellProviderSelector = mkSelector "initWithTableView:cellProvider:"

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

-- | @Selector@ for @applySnapshot:animatingDifferences:completion:@
applySnapshot_animatingDifferences_completionSelector :: Selector
applySnapshot_animatingDifferences_completionSelector = mkSelector "applySnapshot:animatingDifferences:completion:"

-- | @Selector@ for @itemIdentifierForRow:@
itemIdentifierForRowSelector :: Selector
itemIdentifierForRowSelector = mkSelector "itemIdentifierForRow:"

-- | @Selector@ for @rowForItemIdentifier:@
rowForItemIdentifierSelector :: Selector
rowForItemIdentifierSelector = mkSelector "rowForItemIdentifier:"

-- | @Selector@ for @sectionIdentifierForRow:@
sectionIdentifierForRowSelector :: Selector
sectionIdentifierForRowSelector = mkSelector "sectionIdentifierForRow:"

-- | @Selector@ for @rowForSectionIdentifier:@
rowForSectionIdentifierSelector :: Selector
rowForSectionIdentifierSelector = mkSelector "rowForSectionIdentifier:"

-- | @Selector@ for @rowViewProvider@
rowViewProviderSelector :: Selector
rowViewProviderSelector = mkSelector "rowViewProvider"

-- | @Selector@ for @setRowViewProvider:@
setRowViewProviderSelector :: Selector
setRowViewProviderSelector = mkSelector "setRowViewProvider:"

-- | @Selector@ for @sectionHeaderViewProvider@
sectionHeaderViewProviderSelector :: Selector
sectionHeaderViewProviderSelector = mkSelector "sectionHeaderViewProvider"

-- | @Selector@ for @setSectionHeaderViewProvider:@
setSectionHeaderViewProviderSelector :: Selector
setSectionHeaderViewProviderSelector = mkSelector "setSectionHeaderViewProvider:"

-- | @Selector@ for @defaultRowAnimation@
defaultRowAnimationSelector :: Selector
defaultRowAnimationSelector = mkSelector "defaultRowAnimation"

-- | @Selector@ for @setDefaultRowAnimation:@
setDefaultRowAnimationSelector :: Selector
setDefaultRowAnimationSelector = mkSelector "setDefaultRowAnimation:"

