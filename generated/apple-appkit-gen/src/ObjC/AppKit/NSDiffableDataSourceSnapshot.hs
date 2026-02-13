{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSDiffableDataSourceSnapshot@.
module ObjC.AppKit.NSDiffableDataSourceSnapshot
  ( NSDiffableDataSourceSnapshot
  , IsNSDiffableDataSourceSnapshot(..)
  , numberOfItemsInSection
  , itemIdentifiersInSectionWithIdentifier
  , sectionIdentifierForSectionContainingItemIdentifier
  , indexOfItemIdentifier
  , indexOfSectionIdentifier
  , appendItemsWithIdentifiers
  , appendItemsWithIdentifiers_intoSectionWithIdentifier
  , insertItemsWithIdentifiers_beforeItemWithIdentifier
  , insertItemsWithIdentifiers_afterItemWithIdentifier
  , deleteItemsWithIdentifiers
  , deleteAllItems
  , moveItemWithIdentifier_beforeItemWithIdentifier
  , moveItemWithIdentifier_afterItemWithIdentifier
  , reloadItemsWithIdentifiers
  , appendSectionsWithIdentifiers
  , insertSectionsWithIdentifiers_beforeSectionWithIdentifier
  , insertSectionsWithIdentifiers_afterSectionWithIdentifier
  , deleteSectionsWithIdentifiers
  , moveSectionWithIdentifier_beforeSectionWithIdentifier
  , moveSectionWithIdentifier_afterSectionWithIdentifier
  , reloadSectionsWithIdentifiers
  , numberOfItems
  , numberOfSections
  , sectionIdentifiers
  , itemIdentifiers
  , appendItemsWithIdentifiersSelector
  , appendItemsWithIdentifiers_intoSectionWithIdentifierSelector
  , appendSectionsWithIdentifiersSelector
  , deleteAllItemsSelector
  , deleteItemsWithIdentifiersSelector
  , deleteSectionsWithIdentifiersSelector
  , indexOfItemIdentifierSelector
  , indexOfSectionIdentifierSelector
  , insertItemsWithIdentifiers_afterItemWithIdentifierSelector
  , insertItemsWithIdentifiers_beforeItemWithIdentifierSelector
  , insertSectionsWithIdentifiers_afterSectionWithIdentifierSelector
  , insertSectionsWithIdentifiers_beforeSectionWithIdentifierSelector
  , itemIdentifiersInSectionWithIdentifierSelector
  , itemIdentifiersSelector
  , moveItemWithIdentifier_afterItemWithIdentifierSelector
  , moveItemWithIdentifier_beforeItemWithIdentifierSelector
  , moveSectionWithIdentifier_afterSectionWithIdentifierSelector
  , moveSectionWithIdentifier_beforeSectionWithIdentifierSelector
  , numberOfItemsInSectionSelector
  , numberOfItemsSelector
  , numberOfSectionsSelector
  , reloadItemsWithIdentifiersSelector
  , reloadSectionsWithIdentifiersSelector
  , sectionIdentifierForSectionContainingItemIdentifierSelector
  , sectionIdentifiersSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- numberOfItemsInSection:@
numberOfItemsInSection :: IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot => nsDiffableDataSourceSnapshot -> RawId -> IO CLong
numberOfItemsInSection nsDiffableDataSourceSnapshot sectionIdentifier =
  sendMessage nsDiffableDataSourceSnapshot numberOfItemsInSectionSelector sectionIdentifier

-- | @- itemIdentifiersInSectionWithIdentifier:@
itemIdentifiersInSectionWithIdentifier :: IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot => nsDiffableDataSourceSnapshot -> RawId -> IO (Id NSArray)
itemIdentifiersInSectionWithIdentifier nsDiffableDataSourceSnapshot sectionIdentifier =
  sendMessage nsDiffableDataSourceSnapshot itemIdentifiersInSectionWithIdentifierSelector sectionIdentifier

-- | @- sectionIdentifierForSectionContainingItemIdentifier:@
sectionIdentifierForSectionContainingItemIdentifier :: IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot => nsDiffableDataSourceSnapshot -> RawId -> IO RawId
sectionIdentifierForSectionContainingItemIdentifier nsDiffableDataSourceSnapshot itemIdentifier =
  sendMessage nsDiffableDataSourceSnapshot sectionIdentifierForSectionContainingItemIdentifierSelector itemIdentifier

-- | @- indexOfItemIdentifier:@
indexOfItemIdentifier :: IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot => nsDiffableDataSourceSnapshot -> RawId -> IO CLong
indexOfItemIdentifier nsDiffableDataSourceSnapshot itemIdentifier =
  sendMessage nsDiffableDataSourceSnapshot indexOfItemIdentifierSelector itemIdentifier

-- | @- indexOfSectionIdentifier:@
indexOfSectionIdentifier :: IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot => nsDiffableDataSourceSnapshot -> RawId -> IO CLong
indexOfSectionIdentifier nsDiffableDataSourceSnapshot sectionIdentifier =
  sendMessage nsDiffableDataSourceSnapshot indexOfSectionIdentifierSelector sectionIdentifier

-- | @- appendItemsWithIdentifiers:@
appendItemsWithIdentifiers :: (IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot, IsNSArray identifiers) => nsDiffableDataSourceSnapshot -> identifiers -> IO ()
appendItemsWithIdentifiers nsDiffableDataSourceSnapshot identifiers =
  sendMessage nsDiffableDataSourceSnapshot appendItemsWithIdentifiersSelector (toNSArray identifiers)

-- | @- appendItemsWithIdentifiers:intoSectionWithIdentifier:@
appendItemsWithIdentifiers_intoSectionWithIdentifier :: (IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot, IsNSArray identifiers) => nsDiffableDataSourceSnapshot -> identifiers -> RawId -> IO ()
appendItemsWithIdentifiers_intoSectionWithIdentifier nsDiffableDataSourceSnapshot identifiers sectionIdentifier =
  sendMessage nsDiffableDataSourceSnapshot appendItemsWithIdentifiers_intoSectionWithIdentifierSelector (toNSArray identifiers) sectionIdentifier

-- | @- insertItemsWithIdentifiers:beforeItemWithIdentifier:@
insertItemsWithIdentifiers_beforeItemWithIdentifier :: (IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot, IsNSArray identifiers) => nsDiffableDataSourceSnapshot -> identifiers -> RawId -> IO ()
insertItemsWithIdentifiers_beforeItemWithIdentifier nsDiffableDataSourceSnapshot identifiers itemIdentifier =
  sendMessage nsDiffableDataSourceSnapshot insertItemsWithIdentifiers_beforeItemWithIdentifierSelector (toNSArray identifiers) itemIdentifier

-- | @- insertItemsWithIdentifiers:afterItemWithIdentifier:@
insertItemsWithIdentifiers_afterItemWithIdentifier :: (IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot, IsNSArray identifiers) => nsDiffableDataSourceSnapshot -> identifiers -> RawId -> IO ()
insertItemsWithIdentifiers_afterItemWithIdentifier nsDiffableDataSourceSnapshot identifiers itemIdentifier =
  sendMessage nsDiffableDataSourceSnapshot insertItemsWithIdentifiers_afterItemWithIdentifierSelector (toNSArray identifiers) itemIdentifier

-- | @- deleteItemsWithIdentifiers:@
deleteItemsWithIdentifiers :: (IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot, IsNSArray identifiers) => nsDiffableDataSourceSnapshot -> identifiers -> IO ()
deleteItemsWithIdentifiers nsDiffableDataSourceSnapshot identifiers =
  sendMessage nsDiffableDataSourceSnapshot deleteItemsWithIdentifiersSelector (toNSArray identifiers)

-- | @- deleteAllItems@
deleteAllItems :: IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot => nsDiffableDataSourceSnapshot -> IO ()
deleteAllItems nsDiffableDataSourceSnapshot =
  sendMessage nsDiffableDataSourceSnapshot deleteAllItemsSelector

-- | @- moveItemWithIdentifier:beforeItemWithIdentifier:@
moveItemWithIdentifier_beforeItemWithIdentifier :: IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot => nsDiffableDataSourceSnapshot -> RawId -> RawId -> IO ()
moveItemWithIdentifier_beforeItemWithIdentifier nsDiffableDataSourceSnapshot fromIdentifier toIdentifier =
  sendMessage nsDiffableDataSourceSnapshot moveItemWithIdentifier_beforeItemWithIdentifierSelector fromIdentifier toIdentifier

-- | @- moveItemWithIdentifier:afterItemWithIdentifier:@
moveItemWithIdentifier_afterItemWithIdentifier :: IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot => nsDiffableDataSourceSnapshot -> RawId -> RawId -> IO ()
moveItemWithIdentifier_afterItemWithIdentifier nsDiffableDataSourceSnapshot fromIdentifier toIdentifier =
  sendMessage nsDiffableDataSourceSnapshot moveItemWithIdentifier_afterItemWithIdentifierSelector fromIdentifier toIdentifier

-- | @- reloadItemsWithIdentifiers:@
reloadItemsWithIdentifiers :: (IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot, IsNSArray identifiers) => nsDiffableDataSourceSnapshot -> identifiers -> IO ()
reloadItemsWithIdentifiers nsDiffableDataSourceSnapshot identifiers =
  sendMessage nsDiffableDataSourceSnapshot reloadItemsWithIdentifiersSelector (toNSArray identifiers)

-- | @- appendSectionsWithIdentifiers:@
appendSectionsWithIdentifiers :: (IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot, IsNSArray sectionIdentifiers) => nsDiffableDataSourceSnapshot -> sectionIdentifiers -> IO ()
appendSectionsWithIdentifiers nsDiffableDataSourceSnapshot sectionIdentifiers =
  sendMessage nsDiffableDataSourceSnapshot appendSectionsWithIdentifiersSelector (toNSArray sectionIdentifiers)

-- | @- insertSectionsWithIdentifiers:beforeSectionWithIdentifier:@
insertSectionsWithIdentifiers_beforeSectionWithIdentifier :: (IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot, IsNSArray sectionIdentifiers) => nsDiffableDataSourceSnapshot -> sectionIdentifiers -> RawId -> IO ()
insertSectionsWithIdentifiers_beforeSectionWithIdentifier nsDiffableDataSourceSnapshot sectionIdentifiers toSectionIdentifier =
  sendMessage nsDiffableDataSourceSnapshot insertSectionsWithIdentifiers_beforeSectionWithIdentifierSelector (toNSArray sectionIdentifiers) toSectionIdentifier

-- | @- insertSectionsWithIdentifiers:afterSectionWithIdentifier:@
insertSectionsWithIdentifiers_afterSectionWithIdentifier :: (IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot, IsNSArray sectionIdentifiers) => nsDiffableDataSourceSnapshot -> sectionIdentifiers -> RawId -> IO ()
insertSectionsWithIdentifiers_afterSectionWithIdentifier nsDiffableDataSourceSnapshot sectionIdentifiers toSectionIdentifier =
  sendMessage nsDiffableDataSourceSnapshot insertSectionsWithIdentifiers_afterSectionWithIdentifierSelector (toNSArray sectionIdentifiers) toSectionIdentifier

-- | @- deleteSectionsWithIdentifiers:@
deleteSectionsWithIdentifiers :: (IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot, IsNSArray sectionIdentifiers) => nsDiffableDataSourceSnapshot -> sectionIdentifiers -> IO ()
deleteSectionsWithIdentifiers nsDiffableDataSourceSnapshot sectionIdentifiers =
  sendMessage nsDiffableDataSourceSnapshot deleteSectionsWithIdentifiersSelector (toNSArray sectionIdentifiers)

-- | @- moveSectionWithIdentifier:beforeSectionWithIdentifier:@
moveSectionWithIdentifier_beforeSectionWithIdentifier :: IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot => nsDiffableDataSourceSnapshot -> RawId -> RawId -> IO ()
moveSectionWithIdentifier_beforeSectionWithIdentifier nsDiffableDataSourceSnapshot fromSectionIdentifier toSectionIdentifier =
  sendMessage nsDiffableDataSourceSnapshot moveSectionWithIdentifier_beforeSectionWithIdentifierSelector fromSectionIdentifier toSectionIdentifier

-- | @- moveSectionWithIdentifier:afterSectionWithIdentifier:@
moveSectionWithIdentifier_afterSectionWithIdentifier :: IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot => nsDiffableDataSourceSnapshot -> RawId -> RawId -> IO ()
moveSectionWithIdentifier_afterSectionWithIdentifier nsDiffableDataSourceSnapshot fromSectionIdentifier toSectionIdentifier =
  sendMessage nsDiffableDataSourceSnapshot moveSectionWithIdentifier_afterSectionWithIdentifierSelector fromSectionIdentifier toSectionIdentifier

-- | @- reloadSectionsWithIdentifiers:@
reloadSectionsWithIdentifiers :: (IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot, IsNSArray sectionIdentifiers) => nsDiffableDataSourceSnapshot -> sectionIdentifiers -> IO ()
reloadSectionsWithIdentifiers nsDiffableDataSourceSnapshot sectionIdentifiers =
  sendMessage nsDiffableDataSourceSnapshot reloadSectionsWithIdentifiersSelector (toNSArray sectionIdentifiers)

-- | @- numberOfItems@
numberOfItems :: IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot => nsDiffableDataSourceSnapshot -> IO CLong
numberOfItems nsDiffableDataSourceSnapshot =
  sendMessage nsDiffableDataSourceSnapshot numberOfItemsSelector

-- | @- numberOfSections@
numberOfSections :: IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot => nsDiffableDataSourceSnapshot -> IO CLong
numberOfSections nsDiffableDataSourceSnapshot =
  sendMessage nsDiffableDataSourceSnapshot numberOfSectionsSelector

-- | @- sectionIdentifiers@
sectionIdentifiers :: IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot => nsDiffableDataSourceSnapshot -> IO (Id NSArray)
sectionIdentifiers nsDiffableDataSourceSnapshot =
  sendMessage nsDiffableDataSourceSnapshot sectionIdentifiersSelector

-- | @- itemIdentifiers@
itemIdentifiers :: IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot => nsDiffableDataSourceSnapshot -> IO (Id NSArray)
itemIdentifiers nsDiffableDataSourceSnapshot =
  sendMessage nsDiffableDataSourceSnapshot itemIdentifiersSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @numberOfItemsInSection:@
numberOfItemsInSectionSelector :: Selector '[RawId] CLong
numberOfItemsInSectionSelector = mkSelector "numberOfItemsInSection:"

-- | @Selector@ for @itemIdentifiersInSectionWithIdentifier:@
itemIdentifiersInSectionWithIdentifierSelector :: Selector '[RawId] (Id NSArray)
itemIdentifiersInSectionWithIdentifierSelector = mkSelector "itemIdentifiersInSectionWithIdentifier:"

-- | @Selector@ for @sectionIdentifierForSectionContainingItemIdentifier:@
sectionIdentifierForSectionContainingItemIdentifierSelector :: Selector '[RawId] RawId
sectionIdentifierForSectionContainingItemIdentifierSelector = mkSelector "sectionIdentifierForSectionContainingItemIdentifier:"

-- | @Selector@ for @indexOfItemIdentifier:@
indexOfItemIdentifierSelector :: Selector '[RawId] CLong
indexOfItemIdentifierSelector = mkSelector "indexOfItemIdentifier:"

-- | @Selector@ for @indexOfSectionIdentifier:@
indexOfSectionIdentifierSelector :: Selector '[RawId] CLong
indexOfSectionIdentifierSelector = mkSelector "indexOfSectionIdentifier:"

-- | @Selector@ for @appendItemsWithIdentifiers:@
appendItemsWithIdentifiersSelector :: Selector '[Id NSArray] ()
appendItemsWithIdentifiersSelector = mkSelector "appendItemsWithIdentifiers:"

-- | @Selector@ for @appendItemsWithIdentifiers:intoSectionWithIdentifier:@
appendItemsWithIdentifiers_intoSectionWithIdentifierSelector :: Selector '[Id NSArray, RawId] ()
appendItemsWithIdentifiers_intoSectionWithIdentifierSelector = mkSelector "appendItemsWithIdentifiers:intoSectionWithIdentifier:"

-- | @Selector@ for @insertItemsWithIdentifiers:beforeItemWithIdentifier:@
insertItemsWithIdentifiers_beforeItemWithIdentifierSelector :: Selector '[Id NSArray, RawId] ()
insertItemsWithIdentifiers_beforeItemWithIdentifierSelector = mkSelector "insertItemsWithIdentifiers:beforeItemWithIdentifier:"

-- | @Selector@ for @insertItemsWithIdentifiers:afterItemWithIdentifier:@
insertItemsWithIdentifiers_afterItemWithIdentifierSelector :: Selector '[Id NSArray, RawId] ()
insertItemsWithIdentifiers_afterItemWithIdentifierSelector = mkSelector "insertItemsWithIdentifiers:afterItemWithIdentifier:"

-- | @Selector@ for @deleteItemsWithIdentifiers:@
deleteItemsWithIdentifiersSelector :: Selector '[Id NSArray] ()
deleteItemsWithIdentifiersSelector = mkSelector "deleteItemsWithIdentifiers:"

-- | @Selector@ for @deleteAllItems@
deleteAllItemsSelector :: Selector '[] ()
deleteAllItemsSelector = mkSelector "deleteAllItems"

-- | @Selector@ for @moveItemWithIdentifier:beforeItemWithIdentifier:@
moveItemWithIdentifier_beforeItemWithIdentifierSelector :: Selector '[RawId, RawId] ()
moveItemWithIdentifier_beforeItemWithIdentifierSelector = mkSelector "moveItemWithIdentifier:beforeItemWithIdentifier:"

-- | @Selector@ for @moveItemWithIdentifier:afterItemWithIdentifier:@
moveItemWithIdentifier_afterItemWithIdentifierSelector :: Selector '[RawId, RawId] ()
moveItemWithIdentifier_afterItemWithIdentifierSelector = mkSelector "moveItemWithIdentifier:afterItemWithIdentifier:"

-- | @Selector@ for @reloadItemsWithIdentifiers:@
reloadItemsWithIdentifiersSelector :: Selector '[Id NSArray] ()
reloadItemsWithIdentifiersSelector = mkSelector "reloadItemsWithIdentifiers:"

-- | @Selector@ for @appendSectionsWithIdentifiers:@
appendSectionsWithIdentifiersSelector :: Selector '[Id NSArray] ()
appendSectionsWithIdentifiersSelector = mkSelector "appendSectionsWithIdentifiers:"

-- | @Selector@ for @insertSectionsWithIdentifiers:beforeSectionWithIdentifier:@
insertSectionsWithIdentifiers_beforeSectionWithIdentifierSelector :: Selector '[Id NSArray, RawId] ()
insertSectionsWithIdentifiers_beforeSectionWithIdentifierSelector = mkSelector "insertSectionsWithIdentifiers:beforeSectionWithIdentifier:"

-- | @Selector@ for @insertSectionsWithIdentifiers:afterSectionWithIdentifier:@
insertSectionsWithIdentifiers_afterSectionWithIdentifierSelector :: Selector '[Id NSArray, RawId] ()
insertSectionsWithIdentifiers_afterSectionWithIdentifierSelector = mkSelector "insertSectionsWithIdentifiers:afterSectionWithIdentifier:"

-- | @Selector@ for @deleteSectionsWithIdentifiers:@
deleteSectionsWithIdentifiersSelector :: Selector '[Id NSArray] ()
deleteSectionsWithIdentifiersSelector = mkSelector "deleteSectionsWithIdentifiers:"

-- | @Selector@ for @moveSectionWithIdentifier:beforeSectionWithIdentifier:@
moveSectionWithIdentifier_beforeSectionWithIdentifierSelector :: Selector '[RawId, RawId] ()
moveSectionWithIdentifier_beforeSectionWithIdentifierSelector = mkSelector "moveSectionWithIdentifier:beforeSectionWithIdentifier:"

-- | @Selector@ for @moveSectionWithIdentifier:afterSectionWithIdentifier:@
moveSectionWithIdentifier_afterSectionWithIdentifierSelector :: Selector '[RawId, RawId] ()
moveSectionWithIdentifier_afterSectionWithIdentifierSelector = mkSelector "moveSectionWithIdentifier:afterSectionWithIdentifier:"

-- | @Selector@ for @reloadSectionsWithIdentifiers:@
reloadSectionsWithIdentifiersSelector :: Selector '[Id NSArray] ()
reloadSectionsWithIdentifiersSelector = mkSelector "reloadSectionsWithIdentifiers:"

-- | @Selector@ for @numberOfItems@
numberOfItemsSelector :: Selector '[] CLong
numberOfItemsSelector = mkSelector "numberOfItems"

-- | @Selector@ for @numberOfSections@
numberOfSectionsSelector :: Selector '[] CLong
numberOfSectionsSelector = mkSelector "numberOfSections"

-- | @Selector@ for @sectionIdentifiers@
sectionIdentifiersSelector :: Selector '[] (Id NSArray)
sectionIdentifiersSelector = mkSelector "sectionIdentifiers"

-- | @Selector@ for @itemIdentifiers@
itemIdentifiersSelector :: Selector '[] (Id NSArray)
itemIdentifiersSelector = mkSelector "itemIdentifiers"

