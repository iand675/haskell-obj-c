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
  , numberOfItemsInSectionSelector
  , itemIdentifiersInSectionWithIdentifierSelector
  , sectionIdentifierForSectionContainingItemIdentifierSelector
  , indexOfItemIdentifierSelector
  , indexOfSectionIdentifierSelector
  , appendItemsWithIdentifiersSelector
  , appendItemsWithIdentifiers_intoSectionWithIdentifierSelector
  , insertItemsWithIdentifiers_beforeItemWithIdentifierSelector
  , insertItemsWithIdentifiers_afterItemWithIdentifierSelector
  , deleteItemsWithIdentifiersSelector
  , deleteAllItemsSelector
  , moveItemWithIdentifier_beforeItemWithIdentifierSelector
  , moveItemWithIdentifier_afterItemWithIdentifierSelector
  , reloadItemsWithIdentifiersSelector
  , appendSectionsWithIdentifiersSelector
  , insertSectionsWithIdentifiers_beforeSectionWithIdentifierSelector
  , insertSectionsWithIdentifiers_afterSectionWithIdentifierSelector
  , deleteSectionsWithIdentifiersSelector
  , moveSectionWithIdentifier_beforeSectionWithIdentifierSelector
  , moveSectionWithIdentifier_afterSectionWithIdentifierSelector
  , reloadSectionsWithIdentifiersSelector
  , numberOfItemsSelector
  , numberOfSectionsSelector
  , sectionIdentifiersSelector
  , itemIdentifiersSelector


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

-- | @- numberOfItemsInSection:@
numberOfItemsInSection :: IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot => nsDiffableDataSourceSnapshot -> RawId -> IO CLong
numberOfItemsInSection nsDiffableDataSourceSnapshot  sectionIdentifier =
  sendMsg nsDiffableDataSourceSnapshot (mkSelector "numberOfItemsInSection:") retCLong [argPtr (castPtr (unRawId sectionIdentifier) :: Ptr ())]

-- | @- itemIdentifiersInSectionWithIdentifier:@
itemIdentifiersInSectionWithIdentifier :: IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot => nsDiffableDataSourceSnapshot -> RawId -> IO (Id NSArray)
itemIdentifiersInSectionWithIdentifier nsDiffableDataSourceSnapshot  sectionIdentifier =
  sendMsg nsDiffableDataSourceSnapshot (mkSelector "itemIdentifiersInSectionWithIdentifier:") (retPtr retVoid) [argPtr (castPtr (unRawId sectionIdentifier) :: Ptr ())] >>= retainedObject . castPtr

-- | @- sectionIdentifierForSectionContainingItemIdentifier:@
sectionIdentifierForSectionContainingItemIdentifier :: IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot => nsDiffableDataSourceSnapshot -> RawId -> IO RawId
sectionIdentifierForSectionContainingItemIdentifier nsDiffableDataSourceSnapshot  itemIdentifier =
  fmap (RawId . castPtr) $ sendMsg nsDiffableDataSourceSnapshot (mkSelector "sectionIdentifierForSectionContainingItemIdentifier:") (retPtr retVoid) [argPtr (castPtr (unRawId itemIdentifier) :: Ptr ())]

-- | @- indexOfItemIdentifier:@
indexOfItemIdentifier :: IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot => nsDiffableDataSourceSnapshot -> RawId -> IO CLong
indexOfItemIdentifier nsDiffableDataSourceSnapshot  itemIdentifier =
  sendMsg nsDiffableDataSourceSnapshot (mkSelector "indexOfItemIdentifier:") retCLong [argPtr (castPtr (unRawId itemIdentifier) :: Ptr ())]

-- | @- indexOfSectionIdentifier:@
indexOfSectionIdentifier :: IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot => nsDiffableDataSourceSnapshot -> RawId -> IO CLong
indexOfSectionIdentifier nsDiffableDataSourceSnapshot  sectionIdentifier =
  sendMsg nsDiffableDataSourceSnapshot (mkSelector "indexOfSectionIdentifier:") retCLong [argPtr (castPtr (unRawId sectionIdentifier) :: Ptr ())]

-- | @- appendItemsWithIdentifiers:@
appendItemsWithIdentifiers :: (IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot, IsNSArray identifiers) => nsDiffableDataSourceSnapshot -> identifiers -> IO ()
appendItemsWithIdentifiers nsDiffableDataSourceSnapshot  identifiers =
withObjCPtr identifiers $ \raw_identifiers ->
    sendMsg nsDiffableDataSourceSnapshot (mkSelector "appendItemsWithIdentifiers:") retVoid [argPtr (castPtr raw_identifiers :: Ptr ())]

-- | @- appendItemsWithIdentifiers:intoSectionWithIdentifier:@
appendItemsWithIdentifiers_intoSectionWithIdentifier :: (IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot, IsNSArray identifiers) => nsDiffableDataSourceSnapshot -> identifiers -> RawId -> IO ()
appendItemsWithIdentifiers_intoSectionWithIdentifier nsDiffableDataSourceSnapshot  identifiers sectionIdentifier =
withObjCPtr identifiers $ \raw_identifiers ->
    sendMsg nsDiffableDataSourceSnapshot (mkSelector "appendItemsWithIdentifiers:intoSectionWithIdentifier:") retVoid [argPtr (castPtr raw_identifiers :: Ptr ()), argPtr (castPtr (unRawId sectionIdentifier) :: Ptr ())]

-- | @- insertItemsWithIdentifiers:beforeItemWithIdentifier:@
insertItemsWithIdentifiers_beforeItemWithIdentifier :: (IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot, IsNSArray identifiers) => nsDiffableDataSourceSnapshot -> identifiers -> RawId -> IO ()
insertItemsWithIdentifiers_beforeItemWithIdentifier nsDiffableDataSourceSnapshot  identifiers itemIdentifier =
withObjCPtr identifiers $ \raw_identifiers ->
    sendMsg nsDiffableDataSourceSnapshot (mkSelector "insertItemsWithIdentifiers:beforeItemWithIdentifier:") retVoid [argPtr (castPtr raw_identifiers :: Ptr ()), argPtr (castPtr (unRawId itemIdentifier) :: Ptr ())]

-- | @- insertItemsWithIdentifiers:afterItemWithIdentifier:@
insertItemsWithIdentifiers_afterItemWithIdentifier :: (IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot, IsNSArray identifiers) => nsDiffableDataSourceSnapshot -> identifiers -> RawId -> IO ()
insertItemsWithIdentifiers_afterItemWithIdentifier nsDiffableDataSourceSnapshot  identifiers itemIdentifier =
withObjCPtr identifiers $ \raw_identifiers ->
    sendMsg nsDiffableDataSourceSnapshot (mkSelector "insertItemsWithIdentifiers:afterItemWithIdentifier:") retVoid [argPtr (castPtr raw_identifiers :: Ptr ()), argPtr (castPtr (unRawId itemIdentifier) :: Ptr ())]

-- | @- deleteItemsWithIdentifiers:@
deleteItemsWithIdentifiers :: (IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot, IsNSArray identifiers) => nsDiffableDataSourceSnapshot -> identifiers -> IO ()
deleteItemsWithIdentifiers nsDiffableDataSourceSnapshot  identifiers =
withObjCPtr identifiers $ \raw_identifiers ->
    sendMsg nsDiffableDataSourceSnapshot (mkSelector "deleteItemsWithIdentifiers:") retVoid [argPtr (castPtr raw_identifiers :: Ptr ())]

-- | @- deleteAllItems@
deleteAllItems :: IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot => nsDiffableDataSourceSnapshot -> IO ()
deleteAllItems nsDiffableDataSourceSnapshot  =
  sendMsg nsDiffableDataSourceSnapshot (mkSelector "deleteAllItems") retVoid []

-- | @- moveItemWithIdentifier:beforeItemWithIdentifier:@
moveItemWithIdentifier_beforeItemWithIdentifier :: IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot => nsDiffableDataSourceSnapshot -> RawId -> RawId -> IO ()
moveItemWithIdentifier_beforeItemWithIdentifier nsDiffableDataSourceSnapshot  fromIdentifier toIdentifier =
  sendMsg nsDiffableDataSourceSnapshot (mkSelector "moveItemWithIdentifier:beforeItemWithIdentifier:") retVoid [argPtr (castPtr (unRawId fromIdentifier) :: Ptr ()), argPtr (castPtr (unRawId toIdentifier) :: Ptr ())]

-- | @- moveItemWithIdentifier:afterItemWithIdentifier:@
moveItemWithIdentifier_afterItemWithIdentifier :: IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot => nsDiffableDataSourceSnapshot -> RawId -> RawId -> IO ()
moveItemWithIdentifier_afterItemWithIdentifier nsDiffableDataSourceSnapshot  fromIdentifier toIdentifier =
  sendMsg nsDiffableDataSourceSnapshot (mkSelector "moveItemWithIdentifier:afterItemWithIdentifier:") retVoid [argPtr (castPtr (unRawId fromIdentifier) :: Ptr ()), argPtr (castPtr (unRawId toIdentifier) :: Ptr ())]

-- | @- reloadItemsWithIdentifiers:@
reloadItemsWithIdentifiers :: (IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot, IsNSArray identifiers) => nsDiffableDataSourceSnapshot -> identifiers -> IO ()
reloadItemsWithIdentifiers nsDiffableDataSourceSnapshot  identifiers =
withObjCPtr identifiers $ \raw_identifiers ->
    sendMsg nsDiffableDataSourceSnapshot (mkSelector "reloadItemsWithIdentifiers:") retVoid [argPtr (castPtr raw_identifiers :: Ptr ())]

-- | @- appendSectionsWithIdentifiers:@
appendSectionsWithIdentifiers :: (IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot, IsNSArray sectionIdentifiers) => nsDiffableDataSourceSnapshot -> sectionIdentifiers -> IO ()
appendSectionsWithIdentifiers nsDiffableDataSourceSnapshot  sectionIdentifiers =
withObjCPtr sectionIdentifiers $ \raw_sectionIdentifiers ->
    sendMsg nsDiffableDataSourceSnapshot (mkSelector "appendSectionsWithIdentifiers:") retVoid [argPtr (castPtr raw_sectionIdentifiers :: Ptr ())]

-- | @- insertSectionsWithIdentifiers:beforeSectionWithIdentifier:@
insertSectionsWithIdentifiers_beforeSectionWithIdentifier :: (IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot, IsNSArray sectionIdentifiers) => nsDiffableDataSourceSnapshot -> sectionIdentifiers -> RawId -> IO ()
insertSectionsWithIdentifiers_beforeSectionWithIdentifier nsDiffableDataSourceSnapshot  sectionIdentifiers toSectionIdentifier =
withObjCPtr sectionIdentifiers $ \raw_sectionIdentifiers ->
    sendMsg nsDiffableDataSourceSnapshot (mkSelector "insertSectionsWithIdentifiers:beforeSectionWithIdentifier:") retVoid [argPtr (castPtr raw_sectionIdentifiers :: Ptr ()), argPtr (castPtr (unRawId toSectionIdentifier) :: Ptr ())]

-- | @- insertSectionsWithIdentifiers:afterSectionWithIdentifier:@
insertSectionsWithIdentifiers_afterSectionWithIdentifier :: (IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot, IsNSArray sectionIdentifiers) => nsDiffableDataSourceSnapshot -> sectionIdentifiers -> RawId -> IO ()
insertSectionsWithIdentifiers_afterSectionWithIdentifier nsDiffableDataSourceSnapshot  sectionIdentifiers toSectionIdentifier =
withObjCPtr sectionIdentifiers $ \raw_sectionIdentifiers ->
    sendMsg nsDiffableDataSourceSnapshot (mkSelector "insertSectionsWithIdentifiers:afterSectionWithIdentifier:") retVoid [argPtr (castPtr raw_sectionIdentifiers :: Ptr ()), argPtr (castPtr (unRawId toSectionIdentifier) :: Ptr ())]

-- | @- deleteSectionsWithIdentifiers:@
deleteSectionsWithIdentifiers :: (IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot, IsNSArray sectionIdentifiers) => nsDiffableDataSourceSnapshot -> sectionIdentifiers -> IO ()
deleteSectionsWithIdentifiers nsDiffableDataSourceSnapshot  sectionIdentifiers =
withObjCPtr sectionIdentifiers $ \raw_sectionIdentifiers ->
    sendMsg nsDiffableDataSourceSnapshot (mkSelector "deleteSectionsWithIdentifiers:") retVoid [argPtr (castPtr raw_sectionIdentifiers :: Ptr ())]

-- | @- moveSectionWithIdentifier:beforeSectionWithIdentifier:@
moveSectionWithIdentifier_beforeSectionWithIdentifier :: IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot => nsDiffableDataSourceSnapshot -> RawId -> RawId -> IO ()
moveSectionWithIdentifier_beforeSectionWithIdentifier nsDiffableDataSourceSnapshot  fromSectionIdentifier toSectionIdentifier =
  sendMsg nsDiffableDataSourceSnapshot (mkSelector "moveSectionWithIdentifier:beforeSectionWithIdentifier:") retVoid [argPtr (castPtr (unRawId fromSectionIdentifier) :: Ptr ()), argPtr (castPtr (unRawId toSectionIdentifier) :: Ptr ())]

-- | @- moveSectionWithIdentifier:afterSectionWithIdentifier:@
moveSectionWithIdentifier_afterSectionWithIdentifier :: IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot => nsDiffableDataSourceSnapshot -> RawId -> RawId -> IO ()
moveSectionWithIdentifier_afterSectionWithIdentifier nsDiffableDataSourceSnapshot  fromSectionIdentifier toSectionIdentifier =
  sendMsg nsDiffableDataSourceSnapshot (mkSelector "moveSectionWithIdentifier:afterSectionWithIdentifier:") retVoid [argPtr (castPtr (unRawId fromSectionIdentifier) :: Ptr ()), argPtr (castPtr (unRawId toSectionIdentifier) :: Ptr ())]

-- | @- reloadSectionsWithIdentifiers:@
reloadSectionsWithIdentifiers :: (IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot, IsNSArray sectionIdentifiers) => nsDiffableDataSourceSnapshot -> sectionIdentifiers -> IO ()
reloadSectionsWithIdentifiers nsDiffableDataSourceSnapshot  sectionIdentifiers =
withObjCPtr sectionIdentifiers $ \raw_sectionIdentifiers ->
    sendMsg nsDiffableDataSourceSnapshot (mkSelector "reloadSectionsWithIdentifiers:") retVoid [argPtr (castPtr raw_sectionIdentifiers :: Ptr ())]

-- | @- numberOfItems@
numberOfItems :: IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot => nsDiffableDataSourceSnapshot -> IO CLong
numberOfItems nsDiffableDataSourceSnapshot  =
  sendMsg nsDiffableDataSourceSnapshot (mkSelector "numberOfItems") retCLong []

-- | @- numberOfSections@
numberOfSections :: IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot => nsDiffableDataSourceSnapshot -> IO CLong
numberOfSections nsDiffableDataSourceSnapshot  =
  sendMsg nsDiffableDataSourceSnapshot (mkSelector "numberOfSections") retCLong []

-- | @- sectionIdentifiers@
sectionIdentifiers :: IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot => nsDiffableDataSourceSnapshot -> IO (Id NSArray)
sectionIdentifiers nsDiffableDataSourceSnapshot  =
  sendMsg nsDiffableDataSourceSnapshot (mkSelector "sectionIdentifiers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- itemIdentifiers@
itemIdentifiers :: IsNSDiffableDataSourceSnapshot nsDiffableDataSourceSnapshot => nsDiffableDataSourceSnapshot -> IO (Id NSArray)
itemIdentifiers nsDiffableDataSourceSnapshot  =
  sendMsg nsDiffableDataSourceSnapshot (mkSelector "itemIdentifiers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @numberOfItemsInSection:@
numberOfItemsInSectionSelector :: Selector
numberOfItemsInSectionSelector = mkSelector "numberOfItemsInSection:"

-- | @Selector@ for @itemIdentifiersInSectionWithIdentifier:@
itemIdentifiersInSectionWithIdentifierSelector :: Selector
itemIdentifiersInSectionWithIdentifierSelector = mkSelector "itemIdentifiersInSectionWithIdentifier:"

-- | @Selector@ for @sectionIdentifierForSectionContainingItemIdentifier:@
sectionIdentifierForSectionContainingItemIdentifierSelector :: Selector
sectionIdentifierForSectionContainingItemIdentifierSelector = mkSelector "sectionIdentifierForSectionContainingItemIdentifier:"

-- | @Selector@ for @indexOfItemIdentifier:@
indexOfItemIdentifierSelector :: Selector
indexOfItemIdentifierSelector = mkSelector "indexOfItemIdentifier:"

-- | @Selector@ for @indexOfSectionIdentifier:@
indexOfSectionIdentifierSelector :: Selector
indexOfSectionIdentifierSelector = mkSelector "indexOfSectionIdentifier:"

-- | @Selector@ for @appendItemsWithIdentifiers:@
appendItemsWithIdentifiersSelector :: Selector
appendItemsWithIdentifiersSelector = mkSelector "appendItemsWithIdentifiers:"

-- | @Selector@ for @appendItemsWithIdentifiers:intoSectionWithIdentifier:@
appendItemsWithIdentifiers_intoSectionWithIdentifierSelector :: Selector
appendItemsWithIdentifiers_intoSectionWithIdentifierSelector = mkSelector "appendItemsWithIdentifiers:intoSectionWithIdentifier:"

-- | @Selector@ for @insertItemsWithIdentifiers:beforeItemWithIdentifier:@
insertItemsWithIdentifiers_beforeItemWithIdentifierSelector :: Selector
insertItemsWithIdentifiers_beforeItemWithIdentifierSelector = mkSelector "insertItemsWithIdentifiers:beforeItemWithIdentifier:"

-- | @Selector@ for @insertItemsWithIdentifiers:afterItemWithIdentifier:@
insertItemsWithIdentifiers_afterItemWithIdentifierSelector :: Selector
insertItemsWithIdentifiers_afterItemWithIdentifierSelector = mkSelector "insertItemsWithIdentifiers:afterItemWithIdentifier:"

-- | @Selector@ for @deleteItemsWithIdentifiers:@
deleteItemsWithIdentifiersSelector :: Selector
deleteItemsWithIdentifiersSelector = mkSelector "deleteItemsWithIdentifiers:"

-- | @Selector@ for @deleteAllItems@
deleteAllItemsSelector :: Selector
deleteAllItemsSelector = mkSelector "deleteAllItems"

-- | @Selector@ for @moveItemWithIdentifier:beforeItemWithIdentifier:@
moveItemWithIdentifier_beforeItemWithIdentifierSelector :: Selector
moveItemWithIdentifier_beforeItemWithIdentifierSelector = mkSelector "moveItemWithIdentifier:beforeItemWithIdentifier:"

-- | @Selector@ for @moveItemWithIdentifier:afterItemWithIdentifier:@
moveItemWithIdentifier_afterItemWithIdentifierSelector :: Selector
moveItemWithIdentifier_afterItemWithIdentifierSelector = mkSelector "moveItemWithIdentifier:afterItemWithIdentifier:"

-- | @Selector@ for @reloadItemsWithIdentifiers:@
reloadItemsWithIdentifiersSelector :: Selector
reloadItemsWithIdentifiersSelector = mkSelector "reloadItemsWithIdentifiers:"

-- | @Selector@ for @appendSectionsWithIdentifiers:@
appendSectionsWithIdentifiersSelector :: Selector
appendSectionsWithIdentifiersSelector = mkSelector "appendSectionsWithIdentifiers:"

-- | @Selector@ for @insertSectionsWithIdentifiers:beforeSectionWithIdentifier:@
insertSectionsWithIdentifiers_beforeSectionWithIdentifierSelector :: Selector
insertSectionsWithIdentifiers_beforeSectionWithIdentifierSelector = mkSelector "insertSectionsWithIdentifiers:beforeSectionWithIdentifier:"

-- | @Selector@ for @insertSectionsWithIdentifiers:afterSectionWithIdentifier:@
insertSectionsWithIdentifiers_afterSectionWithIdentifierSelector :: Selector
insertSectionsWithIdentifiers_afterSectionWithIdentifierSelector = mkSelector "insertSectionsWithIdentifiers:afterSectionWithIdentifier:"

-- | @Selector@ for @deleteSectionsWithIdentifiers:@
deleteSectionsWithIdentifiersSelector :: Selector
deleteSectionsWithIdentifiersSelector = mkSelector "deleteSectionsWithIdentifiers:"

-- | @Selector@ for @moveSectionWithIdentifier:beforeSectionWithIdentifier:@
moveSectionWithIdentifier_beforeSectionWithIdentifierSelector :: Selector
moveSectionWithIdentifier_beforeSectionWithIdentifierSelector = mkSelector "moveSectionWithIdentifier:beforeSectionWithIdentifier:"

-- | @Selector@ for @moveSectionWithIdentifier:afterSectionWithIdentifier:@
moveSectionWithIdentifier_afterSectionWithIdentifierSelector :: Selector
moveSectionWithIdentifier_afterSectionWithIdentifierSelector = mkSelector "moveSectionWithIdentifier:afterSectionWithIdentifier:"

-- | @Selector@ for @reloadSectionsWithIdentifiers:@
reloadSectionsWithIdentifiersSelector :: Selector
reloadSectionsWithIdentifiersSelector = mkSelector "reloadSectionsWithIdentifiers:"

-- | @Selector@ for @numberOfItems@
numberOfItemsSelector :: Selector
numberOfItemsSelector = mkSelector "numberOfItems"

-- | @Selector@ for @numberOfSections@
numberOfSectionsSelector :: Selector
numberOfSectionsSelector = mkSelector "numberOfSections"

-- | @Selector@ for @sectionIdentifiers@
sectionIdentifiersSelector :: Selector
sectionIdentifiersSelector = mkSelector "sectionIdentifiers"

-- | @Selector@ for @itemIdentifiers@
itemIdentifiersSelector :: Selector
itemIdentifiersSelector = mkSelector "itemIdentifiers"

