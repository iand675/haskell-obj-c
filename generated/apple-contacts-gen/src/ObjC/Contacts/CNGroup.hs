{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An immutable value object representing a group.
--
-- CNGroup is thread safe.
--
-- Generated bindings for @CNGroup@.
module ObjC.Contacts.CNGroup
  ( CNGroup
  , IsCNGroup(..)
  , predicateForGroupsWithIdentifiers
  , predicateForSubgroupsInGroupWithIdentifier
  , predicateForGroupsInContainerWithIdentifier
  , identifier
  , name
  , identifierSelector
  , nameSelector
  , predicateForGroupsInContainerWithIdentifierSelector
  , predicateForGroupsWithIdentifiersSelector
  , predicateForSubgroupsInGroupWithIdentifierSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Contacts.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ predicateForGroupsWithIdentifiers:@
predicateForGroupsWithIdentifiers :: IsNSArray identifiers => identifiers -> IO (Id NSPredicate)
predicateForGroupsWithIdentifiers identifiers =
  do
    cls' <- getRequiredClass "CNGroup"
    sendClassMessage cls' predicateForGroupsWithIdentifiersSelector (toNSArray identifiers)

-- | @+ predicateForSubgroupsInGroupWithIdentifier:@
predicateForSubgroupsInGroupWithIdentifier :: IsNSString parentGroupIdentifier => parentGroupIdentifier -> IO (Id NSPredicate)
predicateForSubgroupsInGroupWithIdentifier parentGroupIdentifier =
  do
    cls' <- getRequiredClass "CNGroup"
    sendClassMessage cls' predicateForSubgroupsInGroupWithIdentifierSelector (toNSString parentGroupIdentifier)

-- | @+ predicateForGroupsInContainerWithIdentifier:@
predicateForGroupsInContainerWithIdentifier :: IsNSString containerIdentifier => containerIdentifier -> IO (Id NSPredicate)
predicateForGroupsInContainerWithIdentifier containerIdentifier =
  do
    cls' <- getRequiredClass "CNGroup"
    sendClassMessage cls' predicateForGroupsInContainerWithIdentifierSelector (toNSString containerIdentifier)

-- | The identifier is unique among groups on the device. It can be saved and used for fetching groups next application launch.
--
-- ObjC selector: @- identifier@
identifier :: IsCNGroup cnGroup => cnGroup -> IO (Id NSString)
identifier cnGroup =
  sendMessage cnGroup identifierSelector

-- | @- name@
name :: IsCNGroup cnGroup => cnGroup -> IO (Id NSString)
name cnGroup =
  sendMessage cnGroup nameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @predicateForGroupsWithIdentifiers:@
predicateForGroupsWithIdentifiersSelector :: Selector '[Id NSArray] (Id NSPredicate)
predicateForGroupsWithIdentifiersSelector = mkSelector "predicateForGroupsWithIdentifiers:"

-- | @Selector@ for @predicateForSubgroupsInGroupWithIdentifier:@
predicateForSubgroupsInGroupWithIdentifierSelector :: Selector '[Id NSString] (Id NSPredicate)
predicateForSubgroupsInGroupWithIdentifierSelector = mkSelector "predicateForSubgroupsInGroupWithIdentifier:"

-- | @Selector@ for @predicateForGroupsInContainerWithIdentifier:@
predicateForGroupsInContainerWithIdentifierSelector :: Selector '[Id NSString] (Id NSPredicate)
predicateForGroupsInContainerWithIdentifierSelector = mkSelector "predicateForGroupsInContainerWithIdentifier:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

