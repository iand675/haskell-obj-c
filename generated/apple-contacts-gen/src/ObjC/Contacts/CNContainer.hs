{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An immutable value object representing a container.
--
-- CNContainer is thread safe.
--
-- Generated bindings for @CNContainer@.
module ObjC.Contacts.CNContainer
  ( CNContainer
  , IsCNContainer(..)
  , predicateForContainersWithIdentifiers
  , predicateForContainerOfContactWithIdentifier
  , predicateForContainerOfGroupWithIdentifier
  , identifier
  , name
  , type_
  , identifierSelector
  , nameSelector
  , predicateForContainerOfContactWithIdentifierSelector
  , predicateForContainerOfGroupWithIdentifierSelector
  , predicateForContainersWithIdentifiersSelector
  , typeSelector

  -- * Enum types
  , CNContainerType(CNContainerType)
  , pattern CNContainerTypeUnassigned
  , pattern CNContainerTypeLocal
  , pattern CNContainerTypeExchange
  , pattern CNContainerTypeCardDAV

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Contacts.Internal.Classes
import ObjC.Contacts.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ predicateForContainersWithIdentifiers:@
predicateForContainersWithIdentifiers :: IsNSArray identifiers => identifiers -> IO (Id NSPredicate)
predicateForContainersWithIdentifiers identifiers =
  do
    cls' <- getRequiredClass "CNContainer"
    sendClassMessage cls' predicateForContainersWithIdentifiersSelector (toNSArray identifiers)

-- | If the identifier is for a unified contact then the fetch will return an empty array. To fetch the containers of a unified contact, first fetch the linked contacts then fetch the container of each linked contact.
--
-- ObjC selector: @+ predicateForContainerOfContactWithIdentifier:@
predicateForContainerOfContactWithIdentifier :: IsNSString contactIdentifier => contactIdentifier -> IO (Id NSPredicate)
predicateForContainerOfContactWithIdentifier contactIdentifier =
  do
    cls' <- getRequiredClass "CNContainer"
    sendClassMessage cls' predicateForContainerOfContactWithIdentifierSelector (toNSString contactIdentifier)

-- | @+ predicateForContainerOfGroupWithIdentifier:@
predicateForContainerOfGroupWithIdentifier :: IsNSString groupIdentifier => groupIdentifier -> IO (Id NSPredicate)
predicateForContainerOfGroupWithIdentifier groupIdentifier =
  do
    cls' <- getRequiredClass "CNContainer"
    sendClassMessage cls' predicateForContainerOfGroupWithIdentifierSelector (toNSString groupIdentifier)

-- | The identifier is unique among containers on the device. It can be saved and used for fetching containers next application launch.
--
-- ObjC selector: @- identifier@
identifier :: IsCNContainer cnContainer => cnContainer -> IO (Id NSString)
identifier cnContainer =
  sendMessage cnContainer identifierSelector

-- | @- name@
name :: IsCNContainer cnContainer => cnContainer -> IO (Id NSString)
name cnContainer =
  sendMessage cnContainer nameSelector

-- | @- type@
type_ :: IsCNContainer cnContainer => cnContainer -> IO CNContainerType
type_ cnContainer =
  sendMessage cnContainer typeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @predicateForContainersWithIdentifiers:@
predicateForContainersWithIdentifiersSelector :: Selector '[Id NSArray] (Id NSPredicate)
predicateForContainersWithIdentifiersSelector = mkSelector "predicateForContainersWithIdentifiers:"

-- | @Selector@ for @predicateForContainerOfContactWithIdentifier:@
predicateForContainerOfContactWithIdentifierSelector :: Selector '[Id NSString] (Id NSPredicate)
predicateForContainerOfContactWithIdentifierSelector = mkSelector "predicateForContainerOfContactWithIdentifier:"

-- | @Selector@ for @predicateForContainerOfGroupWithIdentifier:@
predicateForContainerOfGroupWithIdentifierSelector :: Selector '[Id NSString] (Id NSPredicate)
predicateForContainerOfGroupWithIdentifierSelector = mkSelector "predicateForContainerOfGroupWithIdentifier:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @type@
typeSelector :: Selector '[] CNContainerType
typeSelector = mkSelector "type"

