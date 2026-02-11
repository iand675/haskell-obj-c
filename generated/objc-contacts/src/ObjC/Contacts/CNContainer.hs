{-# LANGUAGE PatternSynonyms #-}
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
  , predicateForContainersWithIdentifiersSelector
  , predicateForContainerOfContactWithIdentifierSelector
  , predicateForContainerOfGroupWithIdentifierSelector
  , identifierSelector
  , nameSelector
  , typeSelector

  -- * Enum types
  , CNContainerType(CNContainerType)
  , pattern CNContainerTypeUnassigned
  , pattern CNContainerTypeLocal
  , pattern CNContainerTypeExchange
  , pattern CNContainerTypeCardDAV

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

import ObjC.Contacts.Internal.Classes
import ObjC.Contacts.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ predicateForContainersWithIdentifiers:@
predicateForContainersWithIdentifiers :: IsNSArray identifiers => identifiers -> IO (Id NSPredicate)
predicateForContainersWithIdentifiers identifiers =
  do
    cls' <- getRequiredClass "CNContainer"
    withObjCPtr identifiers $ \raw_identifiers ->
      sendClassMsg cls' (mkSelector "predicateForContainersWithIdentifiers:") (retPtr retVoid) [argPtr (castPtr raw_identifiers :: Ptr ())] >>= retainedObject . castPtr

-- | If the identifier is for a unified contact then the fetch will return an empty array. To fetch the containers of a unified contact, first fetch the linked contacts then fetch the container of each linked contact.
--
-- ObjC selector: @+ predicateForContainerOfContactWithIdentifier:@
predicateForContainerOfContactWithIdentifier :: IsNSString contactIdentifier => contactIdentifier -> IO (Id NSPredicate)
predicateForContainerOfContactWithIdentifier contactIdentifier =
  do
    cls' <- getRequiredClass "CNContainer"
    withObjCPtr contactIdentifier $ \raw_contactIdentifier ->
      sendClassMsg cls' (mkSelector "predicateForContainerOfContactWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_contactIdentifier :: Ptr ())] >>= retainedObject . castPtr

-- | @+ predicateForContainerOfGroupWithIdentifier:@
predicateForContainerOfGroupWithIdentifier :: IsNSString groupIdentifier => groupIdentifier -> IO (Id NSPredicate)
predicateForContainerOfGroupWithIdentifier groupIdentifier =
  do
    cls' <- getRequiredClass "CNContainer"
    withObjCPtr groupIdentifier $ \raw_groupIdentifier ->
      sendClassMsg cls' (mkSelector "predicateForContainerOfGroupWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_groupIdentifier :: Ptr ())] >>= retainedObject . castPtr

-- | The identifier is unique among containers on the device. It can be saved and used for fetching containers next application launch.
--
-- ObjC selector: @- identifier@
identifier :: IsCNContainer cnContainer => cnContainer -> IO (Id NSString)
identifier cnContainer  =
  sendMsg cnContainer (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- name@
name :: IsCNContainer cnContainer => cnContainer -> IO (Id NSString)
name cnContainer  =
  sendMsg cnContainer (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- type@
type_ :: IsCNContainer cnContainer => cnContainer -> IO CNContainerType
type_ cnContainer  =
  fmap (coerce :: CLong -> CNContainerType) $ sendMsg cnContainer (mkSelector "type") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @predicateForContainersWithIdentifiers:@
predicateForContainersWithIdentifiersSelector :: Selector
predicateForContainersWithIdentifiersSelector = mkSelector "predicateForContainersWithIdentifiers:"

-- | @Selector@ for @predicateForContainerOfContactWithIdentifier:@
predicateForContainerOfContactWithIdentifierSelector :: Selector
predicateForContainerOfContactWithIdentifierSelector = mkSelector "predicateForContainerOfContactWithIdentifier:"

-- | @Selector@ for @predicateForContainerOfGroupWithIdentifier:@
predicateForContainerOfGroupWithIdentifierSelector :: Selector
predicateForContainerOfGroupWithIdentifierSelector = mkSelector "predicateForContainerOfGroupWithIdentifier:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

