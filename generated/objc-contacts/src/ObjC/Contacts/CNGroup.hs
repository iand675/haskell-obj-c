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
  , predicateForGroupsWithIdentifiersSelector
  , predicateForSubgroupsInGroupWithIdentifierSelector
  , predicateForGroupsInContainerWithIdentifierSelector
  , identifierSelector
  , nameSelector


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
import ObjC.Foundation.Internal.Classes

-- | @+ predicateForGroupsWithIdentifiers:@
predicateForGroupsWithIdentifiers :: IsNSArray identifiers => identifiers -> IO (Id NSPredicate)
predicateForGroupsWithIdentifiers identifiers =
  do
    cls' <- getRequiredClass "CNGroup"
    withObjCPtr identifiers $ \raw_identifiers ->
      sendClassMsg cls' (mkSelector "predicateForGroupsWithIdentifiers:") (retPtr retVoid) [argPtr (castPtr raw_identifiers :: Ptr ())] >>= retainedObject . castPtr

-- | @+ predicateForSubgroupsInGroupWithIdentifier:@
predicateForSubgroupsInGroupWithIdentifier :: IsNSString parentGroupIdentifier => parentGroupIdentifier -> IO (Id NSPredicate)
predicateForSubgroupsInGroupWithIdentifier parentGroupIdentifier =
  do
    cls' <- getRequiredClass "CNGroup"
    withObjCPtr parentGroupIdentifier $ \raw_parentGroupIdentifier ->
      sendClassMsg cls' (mkSelector "predicateForSubgroupsInGroupWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_parentGroupIdentifier :: Ptr ())] >>= retainedObject . castPtr

-- | @+ predicateForGroupsInContainerWithIdentifier:@
predicateForGroupsInContainerWithIdentifier :: IsNSString containerIdentifier => containerIdentifier -> IO (Id NSPredicate)
predicateForGroupsInContainerWithIdentifier containerIdentifier =
  do
    cls' <- getRequiredClass "CNGroup"
    withObjCPtr containerIdentifier $ \raw_containerIdentifier ->
      sendClassMsg cls' (mkSelector "predicateForGroupsInContainerWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_containerIdentifier :: Ptr ())] >>= retainedObject . castPtr

-- | The identifier is unique among groups on the device. It can be saved and used for fetching groups next application launch.
--
-- ObjC selector: @- identifier@
identifier :: IsCNGroup cnGroup => cnGroup -> IO (Id NSString)
identifier cnGroup  =
  sendMsg cnGroup (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- name@
name :: IsCNGroup cnGroup => cnGroup -> IO (Id NSString)
name cnGroup  =
  sendMsg cnGroup (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @predicateForGroupsWithIdentifiers:@
predicateForGroupsWithIdentifiersSelector :: Selector
predicateForGroupsWithIdentifiersSelector = mkSelector "predicateForGroupsWithIdentifiers:"

-- | @Selector@ for @predicateForSubgroupsInGroupWithIdentifier:@
predicateForSubgroupsInGroupWithIdentifierSelector :: Selector
predicateForSubgroupsInGroupWithIdentifierSelector = mkSelector "predicateForSubgroupsInGroupWithIdentifier:"

-- | @Selector@ for @predicateForGroupsInContainerWithIdentifier:@
predicateForGroupsInContainerWithIdentifierSelector :: Selector
predicateForGroupsInContainerWithIdentifierSelector = mkSelector "predicateForGroupsInContainerWithIdentifier:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

