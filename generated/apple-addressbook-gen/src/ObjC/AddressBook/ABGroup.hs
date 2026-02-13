{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ABGroup@.
module ObjC.AddressBook.ABGroup
  ( ABGroup
  , IsABGroup(..)
  , members
  , addMember
  , removeMember
  , subgroups
  , addSubgroup
  , removeSubgroup
  , parentGroups
  , setDistributionIdentifier_forProperty_person
  , distributionIdentifierForProperty_person
  , searchElementForProperty_label_key_value_comparison
  , addPropertiesAndTypes
  , removeProperties
  , properties
  , typeOfProperty
  , addMemberSelector
  , addPropertiesAndTypesSelector
  , addSubgroupSelector
  , distributionIdentifierForProperty_personSelector
  , membersSelector
  , parentGroupsSelector
  , propertiesSelector
  , removeMemberSelector
  , removePropertiesSelector
  , removeSubgroupSelector
  , searchElementForProperty_label_key_value_comparisonSelector
  , setDistributionIdentifier_forProperty_personSelector
  , subgroupsSelector
  , typeOfPropertySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AddressBook.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- members@
members :: IsABGroup abGroup => abGroup -> IO (Id NSArray)
members abGroup =
  sendMessage abGroup membersSelector

-- | @- addMember:@
addMember :: (IsABGroup abGroup, IsABPerson person) => abGroup -> person -> IO Bool
addMember abGroup person =
  sendMessage abGroup addMemberSelector (toABPerson person)

-- | @- removeMember:@
removeMember :: (IsABGroup abGroup, IsABPerson person) => abGroup -> person -> IO Bool
removeMember abGroup person =
  sendMessage abGroup removeMemberSelector (toABPerson person)

-- | @- subgroups@
subgroups :: IsABGroup abGroup => abGroup -> IO (Id NSArray)
subgroups abGroup =
  sendMessage abGroup subgroupsSelector

-- | @- addSubgroup:@
addSubgroup :: (IsABGroup abGroup, IsABGroup group) => abGroup -> group -> IO Bool
addSubgroup abGroup group =
  sendMessage abGroup addSubgroupSelector (toABGroup group)

-- | @- removeSubgroup:@
removeSubgroup :: (IsABGroup abGroup, IsABGroup group) => abGroup -> group -> IO Bool
removeSubgroup abGroup group =
  sendMessage abGroup removeSubgroupSelector (toABGroup group)

-- | @- parentGroups@
parentGroups :: IsABGroup abGroup => abGroup -> IO (Id NSArray)
parentGroups abGroup =
  sendMessage abGroup parentGroupsSelector

-- | @- setDistributionIdentifier:forProperty:person:@
setDistributionIdentifier_forProperty_person :: (IsABGroup abGroup, IsNSString identifier, IsNSString property, IsABPerson person) => abGroup -> identifier -> property -> person -> IO Bool
setDistributionIdentifier_forProperty_person abGroup identifier property person =
  sendMessage abGroup setDistributionIdentifier_forProperty_personSelector (toNSString identifier) (toNSString property) (toABPerson person)

-- | @- distributionIdentifierForProperty:person:@
distributionIdentifierForProperty_person :: (IsABGroup abGroup, IsNSString property, IsABPerson person) => abGroup -> property -> person -> IO (Id NSString)
distributionIdentifierForProperty_person abGroup property person =
  sendMessage abGroup distributionIdentifierForProperty_personSelector (toNSString property) (toABPerson person)

-- | @+ searchElementForProperty:label:key:value:comparison:@
searchElementForProperty_label_key_value_comparison :: (IsNSString property, IsNSString label, IsNSString key) => property -> label -> key -> RawId -> CLong -> IO (Id ABSearchElement)
searchElementForProperty_label_key_value_comparison property label key value comparison =
  do
    cls' <- getRequiredClass "ABGroup"
    sendClassMessage cls' searchElementForProperty_label_key_value_comparisonSelector (toNSString property) (toNSString label) (toNSString key) value comparison

-- | @+ addPropertiesAndTypes:@
addPropertiesAndTypes :: IsNSDictionary properties => properties -> IO CLong
addPropertiesAndTypes properties =
  do
    cls' <- getRequiredClass "ABGroup"
    sendClassMessage cls' addPropertiesAndTypesSelector (toNSDictionary properties)

-- | @+ removeProperties:@
removeProperties :: IsNSArray properties => properties -> IO CLong
removeProperties properties =
  do
    cls' <- getRequiredClass "ABGroup"
    sendClassMessage cls' removePropertiesSelector (toNSArray properties)

-- | @+ properties@
properties :: IO (Id NSArray)
properties  =
  do
    cls' <- getRequiredClass "ABGroup"
    sendClassMessage cls' propertiesSelector

-- | @+ typeOfProperty:@
typeOfProperty :: IsNSString property => property -> IO CLong
typeOfProperty property =
  do
    cls' <- getRequiredClass "ABGroup"
    sendClassMessage cls' typeOfPropertySelector (toNSString property)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @members@
membersSelector :: Selector '[] (Id NSArray)
membersSelector = mkSelector "members"

-- | @Selector@ for @addMember:@
addMemberSelector :: Selector '[Id ABPerson] Bool
addMemberSelector = mkSelector "addMember:"

-- | @Selector@ for @removeMember:@
removeMemberSelector :: Selector '[Id ABPerson] Bool
removeMemberSelector = mkSelector "removeMember:"

-- | @Selector@ for @subgroups@
subgroupsSelector :: Selector '[] (Id NSArray)
subgroupsSelector = mkSelector "subgroups"

-- | @Selector@ for @addSubgroup:@
addSubgroupSelector :: Selector '[Id ABGroup] Bool
addSubgroupSelector = mkSelector "addSubgroup:"

-- | @Selector@ for @removeSubgroup:@
removeSubgroupSelector :: Selector '[Id ABGroup] Bool
removeSubgroupSelector = mkSelector "removeSubgroup:"

-- | @Selector@ for @parentGroups@
parentGroupsSelector :: Selector '[] (Id NSArray)
parentGroupsSelector = mkSelector "parentGroups"

-- | @Selector@ for @setDistributionIdentifier:forProperty:person:@
setDistributionIdentifier_forProperty_personSelector :: Selector '[Id NSString, Id NSString, Id ABPerson] Bool
setDistributionIdentifier_forProperty_personSelector = mkSelector "setDistributionIdentifier:forProperty:person:"

-- | @Selector@ for @distributionIdentifierForProperty:person:@
distributionIdentifierForProperty_personSelector :: Selector '[Id NSString, Id ABPerson] (Id NSString)
distributionIdentifierForProperty_personSelector = mkSelector "distributionIdentifierForProperty:person:"

-- | @Selector@ for @searchElementForProperty:label:key:value:comparison:@
searchElementForProperty_label_key_value_comparisonSelector :: Selector '[Id NSString, Id NSString, Id NSString, RawId, CLong] (Id ABSearchElement)
searchElementForProperty_label_key_value_comparisonSelector = mkSelector "searchElementForProperty:label:key:value:comparison:"

-- | @Selector@ for @addPropertiesAndTypes:@
addPropertiesAndTypesSelector :: Selector '[Id NSDictionary] CLong
addPropertiesAndTypesSelector = mkSelector "addPropertiesAndTypes:"

-- | @Selector@ for @removeProperties:@
removePropertiesSelector :: Selector '[Id NSArray] CLong
removePropertiesSelector = mkSelector "removeProperties:"

-- | @Selector@ for @properties@
propertiesSelector :: Selector '[] (Id NSArray)
propertiesSelector = mkSelector "properties"

-- | @Selector@ for @typeOfProperty:@
typeOfPropertySelector :: Selector '[Id NSString] CLong
typeOfPropertySelector = mkSelector "typeOfProperty:"

