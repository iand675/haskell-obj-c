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
  , membersSelector
  , addMemberSelector
  , removeMemberSelector
  , subgroupsSelector
  , addSubgroupSelector
  , removeSubgroupSelector
  , parentGroupsSelector
  , setDistributionIdentifier_forProperty_personSelector
  , distributionIdentifierForProperty_personSelector
  , searchElementForProperty_label_key_value_comparisonSelector
  , addPropertiesAndTypesSelector
  , removePropertiesSelector
  , propertiesSelector
  , typeOfPropertySelector


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

import ObjC.AddressBook.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- members@
members :: IsABGroup abGroup => abGroup -> IO (Id NSArray)
members abGroup  =
  sendMsg abGroup (mkSelector "members") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- addMember:@
addMember :: (IsABGroup abGroup, IsABPerson person) => abGroup -> person -> IO Bool
addMember abGroup  person =
withObjCPtr person $ \raw_person ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg abGroup (mkSelector "addMember:") retCULong [argPtr (castPtr raw_person :: Ptr ())]

-- | @- removeMember:@
removeMember :: (IsABGroup abGroup, IsABPerson person) => abGroup -> person -> IO Bool
removeMember abGroup  person =
withObjCPtr person $ \raw_person ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg abGroup (mkSelector "removeMember:") retCULong [argPtr (castPtr raw_person :: Ptr ())]

-- | @- subgroups@
subgroups :: IsABGroup abGroup => abGroup -> IO (Id NSArray)
subgroups abGroup  =
  sendMsg abGroup (mkSelector "subgroups") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- addSubgroup:@
addSubgroup :: (IsABGroup abGroup, IsABGroup group) => abGroup -> group -> IO Bool
addSubgroup abGroup  group =
withObjCPtr group $ \raw_group ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg abGroup (mkSelector "addSubgroup:") retCULong [argPtr (castPtr raw_group :: Ptr ())]

-- | @- removeSubgroup:@
removeSubgroup :: (IsABGroup abGroup, IsABGroup group) => abGroup -> group -> IO Bool
removeSubgroup abGroup  group =
withObjCPtr group $ \raw_group ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg abGroup (mkSelector "removeSubgroup:") retCULong [argPtr (castPtr raw_group :: Ptr ())]

-- | @- parentGroups@
parentGroups :: IsABGroup abGroup => abGroup -> IO (Id NSArray)
parentGroups abGroup  =
  sendMsg abGroup (mkSelector "parentGroups") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDistributionIdentifier:forProperty:person:@
setDistributionIdentifier_forProperty_person :: (IsABGroup abGroup, IsNSString identifier, IsNSString property, IsABPerson person) => abGroup -> identifier -> property -> person -> IO Bool
setDistributionIdentifier_forProperty_person abGroup  identifier property person =
withObjCPtr identifier $ \raw_identifier ->
  withObjCPtr property $ \raw_property ->
    withObjCPtr person $ \raw_person ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg abGroup (mkSelector "setDistributionIdentifier:forProperty:person:") retCULong [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_property :: Ptr ()), argPtr (castPtr raw_person :: Ptr ())]

-- | @- distributionIdentifierForProperty:person:@
distributionIdentifierForProperty_person :: (IsABGroup abGroup, IsNSString property, IsABPerson person) => abGroup -> property -> person -> IO (Id NSString)
distributionIdentifierForProperty_person abGroup  property person =
withObjCPtr property $ \raw_property ->
  withObjCPtr person $ \raw_person ->
      sendMsg abGroup (mkSelector "distributionIdentifierForProperty:person:") (retPtr retVoid) [argPtr (castPtr raw_property :: Ptr ()), argPtr (castPtr raw_person :: Ptr ())] >>= retainedObject . castPtr

-- | @+ searchElementForProperty:label:key:value:comparison:@
searchElementForProperty_label_key_value_comparison :: (IsNSString property, IsNSString label, IsNSString key) => property -> label -> key -> RawId -> CLong -> IO (Id ABSearchElement)
searchElementForProperty_label_key_value_comparison property label key value comparison =
  do
    cls' <- getRequiredClass "ABGroup"
    withObjCPtr property $ \raw_property ->
      withObjCPtr label $ \raw_label ->
        withObjCPtr key $ \raw_key ->
          sendClassMsg cls' (mkSelector "searchElementForProperty:label:key:value:comparison:") (retPtr retVoid) [argPtr (castPtr raw_property :: Ptr ()), argPtr (castPtr raw_label :: Ptr ()), argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr (unRawId value) :: Ptr ()), argCLong (fromIntegral comparison)] >>= retainedObject . castPtr

-- | @+ addPropertiesAndTypes:@
addPropertiesAndTypes :: IsNSDictionary properties => properties -> IO CLong
addPropertiesAndTypes properties =
  do
    cls' <- getRequiredClass "ABGroup"
    withObjCPtr properties $ \raw_properties ->
      sendClassMsg cls' (mkSelector "addPropertiesAndTypes:") retCLong [argPtr (castPtr raw_properties :: Ptr ())]

-- | @+ removeProperties:@
removeProperties :: IsNSArray properties => properties -> IO CLong
removeProperties properties =
  do
    cls' <- getRequiredClass "ABGroup"
    withObjCPtr properties $ \raw_properties ->
      sendClassMsg cls' (mkSelector "removeProperties:") retCLong [argPtr (castPtr raw_properties :: Ptr ())]

-- | @+ properties@
properties :: IO (Id NSArray)
properties  =
  do
    cls' <- getRequiredClass "ABGroup"
    sendClassMsg cls' (mkSelector "properties") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ typeOfProperty:@
typeOfProperty :: IsNSString property => property -> IO CLong
typeOfProperty property =
  do
    cls' <- getRequiredClass "ABGroup"
    withObjCPtr property $ \raw_property ->
      sendClassMsg cls' (mkSelector "typeOfProperty:") retCLong [argPtr (castPtr raw_property :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @members@
membersSelector :: Selector
membersSelector = mkSelector "members"

-- | @Selector@ for @addMember:@
addMemberSelector :: Selector
addMemberSelector = mkSelector "addMember:"

-- | @Selector@ for @removeMember:@
removeMemberSelector :: Selector
removeMemberSelector = mkSelector "removeMember:"

-- | @Selector@ for @subgroups@
subgroupsSelector :: Selector
subgroupsSelector = mkSelector "subgroups"

-- | @Selector@ for @addSubgroup:@
addSubgroupSelector :: Selector
addSubgroupSelector = mkSelector "addSubgroup:"

-- | @Selector@ for @removeSubgroup:@
removeSubgroupSelector :: Selector
removeSubgroupSelector = mkSelector "removeSubgroup:"

-- | @Selector@ for @parentGroups@
parentGroupsSelector :: Selector
parentGroupsSelector = mkSelector "parentGroups"

-- | @Selector@ for @setDistributionIdentifier:forProperty:person:@
setDistributionIdentifier_forProperty_personSelector :: Selector
setDistributionIdentifier_forProperty_personSelector = mkSelector "setDistributionIdentifier:forProperty:person:"

-- | @Selector@ for @distributionIdentifierForProperty:person:@
distributionIdentifierForProperty_personSelector :: Selector
distributionIdentifierForProperty_personSelector = mkSelector "distributionIdentifierForProperty:person:"

-- | @Selector@ for @searchElementForProperty:label:key:value:comparison:@
searchElementForProperty_label_key_value_comparisonSelector :: Selector
searchElementForProperty_label_key_value_comparisonSelector = mkSelector "searchElementForProperty:label:key:value:comparison:"

-- | @Selector@ for @addPropertiesAndTypes:@
addPropertiesAndTypesSelector :: Selector
addPropertiesAndTypesSelector = mkSelector "addPropertiesAndTypes:"

-- | @Selector@ for @removeProperties:@
removePropertiesSelector :: Selector
removePropertiesSelector = mkSelector "removeProperties:"

-- | @Selector@ for @properties@
propertiesSelector :: Selector
propertiesSelector = mkSelector "properties"

-- | @Selector@ for @typeOfProperty:@
typeOfPropertySelector :: Selector
typeOfPropertySelector = mkSelector "typeOfProperty:"

