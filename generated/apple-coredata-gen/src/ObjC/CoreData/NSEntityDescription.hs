{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSEntityDescription@.
module ObjC.CoreData.NSEntityDescription
  ( NSEntityDescription
  , IsNSEntityDescription(..)
  , entityForName_inManagedObjectContext
  , insertNewObjectForEntityForName_inManagedObjectContext
  , relationshipsWithDestinationEntity
  , isKindOfEntity
  , managedObjectModel
  , managedObjectClassName
  , setManagedObjectClassName
  , name
  , setName
  , abstract
  , setAbstract
  , subentitiesByName
  , subentities
  , setSubentities
  , superentity
  , propertiesByName
  , properties
  , setProperties
  , userInfo
  , setUserInfo
  , attributesByName
  , relationshipsByName
  , versionHash
  , versionHashModifier
  , setVersionHashModifier
  , renamingIdentifier
  , setRenamingIdentifier
  , indexes
  , setIndexes
  , uniquenessConstraints
  , setUniquenessConstraints
  , compoundIndexes
  , setCompoundIndexes
  , coreSpotlightDisplayNameExpression
  , setCoreSpotlightDisplayNameExpression
  , abstractSelector
  , attributesByNameSelector
  , compoundIndexesSelector
  , coreSpotlightDisplayNameExpressionSelector
  , entityForName_inManagedObjectContextSelector
  , indexesSelector
  , insertNewObjectForEntityForName_inManagedObjectContextSelector
  , isKindOfEntitySelector
  , managedObjectClassNameSelector
  , managedObjectModelSelector
  , nameSelector
  , propertiesByNameSelector
  , propertiesSelector
  , relationshipsByNameSelector
  , relationshipsWithDestinationEntitySelector
  , renamingIdentifierSelector
  , setAbstractSelector
  , setCompoundIndexesSelector
  , setCoreSpotlightDisplayNameExpressionSelector
  , setIndexesSelector
  , setManagedObjectClassNameSelector
  , setNameSelector
  , setPropertiesSelector
  , setRenamingIdentifierSelector
  , setSubentitiesSelector
  , setUniquenessConstraintsSelector
  , setUserInfoSelector
  , setVersionHashModifierSelector
  , subentitiesByNameSelector
  , subentitiesSelector
  , superentitySelector
  , uniquenessConstraintsSelector
  , userInfoSelector
  , versionHashModifierSelector
  , versionHashSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ entityForName:inManagedObjectContext:@
entityForName_inManagedObjectContext :: (IsNSString entityName, IsNSManagedObjectContext context) => entityName -> context -> IO (Id NSEntityDescription)
entityForName_inManagedObjectContext entityName context =
  do
    cls' <- getRequiredClass "NSEntityDescription"
    sendClassMessage cls' entityForName_inManagedObjectContextSelector (toNSString entityName) (toNSManagedObjectContext context)

-- | @+ insertNewObjectForEntityForName:inManagedObjectContext:@
insertNewObjectForEntityForName_inManagedObjectContext :: (IsNSString entityName, IsNSManagedObjectContext context) => entityName -> context -> IO (Id NSManagedObject)
insertNewObjectForEntityForName_inManagedObjectContext entityName context =
  do
    cls' <- getRequiredClass "NSEntityDescription"
    sendClassMessage cls' insertNewObjectForEntityForName_inManagedObjectContextSelector (toNSString entityName) (toNSManagedObjectContext context)

-- | @- relationshipsWithDestinationEntity:@
relationshipsWithDestinationEntity :: (IsNSEntityDescription nsEntityDescription, IsNSEntityDescription entity) => nsEntityDescription -> entity -> IO (Id NSArray)
relationshipsWithDestinationEntity nsEntityDescription entity =
  sendMessage nsEntityDescription relationshipsWithDestinationEntitySelector (toNSEntityDescription entity)

-- | @- isKindOfEntity:@
isKindOfEntity :: (IsNSEntityDescription nsEntityDescription, IsNSEntityDescription entity) => nsEntityDescription -> entity -> IO Bool
isKindOfEntity nsEntityDescription entity =
  sendMessage nsEntityDescription isKindOfEntitySelector (toNSEntityDescription entity)

-- | @- managedObjectModel@
managedObjectModel :: IsNSEntityDescription nsEntityDescription => nsEntityDescription -> IO (Id NSManagedObjectModel)
managedObjectModel nsEntityDescription =
  sendMessage nsEntityDescription managedObjectModelSelector

-- | @- managedObjectClassName@
managedObjectClassName :: IsNSEntityDescription nsEntityDescription => nsEntityDescription -> IO (Id NSString)
managedObjectClassName nsEntityDescription =
  sendMessage nsEntityDescription managedObjectClassNameSelector

-- | @- setManagedObjectClassName:@
setManagedObjectClassName :: (IsNSEntityDescription nsEntityDescription, IsNSString value) => nsEntityDescription -> value -> IO ()
setManagedObjectClassName nsEntityDescription value =
  sendMessage nsEntityDescription setManagedObjectClassNameSelector (toNSString value)

-- | @- name@
name :: IsNSEntityDescription nsEntityDescription => nsEntityDescription -> IO (Id NSString)
name nsEntityDescription =
  sendMessage nsEntityDescription nameSelector

-- | @- setName:@
setName :: (IsNSEntityDescription nsEntityDescription, IsNSString value) => nsEntityDescription -> value -> IO ()
setName nsEntityDescription value =
  sendMessage nsEntityDescription setNameSelector (toNSString value)

-- | @- abstract@
abstract :: IsNSEntityDescription nsEntityDescription => nsEntityDescription -> IO Bool
abstract nsEntityDescription =
  sendMessage nsEntityDescription abstractSelector

-- | @- setAbstract:@
setAbstract :: IsNSEntityDescription nsEntityDescription => nsEntityDescription -> Bool -> IO ()
setAbstract nsEntityDescription value =
  sendMessage nsEntityDescription setAbstractSelector value

-- | @- subentitiesByName@
subentitiesByName :: IsNSEntityDescription nsEntityDescription => nsEntityDescription -> IO (Id NSDictionary)
subentitiesByName nsEntityDescription =
  sendMessage nsEntityDescription subentitiesByNameSelector

-- | @- subentities@
subentities :: IsNSEntityDescription nsEntityDescription => nsEntityDescription -> IO (Id NSArray)
subentities nsEntityDescription =
  sendMessage nsEntityDescription subentitiesSelector

-- | @- setSubentities:@
setSubentities :: (IsNSEntityDescription nsEntityDescription, IsNSArray value) => nsEntityDescription -> value -> IO ()
setSubentities nsEntityDescription value =
  sendMessage nsEntityDescription setSubentitiesSelector (toNSArray value)

-- | @- superentity@
superentity :: IsNSEntityDescription nsEntityDescription => nsEntityDescription -> IO (Id NSEntityDescription)
superentity nsEntityDescription =
  sendMessage nsEntityDescription superentitySelector

-- | @- propertiesByName@
propertiesByName :: IsNSEntityDescription nsEntityDescription => nsEntityDescription -> IO (Id NSDictionary)
propertiesByName nsEntityDescription =
  sendMessage nsEntityDescription propertiesByNameSelector

-- | @- properties@
properties :: IsNSEntityDescription nsEntityDescription => nsEntityDescription -> IO (Id NSArray)
properties nsEntityDescription =
  sendMessage nsEntityDescription propertiesSelector

-- | @- setProperties:@
setProperties :: (IsNSEntityDescription nsEntityDescription, IsNSArray value) => nsEntityDescription -> value -> IO ()
setProperties nsEntityDescription value =
  sendMessage nsEntityDescription setPropertiesSelector (toNSArray value)

-- | @- userInfo@
userInfo :: IsNSEntityDescription nsEntityDescription => nsEntityDescription -> IO (Id NSDictionary)
userInfo nsEntityDescription =
  sendMessage nsEntityDescription userInfoSelector

-- | @- setUserInfo:@
setUserInfo :: (IsNSEntityDescription nsEntityDescription, IsNSDictionary value) => nsEntityDescription -> value -> IO ()
setUserInfo nsEntityDescription value =
  sendMessage nsEntityDescription setUserInfoSelector (toNSDictionary value)

-- | @- attributesByName@
attributesByName :: IsNSEntityDescription nsEntityDescription => nsEntityDescription -> IO (Id NSDictionary)
attributesByName nsEntityDescription =
  sendMessage nsEntityDescription attributesByNameSelector

-- | @- relationshipsByName@
relationshipsByName :: IsNSEntityDescription nsEntityDescription => nsEntityDescription -> IO (Id NSDictionary)
relationshipsByName nsEntityDescription =
  sendMessage nsEntityDescription relationshipsByNameSelector

-- | @- versionHash@
versionHash :: IsNSEntityDescription nsEntityDescription => nsEntityDescription -> IO (Id NSData)
versionHash nsEntityDescription =
  sendMessage nsEntityDescription versionHashSelector

-- | @- versionHashModifier@
versionHashModifier :: IsNSEntityDescription nsEntityDescription => nsEntityDescription -> IO (Id NSString)
versionHashModifier nsEntityDescription =
  sendMessage nsEntityDescription versionHashModifierSelector

-- | @- setVersionHashModifier:@
setVersionHashModifier :: (IsNSEntityDescription nsEntityDescription, IsNSString value) => nsEntityDescription -> value -> IO ()
setVersionHashModifier nsEntityDescription value =
  sendMessage nsEntityDescription setVersionHashModifierSelector (toNSString value)

-- | @- renamingIdentifier@
renamingIdentifier :: IsNSEntityDescription nsEntityDescription => nsEntityDescription -> IO (Id NSString)
renamingIdentifier nsEntityDescription =
  sendMessage nsEntityDescription renamingIdentifierSelector

-- | @- setRenamingIdentifier:@
setRenamingIdentifier :: (IsNSEntityDescription nsEntityDescription, IsNSString value) => nsEntityDescription -> value -> IO ()
setRenamingIdentifier nsEntityDescription value =
  sendMessage nsEntityDescription setRenamingIdentifierSelector (toNSString value)

-- | @- indexes@
indexes :: IsNSEntityDescription nsEntityDescription => nsEntityDescription -> IO (Id NSArray)
indexes nsEntityDescription =
  sendMessage nsEntityDescription indexesSelector

-- | @- setIndexes:@
setIndexes :: (IsNSEntityDescription nsEntityDescription, IsNSArray value) => nsEntityDescription -> value -> IO ()
setIndexes nsEntityDescription value =
  sendMessage nsEntityDescription setIndexesSelector (toNSArray value)

-- | @- uniquenessConstraints@
uniquenessConstraints :: IsNSEntityDescription nsEntityDescription => nsEntityDescription -> IO (Id NSArray)
uniquenessConstraints nsEntityDescription =
  sendMessage nsEntityDescription uniquenessConstraintsSelector

-- | @- setUniquenessConstraints:@
setUniquenessConstraints :: (IsNSEntityDescription nsEntityDescription, IsNSArray value) => nsEntityDescription -> value -> IO ()
setUniquenessConstraints nsEntityDescription value =
  sendMessage nsEntityDescription setUniquenessConstraintsSelector (toNSArray value)

-- | @- compoundIndexes@
compoundIndexes :: IsNSEntityDescription nsEntityDescription => nsEntityDescription -> IO (Id NSArray)
compoundIndexes nsEntityDescription =
  sendMessage nsEntityDescription compoundIndexesSelector

-- | @- setCompoundIndexes:@
setCompoundIndexes :: (IsNSEntityDescription nsEntityDescription, IsNSArray value) => nsEntityDescription -> value -> IO ()
setCompoundIndexes nsEntityDescription value =
  sendMessage nsEntityDescription setCompoundIndexesSelector (toNSArray value)

-- | @- coreSpotlightDisplayNameExpression@
coreSpotlightDisplayNameExpression :: IsNSEntityDescription nsEntityDescription => nsEntityDescription -> IO (Id NSExpression)
coreSpotlightDisplayNameExpression nsEntityDescription =
  sendMessage nsEntityDescription coreSpotlightDisplayNameExpressionSelector

-- | @- setCoreSpotlightDisplayNameExpression:@
setCoreSpotlightDisplayNameExpression :: (IsNSEntityDescription nsEntityDescription, IsNSExpression value) => nsEntityDescription -> value -> IO ()
setCoreSpotlightDisplayNameExpression nsEntityDescription value =
  sendMessage nsEntityDescription setCoreSpotlightDisplayNameExpressionSelector (toNSExpression value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @entityForName:inManagedObjectContext:@
entityForName_inManagedObjectContextSelector :: Selector '[Id NSString, Id NSManagedObjectContext] (Id NSEntityDescription)
entityForName_inManagedObjectContextSelector = mkSelector "entityForName:inManagedObjectContext:"

-- | @Selector@ for @insertNewObjectForEntityForName:inManagedObjectContext:@
insertNewObjectForEntityForName_inManagedObjectContextSelector :: Selector '[Id NSString, Id NSManagedObjectContext] (Id NSManagedObject)
insertNewObjectForEntityForName_inManagedObjectContextSelector = mkSelector "insertNewObjectForEntityForName:inManagedObjectContext:"

-- | @Selector@ for @relationshipsWithDestinationEntity:@
relationshipsWithDestinationEntitySelector :: Selector '[Id NSEntityDescription] (Id NSArray)
relationshipsWithDestinationEntitySelector = mkSelector "relationshipsWithDestinationEntity:"

-- | @Selector@ for @isKindOfEntity:@
isKindOfEntitySelector :: Selector '[Id NSEntityDescription] Bool
isKindOfEntitySelector = mkSelector "isKindOfEntity:"

-- | @Selector@ for @managedObjectModel@
managedObjectModelSelector :: Selector '[] (Id NSManagedObjectModel)
managedObjectModelSelector = mkSelector "managedObjectModel"

-- | @Selector@ for @managedObjectClassName@
managedObjectClassNameSelector :: Selector '[] (Id NSString)
managedObjectClassNameSelector = mkSelector "managedObjectClassName"

-- | @Selector@ for @setManagedObjectClassName:@
setManagedObjectClassNameSelector :: Selector '[Id NSString] ()
setManagedObjectClassNameSelector = mkSelector "setManagedObjectClassName:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @abstract@
abstractSelector :: Selector '[] Bool
abstractSelector = mkSelector "abstract"

-- | @Selector@ for @setAbstract:@
setAbstractSelector :: Selector '[Bool] ()
setAbstractSelector = mkSelector "setAbstract:"

-- | @Selector@ for @subentitiesByName@
subentitiesByNameSelector :: Selector '[] (Id NSDictionary)
subentitiesByNameSelector = mkSelector "subentitiesByName"

-- | @Selector@ for @subentities@
subentitiesSelector :: Selector '[] (Id NSArray)
subentitiesSelector = mkSelector "subentities"

-- | @Selector@ for @setSubentities:@
setSubentitiesSelector :: Selector '[Id NSArray] ()
setSubentitiesSelector = mkSelector "setSubentities:"

-- | @Selector@ for @superentity@
superentitySelector :: Selector '[] (Id NSEntityDescription)
superentitySelector = mkSelector "superentity"

-- | @Selector@ for @propertiesByName@
propertiesByNameSelector :: Selector '[] (Id NSDictionary)
propertiesByNameSelector = mkSelector "propertiesByName"

-- | @Selector@ for @properties@
propertiesSelector :: Selector '[] (Id NSArray)
propertiesSelector = mkSelector "properties"

-- | @Selector@ for @setProperties:@
setPropertiesSelector :: Selector '[Id NSArray] ()
setPropertiesSelector = mkSelector "setProperties:"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector '[] (Id NSDictionary)
userInfoSelector = mkSelector "userInfo"

-- | @Selector@ for @setUserInfo:@
setUserInfoSelector :: Selector '[Id NSDictionary] ()
setUserInfoSelector = mkSelector "setUserInfo:"

-- | @Selector@ for @attributesByName@
attributesByNameSelector :: Selector '[] (Id NSDictionary)
attributesByNameSelector = mkSelector "attributesByName"

-- | @Selector@ for @relationshipsByName@
relationshipsByNameSelector :: Selector '[] (Id NSDictionary)
relationshipsByNameSelector = mkSelector "relationshipsByName"

-- | @Selector@ for @versionHash@
versionHashSelector :: Selector '[] (Id NSData)
versionHashSelector = mkSelector "versionHash"

-- | @Selector@ for @versionHashModifier@
versionHashModifierSelector :: Selector '[] (Id NSString)
versionHashModifierSelector = mkSelector "versionHashModifier"

-- | @Selector@ for @setVersionHashModifier:@
setVersionHashModifierSelector :: Selector '[Id NSString] ()
setVersionHashModifierSelector = mkSelector "setVersionHashModifier:"

-- | @Selector@ for @renamingIdentifier@
renamingIdentifierSelector :: Selector '[] (Id NSString)
renamingIdentifierSelector = mkSelector "renamingIdentifier"

-- | @Selector@ for @setRenamingIdentifier:@
setRenamingIdentifierSelector :: Selector '[Id NSString] ()
setRenamingIdentifierSelector = mkSelector "setRenamingIdentifier:"

-- | @Selector@ for @indexes@
indexesSelector :: Selector '[] (Id NSArray)
indexesSelector = mkSelector "indexes"

-- | @Selector@ for @setIndexes:@
setIndexesSelector :: Selector '[Id NSArray] ()
setIndexesSelector = mkSelector "setIndexes:"

-- | @Selector@ for @uniquenessConstraints@
uniquenessConstraintsSelector :: Selector '[] (Id NSArray)
uniquenessConstraintsSelector = mkSelector "uniquenessConstraints"

-- | @Selector@ for @setUniquenessConstraints:@
setUniquenessConstraintsSelector :: Selector '[Id NSArray] ()
setUniquenessConstraintsSelector = mkSelector "setUniquenessConstraints:"

-- | @Selector@ for @compoundIndexes@
compoundIndexesSelector :: Selector '[] (Id NSArray)
compoundIndexesSelector = mkSelector "compoundIndexes"

-- | @Selector@ for @setCompoundIndexes:@
setCompoundIndexesSelector :: Selector '[Id NSArray] ()
setCompoundIndexesSelector = mkSelector "setCompoundIndexes:"

-- | @Selector@ for @coreSpotlightDisplayNameExpression@
coreSpotlightDisplayNameExpressionSelector :: Selector '[] (Id NSExpression)
coreSpotlightDisplayNameExpressionSelector = mkSelector "coreSpotlightDisplayNameExpression"

-- | @Selector@ for @setCoreSpotlightDisplayNameExpression:@
setCoreSpotlightDisplayNameExpressionSelector :: Selector '[Id NSExpression] ()
setCoreSpotlightDisplayNameExpressionSelector = mkSelector "setCoreSpotlightDisplayNameExpression:"

