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
  , entityForName_inManagedObjectContextSelector
  , insertNewObjectForEntityForName_inManagedObjectContextSelector
  , relationshipsWithDestinationEntitySelector
  , isKindOfEntitySelector
  , managedObjectModelSelector
  , managedObjectClassNameSelector
  , setManagedObjectClassNameSelector
  , nameSelector
  , setNameSelector
  , abstractSelector
  , setAbstractSelector
  , subentitiesByNameSelector
  , subentitiesSelector
  , setSubentitiesSelector
  , superentitySelector
  , propertiesByNameSelector
  , propertiesSelector
  , setPropertiesSelector
  , userInfoSelector
  , setUserInfoSelector
  , attributesByNameSelector
  , relationshipsByNameSelector
  , versionHashSelector
  , versionHashModifierSelector
  , setVersionHashModifierSelector
  , renamingIdentifierSelector
  , setRenamingIdentifierSelector
  , indexesSelector
  , setIndexesSelector
  , uniquenessConstraintsSelector
  , setUniquenessConstraintsSelector
  , compoundIndexesSelector
  , setCompoundIndexesSelector
  , coreSpotlightDisplayNameExpressionSelector
  , setCoreSpotlightDisplayNameExpressionSelector


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

import ObjC.CoreData.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ entityForName:inManagedObjectContext:@
entityForName_inManagedObjectContext :: (IsNSString entityName, IsNSManagedObjectContext context) => entityName -> context -> IO (Id NSEntityDescription)
entityForName_inManagedObjectContext entityName context =
  do
    cls' <- getRequiredClass "NSEntityDescription"
    withObjCPtr entityName $ \raw_entityName ->
      withObjCPtr context $ \raw_context ->
        sendClassMsg cls' (mkSelector "entityForName:inManagedObjectContext:") (retPtr retVoid) [argPtr (castPtr raw_entityName :: Ptr ()), argPtr (castPtr raw_context :: Ptr ())] >>= retainedObject . castPtr

-- | @+ insertNewObjectForEntityForName:inManagedObjectContext:@
insertNewObjectForEntityForName_inManagedObjectContext :: (IsNSString entityName, IsNSManagedObjectContext context) => entityName -> context -> IO (Id NSManagedObject)
insertNewObjectForEntityForName_inManagedObjectContext entityName context =
  do
    cls' <- getRequiredClass "NSEntityDescription"
    withObjCPtr entityName $ \raw_entityName ->
      withObjCPtr context $ \raw_context ->
        sendClassMsg cls' (mkSelector "insertNewObjectForEntityForName:inManagedObjectContext:") (retPtr retVoid) [argPtr (castPtr raw_entityName :: Ptr ()), argPtr (castPtr raw_context :: Ptr ())] >>= retainedObject . castPtr

-- | @- relationshipsWithDestinationEntity:@
relationshipsWithDestinationEntity :: (IsNSEntityDescription nsEntityDescription, IsNSEntityDescription entity) => nsEntityDescription -> entity -> IO (Id NSArray)
relationshipsWithDestinationEntity nsEntityDescription  entity =
withObjCPtr entity $ \raw_entity ->
    sendMsg nsEntityDescription (mkSelector "relationshipsWithDestinationEntity:") (retPtr retVoid) [argPtr (castPtr raw_entity :: Ptr ())] >>= retainedObject . castPtr

-- | @- isKindOfEntity:@
isKindOfEntity :: (IsNSEntityDescription nsEntityDescription, IsNSEntityDescription entity) => nsEntityDescription -> entity -> IO Bool
isKindOfEntity nsEntityDescription  entity =
withObjCPtr entity $ \raw_entity ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsEntityDescription (mkSelector "isKindOfEntity:") retCULong [argPtr (castPtr raw_entity :: Ptr ())]

-- | @- managedObjectModel@
managedObjectModel :: IsNSEntityDescription nsEntityDescription => nsEntityDescription -> IO (Id NSManagedObjectModel)
managedObjectModel nsEntityDescription  =
  sendMsg nsEntityDescription (mkSelector "managedObjectModel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- managedObjectClassName@
managedObjectClassName :: IsNSEntityDescription nsEntityDescription => nsEntityDescription -> IO (Id NSString)
managedObjectClassName nsEntityDescription  =
  sendMsg nsEntityDescription (mkSelector "managedObjectClassName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setManagedObjectClassName:@
setManagedObjectClassName :: (IsNSEntityDescription nsEntityDescription, IsNSString value) => nsEntityDescription -> value -> IO ()
setManagedObjectClassName nsEntityDescription  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsEntityDescription (mkSelector "setManagedObjectClassName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- name@
name :: IsNSEntityDescription nsEntityDescription => nsEntityDescription -> IO (Id NSString)
name nsEntityDescription  =
  sendMsg nsEntityDescription (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsNSEntityDescription nsEntityDescription, IsNSString value) => nsEntityDescription -> value -> IO ()
setName nsEntityDescription  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsEntityDescription (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- abstract@
abstract :: IsNSEntityDescription nsEntityDescription => nsEntityDescription -> IO Bool
abstract nsEntityDescription  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsEntityDescription (mkSelector "abstract") retCULong []

-- | @- setAbstract:@
setAbstract :: IsNSEntityDescription nsEntityDescription => nsEntityDescription -> Bool -> IO ()
setAbstract nsEntityDescription  value =
  sendMsg nsEntityDescription (mkSelector "setAbstract:") retVoid [argCULong (if value then 1 else 0)]

-- | @- subentitiesByName@
subentitiesByName :: IsNSEntityDescription nsEntityDescription => nsEntityDescription -> IO (Id NSDictionary)
subentitiesByName nsEntityDescription  =
  sendMsg nsEntityDescription (mkSelector "subentitiesByName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- subentities@
subentities :: IsNSEntityDescription nsEntityDescription => nsEntityDescription -> IO (Id NSArray)
subentities nsEntityDescription  =
  sendMsg nsEntityDescription (mkSelector "subentities") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSubentities:@
setSubentities :: (IsNSEntityDescription nsEntityDescription, IsNSArray value) => nsEntityDescription -> value -> IO ()
setSubentities nsEntityDescription  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsEntityDescription (mkSelector "setSubentities:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- superentity@
superentity :: IsNSEntityDescription nsEntityDescription => nsEntityDescription -> IO (Id NSEntityDescription)
superentity nsEntityDescription  =
  sendMsg nsEntityDescription (mkSelector "superentity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- propertiesByName@
propertiesByName :: IsNSEntityDescription nsEntityDescription => nsEntityDescription -> IO (Id NSDictionary)
propertiesByName nsEntityDescription  =
  sendMsg nsEntityDescription (mkSelector "propertiesByName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- properties@
properties :: IsNSEntityDescription nsEntityDescription => nsEntityDescription -> IO (Id NSArray)
properties nsEntityDescription  =
  sendMsg nsEntityDescription (mkSelector "properties") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setProperties:@
setProperties :: (IsNSEntityDescription nsEntityDescription, IsNSArray value) => nsEntityDescription -> value -> IO ()
setProperties nsEntityDescription  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsEntityDescription (mkSelector "setProperties:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userInfo@
userInfo :: IsNSEntityDescription nsEntityDescription => nsEntityDescription -> IO (Id NSDictionary)
userInfo nsEntityDescription  =
  sendMsg nsEntityDescription (mkSelector "userInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserInfo:@
setUserInfo :: (IsNSEntityDescription nsEntityDescription, IsNSDictionary value) => nsEntityDescription -> value -> IO ()
setUserInfo nsEntityDescription  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsEntityDescription (mkSelector "setUserInfo:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- attributesByName@
attributesByName :: IsNSEntityDescription nsEntityDescription => nsEntityDescription -> IO (Id NSDictionary)
attributesByName nsEntityDescription  =
  sendMsg nsEntityDescription (mkSelector "attributesByName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- relationshipsByName@
relationshipsByName :: IsNSEntityDescription nsEntityDescription => nsEntityDescription -> IO (Id NSDictionary)
relationshipsByName nsEntityDescription  =
  sendMsg nsEntityDescription (mkSelector "relationshipsByName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- versionHash@
versionHash :: IsNSEntityDescription nsEntityDescription => nsEntityDescription -> IO (Id NSData)
versionHash nsEntityDescription  =
  sendMsg nsEntityDescription (mkSelector "versionHash") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- versionHashModifier@
versionHashModifier :: IsNSEntityDescription nsEntityDescription => nsEntityDescription -> IO (Id NSString)
versionHashModifier nsEntityDescription  =
  sendMsg nsEntityDescription (mkSelector "versionHashModifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVersionHashModifier:@
setVersionHashModifier :: (IsNSEntityDescription nsEntityDescription, IsNSString value) => nsEntityDescription -> value -> IO ()
setVersionHashModifier nsEntityDescription  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsEntityDescription (mkSelector "setVersionHashModifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- renamingIdentifier@
renamingIdentifier :: IsNSEntityDescription nsEntityDescription => nsEntityDescription -> IO (Id NSString)
renamingIdentifier nsEntityDescription  =
  sendMsg nsEntityDescription (mkSelector "renamingIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRenamingIdentifier:@
setRenamingIdentifier :: (IsNSEntityDescription nsEntityDescription, IsNSString value) => nsEntityDescription -> value -> IO ()
setRenamingIdentifier nsEntityDescription  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsEntityDescription (mkSelector "setRenamingIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- indexes@
indexes :: IsNSEntityDescription nsEntityDescription => nsEntityDescription -> IO (Id NSArray)
indexes nsEntityDescription  =
  sendMsg nsEntityDescription (mkSelector "indexes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIndexes:@
setIndexes :: (IsNSEntityDescription nsEntityDescription, IsNSArray value) => nsEntityDescription -> value -> IO ()
setIndexes nsEntityDescription  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsEntityDescription (mkSelector "setIndexes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- uniquenessConstraints@
uniquenessConstraints :: IsNSEntityDescription nsEntityDescription => nsEntityDescription -> IO (Id NSArray)
uniquenessConstraints nsEntityDescription  =
  sendMsg nsEntityDescription (mkSelector "uniquenessConstraints") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUniquenessConstraints:@
setUniquenessConstraints :: (IsNSEntityDescription nsEntityDescription, IsNSArray value) => nsEntityDescription -> value -> IO ()
setUniquenessConstraints nsEntityDescription  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsEntityDescription (mkSelector "setUniquenessConstraints:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- compoundIndexes@
compoundIndexes :: IsNSEntityDescription nsEntityDescription => nsEntityDescription -> IO (Id NSArray)
compoundIndexes nsEntityDescription  =
  sendMsg nsEntityDescription (mkSelector "compoundIndexes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCompoundIndexes:@
setCompoundIndexes :: (IsNSEntityDescription nsEntityDescription, IsNSArray value) => nsEntityDescription -> value -> IO ()
setCompoundIndexes nsEntityDescription  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsEntityDescription (mkSelector "setCompoundIndexes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- coreSpotlightDisplayNameExpression@
coreSpotlightDisplayNameExpression :: IsNSEntityDescription nsEntityDescription => nsEntityDescription -> IO (Id NSExpression)
coreSpotlightDisplayNameExpression nsEntityDescription  =
  sendMsg nsEntityDescription (mkSelector "coreSpotlightDisplayNameExpression") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCoreSpotlightDisplayNameExpression:@
setCoreSpotlightDisplayNameExpression :: (IsNSEntityDescription nsEntityDescription, IsNSExpression value) => nsEntityDescription -> value -> IO ()
setCoreSpotlightDisplayNameExpression nsEntityDescription  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsEntityDescription (mkSelector "setCoreSpotlightDisplayNameExpression:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @entityForName:inManagedObjectContext:@
entityForName_inManagedObjectContextSelector :: Selector
entityForName_inManagedObjectContextSelector = mkSelector "entityForName:inManagedObjectContext:"

-- | @Selector@ for @insertNewObjectForEntityForName:inManagedObjectContext:@
insertNewObjectForEntityForName_inManagedObjectContextSelector :: Selector
insertNewObjectForEntityForName_inManagedObjectContextSelector = mkSelector "insertNewObjectForEntityForName:inManagedObjectContext:"

-- | @Selector@ for @relationshipsWithDestinationEntity:@
relationshipsWithDestinationEntitySelector :: Selector
relationshipsWithDestinationEntitySelector = mkSelector "relationshipsWithDestinationEntity:"

-- | @Selector@ for @isKindOfEntity:@
isKindOfEntitySelector :: Selector
isKindOfEntitySelector = mkSelector "isKindOfEntity:"

-- | @Selector@ for @managedObjectModel@
managedObjectModelSelector :: Selector
managedObjectModelSelector = mkSelector "managedObjectModel"

-- | @Selector@ for @managedObjectClassName@
managedObjectClassNameSelector :: Selector
managedObjectClassNameSelector = mkSelector "managedObjectClassName"

-- | @Selector@ for @setManagedObjectClassName:@
setManagedObjectClassNameSelector :: Selector
setManagedObjectClassNameSelector = mkSelector "setManagedObjectClassName:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @abstract@
abstractSelector :: Selector
abstractSelector = mkSelector "abstract"

-- | @Selector@ for @setAbstract:@
setAbstractSelector :: Selector
setAbstractSelector = mkSelector "setAbstract:"

-- | @Selector@ for @subentitiesByName@
subentitiesByNameSelector :: Selector
subentitiesByNameSelector = mkSelector "subentitiesByName"

-- | @Selector@ for @subentities@
subentitiesSelector :: Selector
subentitiesSelector = mkSelector "subentities"

-- | @Selector@ for @setSubentities:@
setSubentitiesSelector :: Selector
setSubentitiesSelector = mkSelector "setSubentities:"

-- | @Selector@ for @superentity@
superentitySelector :: Selector
superentitySelector = mkSelector "superentity"

-- | @Selector@ for @propertiesByName@
propertiesByNameSelector :: Selector
propertiesByNameSelector = mkSelector "propertiesByName"

-- | @Selector@ for @properties@
propertiesSelector :: Selector
propertiesSelector = mkSelector "properties"

-- | @Selector@ for @setProperties:@
setPropertiesSelector :: Selector
setPropertiesSelector = mkSelector "setProperties:"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector
userInfoSelector = mkSelector "userInfo"

-- | @Selector@ for @setUserInfo:@
setUserInfoSelector :: Selector
setUserInfoSelector = mkSelector "setUserInfo:"

-- | @Selector@ for @attributesByName@
attributesByNameSelector :: Selector
attributesByNameSelector = mkSelector "attributesByName"

-- | @Selector@ for @relationshipsByName@
relationshipsByNameSelector :: Selector
relationshipsByNameSelector = mkSelector "relationshipsByName"

-- | @Selector@ for @versionHash@
versionHashSelector :: Selector
versionHashSelector = mkSelector "versionHash"

-- | @Selector@ for @versionHashModifier@
versionHashModifierSelector :: Selector
versionHashModifierSelector = mkSelector "versionHashModifier"

-- | @Selector@ for @setVersionHashModifier:@
setVersionHashModifierSelector :: Selector
setVersionHashModifierSelector = mkSelector "setVersionHashModifier:"

-- | @Selector@ for @renamingIdentifier@
renamingIdentifierSelector :: Selector
renamingIdentifierSelector = mkSelector "renamingIdentifier"

-- | @Selector@ for @setRenamingIdentifier:@
setRenamingIdentifierSelector :: Selector
setRenamingIdentifierSelector = mkSelector "setRenamingIdentifier:"

-- | @Selector@ for @indexes@
indexesSelector :: Selector
indexesSelector = mkSelector "indexes"

-- | @Selector@ for @setIndexes:@
setIndexesSelector :: Selector
setIndexesSelector = mkSelector "setIndexes:"

-- | @Selector@ for @uniquenessConstraints@
uniquenessConstraintsSelector :: Selector
uniquenessConstraintsSelector = mkSelector "uniquenessConstraints"

-- | @Selector@ for @setUniquenessConstraints:@
setUniquenessConstraintsSelector :: Selector
setUniquenessConstraintsSelector = mkSelector "setUniquenessConstraints:"

-- | @Selector@ for @compoundIndexes@
compoundIndexesSelector :: Selector
compoundIndexesSelector = mkSelector "compoundIndexes"

-- | @Selector@ for @setCompoundIndexes:@
setCompoundIndexesSelector :: Selector
setCompoundIndexesSelector = mkSelector "setCompoundIndexes:"

-- | @Selector@ for @coreSpotlightDisplayNameExpression@
coreSpotlightDisplayNameExpressionSelector :: Selector
coreSpotlightDisplayNameExpressionSelector = mkSelector "coreSpotlightDisplayNameExpression"

-- | @Selector@ for @setCoreSpotlightDisplayNameExpression:@
setCoreSpotlightDisplayNameExpressionSelector :: Selector
setCoreSpotlightDisplayNameExpressionSelector = mkSelector "setCoreSpotlightDisplayNameExpression:"

