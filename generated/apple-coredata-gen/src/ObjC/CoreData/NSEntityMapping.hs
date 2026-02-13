{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSEntityMapping@.
module ObjC.CoreData.NSEntityMapping
  ( NSEntityMapping
  , IsNSEntityMapping(..)
  , name
  , setName
  , mappingType
  , setMappingType
  , sourceEntityName
  , setSourceEntityName
  , sourceEntityVersionHash
  , setSourceEntityVersionHash
  , destinationEntityName
  , setDestinationEntityName
  , destinationEntityVersionHash
  , setDestinationEntityVersionHash
  , attributeMappings
  , setAttributeMappings
  , relationshipMappings
  , setRelationshipMappings
  , sourceExpression
  , setSourceExpression
  , userInfo
  , setUserInfo
  , entityMigrationPolicyClassName
  , setEntityMigrationPolicyClassName
  , attributeMappingsSelector
  , destinationEntityNameSelector
  , destinationEntityVersionHashSelector
  , entityMigrationPolicyClassNameSelector
  , mappingTypeSelector
  , nameSelector
  , relationshipMappingsSelector
  , setAttributeMappingsSelector
  , setDestinationEntityNameSelector
  , setDestinationEntityVersionHashSelector
  , setEntityMigrationPolicyClassNameSelector
  , setMappingTypeSelector
  , setNameSelector
  , setRelationshipMappingsSelector
  , setSourceEntityNameSelector
  , setSourceEntityVersionHashSelector
  , setSourceExpressionSelector
  , setUserInfoSelector
  , sourceEntityNameSelector
  , sourceEntityVersionHashSelector
  , sourceExpressionSelector
  , userInfoSelector

  -- * Enum types
  , NSEntityMappingType(NSEntityMappingType)
  , pattern NSUndefinedEntityMappingType
  , pattern NSCustomEntityMappingType
  , pattern NSAddEntityMappingType
  , pattern NSRemoveEntityMappingType
  , pattern NSCopyEntityMappingType
  , pattern NSTransformEntityMappingType

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.CoreData.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- name@
name :: IsNSEntityMapping nsEntityMapping => nsEntityMapping -> IO (Id NSString)
name nsEntityMapping =
  sendMessage nsEntityMapping nameSelector

-- | @- setName:@
setName :: (IsNSEntityMapping nsEntityMapping, IsNSString value) => nsEntityMapping -> value -> IO ()
setName nsEntityMapping value =
  sendMessage nsEntityMapping setNameSelector (toNSString value)

-- | @- mappingType@
mappingType :: IsNSEntityMapping nsEntityMapping => nsEntityMapping -> IO NSEntityMappingType
mappingType nsEntityMapping =
  sendMessage nsEntityMapping mappingTypeSelector

-- | @- setMappingType:@
setMappingType :: IsNSEntityMapping nsEntityMapping => nsEntityMapping -> NSEntityMappingType -> IO ()
setMappingType nsEntityMapping value =
  sendMessage nsEntityMapping setMappingTypeSelector value

-- | @- sourceEntityName@
sourceEntityName :: IsNSEntityMapping nsEntityMapping => nsEntityMapping -> IO (Id NSString)
sourceEntityName nsEntityMapping =
  sendMessage nsEntityMapping sourceEntityNameSelector

-- | @- setSourceEntityName:@
setSourceEntityName :: (IsNSEntityMapping nsEntityMapping, IsNSString value) => nsEntityMapping -> value -> IO ()
setSourceEntityName nsEntityMapping value =
  sendMessage nsEntityMapping setSourceEntityNameSelector (toNSString value)

-- | @- sourceEntityVersionHash@
sourceEntityVersionHash :: IsNSEntityMapping nsEntityMapping => nsEntityMapping -> IO (Id NSData)
sourceEntityVersionHash nsEntityMapping =
  sendMessage nsEntityMapping sourceEntityVersionHashSelector

-- | @- setSourceEntityVersionHash:@
setSourceEntityVersionHash :: (IsNSEntityMapping nsEntityMapping, IsNSData value) => nsEntityMapping -> value -> IO ()
setSourceEntityVersionHash nsEntityMapping value =
  sendMessage nsEntityMapping setSourceEntityVersionHashSelector (toNSData value)

-- | @- destinationEntityName@
destinationEntityName :: IsNSEntityMapping nsEntityMapping => nsEntityMapping -> IO (Id NSString)
destinationEntityName nsEntityMapping =
  sendMessage nsEntityMapping destinationEntityNameSelector

-- | @- setDestinationEntityName:@
setDestinationEntityName :: (IsNSEntityMapping nsEntityMapping, IsNSString value) => nsEntityMapping -> value -> IO ()
setDestinationEntityName nsEntityMapping value =
  sendMessage nsEntityMapping setDestinationEntityNameSelector (toNSString value)

-- | @- destinationEntityVersionHash@
destinationEntityVersionHash :: IsNSEntityMapping nsEntityMapping => nsEntityMapping -> IO (Id NSData)
destinationEntityVersionHash nsEntityMapping =
  sendMessage nsEntityMapping destinationEntityVersionHashSelector

-- | @- setDestinationEntityVersionHash:@
setDestinationEntityVersionHash :: (IsNSEntityMapping nsEntityMapping, IsNSData value) => nsEntityMapping -> value -> IO ()
setDestinationEntityVersionHash nsEntityMapping value =
  sendMessage nsEntityMapping setDestinationEntityVersionHashSelector (toNSData value)

-- | @- attributeMappings@
attributeMappings :: IsNSEntityMapping nsEntityMapping => nsEntityMapping -> IO (Id NSArray)
attributeMappings nsEntityMapping =
  sendMessage nsEntityMapping attributeMappingsSelector

-- | @- setAttributeMappings:@
setAttributeMappings :: (IsNSEntityMapping nsEntityMapping, IsNSArray value) => nsEntityMapping -> value -> IO ()
setAttributeMappings nsEntityMapping value =
  sendMessage nsEntityMapping setAttributeMappingsSelector (toNSArray value)

-- | @- relationshipMappings@
relationshipMappings :: IsNSEntityMapping nsEntityMapping => nsEntityMapping -> IO (Id NSArray)
relationshipMappings nsEntityMapping =
  sendMessage nsEntityMapping relationshipMappingsSelector

-- | @- setRelationshipMappings:@
setRelationshipMappings :: (IsNSEntityMapping nsEntityMapping, IsNSArray value) => nsEntityMapping -> value -> IO ()
setRelationshipMappings nsEntityMapping value =
  sendMessage nsEntityMapping setRelationshipMappingsSelector (toNSArray value)

-- | @- sourceExpression@
sourceExpression :: IsNSEntityMapping nsEntityMapping => nsEntityMapping -> IO (Id NSExpression)
sourceExpression nsEntityMapping =
  sendMessage nsEntityMapping sourceExpressionSelector

-- | @- setSourceExpression:@
setSourceExpression :: (IsNSEntityMapping nsEntityMapping, IsNSExpression value) => nsEntityMapping -> value -> IO ()
setSourceExpression nsEntityMapping value =
  sendMessage nsEntityMapping setSourceExpressionSelector (toNSExpression value)

-- | @- userInfo@
userInfo :: IsNSEntityMapping nsEntityMapping => nsEntityMapping -> IO (Id NSDictionary)
userInfo nsEntityMapping =
  sendMessage nsEntityMapping userInfoSelector

-- | @- setUserInfo:@
setUserInfo :: (IsNSEntityMapping nsEntityMapping, IsNSDictionary value) => nsEntityMapping -> value -> IO ()
setUserInfo nsEntityMapping value =
  sendMessage nsEntityMapping setUserInfoSelector (toNSDictionary value)

-- | @- entityMigrationPolicyClassName@
entityMigrationPolicyClassName :: IsNSEntityMapping nsEntityMapping => nsEntityMapping -> IO (Id NSString)
entityMigrationPolicyClassName nsEntityMapping =
  sendMessage nsEntityMapping entityMigrationPolicyClassNameSelector

-- | @- setEntityMigrationPolicyClassName:@
setEntityMigrationPolicyClassName :: (IsNSEntityMapping nsEntityMapping, IsNSString value) => nsEntityMapping -> value -> IO ()
setEntityMigrationPolicyClassName nsEntityMapping value =
  sendMessage nsEntityMapping setEntityMigrationPolicyClassNameSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @mappingType@
mappingTypeSelector :: Selector '[] NSEntityMappingType
mappingTypeSelector = mkSelector "mappingType"

-- | @Selector@ for @setMappingType:@
setMappingTypeSelector :: Selector '[NSEntityMappingType] ()
setMappingTypeSelector = mkSelector "setMappingType:"

-- | @Selector@ for @sourceEntityName@
sourceEntityNameSelector :: Selector '[] (Id NSString)
sourceEntityNameSelector = mkSelector "sourceEntityName"

-- | @Selector@ for @setSourceEntityName:@
setSourceEntityNameSelector :: Selector '[Id NSString] ()
setSourceEntityNameSelector = mkSelector "setSourceEntityName:"

-- | @Selector@ for @sourceEntityVersionHash@
sourceEntityVersionHashSelector :: Selector '[] (Id NSData)
sourceEntityVersionHashSelector = mkSelector "sourceEntityVersionHash"

-- | @Selector@ for @setSourceEntityVersionHash:@
setSourceEntityVersionHashSelector :: Selector '[Id NSData] ()
setSourceEntityVersionHashSelector = mkSelector "setSourceEntityVersionHash:"

-- | @Selector@ for @destinationEntityName@
destinationEntityNameSelector :: Selector '[] (Id NSString)
destinationEntityNameSelector = mkSelector "destinationEntityName"

-- | @Selector@ for @setDestinationEntityName:@
setDestinationEntityNameSelector :: Selector '[Id NSString] ()
setDestinationEntityNameSelector = mkSelector "setDestinationEntityName:"

-- | @Selector@ for @destinationEntityVersionHash@
destinationEntityVersionHashSelector :: Selector '[] (Id NSData)
destinationEntityVersionHashSelector = mkSelector "destinationEntityVersionHash"

-- | @Selector@ for @setDestinationEntityVersionHash:@
setDestinationEntityVersionHashSelector :: Selector '[Id NSData] ()
setDestinationEntityVersionHashSelector = mkSelector "setDestinationEntityVersionHash:"

-- | @Selector@ for @attributeMappings@
attributeMappingsSelector :: Selector '[] (Id NSArray)
attributeMappingsSelector = mkSelector "attributeMappings"

-- | @Selector@ for @setAttributeMappings:@
setAttributeMappingsSelector :: Selector '[Id NSArray] ()
setAttributeMappingsSelector = mkSelector "setAttributeMappings:"

-- | @Selector@ for @relationshipMappings@
relationshipMappingsSelector :: Selector '[] (Id NSArray)
relationshipMappingsSelector = mkSelector "relationshipMappings"

-- | @Selector@ for @setRelationshipMappings:@
setRelationshipMappingsSelector :: Selector '[Id NSArray] ()
setRelationshipMappingsSelector = mkSelector "setRelationshipMappings:"

-- | @Selector@ for @sourceExpression@
sourceExpressionSelector :: Selector '[] (Id NSExpression)
sourceExpressionSelector = mkSelector "sourceExpression"

-- | @Selector@ for @setSourceExpression:@
setSourceExpressionSelector :: Selector '[Id NSExpression] ()
setSourceExpressionSelector = mkSelector "setSourceExpression:"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector '[] (Id NSDictionary)
userInfoSelector = mkSelector "userInfo"

-- | @Selector@ for @setUserInfo:@
setUserInfoSelector :: Selector '[Id NSDictionary] ()
setUserInfoSelector = mkSelector "setUserInfo:"

-- | @Selector@ for @entityMigrationPolicyClassName@
entityMigrationPolicyClassNameSelector :: Selector '[] (Id NSString)
entityMigrationPolicyClassNameSelector = mkSelector "entityMigrationPolicyClassName"

-- | @Selector@ for @setEntityMigrationPolicyClassName:@
setEntityMigrationPolicyClassNameSelector :: Selector '[Id NSString] ()
setEntityMigrationPolicyClassNameSelector = mkSelector "setEntityMigrationPolicyClassName:"

