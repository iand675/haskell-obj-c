{-# LANGUAGE PatternSynonyms #-}
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
  , nameSelector
  , setNameSelector
  , mappingTypeSelector
  , setMappingTypeSelector
  , sourceEntityNameSelector
  , setSourceEntityNameSelector
  , sourceEntityVersionHashSelector
  , setSourceEntityVersionHashSelector
  , destinationEntityNameSelector
  , setDestinationEntityNameSelector
  , destinationEntityVersionHashSelector
  , setDestinationEntityVersionHashSelector
  , attributeMappingsSelector
  , setAttributeMappingsSelector
  , relationshipMappingsSelector
  , setRelationshipMappingsSelector
  , sourceExpressionSelector
  , setSourceExpressionSelector
  , userInfoSelector
  , setUserInfoSelector
  , entityMigrationPolicyClassNameSelector
  , setEntityMigrationPolicyClassNameSelector

  -- * Enum types
  , NSEntityMappingType(NSEntityMappingType)
  , pattern NSUndefinedEntityMappingType
  , pattern NSCustomEntityMappingType
  , pattern NSAddEntityMappingType
  , pattern NSRemoveEntityMappingType
  , pattern NSCopyEntityMappingType
  , pattern NSTransformEntityMappingType

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
import ObjC.CoreData.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- name@
name :: IsNSEntityMapping nsEntityMapping => nsEntityMapping -> IO (Id NSString)
name nsEntityMapping  =
  sendMsg nsEntityMapping (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsNSEntityMapping nsEntityMapping, IsNSString value) => nsEntityMapping -> value -> IO ()
setName nsEntityMapping  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsEntityMapping (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- mappingType@
mappingType :: IsNSEntityMapping nsEntityMapping => nsEntityMapping -> IO NSEntityMappingType
mappingType nsEntityMapping  =
  fmap (coerce :: CULong -> NSEntityMappingType) $ sendMsg nsEntityMapping (mkSelector "mappingType") retCULong []

-- | @- setMappingType:@
setMappingType :: IsNSEntityMapping nsEntityMapping => nsEntityMapping -> NSEntityMappingType -> IO ()
setMappingType nsEntityMapping  value =
  sendMsg nsEntityMapping (mkSelector "setMappingType:") retVoid [argCULong (coerce value)]

-- | @- sourceEntityName@
sourceEntityName :: IsNSEntityMapping nsEntityMapping => nsEntityMapping -> IO (Id NSString)
sourceEntityName nsEntityMapping  =
  sendMsg nsEntityMapping (mkSelector "sourceEntityName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSourceEntityName:@
setSourceEntityName :: (IsNSEntityMapping nsEntityMapping, IsNSString value) => nsEntityMapping -> value -> IO ()
setSourceEntityName nsEntityMapping  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsEntityMapping (mkSelector "setSourceEntityName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sourceEntityVersionHash@
sourceEntityVersionHash :: IsNSEntityMapping nsEntityMapping => nsEntityMapping -> IO (Id NSData)
sourceEntityVersionHash nsEntityMapping  =
  sendMsg nsEntityMapping (mkSelector "sourceEntityVersionHash") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSourceEntityVersionHash:@
setSourceEntityVersionHash :: (IsNSEntityMapping nsEntityMapping, IsNSData value) => nsEntityMapping -> value -> IO ()
setSourceEntityVersionHash nsEntityMapping  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsEntityMapping (mkSelector "setSourceEntityVersionHash:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- destinationEntityName@
destinationEntityName :: IsNSEntityMapping nsEntityMapping => nsEntityMapping -> IO (Id NSString)
destinationEntityName nsEntityMapping  =
  sendMsg nsEntityMapping (mkSelector "destinationEntityName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDestinationEntityName:@
setDestinationEntityName :: (IsNSEntityMapping nsEntityMapping, IsNSString value) => nsEntityMapping -> value -> IO ()
setDestinationEntityName nsEntityMapping  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsEntityMapping (mkSelector "setDestinationEntityName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- destinationEntityVersionHash@
destinationEntityVersionHash :: IsNSEntityMapping nsEntityMapping => nsEntityMapping -> IO (Id NSData)
destinationEntityVersionHash nsEntityMapping  =
  sendMsg nsEntityMapping (mkSelector "destinationEntityVersionHash") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDestinationEntityVersionHash:@
setDestinationEntityVersionHash :: (IsNSEntityMapping nsEntityMapping, IsNSData value) => nsEntityMapping -> value -> IO ()
setDestinationEntityVersionHash nsEntityMapping  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsEntityMapping (mkSelector "setDestinationEntityVersionHash:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- attributeMappings@
attributeMappings :: IsNSEntityMapping nsEntityMapping => nsEntityMapping -> IO (Id NSArray)
attributeMappings nsEntityMapping  =
  sendMsg nsEntityMapping (mkSelector "attributeMappings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAttributeMappings:@
setAttributeMappings :: (IsNSEntityMapping nsEntityMapping, IsNSArray value) => nsEntityMapping -> value -> IO ()
setAttributeMappings nsEntityMapping  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsEntityMapping (mkSelector "setAttributeMappings:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- relationshipMappings@
relationshipMappings :: IsNSEntityMapping nsEntityMapping => nsEntityMapping -> IO (Id NSArray)
relationshipMappings nsEntityMapping  =
  sendMsg nsEntityMapping (mkSelector "relationshipMappings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRelationshipMappings:@
setRelationshipMappings :: (IsNSEntityMapping nsEntityMapping, IsNSArray value) => nsEntityMapping -> value -> IO ()
setRelationshipMappings nsEntityMapping  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsEntityMapping (mkSelector "setRelationshipMappings:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sourceExpression@
sourceExpression :: IsNSEntityMapping nsEntityMapping => nsEntityMapping -> IO (Id NSExpression)
sourceExpression nsEntityMapping  =
  sendMsg nsEntityMapping (mkSelector "sourceExpression") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSourceExpression:@
setSourceExpression :: (IsNSEntityMapping nsEntityMapping, IsNSExpression value) => nsEntityMapping -> value -> IO ()
setSourceExpression nsEntityMapping  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsEntityMapping (mkSelector "setSourceExpression:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- userInfo@
userInfo :: IsNSEntityMapping nsEntityMapping => nsEntityMapping -> IO (Id NSDictionary)
userInfo nsEntityMapping  =
  sendMsg nsEntityMapping (mkSelector "userInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserInfo:@
setUserInfo :: (IsNSEntityMapping nsEntityMapping, IsNSDictionary value) => nsEntityMapping -> value -> IO ()
setUserInfo nsEntityMapping  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsEntityMapping (mkSelector "setUserInfo:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- entityMigrationPolicyClassName@
entityMigrationPolicyClassName :: IsNSEntityMapping nsEntityMapping => nsEntityMapping -> IO (Id NSString)
entityMigrationPolicyClassName nsEntityMapping  =
  sendMsg nsEntityMapping (mkSelector "entityMigrationPolicyClassName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEntityMigrationPolicyClassName:@
setEntityMigrationPolicyClassName :: (IsNSEntityMapping nsEntityMapping, IsNSString value) => nsEntityMapping -> value -> IO ()
setEntityMigrationPolicyClassName nsEntityMapping  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsEntityMapping (mkSelector "setEntityMigrationPolicyClassName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @mappingType@
mappingTypeSelector :: Selector
mappingTypeSelector = mkSelector "mappingType"

-- | @Selector@ for @setMappingType:@
setMappingTypeSelector :: Selector
setMappingTypeSelector = mkSelector "setMappingType:"

-- | @Selector@ for @sourceEntityName@
sourceEntityNameSelector :: Selector
sourceEntityNameSelector = mkSelector "sourceEntityName"

-- | @Selector@ for @setSourceEntityName:@
setSourceEntityNameSelector :: Selector
setSourceEntityNameSelector = mkSelector "setSourceEntityName:"

-- | @Selector@ for @sourceEntityVersionHash@
sourceEntityVersionHashSelector :: Selector
sourceEntityVersionHashSelector = mkSelector "sourceEntityVersionHash"

-- | @Selector@ for @setSourceEntityVersionHash:@
setSourceEntityVersionHashSelector :: Selector
setSourceEntityVersionHashSelector = mkSelector "setSourceEntityVersionHash:"

-- | @Selector@ for @destinationEntityName@
destinationEntityNameSelector :: Selector
destinationEntityNameSelector = mkSelector "destinationEntityName"

-- | @Selector@ for @setDestinationEntityName:@
setDestinationEntityNameSelector :: Selector
setDestinationEntityNameSelector = mkSelector "setDestinationEntityName:"

-- | @Selector@ for @destinationEntityVersionHash@
destinationEntityVersionHashSelector :: Selector
destinationEntityVersionHashSelector = mkSelector "destinationEntityVersionHash"

-- | @Selector@ for @setDestinationEntityVersionHash:@
setDestinationEntityVersionHashSelector :: Selector
setDestinationEntityVersionHashSelector = mkSelector "setDestinationEntityVersionHash:"

-- | @Selector@ for @attributeMappings@
attributeMappingsSelector :: Selector
attributeMappingsSelector = mkSelector "attributeMappings"

-- | @Selector@ for @setAttributeMappings:@
setAttributeMappingsSelector :: Selector
setAttributeMappingsSelector = mkSelector "setAttributeMappings:"

-- | @Selector@ for @relationshipMappings@
relationshipMappingsSelector :: Selector
relationshipMappingsSelector = mkSelector "relationshipMappings"

-- | @Selector@ for @setRelationshipMappings:@
setRelationshipMappingsSelector :: Selector
setRelationshipMappingsSelector = mkSelector "setRelationshipMappings:"

-- | @Selector@ for @sourceExpression@
sourceExpressionSelector :: Selector
sourceExpressionSelector = mkSelector "sourceExpression"

-- | @Selector@ for @setSourceExpression:@
setSourceExpressionSelector :: Selector
setSourceExpressionSelector = mkSelector "setSourceExpression:"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector
userInfoSelector = mkSelector "userInfo"

-- | @Selector@ for @setUserInfo:@
setUserInfoSelector :: Selector
setUserInfoSelector = mkSelector "setUserInfo:"

-- | @Selector@ for @entityMigrationPolicyClassName@
entityMigrationPolicyClassNameSelector :: Selector
entityMigrationPolicyClassNameSelector = mkSelector "entityMigrationPolicyClassName"

-- | @Selector@ for @setEntityMigrationPolicyClassName:@
setEntityMigrationPolicyClassNameSelector :: Selector
setEntityMigrationPolicyClassNameSelector = mkSelector "setEntityMigrationPolicyClassName:"

