{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSAttributeDescription@.
module ObjC.CoreData.NSAttributeDescription
  ( NSAttributeDescription
  , IsNSAttributeDescription(..)
  , attributeType
  , setAttributeType
  , attributeValueClassName
  , setAttributeValueClassName
  , defaultValue
  , setDefaultValue
  , versionHash
  , valueTransformerName
  , setValueTransformerName
  , allowsExternalBinaryDataStorage
  , setAllowsExternalBinaryDataStorage
  , preservesValueInHistoryOnDeletion
  , setPreservesValueInHistoryOnDeletion
  , allowsCloudEncryption
  , setAllowsCloudEncryption
  , allowsCloudEncryptionSelector
  , allowsExternalBinaryDataStorageSelector
  , attributeTypeSelector
  , attributeValueClassNameSelector
  , defaultValueSelector
  , preservesValueInHistoryOnDeletionSelector
  , setAllowsCloudEncryptionSelector
  , setAllowsExternalBinaryDataStorageSelector
  , setAttributeTypeSelector
  , setAttributeValueClassNameSelector
  , setDefaultValueSelector
  , setPreservesValueInHistoryOnDeletionSelector
  , setValueTransformerNameSelector
  , valueTransformerNameSelector
  , versionHashSelector

  -- * Enum types
  , NSAttributeType(NSAttributeType)
  , pattern NSUndefinedAttributeType
  , pattern NSInteger16AttributeType
  , pattern NSInteger32AttributeType
  , pattern NSInteger64AttributeType
  , pattern NSDecimalAttributeType
  , pattern NSDoubleAttributeType
  , pattern NSFloatAttributeType
  , pattern NSStringAttributeType
  , pattern NSBooleanAttributeType
  , pattern NSDateAttributeType
  , pattern NSBinaryDataAttributeType
  , pattern NSUUIDAttributeType
  , pattern NSURIAttributeType
  , pattern NSTransformableAttributeType
  , pattern NSObjectIDAttributeType
  , pattern NSCompositeAttributeType

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

-- | @- attributeType@
attributeType :: IsNSAttributeDescription nsAttributeDescription => nsAttributeDescription -> IO NSAttributeType
attributeType nsAttributeDescription =
  sendMessage nsAttributeDescription attributeTypeSelector

-- | @- setAttributeType:@
setAttributeType :: IsNSAttributeDescription nsAttributeDescription => nsAttributeDescription -> NSAttributeType -> IO ()
setAttributeType nsAttributeDescription value =
  sendMessage nsAttributeDescription setAttributeTypeSelector value

-- | @- attributeValueClassName@
attributeValueClassName :: IsNSAttributeDescription nsAttributeDescription => nsAttributeDescription -> IO (Id NSString)
attributeValueClassName nsAttributeDescription =
  sendMessage nsAttributeDescription attributeValueClassNameSelector

-- | @- setAttributeValueClassName:@
setAttributeValueClassName :: (IsNSAttributeDescription nsAttributeDescription, IsNSString value) => nsAttributeDescription -> value -> IO ()
setAttributeValueClassName nsAttributeDescription value =
  sendMessage nsAttributeDescription setAttributeValueClassNameSelector (toNSString value)

-- | @- defaultValue@
defaultValue :: IsNSAttributeDescription nsAttributeDescription => nsAttributeDescription -> IO RawId
defaultValue nsAttributeDescription =
  sendMessage nsAttributeDescription defaultValueSelector

-- | @- setDefaultValue:@
setDefaultValue :: IsNSAttributeDescription nsAttributeDescription => nsAttributeDescription -> RawId -> IO ()
setDefaultValue nsAttributeDescription value =
  sendMessage nsAttributeDescription setDefaultValueSelector value

-- | @- versionHash@
versionHash :: IsNSAttributeDescription nsAttributeDescription => nsAttributeDescription -> IO (Id NSData)
versionHash nsAttributeDescription =
  sendMessage nsAttributeDescription versionHashSelector

-- | @- valueTransformerName@
valueTransformerName :: IsNSAttributeDescription nsAttributeDescription => nsAttributeDescription -> IO (Id NSString)
valueTransformerName nsAttributeDescription =
  sendMessage nsAttributeDescription valueTransformerNameSelector

-- | @- setValueTransformerName:@
setValueTransformerName :: (IsNSAttributeDescription nsAttributeDescription, IsNSString value) => nsAttributeDescription -> value -> IO ()
setValueTransformerName nsAttributeDescription value =
  sendMessage nsAttributeDescription setValueTransformerNameSelector (toNSString value)

-- | @- allowsExternalBinaryDataStorage@
allowsExternalBinaryDataStorage :: IsNSAttributeDescription nsAttributeDescription => nsAttributeDescription -> IO Bool
allowsExternalBinaryDataStorage nsAttributeDescription =
  sendMessage nsAttributeDescription allowsExternalBinaryDataStorageSelector

-- | @- setAllowsExternalBinaryDataStorage:@
setAllowsExternalBinaryDataStorage :: IsNSAttributeDescription nsAttributeDescription => nsAttributeDescription -> Bool -> IO ()
setAllowsExternalBinaryDataStorage nsAttributeDescription value =
  sendMessage nsAttributeDescription setAllowsExternalBinaryDataStorageSelector value

-- | @- preservesValueInHistoryOnDeletion@
preservesValueInHistoryOnDeletion :: IsNSAttributeDescription nsAttributeDescription => nsAttributeDescription -> IO Bool
preservesValueInHistoryOnDeletion nsAttributeDescription =
  sendMessage nsAttributeDescription preservesValueInHistoryOnDeletionSelector

-- | @- setPreservesValueInHistoryOnDeletion:@
setPreservesValueInHistoryOnDeletion :: IsNSAttributeDescription nsAttributeDescription => nsAttributeDescription -> Bool -> IO ()
setPreservesValueInHistoryOnDeletion nsAttributeDescription value =
  sendMessage nsAttributeDescription setPreservesValueInHistoryOnDeletionSelector value

-- | @- allowsCloudEncryption@
allowsCloudEncryption :: IsNSAttributeDescription nsAttributeDescription => nsAttributeDescription -> IO Bool
allowsCloudEncryption nsAttributeDescription =
  sendMessage nsAttributeDescription allowsCloudEncryptionSelector

-- | @- setAllowsCloudEncryption:@
setAllowsCloudEncryption :: IsNSAttributeDescription nsAttributeDescription => nsAttributeDescription -> Bool -> IO ()
setAllowsCloudEncryption nsAttributeDescription value =
  sendMessage nsAttributeDescription setAllowsCloudEncryptionSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attributeType@
attributeTypeSelector :: Selector '[] NSAttributeType
attributeTypeSelector = mkSelector "attributeType"

-- | @Selector@ for @setAttributeType:@
setAttributeTypeSelector :: Selector '[NSAttributeType] ()
setAttributeTypeSelector = mkSelector "setAttributeType:"

-- | @Selector@ for @attributeValueClassName@
attributeValueClassNameSelector :: Selector '[] (Id NSString)
attributeValueClassNameSelector = mkSelector "attributeValueClassName"

-- | @Selector@ for @setAttributeValueClassName:@
setAttributeValueClassNameSelector :: Selector '[Id NSString] ()
setAttributeValueClassNameSelector = mkSelector "setAttributeValueClassName:"

-- | @Selector@ for @defaultValue@
defaultValueSelector :: Selector '[] RawId
defaultValueSelector = mkSelector "defaultValue"

-- | @Selector@ for @setDefaultValue:@
setDefaultValueSelector :: Selector '[RawId] ()
setDefaultValueSelector = mkSelector "setDefaultValue:"

-- | @Selector@ for @versionHash@
versionHashSelector :: Selector '[] (Id NSData)
versionHashSelector = mkSelector "versionHash"

-- | @Selector@ for @valueTransformerName@
valueTransformerNameSelector :: Selector '[] (Id NSString)
valueTransformerNameSelector = mkSelector "valueTransformerName"

-- | @Selector@ for @setValueTransformerName:@
setValueTransformerNameSelector :: Selector '[Id NSString] ()
setValueTransformerNameSelector = mkSelector "setValueTransformerName:"

-- | @Selector@ for @allowsExternalBinaryDataStorage@
allowsExternalBinaryDataStorageSelector :: Selector '[] Bool
allowsExternalBinaryDataStorageSelector = mkSelector "allowsExternalBinaryDataStorage"

-- | @Selector@ for @setAllowsExternalBinaryDataStorage:@
setAllowsExternalBinaryDataStorageSelector :: Selector '[Bool] ()
setAllowsExternalBinaryDataStorageSelector = mkSelector "setAllowsExternalBinaryDataStorage:"

-- | @Selector@ for @preservesValueInHistoryOnDeletion@
preservesValueInHistoryOnDeletionSelector :: Selector '[] Bool
preservesValueInHistoryOnDeletionSelector = mkSelector "preservesValueInHistoryOnDeletion"

-- | @Selector@ for @setPreservesValueInHistoryOnDeletion:@
setPreservesValueInHistoryOnDeletionSelector :: Selector '[Bool] ()
setPreservesValueInHistoryOnDeletionSelector = mkSelector "setPreservesValueInHistoryOnDeletion:"

-- | @Selector@ for @allowsCloudEncryption@
allowsCloudEncryptionSelector :: Selector '[] Bool
allowsCloudEncryptionSelector = mkSelector "allowsCloudEncryption"

-- | @Selector@ for @setAllowsCloudEncryption:@
setAllowsCloudEncryptionSelector :: Selector '[Bool] ()
setAllowsCloudEncryptionSelector = mkSelector "setAllowsCloudEncryption:"

