{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSAttributeDescription@.
module ObjC.AppKit.NSAttributeDescription
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
  , attributeTypeSelector
  , setAttributeTypeSelector
  , attributeValueClassNameSelector
  , setAttributeValueClassNameSelector
  , defaultValueSelector
  , setDefaultValueSelector
  , versionHashSelector
  , valueTransformerNameSelector
  , setValueTransformerNameSelector
  , allowsExternalBinaryDataStorageSelector
  , setAllowsExternalBinaryDataStorageSelector
  , preservesValueInHistoryOnDeletionSelector
  , setPreservesValueInHistoryOnDeletionSelector
  , allowsCloudEncryptionSelector
  , setAllowsCloudEncryptionSelector

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

import ObjC.AppKit.Internal.Classes
import ObjC.CoreData.Internal.Enums
import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- attributeType@
attributeType :: IsNSAttributeDescription nsAttributeDescription => nsAttributeDescription -> IO NSAttributeType
attributeType nsAttributeDescription  =
  fmap (coerce :: CULong -> NSAttributeType) $ sendMsg nsAttributeDescription (mkSelector "attributeType") retCULong []

-- | @- setAttributeType:@
setAttributeType :: IsNSAttributeDescription nsAttributeDescription => nsAttributeDescription -> NSAttributeType -> IO ()
setAttributeType nsAttributeDescription  value =
  sendMsg nsAttributeDescription (mkSelector "setAttributeType:") retVoid [argCULong (coerce value)]

-- | @- attributeValueClassName@
attributeValueClassName :: IsNSAttributeDescription nsAttributeDescription => nsAttributeDescription -> IO (Id NSString)
attributeValueClassName nsAttributeDescription  =
  sendMsg nsAttributeDescription (mkSelector "attributeValueClassName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAttributeValueClassName:@
setAttributeValueClassName :: (IsNSAttributeDescription nsAttributeDescription, IsNSString value) => nsAttributeDescription -> value -> IO ()
setAttributeValueClassName nsAttributeDescription  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsAttributeDescription (mkSelector "setAttributeValueClassName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- defaultValue@
defaultValue :: IsNSAttributeDescription nsAttributeDescription => nsAttributeDescription -> IO RawId
defaultValue nsAttributeDescription  =
  fmap (RawId . castPtr) $ sendMsg nsAttributeDescription (mkSelector "defaultValue") (retPtr retVoid) []

-- | @- setDefaultValue:@
setDefaultValue :: IsNSAttributeDescription nsAttributeDescription => nsAttributeDescription -> RawId -> IO ()
setDefaultValue nsAttributeDescription  value =
  sendMsg nsAttributeDescription (mkSelector "setDefaultValue:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- versionHash@
versionHash :: IsNSAttributeDescription nsAttributeDescription => nsAttributeDescription -> IO (Id NSData)
versionHash nsAttributeDescription  =
  sendMsg nsAttributeDescription (mkSelector "versionHash") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- valueTransformerName@
valueTransformerName :: IsNSAttributeDescription nsAttributeDescription => nsAttributeDescription -> IO (Id NSString)
valueTransformerName nsAttributeDescription  =
  sendMsg nsAttributeDescription (mkSelector "valueTransformerName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValueTransformerName:@
setValueTransformerName :: (IsNSAttributeDescription nsAttributeDescription, IsNSString value) => nsAttributeDescription -> value -> IO ()
setValueTransformerName nsAttributeDescription  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsAttributeDescription (mkSelector "setValueTransformerName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- allowsExternalBinaryDataStorage@
allowsExternalBinaryDataStorage :: IsNSAttributeDescription nsAttributeDescription => nsAttributeDescription -> IO Bool
allowsExternalBinaryDataStorage nsAttributeDescription  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsAttributeDescription (mkSelector "allowsExternalBinaryDataStorage") retCULong []

-- | @- setAllowsExternalBinaryDataStorage:@
setAllowsExternalBinaryDataStorage :: IsNSAttributeDescription nsAttributeDescription => nsAttributeDescription -> Bool -> IO ()
setAllowsExternalBinaryDataStorage nsAttributeDescription  value =
  sendMsg nsAttributeDescription (mkSelector "setAllowsExternalBinaryDataStorage:") retVoid [argCULong (if value then 1 else 0)]

-- | @- preservesValueInHistoryOnDeletion@
preservesValueInHistoryOnDeletion :: IsNSAttributeDescription nsAttributeDescription => nsAttributeDescription -> IO Bool
preservesValueInHistoryOnDeletion nsAttributeDescription  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsAttributeDescription (mkSelector "preservesValueInHistoryOnDeletion") retCULong []

-- | @- setPreservesValueInHistoryOnDeletion:@
setPreservesValueInHistoryOnDeletion :: IsNSAttributeDescription nsAttributeDescription => nsAttributeDescription -> Bool -> IO ()
setPreservesValueInHistoryOnDeletion nsAttributeDescription  value =
  sendMsg nsAttributeDescription (mkSelector "setPreservesValueInHistoryOnDeletion:") retVoid [argCULong (if value then 1 else 0)]

-- | @- allowsCloudEncryption@
allowsCloudEncryption :: IsNSAttributeDescription nsAttributeDescription => nsAttributeDescription -> IO Bool
allowsCloudEncryption nsAttributeDescription  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsAttributeDescription (mkSelector "allowsCloudEncryption") retCULong []

-- | @- setAllowsCloudEncryption:@
setAllowsCloudEncryption :: IsNSAttributeDescription nsAttributeDescription => nsAttributeDescription -> Bool -> IO ()
setAllowsCloudEncryption nsAttributeDescription  value =
  sendMsg nsAttributeDescription (mkSelector "setAllowsCloudEncryption:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attributeType@
attributeTypeSelector :: Selector
attributeTypeSelector = mkSelector "attributeType"

-- | @Selector@ for @setAttributeType:@
setAttributeTypeSelector :: Selector
setAttributeTypeSelector = mkSelector "setAttributeType:"

-- | @Selector@ for @attributeValueClassName@
attributeValueClassNameSelector :: Selector
attributeValueClassNameSelector = mkSelector "attributeValueClassName"

-- | @Selector@ for @setAttributeValueClassName:@
setAttributeValueClassNameSelector :: Selector
setAttributeValueClassNameSelector = mkSelector "setAttributeValueClassName:"

-- | @Selector@ for @defaultValue@
defaultValueSelector :: Selector
defaultValueSelector = mkSelector "defaultValue"

-- | @Selector@ for @setDefaultValue:@
setDefaultValueSelector :: Selector
setDefaultValueSelector = mkSelector "setDefaultValue:"

-- | @Selector@ for @versionHash@
versionHashSelector :: Selector
versionHashSelector = mkSelector "versionHash"

-- | @Selector@ for @valueTransformerName@
valueTransformerNameSelector :: Selector
valueTransformerNameSelector = mkSelector "valueTransformerName"

-- | @Selector@ for @setValueTransformerName:@
setValueTransformerNameSelector :: Selector
setValueTransformerNameSelector = mkSelector "setValueTransformerName:"

-- | @Selector@ for @allowsExternalBinaryDataStorage@
allowsExternalBinaryDataStorageSelector :: Selector
allowsExternalBinaryDataStorageSelector = mkSelector "allowsExternalBinaryDataStorage"

-- | @Selector@ for @setAllowsExternalBinaryDataStorage:@
setAllowsExternalBinaryDataStorageSelector :: Selector
setAllowsExternalBinaryDataStorageSelector = mkSelector "setAllowsExternalBinaryDataStorage:"

-- | @Selector@ for @preservesValueInHistoryOnDeletion@
preservesValueInHistoryOnDeletionSelector :: Selector
preservesValueInHistoryOnDeletionSelector = mkSelector "preservesValueInHistoryOnDeletion"

-- | @Selector@ for @setPreservesValueInHistoryOnDeletion:@
setPreservesValueInHistoryOnDeletionSelector :: Selector
setPreservesValueInHistoryOnDeletionSelector = mkSelector "setPreservesValueInHistoryOnDeletion:"

-- | @Selector@ for @allowsCloudEncryption@
allowsCloudEncryptionSelector :: Selector
allowsCloudEncryptionSelector = mkSelector "allowsCloudEncryption"

-- | @Selector@ for @setAllowsCloudEncryption:@
setAllowsCloudEncryptionSelector :: Selector
setAllowsCloudEncryptionSelector = mkSelector "setAllowsCloudEncryption:"

