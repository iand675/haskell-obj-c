{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPropertyDescription@.
module ObjC.CoreData.NSPropertyDescription
  ( NSPropertyDescription
  , IsNSPropertyDescription(..)
  , setValidationPredicates_withValidationWarnings
  , entity
  , name
  , setName
  , optional
  , setOptional
  , transient
  , setTransient
  , validationPredicates
  , validationWarnings
  , userInfo
  , setUserInfo
  , indexed
  , setIndexed
  , versionHash
  , versionHashModifier
  , setVersionHashModifier
  , indexedBySpotlight
  , setIndexedBySpotlight
  , storedInExternalRecord
  , setStoredInExternalRecord
  , renamingIdentifier
  , setRenamingIdentifier
  , setValidationPredicates_withValidationWarningsSelector
  , entitySelector
  , nameSelector
  , setNameSelector
  , optionalSelector
  , setOptionalSelector
  , transientSelector
  , setTransientSelector
  , validationPredicatesSelector
  , validationWarningsSelector
  , userInfoSelector
  , setUserInfoSelector
  , indexedSelector
  , setIndexedSelector
  , versionHashSelector
  , versionHashModifierSelector
  , setVersionHashModifierSelector
  , indexedBySpotlightSelector
  , setIndexedBySpotlightSelector
  , storedInExternalRecordSelector
  , setStoredInExternalRecordSelector
  , renamingIdentifierSelector
  , setRenamingIdentifierSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- setValidationPredicates:withValidationWarnings:@
setValidationPredicates_withValidationWarnings :: (IsNSPropertyDescription nsPropertyDescription, IsNSArray validationPredicates, IsNSArray validationWarnings) => nsPropertyDescription -> validationPredicates -> validationWarnings -> IO ()
setValidationPredicates_withValidationWarnings nsPropertyDescription  validationPredicates validationWarnings =
withObjCPtr validationPredicates $ \raw_validationPredicates ->
  withObjCPtr validationWarnings $ \raw_validationWarnings ->
      sendMsg nsPropertyDescription (mkSelector "setValidationPredicates:withValidationWarnings:") retVoid [argPtr (castPtr raw_validationPredicates :: Ptr ()), argPtr (castPtr raw_validationWarnings :: Ptr ())]

-- | @- entity@
entity :: IsNSPropertyDescription nsPropertyDescription => nsPropertyDescription -> IO (Id NSEntityDescription)
entity nsPropertyDescription  =
  sendMsg nsPropertyDescription (mkSelector "entity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- name@
name :: IsNSPropertyDescription nsPropertyDescription => nsPropertyDescription -> IO (Id NSString)
name nsPropertyDescription  =
  sendMsg nsPropertyDescription (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsNSPropertyDescription nsPropertyDescription, IsNSString value) => nsPropertyDescription -> value -> IO ()
setName nsPropertyDescription  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPropertyDescription (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- optional@
optional :: IsNSPropertyDescription nsPropertyDescription => nsPropertyDescription -> IO Bool
optional nsPropertyDescription  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPropertyDescription (mkSelector "optional") retCULong []

-- | @- setOptional:@
setOptional :: IsNSPropertyDescription nsPropertyDescription => nsPropertyDescription -> Bool -> IO ()
setOptional nsPropertyDescription  value =
  sendMsg nsPropertyDescription (mkSelector "setOptional:") retVoid [argCULong (if value then 1 else 0)]

-- | @- transient@
transient :: IsNSPropertyDescription nsPropertyDescription => nsPropertyDescription -> IO Bool
transient nsPropertyDescription  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPropertyDescription (mkSelector "transient") retCULong []

-- | @- setTransient:@
setTransient :: IsNSPropertyDescription nsPropertyDescription => nsPropertyDescription -> Bool -> IO ()
setTransient nsPropertyDescription  value =
  sendMsg nsPropertyDescription (mkSelector "setTransient:") retVoid [argCULong (if value then 1 else 0)]

-- | @- validationPredicates@
validationPredicates :: IsNSPropertyDescription nsPropertyDescription => nsPropertyDescription -> IO (Id NSArray)
validationPredicates nsPropertyDescription  =
  sendMsg nsPropertyDescription (mkSelector "validationPredicates") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- validationWarnings@
validationWarnings :: IsNSPropertyDescription nsPropertyDescription => nsPropertyDescription -> IO (Id NSArray)
validationWarnings nsPropertyDescription  =
  sendMsg nsPropertyDescription (mkSelector "validationWarnings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- userInfo@
userInfo :: IsNSPropertyDescription nsPropertyDescription => nsPropertyDescription -> IO (Id NSDictionary)
userInfo nsPropertyDescription  =
  sendMsg nsPropertyDescription (mkSelector "userInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserInfo:@
setUserInfo :: (IsNSPropertyDescription nsPropertyDescription, IsNSDictionary value) => nsPropertyDescription -> value -> IO ()
setUserInfo nsPropertyDescription  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPropertyDescription (mkSelector "setUserInfo:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- indexed@
indexed :: IsNSPropertyDescription nsPropertyDescription => nsPropertyDescription -> IO Bool
indexed nsPropertyDescription  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPropertyDescription (mkSelector "indexed") retCULong []

-- | @- setIndexed:@
setIndexed :: IsNSPropertyDescription nsPropertyDescription => nsPropertyDescription -> Bool -> IO ()
setIndexed nsPropertyDescription  value =
  sendMsg nsPropertyDescription (mkSelector "setIndexed:") retVoid [argCULong (if value then 1 else 0)]

-- | @- versionHash@
versionHash :: IsNSPropertyDescription nsPropertyDescription => nsPropertyDescription -> IO (Id NSData)
versionHash nsPropertyDescription  =
  sendMsg nsPropertyDescription (mkSelector "versionHash") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- versionHashModifier@
versionHashModifier :: IsNSPropertyDescription nsPropertyDescription => nsPropertyDescription -> IO (Id NSString)
versionHashModifier nsPropertyDescription  =
  sendMsg nsPropertyDescription (mkSelector "versionHashModifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVersionHashModifier:@
setVersionHashModifier :: (IsNSPropertyDescription nsPropertyDescription, IsNSString value) => nsPropertyDescription -> value -> IO ()
setVersionHashModifier nsPropertyDescription  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPropertyDescription (mkSelector "setVersionHashModifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- indexedBySpotlight@
indexedBySpotlight :: IsNSPropertyDescription nsPropertyDescription => nsPropertyDescription -> IO Bool
indexedBySpotlight nsPropertyDescription  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPropertyDescription (mkSelector "indexedBySpotlight") retCULong []

-- | @- setIndexedBySpotlight:@
setIndexedBySpotlight :: IsNSPropertyDescription nsPropertyDescription => nsPropertyDescription -> Bool -> IO ()
setIndexedBySpotlight nsPropertyDescription  value =
  sendMsg nsPropertyDescription (mkSelector "setIndexedBySpotlight:") retVoid [argCULong (if value then 1 else 0)]

-- | @- storedInExternalRecord@
storedInExternalRecord :: IsNSPropertyDescription nsPropertyDescription => nsPropertyDescription -> IO Bool
storedInExternalRecord nsPropertyDescription  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPropertyDescription (mkSelector "storedInExternalRecord") retCULong []

-- | @- setStoredInExternalRecord:@
setStoredInExternalRecord :: IsNSPropertyDescription nsPropertyDescription => nsPropertyDescription -> Bool -> IO ()
setStoredInExternalRecord nsPropertyDescription  value =
  sendMsg nsPropertyDescription (mkSelector "setStoredInExternalRecord:") retVoid [argCULong (if value then 1 else 0)]

-- | @- renamingIdentifier@
renamingIdentifier :: IsNSPropertyDescription nsPropertyDescription => nsPropertyDescription -> IO (Id NSString)
renamingIdentifier nsPropertyDescription  =
  sendMsg nsPropertyDescription (mkSelector "renamingIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRenamingIdentifier:@
setRenamingIdentifier :: (IsNSPropertyDescription nsPropertyDescription, IsNSString value) => nsPropertyDescription -> value -> IO ()
setRenamingIdentifier nsPropertyDescription  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPropertyDescription (mkSelector "setRenamingIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setValidationPredicates:withValidationWarnings:@
setValidationPredicates_withValidationWarningsSelector :: Selector
setValidationPredicates_withValidationWarningsSelector = mkSelector "setValidationPredicates:withValidationWarnings:"

-- | @Selector@ for @entity@
entitySelector :: Selector
entitySelector = mkSelector "entity"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @optional@
optionalSelector :: Selector
optionalSelector = mkSelector "optional"

-- | @Selector@ for @setOptional:@
setOptionalSelector :: Selector
setOptionalSelector = mkSelector "setOptional:"

-- | @Selector@ for @transient@
transientSelector :: Selector
transientSelector = mkSelector "transient"

-- | @Selector@ for @setTransient:@
setTransientSelector :: Selector
setTransientSelector = mkSelector "setTransient:"

-- | @Selector@ for @validationPredicates@
validationPredicatesSelector :: Selector
validationPredicatesSelector = mkSelector "validationPredicates"

-- | @Selector@ for @validationWarnings@
validationWarningsSelector :: Selector
validationWarningsSelector = mkSelector "validationWarnings"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector
userInfoSelector = mkSelector "userInfo"

-- | @Selector@ for @setUserInfo:@
setUserInfoSelector :: Selector
setUserInfoSelector = mkSelector "setUserInfo:"

-- | @Selector@ for @indexed@
indexedSelector :: Selector
indexedSelector = mkSelector "indexed"

-- | @Selector@ for @setIndexed:@
setIndexedSelector :: Selector
setIndexedSelector = mkSelector "setIndexed:"

-- | @Selector@ for @versionHash@
versionHashSelector :: Selector
versionHashSelector = mkSelector "versionHash"

-- | @Selector@ for @versionHashModifier@
versionHashModifierSelector :: Selector
versionHashModifierSelector = mkSelector "versionHashModifier"

-- | @Selector@ for @setVersionHashModifier:@
setVersionHashModifierSelector :: Selector
setVersionHashModifierSelector = mkSelector "setVersionHashModifier:"

-- | @Selector@ for @indexedBySpotlight@
indexedBySpotlightSelector :: Selector
indexedBySpotlightSelector = mkSelector "indexedBySpotlight"

-- | @Selector@ for @setIndexedBySpotlight:@
setIndexedBySpotlightSelector :: Selector
setIndexedBySpotlightSelector = mkSelector "setIndexedBySpotlight:"

-- | @Selector@ for @storedInExternalRecord@
storedInExternalRecordSelector :: Selector
storedInExternalRecordSelector = mkSelector "storedInExternalRecord"

-- | @Selector@ for @setStoredInExternalRecord:@
setStoredInExternalRecordSelector :: Selector
setStoredInExternalRecordSelector = mkSelector "setStoredInExternalRecord:"

-- | @Selector@ for @renamingIdentifier@
renamingIdentifierSelector :: Selector
renamingIdentifierSelector = mkSelector "renamingIdentifier"

-- | @Selector@ for @setRenamingIdentifier:@
setRenamingIdentifierSelector :: Selector
setRenamingIdentifierSelector = mkSelector "setRenamingIdentifier:"

