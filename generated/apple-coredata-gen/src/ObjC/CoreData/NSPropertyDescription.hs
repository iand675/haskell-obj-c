{-# LANGUAGE DataKinds #-}
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
  , entitySelector
  , indexedBySpotlightSelector
  , indexedSelector
  , nameSelector
  , optionalSelector
  , renamingIdentifierSelector
  , setIndexedBySpotlightSelector
  , setIndexedSelector
  , setNameSelector
  , setOptionalSelector
  , setRenamingIdentifierSelector
  , setStoredInExternalRecordSelector
  , setTransientSelector
  , setUserInfoSelector
  , setValidationPredicates_withValidationWarningsSelector
  , setVersionHashModifierSelector
  , storedInExternalRecordSelector
  , transientSelector
  , userInfoSelector
  , validationPredicatesSelector
  , validationWarningsSelector
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

-- | @- setValidationPredicates:withValidationWarnings:@
setValidationPredicates_withValidationWarnings :: (IsNSPropertyDescription nsPropertyDescription, IsNSArray validationPredicates, IsNSArray validationWarnings) => nsPropertyDescription -> validationPredicates -> validationWarnings -> IO ()
setValidationPredicates_withValidationWarnings nsPropertyDescription validationPredicates validationWarnings =
  sendMessage nsPropertyDescription setValidationPredicates_withValidationWarningsSelector (toNSArray validationPredicates) (toNSArray validationWarnings)

-- | @- entity@
entity :: IsNSPropertyDescription nsPropertyDescription => nsPropertyDescription -> IO (Id NSEntityDescription)
entity nsPropertyDescription =
  sendMessage nsPropertyDescription entitySelector

-- | @- name@
name :: IsNSPropertyDescription nsPropertyDescription => nsPropertyDescription -> IO (Id NSString)
name nsPropertyDescription =
  sendMessage nsPropertyDescription nameSelector

-- | @- setName:@
setName :: (IsNSPropertyDescription nsPropertyDescription, IsNSString value) => nsPropertyDescription -> value -> IO ()
setName nsPropertyDescription value =
  sendMessage nsPropertyDescription setNameSelector (toNSString value)

-- | @- optional@
optional :: IsNSPropertyDescription nsPropertyDescription => nsPropertyDescription -> IO Bool
optional nsPropertyDescription =
  sendMessage nsPropertyDescription optionalSelector

-- | @- setOptional:@
setOptional :: IsNSPropertyDescription nsPropertyDescription => nsPropertyDescription -> Bool -> IO ()
setOptional nsPropertyDescription value =
  sendMessage nsPropertyDescription setOptionalSelector value

-- | @- transient@
transient :: IsNSPropertyDescription nsPropertyDescription => nsPropertyDescription -> IO Bool
transient nsPropertyDescription =
  sendMessage nsPropertyDescription transientSelector

-- | @- setTransient:@
setTransient :: IsNSPropertyDescription nsPropertyDescription => nsPropertyDescription -> Bool -> IO ()
setTransient nsPropertyDescription value =
  sendMessage nsPropertyDescription setTransientSelector value

-- | @- validationPredicates@
validationPredicates :: IsNSPropertyDescription nsPropertyDescription => nsPropertyDescription -> IO (Id NSArray)
validationPredicates nsPropertyDescription =
  sendMessage nsPropertyDescription validationPredicatesSelector

-- | @- validationWarnings@
validationWarnings :: IsNSPropertyDescription nsPropertyDescription => nsPropertyDescription -> IO (Id NSArray)
validationWarnings nsPropertyDescription =
  sendMessage nsPropertyDescription validationWarningsSelector

-- | @- userInfo@
userInfo :: IsNSPropertyDescription nsPropertyDescription => nsPropertyDescription -> IO (Id NSDictionary)
userInfo nsPropertyDescription =
  sendMessage nsPropertyDescription userInfoSelector

-- | @- setUserInfo:@
setUserInfo :: (IsNSPropertyDescription nsPropertyDescription, IsNSDictionary value) => nsPropertyDescription -> value -> IO ()
setUserInfo nsPropertyDescription value =
  sendMessage nsPropertyDescription setUserInfoSelector (toNSDictionary value)

-- | @- indexed@
indexed :: IsNSPropertyDescription nsPropertyDescription => nsPropertyDescription -> IO Bool
indexed nsPropertyDescription =
  sendMessage nsPropertyDescription indexedSelector

-- | @- setIndexed:@
setIndexed :: IsNSPropertyDescription nsPropertyDescription => nsPropertyDescription -> Bool -> IO ()
setIndexed nsPropertyDescription value =
  sendMessage nsPropertyDescription setIndexedSelector value

-- | @- versionHash@
versionHash :: IsNSPropertyDescription nsPropertyDescription => nsPropertyDescription -> IO (Id NSData)
versionHash nsPropertyDescription =
  sendMessage nsPropertyDescription versionHashSelector

-- | @- versionHashModifier@
versionHashModifier :: IsNSPropertyDescription nsPropertyDescription => nsPropertyDescription -> IO (Id NSString)
versionHashModifier nsPropertyDescription =
  sendMessage nsPropertyDescription versionHashModifierSelector

-- | @- setVersionHashModifier:@
setVersionHashModifier :: (IsNSPropertyDescription nsPropertyDescription, IsNSString value) => nsPropertyDescription -> value -> IO ()
setVersionHashModifier nsPropertyDescription value =
  sendMessage nsPropertyDescription setVersionHashModifierSelector (toNSString value)

-- | @- indexedBySpotlight@
indexedBySpotlight :: IsNSPropertyDescription nsPropertyDescription => nsPropertyDescription -> IO Bool
indexedBySpotlight nsPropertyDescription =
  sendMessage nsPropertyDescription indexedBySpotlightSelector

-- | @- setIndexedBySpotlight:@
setIndexedBySpotlight :: IsNSPropertyDescription nsPropertyDescription => nsPropertyDescription -> Bool -> IO ()
setIndexedBySpotlight nsPropertyDescription value =
  sendMessage nsPropertyDescription setIndexedBySpotlightSelector value

-- | @- storedInExternalRecord@
storedInExternalRecord :: IsNSPropertyDescription nsPropertyDescription => nsPropertyDescription -> IO Bool
storedInExternalRecord nsPropertyDescription =
  sendMessage nsPropertyDescription storedInExternalRecordSelector

-- | @- setStoredInExternalRecord:@
setStoredInExternalRecord :: IsNSPropertyDescription nsPropertyDescription => nsPropertyDescription -> Bool -> IO ()
setStoredInExternalRecord nsPropertyDescription value =
  sendMessage nsPropertyDescription setStoredInExternalRecordSelector value

-- | @- renamingIdentifier@
renamingIdentifier :: IsNSPropertyDescription nsPropertyDescription => nsPropertyDescription -> IO (Id NSString)
renamingIdentifier nsPropertyDescription =
  sendMessage nsPropertyDescription renamingIdentifierSelector

-- | @- setRenamingIdentifier:@
setRenamingIdentifier :: (IsNSPropertyDescription nsPropertyDescription, IsNSString value) => nsPropertyDescription -> value -> IO ()
setRenamingIdentifier nsPropertyDescription value =
  sendMessage nsPropertyDescription setRenamingIdentifierSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setValidationPredicates:withValidationWarnings:@
setValidationPredicates_withValidationWarningsSelector :: Selector '[Id NSArray, Id NSArray] ()
setValidationPredicates_withValidationWarningsSelector = mkSelector "setValidationPredicates:withValidationWarnings:"

-- | @Selector@ for @entity@
entitySelector :: Selector '[] (Id NSEntityDescription)
entitySelector = mkSelector "entity"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @optional@
optionalSelector :: Selector '[] Bool
optionalSelector = mkSelector "optional"

-- | @Selector@ for @setOptional:@
setOptionalSelector :: Selector '[Bool] ()
setOptionalSelector = mkSelector "setOptional:"

-- | @Selector@ for @transient@
transientSelector :: Selector '[] Bool
transientSelector = mkSelector "transient"

-- | @Selector@ for @setTransient:@
setTransientSelector :: Selector '[Bool] ()
setTransientSelector = mkSelector "setTransient:"

-- | @Selector@ for @validationPredicates@
validationPredicatesSelector :: Selector '[] (Id NSArray)
validationPredicatesSelector = mkSelector "validationPredicates"

-- | @Selector@ for @validationWarnings@
validationWarningsSelector :: Selector '[] (Id NSArray)
validationWarningsSelector = mkSelector "validationWarnings"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector '[] (Id NSDictionary)
userInfoSelector = mkSelector "userInfo"

-- | @Selector@ for @setUserInfo:@
setUserInfoSelector :: Selector '[Id NSDictionary] ()
setUserInfoSelector = mkSelector "setUserInfo:"

-- | @Selector@ for @indexed@
indexedSelector :: Selector '[] Bool
indexedSelector = mkSelector "indexed"

-- | @Selector@ for @setIndexed:@
setIndexedSelector :: Selector '[Bool] ()
setIndexedSelector = mkSelector "setIndexed:"

-- | @Selector@ for @versionHash@
versionHashSelector :: Selector '[] (Id NSData)
versionHashSelector = mkSelector "versionHash"

-- | @Selector@ for @versionHashModifier@
versionHashModifierSelector :: Selector '[] (Id NSString)
versionHashModifierSelector = mkSelector "versionHashModifier"

-- | @Selector@ for @setVersionHashModifier:@
setVersionHashModifierSelector :: Selector '[Id NSString] ()
setVersionHashModifierSelector = mkSelector "setVersionHashModifier:"

-- | @Selector@ for @indexedBySpotlight@
indexedBySpotlightSelector :: Selector '[] Bool
indexedBySpotlightSelector = mkSelector "indexedBySpotlight"

-- | @Selector@ for @setIndexedBySpotlight:@
setIndexedBySpotlightSelector :: Selector '[Bool] ()
setIndexedBySpotlightSelector = mkSelector "setIndexedBySpotlight:"

-- | @Selector@ for @storedInExternalRecord@
storedInExternalRecordSelector :: Selector '[] Bool
storedInExternalRecordSelector = mkSelector "storedInExternalRecord"

-- | @Selector@ for @setStoredInExternalRecord:@
setStoredInExternalRecordSelector :: Selector '[Bool] ()
setStoredInExternalRecordSelector = mkSelector "setStoredInExternalRecord:"

-- | @Selector@ for @renamingIdentifier@
renamingIdentifierSelector :: Selector '[] (Id NSString)
renamingIdentifierSelector = mkSelector "renamingIdentifier"

-- | @Selector@ for @setRenamingIdentifier:@
setRenamingIdentifierSelector :: Selector '[Id NSString] ()
setRenamingIdentifierSelector = mkSelector "setRenamingIdentifier:"

