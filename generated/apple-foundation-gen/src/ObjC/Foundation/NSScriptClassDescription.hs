{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSScriptClassDescription@.
module ObjC.Foundation.NSScriptClassDescription
  ( NSScriptClassDescription
  , IsNSScriptClassDescription(..)
  , classDescriptionForClass
  , initWithSuiteName_className_dictionary
  , matchesAppleEventCode
  , supportsCommand
  , selectorForCommand
  , typeForKey
  , classDescriptionForKey
  , appleEventCodeForKey
  , keyWithAppleEventCode
  , isLocationRequiredToCreateForKey
  , hasPropertyForKey
  , hasOrderedToManyRelationshipForKey
  , hasReadablePropertyForKey
  , hasWritablePropertyForKey
  , isReadOnlyKey
  , suiteName
  , className
  , implementationClassName
  , superclassDescription
  , appleEventCode
  , defaultSubcontainerAttributeKey
  , appleEventCodeForKeySelector
  , appleEventCodeSelector
  , classDescriptionForClassSelector
  , classDescriptionForKeySelector
  , classNameSelector
  , defaultSubcontainerAttributeKeySelector
  , hasOrderedToManyRelationshipForKeySelector
  , hasPropertyForKeySelector
  , hasReadablePropertyForKeySelector
  , hasWritablePropertyForKeySelector
  , implementationClassNameSelector
  , initWithSuiteName_className_dictionarySelector
  , isLocationRequiredToCreateForKeySelector
  , isReadOnlyKeySelector
  , keyWithAppleEventCodeSelector
  , matchesAppleEventCodeSelector
  , selectorForCommandSelector
  , suiteNameSelector
  , superclassDescriptionSelector
  , supportsCommandSelector
  , typeForKeySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ classDescriptionForClass:@
classDescriptionForClass :: Class -> IO (Id NSScriptClassDescription)
classDescriptionForClass aClass =
  do
    cls' <- getRequiredClass "NSScriptClassDescription"
    sendClassMessage cls' classDescriptionForClassSelector aClass

-- | @- initWithSuiteName:className:dictionary:@
initWithSuiteName_className_dictionary :: (IsNSScriptClassDescription nsScriptClassDescription, IsNSString suiteName, IsNSString className, IsNSDictionary classDeclaration) => nsScriptClassDescription -> suiteName -> className -> classDeclaration -> IO (Id NSScriptClassDescription)
initWithSuiteName_className_dictionary nsScriptClassDescription suiteName className classDeclaration =
  sendOwnedMessage nsScriptClassDescription initWithSuiteName_className_dictionarySelector (toNSString suiteName) (toNSString className) (toNSDictionary classDeclaration)

-- | @- matchesAppleEventCode:@
matchesAppleEventCode :: IsNSScriptClassDescription nsScriptClassDescription => nsScriptClassDescription -> CUInt -> IO Bool
matchesAppleEventCode nsScriptClassDescription appleEventCode =
  sendMessage nsScriptClassDescription matchesAppleEventCodeSelector appleEventCode

-- | @- supportsCommand:@
supportsCommand :: (IsNSScriptClassDescription nsScriptClassDescription, IsNSScriptCommandDescription commandDescription) => nsScriptClassDescription -> commandDescription -> IO Bool
supportsCommand nsScriptClassDescription commandDescription =
  sendMessage nsScriptClassDescription supportsCommandSelector (toNSScriptCommandDescription commandDescription)

-- | @- selectorForCommand:@
selectorForCommand :: (IsNSScriptClassDescription nsScriptClassDescription, IsNSScriptCommandDescription commandDescription) => nsScriptClassDescription -> commandDescription -> IO Sel
selectorForCommand nsScriptClassDescription commandDescription =
  sendMessage nsScriptClassDescription selectorForCommandSelector (toNSScriptCommandDescription commandDescription)

-- | @- typeForKey:@
typeForKey :: (IsNSScriptClassDescription nsScriptClassDescription, IsNSString key) => nsScriptClassDescription -> key -> IO (Id NSString)
typeForKey nsScriptClassDescription key =
  sendMessage nsScriptClassDescription typeForKeySelector (toNSString key)

-- | @- classDescriptionForKey:@
classDescriptionForKey :: (IsNSScriptClassDescription nsScriptClassDescription, IsNSString key) => nsScriptClassDescription -> key -> IO (Id NSScriptClassDescription)
classDescriptionForKey nsScriptClassDescription key =
  sendMessage nsScriptClassDescription classDescriptionForKeySelector (toNSString key)

-- | @- appleEventCodeForKey:@
appleEventCodeForKey :: (IsNSScriptClassDescription nsScriptClassDescription, IsNSString key) => nsScriptClassDescription -> key -> IO CUInt
appleEventCodeForKey nsScriptClassDescription key =
  sendMessage nsScriptClassDescription appleEventCodeForKeySelector (toNSString key)

-- | @- keyWithAppleEventCode:@
keyWithAppleEventCode :: IsNSScriptClassDescription nsScriptClassDescription => nsScriptClassDescription -> CUInt -> IO (Id NSString)
keyWithAppleEventCode nsScriptClassDescription appleEventCode =
  sendMessage nsScriptClassDescription keyWithAppleEventCodeSelector appleEventCode

-- | @- isLocationRequiredToCreateForKey:@
isLocationRequiredToCreateForKey :: (IsNSScriptClassDescription nsScriptClassDescription, IsNSString toManyRelationshipKey) => nsScriptClassDescription -> toManyRelationshipKey -> IO Bool
isLocationRequiredToCreateForKey nsScriptClassDescription toManyRelationshipKey =
  sendMessage nsScriptClassDescription isLocationRequiredToCreateForKeySelector (toNSString toManyRelationshipKey)

-- | @- hasPropertyForKey:@
hasPropertyForKey :: (IsNSScriptClassDescription nsScriptClassDescription, IsNSString key) => nsScriptClassDescription -> key -> IO Bool
hasPropertyForKey nsScriptClassDescription key =
  sendMessage nsScriptClassDescription hasPropertyForKeySelector (toNSString key)

-- | @- hasOrderedToManyRelationshipForKey:@
hasOrderedToManyRelationshipForKey :: (IsNSScriptClassDescription nsScriptClassDescription, IsNSString key) => nsScriptClassDescription -> key -> IO Bool
hasOrderedToManyRelationshipForKey nsScriptClassDescription key =
  sendMessage nsScriptClassDescription hasOrderedToManyRelationshipForKeySelector (toNSString key)

-- | @- hasReadablePropertyForKey:@
hasReadablePropertyForKey :: (IsNSScriptClassDescription nsScriptClassDescription, IsNSString key) => nsScriptClassDescription -> key -> IO Bool
hasReadablePropertyForKey nsScriptClassDescription key =
  sendMessage nsScriptClassDescription hasReadablePropertyForKeySelector (toNSString key)

-- | @- hasWritablePropertyForKey:@
hasWritablePropertyForKey :: (IsNSScriptClassDescription nsScriptClassDescription, IsNSString key) => nsScriptClassDescription -> key -> IO Bool
hasWritablePropertyForKey nsScriptClassDescription key =
  sendMessage nsScriptClassDescription hasWritablePropertyForKeySelector (toNSString key)

-- | @- isReadOnlyKey:@
isReadOnlyKey :: (IsNSScriptClassDescription nsScriptClassDescription, IsNSString key) => nsScriptClassDescription -> key -> IO Bool
isReadOnlyKey nsScriptClassDescription key =
  sendMessage nsScriptClassDescription isReadOnlyKeySelector (toNSString key)

-- | @- suiteName@
suiteName :: IsNSScriptClassDescription nsScriptClassDescription => nsScriptClassDescription -> IO (Id NSString)
suiteName nsScriptClassDescription =
  sendMessage nsScriptClassDescription suiteNameSelector

-- | @- className@
className :: IsNSScriptClassDescription nsScriptClassDescription => nsScriptClassDescription -> IO (Id NSString)
className nsScriptClassDescription =
  sendMessage nsScriptClassDescription classNameSelector

-- | @- implementationClassName@
implementationClassName :: IsNSScriptClassDescription nsScriptClassDescription => nsScriptClassDescription -> IO (Id NSString)
implementationClassName nsScriptClassDescription =
  sendMessage nsScriptClassDescription implementationClassNameSelector

-- | @- superclassDescription@
superclassDescription :: IsNSScriptClassDescription nsScriptClassDescription => nsScriptClassDescription -> IO (Id NSScriptClassDescription)
superclassDescription nsScriptClassDescription =
  sendMessage nsScriptClassDescription superclassDescriptionSelector

-- | @- appleEventCode@
appleEventCode :: IsNSScriptClassDescription nsScriptClassDescription => nsScriptClassDescription -> IO CUInt
appleEventCode nsScriptClassDescription =
  sendMessage nsScriptClassDescription appleEventCodeSelector

-- | @- defaultSubcontainerAttributeKey@
defaultSubcontainerAttributeKey :: IsNSScriptClassDescription nsScriptClassDescription => nsScriptClassDescription -> IO (Id NSString)
defaultSubcontainerAttributeKey nsScriptClassDescription =
  sendMessage nsScriptClassDescription defaultSubcontainerAttributeKeySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @classDescriptionForClass:@
classDescriptionForClassSelector :: Selector '[Class] (Id NSScriptClassDescription)
classDescriptionForClassSelector = mkSelector "classDescriptionForClass:"

-- | @Selector@ for @initWithSuiteName:className:dictionary:@
initWithSuiteName_className_dictionarySelector :: Selector '[Id NSString, Id NSString, Id NSDictionary] (Id NSScriptClassDescription)
initWithSuiteName_className_dictionarySelector = mkSelector "initWithSuiteName:className:dictionary:"

-- | @Selector@ for @matchesAppleEventCode:@
matchesAppleEventCodeSelector :: Selector '[CUInt] Bool
matchesAppleEventCodeSelector = mkSelector "matchesAppleEventCode:"

-- | @Selector@ for @supportsCommand:@
supportsCommandSelector :: Selector '[Id NSScriptCommandDescription] Bool
supportsCommandSelector = mkSelector "supportsCommand:"

-- | @Selector@ for @selectorForCommand:@
selectorForCommandSelector :: Selector '[Id NSScriptCommandDescription] Sel
selectorForCommandSelector = mkSelector "selectorForCommand:"

-- | @Selector@ for @typeForKey:@
typeForKeySelector :: Selector '[Id NSString] (Id NSString)
typeForKeySelector = mkSelector "typeForKey:"

-- | @Selector@ for @classDescriptionForKey:@
classDescriptionForKeySelector :: Selector '[Id NSString] (Id NSScriptClassDescription)
classDescriptionForKeySelector = mkSelector "classDescriptionForKey:"

-- | @Selector@ for @appleEventCodeForKey:@
appleEventCodeForKeySelector :: Selector '[Id NSString] CUInt
appleEventCodeForKeySelector = mkSelector "appleEventCodeForKey:"

-- | @Selector@ for @keyWithAppleEventCode:@
keyWithAppleEventCodeSelector :: Selector '[CUInt] (Id NSString)
keyWithAppleEventCodeSelector = mkSelector "keyWithAppleEventCode:"

-- | @Selector@ for @isLocationRequiredToCreateForKey:@
isLocationRequiredToCreateForKeySelector :: Selector '[Id NSString] Bool
isLocationRequiredToCreateForKeySelector = mkSelector "isLocationRequiredToCreateForKey:"

-- | @Selector@ for @hasPropertyForKey:@
hasPropertyForKeySelector :: Selector '[Id NSString] Bool
hasPropertyForKeySelector = mkSelector "hasPropertyForKey:"

-- | @Selector@ for @hasOrderedToManyRelationshipForKey:@
hasOrderedToManyRelationshipForKeySelector :: Selector '[Id NSString] Bool
hasOrderedToManyRelationshipForKeySelector = mkSelector "hasOrderedToManyRelationshipForKey:"

-- | @Selector@ for @hasReadablePropertyForKey:@
hasReadablePropertyForKeySelector :: Selector '[Id NSString] Bool
hasReadablePropertyForKeySelector = mkSelector "hasReadablePropertyForKey:"

-- | @Selector@ for @hasWritablePropertyForKey:@
hasWritablePropertyForKeySelector :: Selector '[Id NSString] Bool
hasWritablePropertyForKeySelector = mkSelector "hasWritablePropertyForKey:"

-- | @Selector@ for @isReadOnlyKey:@
isReadOnlyKeySelector :: Selector '[Id NSString] Bool
isReadOnlyKeySelector = mkSelector "isReadOnlyKey:"

-- | @Selector@ for @suiteName@
suiteNameSelector :: Selector '[] (Id NSString)
suiteNameSelector = mkSelector "suiteName"

-- | @Selector@ for @className@
classNameSelector :: Selector '[] (Id NSString)
classNameSelector = mkSelector "className"

-- | @Selector@ for @implementationClassName@
implementationClassNameSelector :: Selector '[] (Id NSString)
implementationClassNameSelector = mkSelector "implementationClassName"

-- | @Selector@ for @superclassDescription@
superclassDescriptionSelector :: Selector '[] (Id NSScriptClassDescription)
superclassDescriptionSelector = mkSelector "superclassDescription"

-- | @Selector@ for @appleEventCode@
appleEventCodeSelector :: Selector '[] CUInt
appleEventCodeSelector = mkSelector "appleEventCode"

-- | @Selector@ for @defaultSubcontainerAttributeKey@
defaultSubcontainerAttributeKeySelector :: Selector '[] (Id NSString)
defaultSubcontainerAttributeKeySelector = mkSelector "defaultSubcontainerAttributeKey"

