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
  , classDescriptionForClassSelector
  , initWithSuiteName_className_dictionarySelector
  , matchesAppleEventCodeSelector
  , supportsCommandSelector
  , selectorForCommandSelector
  , typeForKeySelector
  , classDescriptionForKeySelector
  , appleEventCodeForKeySelector
  , keyWithAppleEventCodeSelector
  , isLocationRequiredToCreateForKeySelector
  , hasPropertyForKeySelector
  , hasOrderedToManyRelationshipForKeySelector
  , hasReadablePropertyForKeySelector
  , hasWritablePropertyForKeySelector
  , isReadOnlyKeySelector
  , suiteNameSelector
  , classNameSelector
  , implementationClassNameSelector
  , superclassDescriptionSelector
  , appleEventCodeSelector
  , defaultSubcontainerAttributeKeySelector


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

import ObjC.Foundation.Internal.Classes

-- | @+ classDescriptionForClass:@
classDescriptionForClass :: Class -> IO (Id NSScriptClassDescription)
classDescriptionForClass aClass =
  do
    cls' <- getRequiredClass "NSScriptClassDescription"
    sendClassMsg cls' (mkSelector "classDescriptionForClass:") (retPtr retVoid) [argPtr (unClass aClass)] >>= retainedObject . castPtr

-- | @- initWithSuiteName:className:dictionary:@
initWithSuiteName_className_dictionary :: (IsNSScriptClassDescription nsScriptClassDescription, IsNSString suiteName, IsNSString className, IsNSDictionary classDeclaration) => nsScriptClassDescription -> suiteName -> className -> classDeclaration -> IO (Id NSScriptClassDescription)
initWithSuiteName_className_dictionary nsScriptClassDescription  suiteName className classDeclaration =
withObjCPtr suiteName $ \raw_suiteName ->
  withObjCPtr className $ \raw_className ->
    withObjCPtr classDeclaration $ \raw_classDeclaration ->
        sendMsg nsScriptClassDescription (mkSelector "initWithSuiteName:className:dictionary:") (retPtr retVoid) [argPtr (castPtr raw_suiteName :: Ptr ()), argPtr (castPtr raw_className :: Ptr ()), argPtr (castPtr raw_classDeclaration :: Ptr ())] >>= ownedObject . castPtr

-- | @- matchesAppleEventCode:@
matchesAppleEventCode :: IsNSScriptClassDescription nsScriptClassDescription => nsScriptClassDescription -> CUInt -> IO Bool
matchesAppleEventCode nsScriptClassDescription  appleEventCode =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScriptClassDescription (mkSelector "matchesAppleEventCode:") retCULong [argCUInt (fromIntegral appleEventCode)]

-- | @- supportsCommand:@
supportsCommand :: (IsNSScriptClassDescription nsScriptClassDescription, IsNSScriptCommandDescription commandDescription) => nsScriptClassDescription -> commandDescription -> IO Bool
supportsCommand nsScriptClassDescription  commandDescription =
withObjCPtr commandDescription $ \raw_commandDescription ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScriptClassDescription (mkSelector "supportsCommand:") retCULong [argPtr (castPtr raw_commandDescription :: Ptr ())]

-- | @- selectorForCommand:@
selectorForCommand :: (IsNSScriptClassDescription nsScriptClassDescription, IsNSScriptCommandDescription commandDescription) => nsScriptClassDescription -> commandDescription -> IO Selector
selectorForCommand nsScriptClassDescription  commandDescription =
withObjCPtr commandDescription $ \raw_commandDescription ->
    fmap (Selector . castPtr) $ sendMsg nsScriptClassDescription (mkSelector "selectorForCommand:") (retPtr retVoid) [argPtr (castPtr raw_commandDescription :: Ptr ())]

-- | @- typeForKey:@
typeForKey :: (IsNSScriptClassDescription nsScriptClassDescription, IsNSString key) => nsScriptClassDescription -> key -> IO (Id NSString)
typeForKey nsScriptClassDescription  key =
withObjCPtr key $ \raw_key ->
    sendMsg nsScriptClassDescription (mkSelector "typeForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())] >>= retainedObject . castPtr

-- | @- classDescriptionForKey:@
classDescriptionForKey :: (IsNSScriptClassDescription nsScriptClassDescription, IsNSString key) => nsScriptClassDescription -> key -> IO (Id NSScriptClassDescription)
classDescriptionForKey nsScriptClassDescription  key =
withObjCPtr key $ \raw_key ->
    sendMsg nsScriptClassDescription (mkSelector "classDescriptionForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())] >>= retainedObject . castPtr

-- | @- appleEventCodeForKey:@
appleEventCodeForKey :: (IsNSScriptClassDescription nsScriptClassDescription, IsNSString key) => nsScriptClassDescription -> key -> IO CUInt
appleEventCodeForKey nsScriptClassDescription  key =
withObjCPtr key $ \raw_key ->
    sendMsg nsScriptClassDescription (mkSelector "appleEventCodeForKey:") retCUInt [argPtr (castPtr raw_key :: Ptr ())]

-- | @- keyWithAppleEventCode:@
keyWithAppleEventCode :: IsNSScriptClassDescription nsScriptClassDescription => nsScriptClassDescription -> CUInt -> IO (Id NSString)
keyWithAppleEventCode nsScriptClassDescription  appleEventCode =
  sendMsg nsScriptClassDescription (mkSelector "keyWithAppleEventCode:") (retPtr retVoid) [argCUInt (fromIntegral appleEventCode)] >>= retainedObject . castPtr

-- | @- isLocationRequiredToCreateForKey:@
isLocationRequiredToCreateForKey :: (IsNSScriptClassDescription nsScriptClassDescription, IsNSString toManyRelationshipKey) => nsScriptClassDescription -> toManyRelationshipKey -> IO Bool
isLocationRequiredToCreateForKey nsScriptClassDescription  toManyRelationshipKey =
withObjCPtr toManyRelationshipKey $ \raw_toManyRelationshipKey ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScriptClassDescription (mkSelector "isLocationRequiredToCreateForKey:") retCULong [argPtr (castPtr raw_toManyRelationshipKey :: Ptr ())]

-- | @- hasPropertyForKey:@
hasPropertyForKey :: (IsNSScriptClassDescription nsScriptClassDescription, IsNSString key) => nsScriptClassDescription -> key -> IO Bool
hasPropertyForKey nsScriptClassDescription  key =
withObjCPtr key $ \raw_key ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScriptClassDescription (mkSelector "hasPropertyForKey:") retCULong [argPtr (castPtr raw_key :: Ptr ())]

-- | @- hasOrderedToManyRelationshipForKey:@
hasOrderedToManyRelationshipForKey :: (IsNSScriptClassDescription nsScriptClassDescription, IsNSString key) => nsScriptClassDescription -> key -> IO Bool
hasOrderedToManyRelationshipForKey nsScriptClassDescription  key =
withObjCPtr key $ \raw_key ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScriptClassDescription (mkSelector "hasOrderedToManyRelationshipForKey:") retCULong [argPtr (castPtr raw_key :: Ptr ())]

-- | @- hasReadablePropertyForKey:@
hasReadablePropertyForKey :: (IsNSScriptClassDescription nsScriptClassDescription, IsNSString key) => nsScriptClassDescription -> key -> IO Bool
hasReadablePropertyForKey nsScriptClassDescription  key =
withObjCPtr key $ \raw_key ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScriptClassDescription (mkSelector "hasReadablePropertyForKey:") retCULong [argPtr (castPtr raw_key :: Ptr ())]

-- | @- hasWritablePropertyForKey:@
hasWritablePropertyForKey :: (IsNSScriptClassDescription nsScriptClassDescription, IsNSString key) => nsScriptClassDescription -> key -> IO Bool
hasWritablePropertyForKey nsScriptClassDescription  key =
withObjCPtr key $ \raw_key ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScriptClassDescription (mkSelector "hasWritablePropertyForKey:") retCULong [argPtr (castPtr raw_key :: Ptr ())]

-- | @- isReadOnlyKey:@
isReadOnlyKey :: (IsNSScriptClassDescription nsScriptClassDescription, IsNSString key) => nsScriptClassDescription -> key -> IO Bool
isReadOnlyKey nsScriptClassDescription  key =
withObjCPtr key $ \raw_key ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScriptClassDescription (mkSelector "isReadOnlyKey:") retCULong [argPtr (castPtr raw_key :: Ptr ())]

-- | @- suiteName@
suiteName :: IsNSScriptClassDescription nsScriptClassDescription => nsScriptClassDescription -> IO (Id NSString)
suiteName nsScriptClassDescription  =
  sendMsg nsScriptClassDescription (mkSelector "suiteName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- className@
className :: IsNSScriptClassDescription nsScriptClassDescription => nsScriptClassDescription -> IO (Id NSString)
className nsScriptClassDescription  =
  sendMsg nsScriptClassDescription (mkSelector "className") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- implementationClassName@
implementationClassName :: IsNSScriptClassDescription nsScriptClassDescription => nsScriptClassDescription -> IO (Id NSString)
implementationClassName nsScriptClassDescription  =
  sendMsg nsScriptClassDescription (mkSelector "implementationClassName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- superclassDescription@
superclassDescription :: IsNSScriptClassDescription nsScriptClassDescription => nsScriptClassDescription -> IO (Id NSScriptClassDescription)
superclassDescription nsScriptClassDescription  =
  sendMsg nsScriptClassDescription (mkSelector "superclassDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- appleEventCode@
appleEventCode :: IsNSScriptClassDescription nsScriptClassDescription => nsScriptClassDescription -> IO CUInt
appleEventCode nsScriptClassDescription  =
  sendMsg nsScriptClassDescription (mkSelector "appleEventCode") retCUInt []

-- | @- defaultSubcontainerAttributeKey@
defaultSubcontainerAttributeKey :: IsNSScriptClassDescription nsScriptClassDescription => nsScriptClassDescription -> IO (Id NSString)
defaultSubcontainerAttributeKey nsScriptClassDescription  =
  sendMsg nsScriptClassDescription (mkSelector "defaultSubcontainerAttributeKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @classDescriptionForClass:@
classDescriptionForClassSelector :: Selector
classDescriptionForClassSelector = mkSelector "classDescriptionForClass:"

-- | @Selector@ for @initWithSuiteName:className:dictionary:@
initWithSuiteName_className_dictionarySelector :: Selector
initWithSuiteName_className_dictionarySelector = mkSelector "initWithSuiteName:className:dictionary:"

-- | @Selector@ for @matchesAppleEventCode:@
matchesAppleEventCodeSelector :: Selector
matchesAppleEventCodeSelector = mkSelector "matchesAppleEventCode:"

-- | @Selector@ for @supportsCommand:@
supportsCommandSelector :: Selector
supportsCommandSelector = mkSelector "supportsCommand:"

-- | @Selector@ for @selectorForCommand:@
selectorForCommandSelector :: Selector
selectorForCommandSelector = mkSelector "selectorForCommand:"

-- | @Selector@ for @typeForKey:@
typeForKeySelector :: Selector
typeForKeySelector = mkSelector "typeForKey:"

-- | @Selector@ for @classDescriptionForKey:@
classDescriptionForKeySelector :: Selector
classDescriptionForKeySelector = mkSelector "classDescriptionForKey:"

-- | @Selector@ for @appleEventCodeForKey:@
appleEventCodeForKeySelector :: Selector
appleEventCodeForKeySelector = mkSelector "appleEventCodeForKey:"

-- | @Selector@ for @keyWithAppleEventCode:@
keyWithAppleEventCodeSelector :: Selector
keyWithAppleEventCodeSelector = mkSelector "keyWithAppleEventCode:"

-- | @Selector@ for @isLocationRequiredToCreateForKey:@
isLocationRequiredToCreateForKeySelector :: Selector
isLocationRequiredToCreateForKeySelector = mkSelector "isLocationRequiredToCreateForKey:"

-- | @Selector@ for @hasPropertyForKey:@
hasPropertyForKeySelector :: Selector
hasPropertyForKeySelector = mkSelector "hasPropertyForKey:"

-- | @Selector@ for @hasOrderedToManyRelationshipForKey:@
hasOrderedToManyRelationshipForKeySelector :: Selector
hasOrderedToManyRelationshipForKeySelector = mkSelector "hasOrderedToManyRelationshipForKey:"

-- | @Selector@ for @hasReadablePropertyForKey:@
hasReadablePropertyForKeySelector :: Selector
hasReadablePropertyForKeySelector = mkSelector "hasReadablePropertyForKey:"

-- | @Selector@ for @hasWritablePropertyForKey:@
hasWritablePropertyForKeySelector :: Selector
hasWritablePropertyForKeySelector = mkSelector "hasWritablePropertyForKey:"

-- | @Selector@ for @isReadOnlyKey:@
isReadOnlyKeySelector :: Selector
isReadOnlyKeySelector = mkSelector "isReadOnlyKey:"

-- | @Selector@ for @suiteName@
suiteNameSelector :: Selector
suiteNameSelector = mkSelector "suiteName"

-- | @Selector@ for @className@
classNameSelector :: Selector
classNameSelector = mkSelector "className"

-- | @Selector@ for @implementationClassName@
implementationClassNameSelector :: Selector
implementationClassNameSelector = mkSelector "implementationClassName"

-- | @Selector@ for @superclassDescription@
superclassDescriptionSelector :: Selector
superclassDescriptionSelector = mkSelector "superclassDescription"

-- | @Selector@ for @appleEventCode@
appleEventCodeSelector :: Selector
appleEventCodeSelector = mkSelector "appleEventCode"

-- | @Selector@ for @defaultSubcontainerAttributeKey@
defaultSubcontainerAttributeKeySelector :: Selector
defaultSubcontainerAttributeKeySelector = mkSelector "defaultSubcontainerAttributeKey"

