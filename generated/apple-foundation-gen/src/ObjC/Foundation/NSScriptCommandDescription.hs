{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSScriptCommandDescription@.
module ObjC.Foundation.NSScriptCommandDescription
  ( NSScriptCommandDescription
  , IsNSScriptCommandDescription(..)
  , init_
  , initWithSuiteName_commandName_dictionary
  , initWithCoder
  , typeForArgumentWithName
  , appleEventCodeForArgumentWithName
  , isOptionalArgumentWithName
  , createCommandInstance
  , createCommandInstanceWithZone
  , suiteName
  , commandName
  , appleEventClassCode
  , appleEventCode
  , commandClassName
  , returnType
  , appleEventCodeForReturnType
  , argumentNames
  , appleEventClassCodeSelector
  , appleEventCodeForArgumentWithNameSelector
  , appleEventCodeForReturnTypeSelector
  , appleEventCodeSelector
  , argumentNamesSelector
  , commandClassNameSelector
  , commandNameSelector
  , createCommandInstanceSelector
  , createCommandInstanceWithZoneSelector
  , initSelector
  , initWithCoderSelector
  , initWithSuiteName_commandName_dictionarySelector
  , isOptionalArgumentWithNameSelector
  , returnTypeSelector
  , suiteNameSelector
  , typeForArgumentWithNameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSScriptCommandDescription nsScriptCommandDescription => nsScriptCommandDescription -> IO RawId
init_ nsScriptCommandDescription =
  sendOwnedMessage nsScriptCommandDescription initSelector

-- | @- initWithSuiteName:commandName:dictionary:@
initWithSuiteName_commandName_dictionary :: (IsNSScriptCommandDescription nsScriptCommandDescription, IsNSString suiteName, IsNSString commandName, IsNSDictionary commandDeclaration) => nsScriptCommandDescription -> suiteName -> commandName -> commandDeclaration -> IO (Id NSScriptCommandDescription)
initWithSuiteName_commandName_dictionary nsScriptCommandDescription suiteName commandName commandDeclaration =
  sendOwnedMessage nsScriptCommandDescription initWithSuiteName_commandName_dictionarySelector (toNSString suiteName) (toNSString commandName) (toNSDictionary commandDeclaration)

-- | @- initWithCoder:@
initWithCoder :: (IsNSScriptCommandDescription nsScriptCommandDescription, IsNSCoder inCoder) => nsScriptCommandDescription -> inCoder -> IO (Id NSScriptCommandDescription)
initWithCoder nsScriptCommandDescription inCoder =
  sendOwnedMessage nsScriptCommandDescription initWithCoderSelector (toNSCoder inCoder)

-- | @- typeForArgumentWithName:@
typeForArgumentWithName :: (IsNSScriptCommandDescription nsScriptCommandDescription, IsNSString argumentName) => nsScriptCommandDescription -> argumentName -> IO (Id NSString)
typeForArgumentWithName nsScriptCommandDescription argumentName =
  sendMessage nsScriptCommandDescription typeForArgumentWithNameSelector (toNSString argumentName)

-- | @- appleEventCodeForArgumentWithName:@
appleEventCodeForArgumentWithName :: (IsNSScriptCommandDescription nsScriptCommandDescription, IsNSString argumentName) => nsScriptCommandDescription -> argumentName -> IO CUInt
appleEventCodeForArgumentWithName nsScriptCommandDescription argumentName =
  sendMessage nsScriptCommandDescription appleEventCodeForArgumentWithNameSelector (toNSString argumentName)

-- | @- isOptionalArgumentWithName:@
isOptionalArgumentWithName :: (IsNSScriptCommandDescription nsScriptCommandDescription, IsNSString argumentName) => nsScriptCommandDescription -> argumentName -> IO Bool
isOptionalArgumentWithName nsScriptCommandDescription argumentName =
  sendMessage nsScriptCommandDescription isOptionalArgumentWithNameSelector (toNSString argumentName)

-- | @- createCommandInstance@
createCommandInstance :: IsNSScriptCommandDescription nsScriptCommandDescription => nsScriptCommandDescription -> IO (Id NSScriptCommand)
createCommandInstance nsScriptCommandDescription =
  sendMessage nsScriptCommandDescription createCommandInstanceSelector

-- | @- createCommandInstanceWithZone:@
createCommandInstanceWithZone :: IsNSScriptCommandDescription nsScriptCommandDescription => nsScriptCommandDescription -> Ptr () -> IO (Id NSScriptCommand)
createCommandInstanceWithZone nsScriptCommandDescription zone =
  sendMessage nsScriptCommandDescription createCommandInstanceWithZoneSelector zone

-- | @- suiteName@
suiteName :: IsNSScriptCommandDescription nsScriptCommandDescription => nsScriptCommandDescription -> IO (Id NSString)
suiteName nsScriptCommandDescription =
  sendMessage nsScriptCommandDescription suiteNameSelector

-- | @- commandName@
commandName :: IsNSScriptCommandDescription nsScriptCommandDescription => nsScriptCommandDescription -> IO (Id NSString)
commandName nsScriptCommandDescription =
  sendMessage nsScriptCommandDescription commandNameSelector

-- | @- appleEventClassCode@
appleEventClassCode :: IsNSScriptCommandDescription nsScriptCommandDescription => nsScriptCommandDescription -> IO CUInt
appleEventClassCode nsScriptCommandDescription =
  sendMessage nsScriptCommandDescription appleEventClassCodeSelector

-- | @- appleEventCode@
appleEventCode :: IsNSScriptCommandDescription nsScriptCommandDescription => nsScriptCommandDescription -> IO CUInt
appleEventCode nsScriptCommandDescription =
  sendMessage nsScriptCommandDescription appleEventCodeSelector

-- | @- commandClassName@
commandClassName :: IsNSScriptCommandDescription nsScriptCommandDescription => nsScriptCommandDescription -> IO (Id NSString)
commandClassName nsScriptCommandDescription =
  sendMessage nsScriptCommandDescription commandClassNameSelector

-- | @- returnType@
returnType :: IsNSScriptCommandDescription nsScriptCommandDescription => nsScriptCommandDescription -> IO (Id NSString)
returnType nsScriptCommandDescription =
  sendMessage nsScriptCommandDescription returnTypeSelector

-- | @- appleEventCodeForReturnType@
appleEventCodeForReturnType :: IsNSScriptCommandDescription nsScriptCommandDescription => nsScriptCommandDescription -> IO CUInt
appleEventCodeForReturnType nsScriptCommandDescription =
  sendMessage nsScriptCommandDescription appleEventCodeForReturnTypeSelector

-- | @- argumentNames@
argumentNames :: IsNSScriptCommandDescription nsScriptCommandDescription => nsScriptCommandDescription -> IO (Id NSArray)
argumentNames nsScriptCommandDescription =
  sendMessage nsScriptCommandDescription argumentNamesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @initWithSuiteName:commandName:dictionary:@
initWithSuiteName_commandName_dictionarySelector :: Selector '[Id NSString, Id NSString, Id NSDictionary] (Id NSScriptCommandDescription)
initWithSuiteName_commandName_dictionarySelector = mkSelector "initWithSuiteName:commandName:dictionary:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSScriptCommandDescription)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @typeForArgumentWithName:@
typeForArgumentWithNameSelector :: Selector '[Id NSString] (Id NSString)
typeForArgumentWithNameSelector = mkSelector "typeForArgumentWithName:"

-- | @Selector@ for @appleEventCodeForArgumentWithName:@
appleEventCodeForArgumentWithNameSelector :: Selector '[Id NSString] CUInt
appleEventCodeForArgumentWithNameSelector = mkSelector "appleEventCodeForArgumentWithName:"

-- | @Selector@ for @isOptionalArgumentWithName:@
isOptionalArgumentWithNameSelector :: Selector '[Id NSString] Bool
isOptionalArgumentWithNameSelector = mkSelector "isOptionalArgumentWithName:"

-- | @Selector@ for @createCommandInstance@
createCommandInstanceSelector :: Selector '[] (Id NSScriptCommand)
createCommandInstanceSelector = mkSelector "createCommandInstance"

-- | @Selector@ for @createCommandInstanceWithZone:@
createCommandInstanceWithZoneSelector :: Selector '[Ptr ()] (Id NSScriptCommand)
createCommandInstanceWithZoneSelector = mkSelector "createCommandInstanceWithZone:"

-- | @Selector@ for @suiteName@
suiteNameSelector :: Selector '[] (Id NSString)
suiteNameSelector = mkSelector "suiteName"

-- | @Selector@ for @commandName@
commandNameSelector :: Selector '[] (Id NSString)
commandNameSelector = mkSelector "commandName"

-- | @Selector@ for @appleEventClassCode@
appleEventClassCodeSelector :: Selector '[] CUInt
appleEventClassCodeSelector = mkSelector "appleEventClassCode"

-- | @Selector@ for @appleEventCode@
appleEventCodeSelector :: Selector '[] CUInt
appleEventCodeSelector = mkSelector "appleEventCode"

-- | @Selector@ for @commandClassName@
commandClassNameSelector :: Selector '[] (Id NSString)
commandClassNameSelector = mkSelector "commandClassName"

-- | @Selector@ for @returnType@
returnTypeSelector :: Selector '[] (Id NSString)
returnTypeSelector = mkSelector "returnType"

-- | @Selector@ for @appleEventCodeForReturnType@
appleEventCodeForReturnTypeSelector :: Selector '[] CUInt
appleEventCodeForReturnTypeSelector = mkSelector "appleEventCodeForReturnType"

-- | @Selector@ for @argumentNames@
argumentNamesSelector :: Selector '[] (Id NSArray)
argumentNamesSelector = mkSelector "argumentNames"

