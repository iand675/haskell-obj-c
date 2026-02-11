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
  , initSelector
  , initWithSuiteName_commandName_dictionarySelector
  , initWithCoderSelector
  , typeForArgumentWithNameSelector
  , appleEventCodeForArgumentWithNameSelector
  , isOptionalArgumentWithNameSelector
  , createCommandInstanceSelector
  , createCommandInstanceWithZoneSelector
  , suiteNameSelector
  , commandNameSelector
  , appleEventClassCodeSelector
  , appleEventCodeSelector
  , commandClassNameSelector
  , returnTypeSelector
  , appleEventCodeForReturnTypeSelector
  , argumentNamesSelector


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

-- | @- init@
init_ :: IsNSScriptCommandDescription nsScriptCommandDescription => nsScriptCommandDescription -> IO RawId
init_ nsScriptCommandDescription  =
  fmap (RawId . castPtr) $ sendMsg nsScriptCommandDescription (mkSelector "init") (retPtr retVoid) []

-- | @- initWithSuiteName:commandName:dictionary:@
initWithSuiteName_commandName_dictionary :: (IsNSScriptCommandDescription nsScriptCommandDescription, IsNSString suiteName, IsNSString commandName, IsNSDictionary commandDeclaration) => nsScriptCommandDescription -> suiteName -> commandName -> commandDeclaration -> IO (Id NSScriptCommandDescription)
initWithSuiteName_commandName_dictionary nsScriptCommandDescription  suiteName commandName commandDeclaration =
withObjCPtr suiteName $ \raw_suiteName ->
  withObjCPtr commandName $ \raw_commandName ->
    withObjCPtr commandDeclaration $ \raw_commandDeclaration ->
        sendMsg nsScriptCommandDescription (mkSelector "initWithSuiteName:commandName:dictionary:") (retPtr retVoid) [argPtr (castPtr raw_suiteName :: Ptr ()), argPtr (castPtr raw_commandName :: Ptr ()), argPtr (castPtr raw_commandDeclaration :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSScriptCommandDescription nsScriptCommandDescription, IsNSCoder inCoder) => nsScriptCommandDescription -> inCoder -> IO (Id NSScriptCommandDescription)
initWithCoder nsScriptCommandDescription  inCoder =
withObjCPtr inCoder $ \raw_inCoder ->
    sendMsg nsScriptCommandDescription (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_inCoder :: Ptr ())] >>= ownedObject . castPtr

-- | @- typeForArgumentWithName:@
typeForArgumentWithName :: (IsNSScriptCommandDescription nsScriptCommandDescription, IsNSString argumentName) => nsScriptCommandDescription -> argumentName -> IO (Id NSString)
typeForArgumentWithName nsScriptCommandDescription  argumentName =
withObjCPtr argumentName $ \raw_argumentName ->
    sendMsg nsScriptCommandDescription (mkSelector "typeForArgumentWithName:") (retPtr retVoid) [argPtr (castPtr raw_argumentName :: Ptr ())] >>= retainedObject . castPtr

-- | @- appleEventCodeForArgumentWithName:@
appleEventCodeForArgumentWithName :: (IsNSScriptCommandDescription nsScriptCommandDescription, IsNSString argumentName) => nsScriptCommandDescription -> argumentName -> IO CUInt
appleEventCodeForArgumentWithName nsScriptCommandDescription  argumentName =
withObjCPtr argumentName $ \raw_argumentName ->
    sendMsg nsScriptCommandDescription (mkSelector "appleEventCodeForArgumentWithName:") retCUInt [argPtr (castPtr raw_argumentName :: Ptr ())]

-- | @- isOptionalArgumentWithName:@
isOptionalArgumentWithName :: (IsNSScriptCommandDescription nsScriptCommandDescription, IsNSString argumentName) => nsScriptCommandDescription -> argumentName -> IO Bool
isOptionalArgumentWithName nsScriptCommandDescription  argumentName =
withObjCPtr argumentName $ \raw_argumentName ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScriptCommandDescription (mkSelector "isOptionalArgumentWithName:") retCULong [argPtr (castPtr raw_argumentName :: Ptr ())]

-- | @- createCommandInstance@
createCommandInstance :: IsNSScriptCommandDescription nsScriptCommandDescription => nsScriptCommandDescription -> IO (Id NSScriptCommand)
createCommandInstance nsScriptCommandDescription  =
  sendMsg nsScriptCommandDescription (mkSelector "createCommandInstance") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- createCommandInstanceWithZone:@
createCommandInstanceWithZone :: IsNSScriptCommandDescription nsScriptCommandDescription => nsScriptCommandDescription -> Ptr () -> IO (Id NSScriptCommand)
createCommandInstanceWithZone nsScriptCommandDescription  zone =
  sendMsg nsScriptCommandDescription (mkSelector "createCommandInstanceWithZone:") (retPtr retVoid) [argPtr zone] >>= retainedObject . castPtr

-- | @- suiteName@
suiteName :: IsNSScriptCommandDescription nsScriptCommandDescription => nsScriptCommandDescription -> IO (Id NSString)
suiteName nsScriptCommandDescription  =
  sendMsg nsScriptCommandDescription (mkSelector "suiteName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- commandName@
commandName :: IsNSScriptCommandDescription nsScriptCommandDescription => nsScriptCommandDescription -> IO (Id NSString)
commandName nsScriptCommandDescription  =
  sendMsg nsScriptCommandDescription (mkSelector "commandName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- appleEventClassCode@
appleEventClassCode :: IsNSScriptCommandDescription nsScriptCommandDescription => nsScriptCommandDescription -> IO CUInt
appleEventClassCode nsScriptCommandDescription  =
  sendMsg nsScriptCommandDescription (mkSelector "appleEventClassCode") retCUInt []

-- | @- appleEventCode@
appleEventCode :: IsNSScriptCommandDescription nsScriptCommandDescription => nsScriptCommandDescription -> IO CUInt
appleEventCode nsScriptCommandDescription  =
  sendMsg nsScriptCommandDescription (mkSelector "appleEventCode") retCUInt []

-- | @- commandClassName@
commandClassName :: IsNSScriptCommandDescription nsScriptCommandDescription => nsScriptCommandDescription -> IO (Id NSString)
commandClassName nsScriptCommandDescription  =
  sendMsg nsScriptCommandDescription (mkSelector "commandClassName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- returnType@
returnType :: IsNSScriptCommandDescription nsScriptCommandDescription => nsScriptCommandDescription -> IO (Id NSString)
returnType nsScriptCommandDescription  =
  sendMsg nsScriptCommandDescription (mkSelector "returnType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- appleEventCodeForReturnType@
appleEventCodeForReturnType :: IsNSScriptCommandDescription nsScriptCommandDescription => nsScriptCommandDescription -> IO CUInt
appleEventCodeForReturnType nsScriptCommandDescription  =
  sendMsg nsScriptCommandDescription (mkSelector "appleEventCodeForReturnType") retCUInt []

-- | @- argumentNames@
argumentNames :: IsNSScriptCommandDescription nsScriptCommandDescription => nsScriptCommandDescription -> IO (Id NSArray)
argumentNames nsScriptCommandDescription  =
  sendMsg nsScriptCommandDescription (mkSelector "argumentNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithSuiteName:commandName:dictionary:@
initWithSuiteName_commandName_dictionarySelector :: Selector
initWithSuiteName_commandName_dictionarySelector = mkSelector "initWithSuiteName:commandName:dictionary:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @typeForArgumentWithName:@
typeForArgumentWithNameSelector :: Selector
typeForArgumentWithNameSelector = mkSelector "typeForArgumentWithName:"

-- | @Selector@ for @appleEventCodeForArgumentWithName:@
appleEventCodeForArgumentWithNameSelector :: Selector
appleEventCodeForArgumentWithNameSelector = mkSelector "appleEventCodeForArgumentWithName:"

-- | @Selector@ for @isOptionalArgumentWithName:@
isOptionalArgumentWithNameSelector :: Selector
isOptionalArgumentWithNameSelector = mkSelector "isOptionalArgumentWithName:"

-- | @Selector@ for @createCommandInstance@
createCommandInstanceSelector :: Selector
createCommandInstanceSelector = mkSelector "createCommandInstance"

-- | @Selector@ for @createCommandInstanceWithZone:@
createCommandInstanceWithZoneSelector :: Selector
createCommandInstanceWithZoneSelector = mkSelector "createCommandInstanceWithZone:"

-- | @Selector@ for @suiteName@
suiteNameSelector :: Selector
suiteNameSelector = mkSelector "suiteName"

-- | @Selector@ for @commandName@
commandNameSelector :: Selector
commandNameSelector = mkSelector "commandName"

-- | @Selector@ for @appleEventClassCode@
appleEventClassCodeSelector :: Selector
appleEventClassCodeSelector = mkSelector "appleEventClassCode"

-- | @Selector@ for @appleEventCode@
appleEventCodeSelector :: Selector
appleEventCodeSelector = mkSelector "appleEventCode"

-- | @Selector@ for @commandClassName@
commandClassNameSelector :: Selector
commandClassNameSelector = mkSelector "commandClassName"

-- | @Selector@ for @returnType@
returnTypeSelector :: Selector
returnTypeSelector = mkSelector "returnType"

-- | @Selector@ for @appleEventCodeForReturnType@
appleEventCodeForReturnTypeSelector :: Selector
appleEventCodeForReturnTypeSelector = mkSelector "appleEventCodeForReturnType"

-- | @Selector@ for @argumentNames@
argumentNamesSelector :: Selector
argumentNamesSelector = mkSelector "argumentNames"

