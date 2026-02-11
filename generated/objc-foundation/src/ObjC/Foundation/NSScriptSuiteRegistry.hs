{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSScriptSuiteRegistry@.
module ObjC.Foundation.NSScriptSuiteRegistry
  ( NSScriptSuiteRegistry
  , IsNSScriptSuiteRegistry(..)
  , sharedScriptSuiteRegistry
  , setSharedScriptSuiteRegistry
  , loadSuitesFromBundle
  , loadSuiteWithDictionary_fromBundle
  , registerClassDescription
  , registerCommandDescription
  , appleEventCodeForSuite
  , bundleForSuite
  , classDescriptionsInSuite
  , commandDescriptionsInSuite
  , suiteForAppleEventCode
  , classDescriptionWithAppleEventCode
  , commandDescriptionWithAppleEventClass_andAppleEventCode
  , aeteResource
  , suiteNames
  , sharedScriptSuiteRegistrySelector
  , setSharedScriptSuiteRegistrySelector
  , loadSuitesFromBundleSelector
  , loadSuiteWithDictionary_fromBundleSelector
  , registerClassDescriptionSelector
  , registerCommandDescriptionSelector
  , appleEventCodeForSuiteSelector
  , bundleForSuiteSelector
  , classDescriptionsInSuiteSelector
  , commandDescriptionsInSuiteSelector
  , suiteForAppleEventCodeSelector
  , classDescriptionWithAppleEventCodeSelector
  , commandDescriptionWithAppleEventClass_andAppleEventCodeSelector
  , aeteResourceSelector
  , suiteNamesSelector


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

-- | @+ sharedScriptSuiteRegistry@
sharedScriptSuiteRegistry :: IO (Id NSScriptSuiteRegistry)
sharedScriptSuiteRegistry  =
  do
    cls' <- getRequiredClass "NSScriptSuiteRegistry"
    sendClassMsg cls' (mkSelector "sharedScriptSuiteRegistry") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ setSharedScriptSuiteRegistry:@
setSharedScriptSuiteRegistry :: IsNSScriptSuiteRegistry registry => registry -> IO ()
setSharedScriptSuiteRegistry registry =
  do
    cls' <- getRequiredClass "NSScriptSuiteRegistry"
    withObjCPtr registry $ \raw_registry ->
      sendClassMsg cls' (mkSelector "setSharedScriptSuiteRegistry:") retVoid [argPtr (castPtr raw_registry :: Ptr ())]

-- | @- loadSuitesFromBundle:@
loadSuitesFromBundle :: (IsNSScriptSuiteRegistry nsScriptSuiteRegistry, IsNSBundle bundle) => nsScriptSuiteRegistry -> bundle -> IO ()
loadSuitesFromBundle nsScriptSuiteRegistry  bundle =
withObjCPtr bundle $ \raw_bundle ->
    sendMsg nsScriptSuiteRegistry (mkSelector "loadSuitesFromBundle:") retVoid [argPtr (castPtr raw_bundle :: Ptr ())]

-- | @- loadSuiteWithDictionary:fromBundle:@
loadSuiteWithDictionary_fromBundle :: (IsNSScriptSuiteRegistry nsScriptSuiteRegistry, IsNSDictionary suiteDeclaration, IsNSBundle bundle) => nsScriptSuiteRegistry -> suiteDeclaration -> bundle -> IO ()
loadSuiteWithDictionary_fromBundle nsScriptSuiteRegistry  suiteDeclaration bundle =
withObjCPtr suiteDeclaration $ \raw_suiteDeclaration ->
  withObjCPtr bundle $ \raw_bundle ->
      sendMsg nsScriptSuiteRegistry (mkSelector "loadSuiteWithDictionary:fromBundle:") retVoid [argPtr (castPtr raw_suiteDeclaration :: Ptr ()), argPtr (castPtr raw_bundle :: Ptr ())]

-- | @- registerClassDescription:@
registerClassDescription :: (IsNSScriptSuiteRegistry nsScriptSuiteRegistry, IsNSScriptClassDescription classDescription) => nsScriptSuiteRegistry -> classDescription -> IO ()
registerClassDescription nsScriptSuiteRegistry  classDescription =
withObjCPtr classDescription $ \raw_classDescription ->
    sendMsg nsScriptSuiteRegistry (mkSelector "registerClassDescription:") retVoid [argPtr (castPtr raw_classDescription :: Ptr ())]

-- | @- registerCommandDescription:@
registerCommandDescription :: (IsNSScriptSuiteRegistry nsScriptSuiteRegistry, IsNSScriptCommandDescription commandDescription) => nsScriptSuiteRegistry -> commandDescription -> IO ()
registerCommandDescription nsScriptSuiteRegistry  commandDescription =
withObjCPtr commandDescription $ \raw_commandDescription ->
    sendMsg nsScriptSuiteRegistry (mkSelector "registerCommandDescription:") retVoid [argPtr (castPtr raw_commandDescription :: Ptr ())]

-- | @- appleEventCodeForSuite:@
appleEventCodeForSuite :: (IsNSScriptSuiteRegistry nsScriptSuiteRegistry, IsNSString suiteName) => nsScriptSuiteRegistry -> suiteName -> IO CUInt
appleEventCodeForSuite nsScriptSuiteRegistry  suiteName =
withObjCPtr suiteName $ \raw_suiteName ->
    sendMsg nsScriptSuiteRegistry (mkSelector "appleEventCodeForSuite:") retCUInt [argPtr (castPtr raw_suiteName :: Ptr ())]

-- | @- bundleForSuite:@
bundleForSuite :: (IsNSScriptSuiteRegistry nsScriptSuiteRegistry, IsNSString suiteName) => nsScriptSuiteRegistry -> suiteName -> IO (Id NSBundle)
bundleForSuite nsScriptSuiteRegistry  suiteName =
withObjCPtr suiteName $ \raw_suiteName ->
    sendMsg nsScriptSuiteRegistry (mkSelector "bundleForSuite:") (retPtr retVoid) [argPtr (castPtr raw_suiteName :: Ptr ())] >>= retainedObject . castPtr

-- | @- classDescriptionsInSuite:@
classDescriptionsInSuite :: (IsNSScriptSuiteRegistry nsScriptSuiteRegistry, IsNSString suiteName) => nsScriptSuiteRegistry -> suiteName -> IO (Id NSDictionary)
classDescriptionsInSuite nsScriptSuiteRegistry  suiteName =
withObjCPtr suiteName $ \raw_suiteName ->
    sendMsg nsScriptSuiteRegistry (mkSelector "classDescriptionsInSuite:") (retPtr retVoid) [argPtr (castPtr raw_suiteName :: Ptr ())] >>= retainedObject . castPtr

-- | @- commandDescriptionsInSuite:@
commandDescriptionsInSuite :: (IsNSScriptSuiteRegistry nsScriptSuiteRegistry, IsNSString suiteName) => nsScriptSuiteRegistry -> suiteName -> IO (Id NSDictionary)
commandDescriptionsInSuite nsScriptSuiteRegistry  suiteName =
withObjCPtr suiteName $ \raw_suiteName ->
    sendMsg nsScriptSuiteRegistry (mkSelector "commandDescriptionsInSuite:") (retPtr retVoid) [argPtr (castPtr raw_suiteName :: Ptr ())] >>= retainedObject . castPtr

-- | @- suiteForAppleEventCode:@
suiteForAppleEventCode :: IsNSScriptSuiteRegistry nsScriptSuiteRegistry => nsScriptSuiteRegistry -> CUInt -> IO (Id NSString)
suiteForAppleEventCode nsScriptSuiteRegistry  appleEventCode =
  sendMsg nsScriptSuiteRegistry (mkSelector "suiteForAppleEventCode:") (retPtr retVoid) [argCUInt (fromIntegral appleEventCode)] >>= retainedObject . castPtr

-- | @- classDescriptionWithAppleEventCode:@
classDescriptionWithAppleEventCode :: IsNSScriptSuiteRegistry nsScriptSuiteRegistry => nsScriptSuiteRegistry -> CUInt -> IO (Id NSScriptClassDescription)
classDescriptionWithAppleEventCode nsScriptSuiteRegistry  appleEventCode =
  sendMsg nsScriptSuiteRegistry (mkSelector "classDescriptionWithAppleEventCode:") (retPtr retVoid) [argCUInt (fromIntegral appleEventCode)] >>= retainedObject . castPtr

-- | @- commandDescriptionWithAppleEventClass:andAppleEventCode:@
commandDescriptionWithAppleEventClass_andAppleEventCode :: IsNSScriptSuiteRegistry nsScriptSuiteRegistry => nsScriptSuiteRegistry -> CUInt -> CUInt -> IO (Id NSScriptCommandDescription)
commandDescriptionWithAppleEventClass_andAppleEventCode nsScriptSuiteRegistry  appleEventClassCode appleEventIDCode =
  sendMsg nsScriptSuiteRegistry (mkSelector "commandDescriptionWithAppleEventClass:andAppleEventCode:") (retPtr retVoid) [argCUInt (fromIntegral appleEventClassCode), argCUInt (fromIntegral appleEventIDCode)] >>= retainedObject . castPtr

-- | @- aeteResource:@
aeteResource :: (IsNSScriptSuiteRegistry nsScriptSuiteRegistry, IsNSString languageName) => nsScriptSuiteRegistry -> languageName -> IO (Id NSData)
aeteResource nsScriptSuiteRegistry  languageName =
withObjCPtr languageName $ \raw_languageName ->
    sendMsg nsScriptSuiteRegistry (mkSelector "aeteResource:") (retPtr retVoid) [argPtr (castPtr raw_languageName :: Ptr ())] >>= retainedObject . castPtr

-- | @- suiteNames@
suiteNames :: IsNSScriptSuiteRegistry nsScriptSuiteRegistry => nsScriptSuiteRegistry -> IO (Id NSArray)
suiteNames nsScriptSuiteRegistry  =
  sendMsg nsScriptSuiteRegistry (mkSelector "suiteNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedScriptSuiteRegistry@
sharedScriptSuiteRegistrySelector :: Selector
sharedScriptSuiteRegistrySelector = mkSelector "sharedScriptSuiteRegistry"

-- | @Selector@ for @setSharedScriptSuiteRegistry:@
setSharedScriptSuiteRegistrySelector :: Selector
setSharedScriptSuiteRegistrySelector = mkSelector "setSharedScriptSuiteRegistry:"

-- | @Selector@ for @loadSuitesFromBundle:@
loadSuitesFromBundleSelector :: Selector
loadSuitesFromBundleSelector = mkSelector "loadSuitesFromBundle:"

-- | @Selector@ for @loadSuiteWithDictionary:fromBundle:@
loadSuiteWithDictionary_fromBundleSelector :: Selector
loadSuiteWithDictionary_fromBundleSelector = mkSelector "loadSuiteWithDictionary:fromBundle:"

-- | @Selector@ for @registerClassDescription:@
registerClassDescriptionSelector :: Selector
registerClassDescriptionSelector = mkSelector "registerClassDescription:"

-- | @Selector@ for @registerCommandDescription:@
registerCommandDescriptionSelector :: Selector
registerCommandDescriptionSelector = mkSelector "registerCommandDescription:"

-- | @Selector@ for @appleEventCodeForSuite:@
appleEventCodeForSuiteSelector :: Selector
appleEventCodeForSuiteSelector = mkSelector "appleEventCodeForSuite:"

-- | @Selector@ for @bundleForSuite:@
bundleForSuiteSelector :: Selector
bundleForSuiteSelector = mkSelector "bundleForSuite:"

-- | @Selector@ for @classDescriptionsInSuite:@
classDescriptionsInSuiteSelector :: Selector
classDescriptionsInSuiteSelector = mkSelector "classDescriptionsInSuite:"

-- | @Selector@ for @commandDescriptionsInSuite:@
commandDescriptionsInSuiteSelector :: Selector
commandDescriptionsInSuiteSelector = mkSelector "commandDescriptionsInSuite:"

-- | @Selector@ for @suiteForAppleEventCode:@
suiteForAppleEventCodeSelector :: Selector
suiteForAppleEventCodeSelector = mkSelector "suiteForAppleEventCode:"

-- | @Selector@ for @classDescriptionWithAppleEventCode:@
classDescriptionWithAppleEventCodeSelector :: Selector
classDescriptionWithAppleEventCodeSelector = mkSelector "classDescriptionWithAppleEventCode:"

-- | @Selector@ for @commandDescriptionWithAppleEventClass:andAppleEventCode:@
commandDescriptionWithAppleEventClass_andAppleEventCodeSelector :: Selector
commandDescriptionWithAppleEventClass_andAppleEventCodeSelector = mkSelector "commandDescriptionWithAppleEventClass:andAppleEventCode:"

-- | @Selector@ for @aeteResource:@
aeteResourceSelector :: Selector
aeteResourceSelector = mkSelector "aeteResource:"

-- | @Selector@ for @suiteNames@
suiteNamesSelector :: Selector
suiteNamesSelector = mkSelector "suiteNames"

