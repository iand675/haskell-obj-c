{-# LANGUAGE DataKinds #-}
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
  , aeteResourceSelector
  , appleEventCodeForSuiteSelector
  , bundleForSuiteSelector
  , classDescriptionWithAppleEventCodeSelector
  , classDescriptionsInSuiteSelector
  , commandDescriptionWithAppleEventClass_andAppleEventCodeSelector
  , commandDescriptionsInSuiteSelector
  , loadSuiteWithDictionary_fromBundleSelector
  , loadSuitesFromBundleSelector
  , registerClassDescriptionSelector
  , registerCommandDescriptionSelector
  , setSharedScriptSuiteRegistrySelector
  , sharedScriptSuiteRegistrySelector
  , suiteForAppleEventCodeSelector
  , suiteNamesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ sharedScriptSuiteRegistry@
sharedScriptSuiteRegistry :: IO (Id NSScriptSuiteRegistry)
sharedScriptSuiteRegistry  =
  do
    cls' <- getRequiredClass "NSScriptSuiteRegistry"
    sendClassMessage cls' sharedScriptSuiteRegistrySelector

-- | @+ setSharedScriptSuiteRegistry:@
setSharedScriptSuiteRegistry :: IsNSScriptSuiteRegistry registry => registry -> IO ()
setSharedScriptSuiteRegistry registry =
  do
    cls' <- getRequiredClass "NSScriptSuiteRegistry"
    sendClassMessage cls' setSharedScriptSuiteRegistrySelector (toNSScriptSuiteRegistry registry)

-- | @- loadSuitesFromBundle:@
loadSuitesFromBundle :: (IsNSScriptSuiteRegistry nsScriptSuiteRegistry, IsNSBundle bundle) => nsScriptSuiteRegistry -> bundle -> IO ()
loadSuitesFromBundle nsScriptSuiteRegistry bundle =
  sendMessage nsScriptSuiteRegistry loadSuitesFromBundleSelector (toNSBundle bundle)

-- | @- loadSuiteWithDictionary:fromBundle:@
loadSuiteWithDictionary_fromBundle :: (IsNSScriptSuiteRegistry nsScriptSuiteRegistry, IsNSDictionary suiteDeclaration, IsNSBundle bundle) => nsScriptSuiteRegistry -> suiteDeclaration -> bundle -> IO ()
loadSuiteWithDictionary_fromBundle nsScriptSuiteRegistry suiteDeclaration bundle =
  sendMessage nsScriptSuiteRegistry loadSuiteWithDictionary_fromBundleSelector (toNSDictionary suiteDeclaration) (toNSBundle bundle)

-- | @- registerClassDescription:@
registerClassDescription :: (IsNSScriptSuiteRegistry nsScriptSuiteRegistry, IsNSScriptClassDescription classDescription) => nsScriptSuiteRegistry -> classDescription -> IO ()
registerClassDescription nsScriptSuiteRegistry classDescription =
  sendMessage nsScriptSuiteRegistry registerClassDescriptionSelector (toNSScriptClassDescription classDescription)

-- | @- registerCommandDescription:@
registerCommandDescription :: (IsNSScriptSuiteRegistry nsScriptSuiteRegistry, IsNSScriptCommandDescription commandDescription) => nsScriptSuiteRegistry -> commandDescription -> IO ()
registerCommandDescription nsScriptSuiteRegistry commandDescription =
  sendMessage nsScriptSuiteRegistry registerCommandDescriptionSelector (toNSScriptCommandDescription commandDescription)

-- | @- appleEventCodeForSuite:@
appleEventCodeForSuite :: (IsNSScriptSuiteRegistry nsScriptSuiteRegistry, IsNSString suiteName) => nsScriptSuiteRegistry -> suiteName -> IO CUInt
appleEventCodeForSuite nsScriptSuiteRegistry suiteName =
  sendMessage nsScriptSuiteRegistry appleEventCodeForSuiteSelector (toNSString suiteName)

-- | @- bundleForSuite:@
bundleForSuite :: (IsNSScriptSuiteRegistry nsScriptSuiteRegistry, IsNSString suiteName) => nsScriptSuiteRegistry -> suiteName -> IO (Id NSBundle)
bundleForSuite nsScriptSuiteRegistry suiteName =
  sendMessage nsScriptSuiteRegistry bundleForSuiteSelector (toNSString suiteName)

-- | @- classDescriptionsInSuite:@
classDescriptionsInSuite :: (IsNSScriptSuiteRegistry nsScriptSuiteRegistry, IsNSString suiteName) => nsScriptSuiteRegistry -> suiteName -> IO (Id NSDictionary)
classDescriptionsInSuite nsScriptSuiteRegistry suiteName =
  sendMessage nsScriptSuiteRegistry classDescriptionsInSuiteSelector (toNSString suiteName)

-- | @- commandDescriptionsInSuite:@
commandDescriptionsInSuite :: (IsNSScriptSuiteRegistry nsScriptSuiteRegistry, IsNSString suiteName) => nsScriptSuiteRegistry -> suiteName -> IO (Id NSDictionary)
commandDescriptionsInSuite nsScriptSuiteRegistry suiteName =
  sendMessage nsScriptSuiteRegistry commandDescriptionsInSuiteSelector (toNSString suiteName)

-- | @- suiteForAppleEventCode:@
suiteForAppleEventCode :: IsNSScriptSuiteRegistry nsScriptSuiteRegistry => nsScriptSuiteRegistry -> CUInt -> IO (Id NSString)
suiteForAppleEventCode nsScriptSuiteRegistry appleEventCode =
  sendMessage nsScriptSuiteRegistry suiteForAppleEventCodeSelector appleEventCode

-- | @- classDescriptionWithAppleEventCode:@
classDescriptionWithAppleEventCode :: IsNSScriptSuiteRegistry nsScriptSuiteRegistry => nsScriptSuiteRegistry -> CUInt -> IO (Id NSScriptClassDescription)
classDescriptionWithAppleEventCode nsScriptSuiteRegistry appleEventCode =
  sendMessage nsScriptSuiteRegistry classDescriptionWithAppleEventCodeSelector appleEventCode

-- | @- commandDescriptionWithAppleEventClass:andAppleEventCode:@
commandDescriptionWithAppleEventClass_andAppleEventCode :: IsNSScriptSuiteRegistry nsScriptSuiteRegistry => nsScriptSuiteRegistry -> CUInt -> CUInt -> IO (Id NSScriptCommandDescription)
commandDescriptionWithAppleEventClass_andAppleEventCode nsScriptSuiteRegistry appleEventClassCode appleEventIDCode =
  sendMessage nsScriptSuiteRegistry commandDescriptionWithAppleEventClass_andAppleEventCodeSelector appleEventClassCode appleEventIDCode

-- | @- aeteResource:@
aeteResource :: (IsNSScriptSuiteRegistry nsScriptSuiteRegistry, IsNSString languageName) => nsScriptSuiteRegistry -> languageName -> IO (Id NSData)
aeteResource nsScriptSuiteRegistry languageName =
  sendMessage nsScriptSuiteRegistry aeteResourceSelector (toNSString languageName)

-- | @- suiteNames@
suiteNames :: IsNSScriptSuiteRegistry nsScriptSuiteRegistry => nsScriptSuiteRegistry -> IO (Id NSArray)
suiteNames nsScriptSuiteRegistry =
  sendMessage nsScriptSuiteRegistry suiteNamesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedScriptSuiteRegistry@
sharedScriptSuiteRegistrySelector :: Selector '[] (Id NSScriptSuiteRegistry)
sharedScriptSuiteRegistrySelector = mkSelector "sharedScriptSuiteRegistry"

-- | @Selector@ for @setSharedScriptSuiteRegistry:@
setSharedScriptSuiteRegistrySelector :: Selector '[Id NSScriptSuiteRegistry] ()
setSharedScriptSuiteRegistrySelector = mkSelector "setSharedScriptSuiteRegistry:"

-- | @Selector@ for @loadSuitesFromBundle:@
loadSuitesFromBundleSelector :: Selector '[Id NSBundle] ()
loadSuitesFromBundleSelector = mkSelector "loadSuitesFromBundle:"

-- | @Selector@ for @loadSuiteWithDictionary:fromBundle:@
loadSuiteWithDictionary_fromBundleSelector :: Selector '[Id NSDictionary, Id NSBundle] ()
loadSuiteWithDictionary_fromBundleSelector = mkSelector "loadSuiteWithDictionary:fromBundle:"

-- | @Selector@ for @registerClassDescription:@
registerClassDescriptionSelector :: Selector '[Id NSScriptClassDescription] ()
registerClassDescriptionSelector = mkSelector "registerClassDescription:"

-- | @Selector@ for @registerCommandDescription:@
registerCommandDescriptionSelector :: Selector '[Id NSScriptCommandDescription] ()
registerCommandDescriptionSelector = mkSelector "registerCommandDescription:"

-- | @Selector@ for @appleEventCodeForSuite:@
appleEventCodeForSuiteSelector :: Selector '[Id NSString] CUInt
appleEventCodeForSuiteSelector = mkSelector "appleEventCodeForSuite:"

-- | @Selector@ for @bundleForSuite:@
bundleForSuiteSelector :: Selector '[Id NSString] (Id NSBundle)
bundleForSuiteSelector = mkSelector "bundleForSuite:"

-- | @Selector@ for @classDescriptionsInSuite:@
classDescriptionsInSuiteSelector :: Selector '[Id NSString] (Id NSDictionary)
classDescriptionsInSuiteSelector = mkSelector "classDescriptionsInSuite:"

-- | @Selector@ for @commandDescriptionsInSuite:@
commandDescriptionsInSuiteSelector :: Selector '[Id NSString] (Id NSDictionary)
commandDescriptionsInSuiteSelector = mkSelector "commandDescriptionsInSuite:"

-- | @Selector@ for @suiteForAppleEventCode:@
suiteForAppleEventCodeSelector :: Selector '[CUInt] (Id NSString)
suiteForAppleEventCodeSelector = mkSelector "suiteForAppleEventCode:"

-- | @Selector@ for @classDescriptionWithAppleEventCode:@
classDescriptionWithAppleEventCodeSelector :: Selector '[CUInt] (Id NSScriptClassDescription)
classDescriptionWithAppleEventCodeSelector = mkSelector "classDescriptionWithAppleEventCode:"

-- | @Selector@ for @commandDescriptionWithAppleEventClass:andAppleEventCode:@
commandDescriptionWithAppleEventClass_andAppleEventCodeSelector :: Selector '[CUInt, CUInt] (Id NSScriptCommandDescription)
commandDescriptionWithAppleEventClass_andAppleEventCodeSelector = mkSelector "commandDescriptionWithAppleEventClass:andAppleEventCode:"

-- | @Selector@ for @aeteResource:@
aeteResourceSelector :: Selector '[Id NSString] (Id NSData)
aeteResourceSelector = mkSelector "aeteResource:"

-- | @Selector@ for @suiteNames@
suiteNamesSelector :: Selector '[] (Id NSArray)
suiteNamesSelector = mkSelector "suiteNames"

