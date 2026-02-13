{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSBundle@.
module ObjC.Foundation.NSBundle
  ( NSBundle
  , IsNSBundle(..)
  , bundleWithPath
  , initWithPath
  , bundleWithURL
  , initWithURL
  , bundleForClass
  , bundleWithIdentifier
  , load
  , unload
  , preflightAndReturnError
  , loadAndReturnError
  , urlForAuxiliaryExecutable
  , pathForAuxiliaryExecutable
  , urlForResource_withExtension_subdirectory_inBundleWithURL
  , urLsForResourcesWithExtension_subdirectory_inBundleWithURL
  , urlForResource_withExtension
  , urlForResource_withExtension_subdirectory
  , urlForResource_withExtension_subdirectory_localization
  , urLsForResourcesWithExtension_subdirectory
  , urLsForResourcesWithExtension_subdirectory_localization
  , nsBundlePathForResource_ofType_inDirectory
  , nsBundlePathsForResourcesOfType_inDirectory
  , pathForResource_ofType
  , pathForResource_ofType_inDirectory
  , pathForResource_ofType_inDirectory_forLocalization
  , pathsForResourcesOfType_inDirectory
  , pathsForResourcesOfType_inDirectory_forLocalization
  , localizedStringForKey_value_table
  , localizedAttributedStringForKey_value_table
  , localizedStringForKey_value_table_localizations
  , objectForInfoDictionaryKey
  , classNamed
  , preferredLocalizationsFromArray
  , preferredLocalizationsFromArray_forPreferences
  , setPreservationPriority_forTags
  , preservationPriorityForTag
  , mainBundle
  , allBundles
  , allFrameworks
  , loaded
  , bundleURL
  , resourceURL
  , executableURL
  , privateFrameworksURL
  , sharedFrameworksURL
  , sharedSupportURL
  , builtInPlugInsURL
  , appStoreReceiptURL
  , bundlePath
  , resourcePath
  , executablePath
  , privateFrameworksPath
  , sharedFrameworksPath
  , sharedSupportPath
  , builtInPlugInsPath
  , bundleIdentifier
  , infoDictionary
  , localizedInfoDictionary
  , principalClass
  , preferredLocalizations
  , localizations
  , developmentLocalization
  , executableArchitectures
  , allBundlesSelector
  , allFrameworksSelector
  , appStoreReceiptURLSelector
  , builtInPlugInsPathSelector
  , builtInPlugInsURLSelector
  , bundleForClassSelector
  , bundleIdentifierSelector
  , bundlePathSelector
  , bundleURLSelector
  , bundleWithIdentifierSelector
  , bundleWithPathSelector
  , bundleWithURLSelector
  , classNamedSelector
  , developmentLocalizationSelector
  , executableArchitecturesSelector
  , executablePathSelector
  , executableURLSelector
  , infoDictionarySelector
  , initWithPathSelector
  , initWithURLSelector
  , loadAndReturnErrorSelector
  , loadSelector
  , loadedSelector
  , localizationsSelector
  , localizedAttributedStringForKey_value_tableSelector
  , localizedInfoDictionarySelector
  , localizedStringForKey_value_tableSelector
  , localizedStringForKey_value_table_localizationsSelector
  , mainBundleSelector
  , nsBundlePathForResource_ofType_inDirectorySelector
  , nsBundlePathsForResourcesOfType_inDirectorySelector
  , objectForInfoDictionaryKeySelector
  , pathForAuxiliaryExecutableSelector
  , pathForResource_ofTypeSelector
  , pathForResource_ofType_inDirectorySelector
  , pathForResource_ofType_inDirectory_forLocalizationSelector
  , pathsForResourcesOfType_inDirectorySelector
  , pathsForResourcesOfType_inDirectory_forLocalizationSelector
  , preferredLocalizationsFromArraySelector
  , preferredLocalizationsFromArray_forPreferencesSelector
  , preferredLocalizationsSelector
  , preflightAndReturnErrorSelector
  , preservationPriorityForTagSelector
  , principalClassSelector
  , privateFrameworksPathSelector
  , privateFrameworksURLSelector
  , resourcePathSelector
  , resourceURLSelector
  , setPreservationPriority_forTagsSelector
  , sharedFrameworksPathSelector
  , sharedFrameworksURLSelector
  , sharedSupportPathSelector
  , sharedSupportURLSelector
  , unloadSelector
  , urLsForResourcesWithExtension_subdirectorySelector
  , urLsForResourcesWithExtension_subdirectory_inBundleWithURLSelector
  , urLsForResourcesWithExtension_subdirectory_localizationSelector
  , urlForAuxiliaryExecutableSelector
  , urlForResource_withExtensionSelector
  , urlForResource_withExtension_subdirectorySelector
  , urlForResource_withExtension_subdirectory_inBundleWithURLSelector
  , urlForResource_withExtension_subdirectory_localizationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ bundleWithPath:@
bundleWithPath :: IsNSString path => path -> IO (Id NSBundle)
bundleWithPath path =
  do
    cls' <- getRequiredClass "NSBundle"
    sendClassMessage cls' bundleWithPathSelector (toNSString path)

-- | @- initWithPath:@
initWithPath :: (IsNSBundle nsBundle, IsNSString path) => nsBundle -> path -> IO (Id NSBundle)
initWithPath nsBundle path =
  sendOwnedMessage nsBundle initWithPathSelector (toNSString path)

-- | @+ bundleWithURL:@
bundleWithURL :: IsNSURL url => url -> IO (Id NSBundle)
bundleWithURL url =
  do
    cls' <- getRequiredClass "NSBundle"
    sendClassMessage cls' bundleWithURLSelector (toNSURL url)

-- | @- initWithURL:@
initWithURL :: (IsNSBundle nsBundle, IsNSURL url) => nsBundle -> url -> IO (Id NSBundle)
initWithURL nsBundle url =
  sendOwnedMessage nsBundle initWithURLSelector (toNSURL url)

-- | @+ bundleForClass:@
bundleForClass :: Class -> IO (Id NSBundle)
bundleForClass aClass =
  do
    cls' <- getRequiredClass "NSBundle"
    sendClassMessage cls' bundleForClassSelector aClass

-- | @+ bundleWithIdentifier:@
bundleWithIdentifier :: IsNSString identifier => identifier -> IO (Id NSBundle)
bundleWithIdentifier identifier =
  do
    cls' <- getRequiredClass "NSBundle"
    sendClassMessage cls' bundleWithIdentifierSelector (toNSString identifier)

-- | @- load@
load :: IsNSBundle nsBundle => nsBundle -> IO Bool
load nsBundle =
  sendMessage nsBundle loadSelector

-- | @- unload@
unload :: IsNSBundle nsBundle => nsBundle -> IO Bool
unload nsBundle =
  sendMessage nsBundle unloadSelector

-- | @- preflightAndReturnError:@
preflightAndReturnError :: (IsNSBundle nsBundle, IsNSError error_) => nsBundle -> error_ -> IO Bool
preflightAndReturnError nsBundle error_ =
  sendMessage nsBundle preflightAndReturnErrorSelector (toNSError error_)

-- | @- loadAndReturnError:@
loadAndReturnError :: (IsNSBundle nsBundle, IsNSError error_) => nsBundle -> error_ -> IO Bool
loadAndReturnError nsBundle error_ =
  sendMessage nsBundle loadAndReturnErrorSelector (toNSError error_)

-- | @- URLForAuxiliaryExecutable:@
urlForAuxiliaryExecutable :: (IsNSBundle nsBundle, IsNSString executableName) => nsBundle -> executableName -> IO (Id NSURL)
urlForAuxiliaryExecutable nsBundle executableName =
  sendMessage nsBundle urlForAuxiliaryExecutableSelector (toNSString executableName)

-- | @- pathForAuxiliaryExecutable:@
pathForAuxiliaryExecutable :: (IsNSBundle nsBundle, IsNSString executableName) => nsBundle -> executableName -> IO (Id NSString)
pathForAuxiliaryExecutable nsBundle executableName =
  sendMessage nsBundle pathForAuxiliaryExecutableSelector (toNSString executableName)

-- | @+ URLForResource:withExtension:subdirectory:inBundleWithURL:@
urlForResource_withExtension_subdirectory_inBundleWithURL :: (IsNSString name, IsNSString ext, IsNSString subpath, IsNSURL bundleURL) => name -> ext -> subpath -> bundleURL -> IO (Id NSURL)
urlForResource_withExtension_subdirectory_inBundleWithURL name ext subpath bundleURL =
  do
    cls' <- getRequiredClass "NSBundle"
    sendClassMessage cls' urlForResource_withExtension_subdirectory_inBundleWithURLSelector (toNSString name) (toNSString ext) (toNSString subpath) (toNSURL bundleURL)

-- | @+ URLsForResourcesWithExtension:subdirectory:inBundleWithURL:@
urLsForResourcesWithExtension_subdirectory_inBundleWithURL :: (IsNSString ext, IsNSString subpath, IsNSURL bundleURL) => ext -> subpath -> bundleURL -> IO (Id NSArray)
urLsForResourcesWithExtension_subdirectory_inBundleWithURL ext subpath bundleURL =
  do
    cls' <- getRequiredClass "NSBundle"
    sendClassMessage cls' urLsForResourcesWithExtension_subdirectory_inBundleWithURLSelector (toNSString ext) (toNSString subpath) (toNSURL bundleURL)

-- | @- URLForResource:withExtension:@
urlForResource_withExtension :: (IsNSBundle nsBundle, IsNSString name, IsNSString ext) => nsBundle -> name -> ext -> IO (Id NSURL)
urlForResource_withExtension nsBundle name ext =
  sendMessage nsBundle urlForResource_withExtensionSelector (toNSString name) (toNSString ext)

-- | @- URLForResource:withExtension:subdirectory:@
urlForResource_withExtension_subdirectory :: (IsNSBundle nsBundle, IsNSString name, IsNSString ext, IsNSString subpath) => nsBundle -> name -> ext -> subpath -> IO (Id NSURL)
urlForResource_withExtension_subdirectory nsBundle name ext subpath =
  sendMessage nsBundle urlForResource_withExtension_subdirectorySelector (toNSString name) (toNSString ext) (toNSString subpath)

-- | @- URLForResource:withExtension:subdirectory:localization:@
urlForResource_withExtension_subdirectory_localization :: (IsNSBundle nsBundle, IsNSString name, IsNSString ext, IsNSString subpath, IsNSString localizationName) => nsBundle -> name -> ext -> subpath -> localizationName -> IO (Id NSURL)
urlForResource_withExtension_subdirectory_localization nsBundle name ext subpath localizationName =
  sendMessage nsBundle urlForResource_withExtension_subdirectory_localizationSelector (toNSString name) (toNSString ext) (toNSString subpath) (toNSString localizationName)

-- | @- URLsForResourcesWithExtension:subdirectory:@
urLsForResourcesWithExtension_subdirectory :: (IsNSBundle nsBundle, IsNSString ext, IsNSString subpath) => nsBundle -> ext -> subpath -> IO (Id NSArray)
urLsForResourcesWithExtension_subdirectory nsBundle ext subpath =
  sendMessage nsBundle urLsForResourcesWithExtension_subdirectorySelector (toNSString ext) (toNSString subpath)

-- | @- URLsForResourcesWithExtension:subdirectory:localization:@
urLsForResourcesWithExtension_subdirectory_localization :: (IsNSBundle nsBundle, IsNSString ext, IsNSString subpath, IsNSString localizationName) => nsBundle -> ext -> subpath -> localizationName -> IO (Id NSArray)
urLsForResourcesWithExtension_subdirectory_localization nsBundle ext subpath localizationName =
  sendMessage nsBundle urLsForResourcesWithExtension_subdirectory_localizationSelector (toNSString ext) (toNSString subpath) (toNSString localizationName)

-- | @+ pathForResource:ofType:inDirectory:@
nsBundlePathForResource_ofType_inDirectory :: (IsNSString name, IsNSString ext, IsNSString bundlePath) => name -> ext -> bundlePath -> IO (Id NSString)
nsBundlePathForResource_ofType_inDirectory name ext bundlePath =
  do
    cls' <- getRequiredClass "NSBundle"
    sendClassMessage cls' nsBundlePathForResource_ofType_inDirectorySelector (toNSString name) (toNSString ext) (toNSString bundlePath)

-- | @+ pathsForResourcesOfType:inDirectory:@
nsBundlePathsForResourcesOfType_inDirectory :: (IsNSString ext, IsNSString bundlePath) => ext -> bundlePath -> IO (Id NSArray)
nsBundlePathsForResourcesOfType_inDirectory ext bundlePath =
  do
    cls' <- getRequiredClass "NSBundle"
    sendClassMessage cls' nsBundlePathsForResourcesOfType_inDirectorySelector (toNSString ext) (toNSString bundlePath)

-- | @- pathForResource:ofType:@
pathForResource_ofType :: (IsNSBundle nsBundle, IsNSString name, IsNSString ext) => nsBundle -> name -> ext -> IO (Id NSString)
pathForResource_ofType nsBundle name ext =
  sendMessage nsBundle pathForResource_ofTypeSelector (toNSString name) (toNSString ext)

-- | @- pathForResource:ofType:inDirectory:@
pathForResource_ofType_inDirectory :: (IsNSBundle nsBundle, IsNSString name, IsNSString ext, IsNSString subpath) => nsBundle -> name -> ext -> subpath -> IO (Id NSString)
pathForResource_ofType_inDirectory nsBundle name ext subpath =
  sendMessage nsBundle pathForResource_ofType_inDirectorySelector (toNSString name) (toNSString ext) (toNSString subpath)

-- | @- pathForResource:ofType:inDirectory:forLocalization:@
pathForResource_ofType_inDirectory_forLocalization :: (IsNSBundle nsBundle, IsNSString name, IsNSString ext, IsNSString subpath, IsNSString localizationName) => nsBundle -> name -> ext -> subpath -> localizationName -> IO (Id NSString)
pathForResource_ofType_inDirectory_forLocalization nsBundle name ext subpath localizationName =
  sendMessage nsBundle pathForResource_ofType_inDirectory_forLocalizationSelector (toNSString name) (toNSString ext) (toNSString subpath) (toNSString localizationName)

-- | @- pathsForResourcesOfType:inDirectory:@
pathsForResourcesOfType_inDirectory :: (IsNSBundle nsBundle, IsNSString ext, IsNSString subpath) => nsBundle -> ext -> subpath -> IO (Id NSArray)
pathsForResourcesOfType_inDirectory nsBundle ext subpath =
  sendMessage nsBundle pathsForResourcesOfType_inDirectorySelector (toNSString ext) (toNSString subpath)

-- | @- pathsForResourcesOfType:inDirectory:forLocalization:@
pathsForResourcesOfType_inDirectory_forLocalization :: (IsNSBundle nsBundle, IsNSString ext, IsNSString subpath, IsNSString localizationName) => nsBundle -> ext -> subpath -> localizationName -> IO (Id NSArray)
pathsForResourcesOfType_inDirectory_forLocalization nsBundle ext subpath localizationName =
  sendMessage nsBundle pathsForResourcesOfType_inDirectory_forLocalizationSelector (toNSString ext) (toNSString subpath) (toNSString localizationName)

-- | @- localizedStringForKey:value:table:@
localizedStringForKey_value_table :: (IsNSBundle nsBundle, IsNSString key, IsNSString value, IsNSString tableName) => nsBundle -> key -> value -> tableName -> IO (Id NSString)
localizedStringForKey_value_table nsBundle key value tableName =
  sendMessage nsBundle localizedStringForKey_value_tableSelector (toNSString key) (toNSString value) (toNSString tableName)

-- | @- localizedAttributedStringForKey:value:table:@
localizedAttributedStringForKey_value_table :: (IsNSBundle nsBundle, IsNSString key, IsNSString value, IsNSString tableName) => nsBundle -> key -> value -> tableName -> IO (Id NSAttributedString)
localizedAttributedStringForKey_value_table nsBundle key value tableName =
  sendMessage nsBundle localizedAttributedStringForKey_value_tableSelector (toNSString key) (toNSString value) (toNSString tableName)

-- | Look up a localized string given a list of available localizations. - Parameters:   - key: The key for the localized string to retrieve.   - value: A default value to return if a localized string for ``key`` cannot be found.   - tableName: The name of the strings file to search. If @nil@, the method uses tables in @Localizable.strings@.   - localizations: An array of BCP 47 language codes corresponding to available localizations. Bundle compares the array against its available localizations, and uses the best result to retrieve the localized string. If empty, we treat it as no localization is available, and may return a fallback. - Returns: A localized version of the string designated by ``key`` in table ``tableName``.
--
-- ObjC selector: @- localizedStringForKey:value:table:localizations:@
localizedStringForKey_value_table_localizations :: (IsNSBundle nsBundle, IsNSString key, IsNSString value, IsNSString tableName, IsNSArray localizations) => nsBundle -> key -> value -> tableName -> localizations -> IO (Id NSString)
localizedStringForKey_value_table_localizations nsBundle key value tableName localizations =
  sendMessage nsBundle localizedStringForKey_value_table_localizationsSelector (toNSString key) (toNSString value) (toNSString tableName) (toNSArray localizations)

-- | @- objectForInfoDictionaryKey:@
objectForInfoDictionaryKey :: (IsNSBundle nsBundle, IsNSString key) => nsBundle -> key -> IO RawId
objectForInfoDictionaryKey nsBundle key =
  sendMessage nsBundle objectForInfoDictionaryKeySelector (toNSString key)

-- | @- classNamed:@
classNamed :: (IsNSBundle nsBundle, IsNSString className) => nsBundle -> className -> IO Class
classNamed nsBundle className =
  sendMessage nsBundle classNamedSelector (toNSString className)

-- | @+ preferredLocalizationsFromArray:@
preferredLocalizationsFromArray :: IsNSArray localizationsArray => localizationsArray -> IO (Id NSArray)
preferredLocalizationsFromArray localizationsArray =
  do
    cls' <- getRequiredClass "NSBundle"
    sendClassMessage cls' preferredLocalizationsFromArraySelector (toNSArray localizationsArray)

-- | @+ preferredLocalizationsFromArray:forPreferences:@
preferredLocalizationsFromArray_forPreferences :: (IsNSArray localizationsArray, IsNSArray preferencesArray) => localizationsArray -> preferencesArray -> IO (Id NSArray)
preferredLocalizationsFromArray_forPreferences localizationsArray preferencesArray =
  do
    cls' <- getRequiredClass "NSBundle"
    sendClassMessage cls' preferredLocalizationsFromArray_forPreferencesSelector (toNSArray localizationsArray) (toNSArray preferencesArray)

-- | @- setPreservationPriority:forTags:@
setPreservationPriority_forTags :: (IsNSBundle nsBundle, IsNSSet tags) => nsBundle -> CDouble -> tags -> IO ()
setPreservationPriority_forTags nsBundle priority tags =
  sendMessage nsBundle setPreservationPriority_forTagsSelector priority (toNSSet tags)

-- | @- preservationPriorityForTag:@
preservationPriorityForTag :: (IsNSBundle nsBundle, IsNSString tag) => nsBundle -> tag -> IO CDouble
preservationPriorityForTag nsBundle tag =
  sendMessage nsBundle preservationPriorityForTagSelector (toNSString tag)

-- | @+ mainBundle@
mainBundle :: IO (Id NSBundle)
mainBundle  =
  do
    cls' <- getRequiredClass "NSBundle"
    sendClassMessage cls' mainBundleSelector

-- | @+ allBundles@
allBundles :: IO (Id NSArray)
allBundles  =
  do
    cls' <- getRequiredClass "NSBundle"
    sendClassMessage cls' allBundlesSelector

-- | @+ allFrameworks@
allFrameworks :: IO (Id NSArray)
allFrameworks  =
  do
    cls' <- getRequiredClass "NSBundle"
    sendClassMessage cls' allFrameworksSelector

-- | @- loaded@
loaded :: IsNSBundle nsBundle => nsBundle -> IO Bool
loaded nsBundle =
  sendMessage nsBundle loadedSelector

-- | @- bundleURL@
bundleURL :: IsNSBundle nsBundle => nsBundle -> IO (Id NSURL)
bundleURL nsBundle =
  sendMessage nsBundle bundleURLSelector

-- | @- resourceURL@
resourceURL :: IsNSBundle nsBundle => nsBundle -> IO (Id NSURL)
resourceURL nsBundle =
  sendMessage nsBundle resourceURLSelector

-- | @- executableURL@
executableURL :: IsNSBundle nsBundle => nsBundle -> IO (Id NSURL)
executableURL nsBundle =
  sendMessage nsBundle executableURLSelector

-- | @- privateFrameworksURL@
privateFrameworksURL :: IsNSBundle nsBundle => nsBundle -> IO (Id NSURL)
privateFrameworksURL nsBundle =
  sendMessage nsBundle privateFrameworksURLSelector

-- | @- sharedFrameworksURL@
sharedFrameworksURL :: IsNSBundle nsBundle => nsBundle -> IO (Id NSURL)
sharedFrameworksURL nsBundle =
  sendMessage nsBundle sharedFrameworksURLSelector

-- | @- sharedSupportURL@
sharedSupportURL :: IsNSBundle nsBundle => nsBundle -> IO (Id NSURL)
sharedSupportURL nsBundle =
  sendMessage nsBundle sharedSupportURLSelector

-- | @- builtInPlugInsURL@
builtInPlugInsURL :: IsNSBundle nsBundle => nsBundle -> IO (Id NSURL)
builtInPlugInsURL nsBundle =
  sendMessage nsBundle builtInPlugInsURLSelector

-- | @- appStoreReceiptURL@
appStoreReceiptURL :: IsNSBundle nsBundle => nsBundle -> IO (Id NSURL)
appStoreReceiptURL nsBundle =
  sendMessage nsBundle appStoreReceiptURLSelector

-- | @- bundlePath@
bundlePath :: IsNSBundle nsBundle => nsBundle -> IO (Id NSString)
bundlePath nsBundle =
  sendMessage nsBundle bundlePathSelector

-- | @- resourcePath@
resourcePath :: IsNSBundle nsBundle => nsBundle -> IO (Id NSString)
resourcePath nsBundle =
  sendMessage nsBundle resourcePathSelector

-- | @- executablePath@
executablePath :: IsNSBundle nsBundle => nsBundle -> IO (Id NSString)
executablePath nsBundle =
  sendMessage nsBundle executablePathSelector

-- | @- privateFrameworksPath@
privateFrameworksPath :: IsNSBundle nsBundle => nsBundle -> IO (Id NSString)
privateFrameworksPath nsBundle =
  sendMessage nsBundle privateFrameworksPathSelector

-- | @- sharedFrameworksPath@
sharedFrameworksPath :: IsNSBundle nsBundle => nsBundle -> IO (Id NSString)
sharedFrameworksPath nsBundle =
  sendMessage nsBundle sharedFrameworksPathSelector

-- | @- sharedSupportPath@
sharedSupportPath :: IsNSBundle nsBundle => nsBundle -> IO (Id NSString)
sharedSupportPath nsBundle =
  sendMessage nsBundle sharedSupportPathSelector

-- | @- builtInPlugInsPath@
builtInPlugInsPath :: IsNSBundle nsBundle => nsBundle -> IO (Id NSString)
builtInPlugInsPath nsBundle =
  sendMessage nsBundle builtInPlugInsPathSelector

-- | @- bundleIdentifier@
bundleIdentifier :: IsNSBundle nsBundle => nsBundle -> IO (Id NSString)
bundleIdentifier nsBundle =
  sendMessage nsBundle bundleIdentifierSelector

-- | @- infoDictionary@
infoDictionary :: IsNSBundle nsBundle => nsBundle -> IO (Id NSDictionary)
infoDictionary nsBundle =
  sendMessage nsBundle infoDictionarySelector

-- | @- localizedInfoDictionary@
localizedInfoDictionary :: IsNSBundle nsBundle => nsBundle -> IO (Id NSDictionary)
localizedInfoDictionary nsBundle =
  sendMessage nsBundle localizedInfoDictionarySelector

-- | @- principalClass@
principalClass :: IsNSBundle nsBundle => nsBundle -> IO Class
principalClass nsBundle =
  sendMessage nsBundle principalClassSelector

-- | @- preferredLocalizations@
preferredLocalizations :: IsNSBundle nsBundle => nsBundle -> IO (Id NSArray)
preferredLocalizations nsBundle =
  sendMessage nsBundle preferredLocalizationsSelector

-- | @- localizations@
localizations :: IsNSBundle nsBundle => nsBundle -> IO (Id NSArray)
localizations nsBundle =
  sendMessage nsBundle localizationsSelector

-- | @- developmentLocalization@
developmentLocalization :: IsNSBundle nsBundle => nsBundle -> IO (Id NSString)
developmentLocalization nsBundle =
  sendMessage nsBundle developmentLocalizationSelector

-- | @- executableArchitectures@
executableArchitectures :: IsNSBundle nsBundle => nsBundle -> IO (Id NSArray)
executableArchitectures nsBundle =
  sendMessage nsBundle executableArchitecturesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bundleWithPath:@
bundleWithPathSelector :: Selector '[Id NSString] (Id NSBundle)
bundleWithPathSelector = mkSelector "bundleWithPath:"

-- | @Selector@ for @initWithPath:@
initWithPathSelector :: Selector '[Id NSString] (Id NSBundle)
initWithPathSelector = mkSelector "initWithPath:"

-- | @Selector@ for @bundleWithURL:@
bundleWithURLSelector :: Selector '[Id NSURL] (Id NSBundle)
bundleWithURLSelector = mkSelector "bundleWithURL:"

-- | @Selector@ for @initWithURL:@
initWithURLSelector :: Selector '[Id NSURL] (Id NSBundle)
initWithURLSelector = mkSelector "initWithURL:"

-- | @Selector@ for @bundleForClass:@
bundleForClassSelector :: Selector '[Class] (Id NSBundle)
bundleForClassSelector = mkSelector "bundleForClass:"

-- | @Selector@ for @bundleWithIdentifier:@
bundleWithIdentifierSelector :: Selector '[Id NSString] (Id NSBundle)
bundleWithIdentifierSelector = mkSelector "bundleWithIdentifier:"

-- | @Selector@ for @load@
loadSelector :: Selector '[] Bool
loadSelector = mkSelector "load"

-- | @Selector@ for @unload@
unloadSelector :: Selector '[] Bool
unloadSelector = mkSelector "unload"

-- | @Selector@ for @preflightAndReturnError:@
preflightAndReturnErrorSelector :: Selector '[Id NSError] Bool
preflightAndReturnErrorSelector = mkSelector "preflightAndReturnError:"

-- | @Selector@ for @loadAndReturnError:@
loadAndReturnErrorSelector :: Selector '[Id NSError] Bool
loadAndReturnErrorSelector = mkSelector "loadAndReturnError:"

-- | @Selector@ for @URLForAuxiliaryExecutable:@
urlForAuxiliaryExecutableSelector :: Selector '[Id NSString] (Id NSURL)
urlForAuxiliaryExecutableSelector = mkSelector "URLForAuxiliaryExecutable:"

-- | @Selector@ for @pathForAuxiliaryExecutable:@
pathForAuxiliaryExecutableSelector :: Selector '[Id NSString] (Id NSString)
pathForAuxiliaryExecutableSelector = mkSelector "pathForAuxiliaryExecutable:"

-- | @Selector@ for @URLForResource:withExtension:subdirectory:inBundleWithURL:@
urlForResource_withExtension_subdirectory_inBundleWithURLSelector :: Selector '[Id NSString, Id NSString, Id NSString, Id NSURL] (Id NSURL)
urlForResource_withExtension_subdirectory_inBundleWithURLSelector = mkSelector "URLForResource:withExtension:subdirectory:inBundleWithURL:"

-- | @Selector@ for @URLsForResourcesWithExtension:subdirectory:inBundleWithURL:@
urLsForResourcesWithExtension_subdirectory_inBundleWithURLSelector :: Selector '[Id NSString, Id NSString, Id NSURL] (Id NSArray)
urLsForResourcesWithExtension_subdirectory_inBundleWithURLSelector = mkSelector "URLsForResourcesWithExtension:subdirectory:inBundleWithURL:"

-- | @Selector@ for @URLForResource:withExtension:@
urlForResource_withExtensionSelector :: Selector '[Id NSString, Id NSString] (Id NSURL)
urlForResource_withExtensionSelector = mkSelector "URLForResource:withExtension:"

-- | @Selector@ for @URLForResource:withExtension:subdirectory:@
urlForResource_withExtension_subdirectorySelector :: Selector '[Id NSString, Id NSString, Id NSString] (Id NSURL)
urlForResource_withExtension_subdirectorySelector = mkSelector "URLForResource:withExtension:subdirectory:"

-- | @Selector@ for @URLForResource:withExtension:subdirectory:localization:@
urlForResource_withExtension_subdirectory_localizationSelector :: Selector '[Id NSString, Id NSString, Id NSString, Id NSString] (Id NSURL)
urlForResource_withExtension_subdirectory_localizationSelector = mkSelector "URLForResource:withExtension:subdirectory:localization:"

-- | @Selector@ for @URLsForResourcesWithExtension:subdirectory:@
urLsForResourcesWithExtension_subdirectorySelector :: Selector '[Id NSString, Id NSString] (Id NSArray)
urLsForResourcesWithExtension_subdirectorySelector = mkSelector "URLsForResourcesWithExtension:subdirectory:"

-- | @Selector@ for @URLsForResourcesWithExtension:subdirectory:localization:@
urLsForResourcesWithExtension_subdirectory_localizationSelector :: Selector '[Id NSString, Id NSString, Id NSString] (Id NSArray)
urLsForResourcesWithExtension_subdirectory_localizationSelector = mkSelector "URLsForResourcesWithExtension:subdirectory:localization:"

-- | @Selector@ for @pathForResource:ofType:inDirectory:@
nsBundlePathForResource_ofType_inDirectorySelector :: Selector '[Id NSString, Id NSString, Id NSString] (Id NSString)
nsBundlePathForResource_ofType_inDirectorySelector = mkSelector "pathForResource:ofType:inDirectory:"

-- | @Selector@ for @pathsForResourcesOfType:inDirectory:@
nsBundlePathsForResourcesOfType_inDirectorySelector :: Selector '[Id NSString, Id NSString] (Id NSArray)
nsBundlePathsForResourcesOfType_inDirectorySelector = mkSelector "pathsForResourcesOfType:inDirectory:"

-- | @Selector@ for @pathForResource:ofType:@
pathForResource_ofTypeSelector :: Selector '[Id NSString, Id NSString] (Id NSString)
pathForResource_ofTypeSelector = mkSelector "pathForResource:ofType:"

-- | @Selector@ for @pathForResource:ofType:inDirectory:@
pathForResource_ofType_inDirectorySelector :: Selector '[Id NSString, Id NSString, Id NSString] (Id NSString)
pathForResource_ofType_inDirectorySelector = mkSelector "pathForResource:ofType:inDirectory:"

-- | @Selector@ for @pathForResource:ofType:inDirectory:forLocalization:@
pathForResource_ofType_inDirectory_forLocalizationSelector :: Selector '[Id NSString, Id NSString, Id NSString, Id NSString] (Id NSString)
pathForResource_ofType_inDirectory_forLocalizationSelector = mkSelector "pathForResource:ofType:inDirectory:forLocalization:"

-- | @Selector@ for @pathsForResourcesOfType:inDirectory:@
pathsForResourcesOfType_inDirectorySelector :: Selector '[Id NSString, Id NSString] (Id NSArray)
pathsForResourcesOfType_inDirectorySelector = mkSelector "pathsForResourcesOfType:inDirectory:"

-- | @Selector@ for @pathsForResourcesOfType:inDirectory:forLocalization:@
pathsForResourcesOfType_inDirectory_forLocalizationSelector :: Selector '[Id NSString, Id NSString, Id NSString] (Id NSArray)
pathsForResourcesOfType_inDirectory_forLocalizationSelector = mkSelector "pathsForResourcesOfType:inDirectory:forLocalization:"

-- | @Selector@ for @localizedStringForKey:value:table:@
localizedStringForKey_value_tableSelector :: Selector '[Id NSString, Id NSString, Id NSString] (Id NSString)
localizedStringForKey_value_tableSelector = mkSelector "localizedStringForKey:value:table:"

-- | @Selector@ for @localizedAttributedStringForKey:value:table:@
localizedAttributedStringForKey_value_tableSelector :: Selector '[Id NSString, Id NSString, Id NSString] (Id NSAttributedString)
localizedAttributedStringForKey_value_tableSelector = mkSelector "localizedAttributedStringForKey:value:table:"

-- | @Selector@ for @localizedStringForKey:value:table:localizations:@
localizedStringForKey_value_table_localizationsSelector :: Selector '[Id NSString, Id NSString, Id NSString, Id NSArray] (Id NSString)
localizedStringForKey_value_table_localizationsSelector = mkSelector "localizedStringForKey:value:table:localizations:"

-- | @Selector@ for @objectForInfoDictionaryKey:@
objectForInfoDictionaryKeySelector :: Selector '[Id NSString] RawId
objectForInfoDictionaryKeySelector = mkSelector "objectForInfoDictionaryKey:"

-- | @Selector@ for @classNamed:@
classNamedSelector :: Selector '[Id NSString] Class
classNamedSelector = mkSelector "classNamed:"

-- | @Selector@ for @preferredLocalizationsFromArray:@
preferredLocalizationsFromArraySelector :: Selector '[Id NSArray] (Id NSArray)
preferredLocalizationsFromArraySelector = mkSelector "preferredLocalizationsFromArray:"

-- | @Selector@ for @preferredLocalizationsFromArray:forPreferences:@
preferredLocalizationsFromArray_forPreferencesSelector :: Selector '[Id NSArray, Id NSArray] (Id NSArray)
preferredLocalizationsFromArray_forPreferencesSelector = mkSelector "preferredLocalizationsFromArray:forPreferences:"

-- | @Selector@ for @setPreservationPriority:forTags:@
setPreservationPriority_forTagsSelector :: Selector '[CDouble, Id NSSet] ()
setPreservationPriority_forTagsSelector = mkSelector "setPreservationPriority:forTags:"

-- | @Selector@ for @preservationPriorityForTag:@
preservationPriorityForTagSelector :: Selector '[Id NSString] CDouble
preservationPriorityForTagSelector = mkSelector "preservationPriorityForTag:"

-- | @Selector@ for @mainBundle@
mainBundleSelector :: Selector '[] (Id NSBundle)
mainBundleSelector = mkSelector "mainBundle"

-- | @Selector@ for @allBundles@
allBundlesSelector :: Selector '[] (Id NSArray)
allBundlesSelector = mkSelector "allBundles"

-- | @Selector@ for @allFrameworks@
allFrameworksSelector :: Selector '[] (Id NSArray)
allFrameworksSelector = mkSelector "allFrameworks"

-- | @Selector@ for @loaded@
loadedSelector :: Selector '[] Bool
loadedSelector = mkSelector "loaded"

-- | @Selector@ for @bundleURL@
bundleURLSelector :: Selector '[] (Id NSURL)
bundleURLSelector = mkSelector "bundleURL"

-- | @Selector@ for @resourceURL@
resourceURLSelector :: Selector '[] (Id NSURL)
resourceURLSelector = mkSelector "resourceURL"

-- | @Selector@ for @executableURL@
executableURLSelector :: Selector '[] (Id NSURL)
executableURLSelector = mkSelector "executableURL"

-- | @Selector@ for @privateFrameworksURL@
privateFrameworksURLSelector :: Selector '[] (Id NSURL)
privateFrameworksURLSelector = mkSelector "privateFrameworksURL"

-- | @Selector@ for @sharedFrameworksURL@
sharedFrameworksURLSelector :: Selector '[] (Id NSURL)
sharedFrameworksURLSelector = mkSelector "sharedFrameworksURL"

-- | @Selector@ for @sharedSupportURL@
sharedSupportURLSelector :: Selector '[] (Id NSURL)
sharedSupportURLSelector = mkSelector "sharedSupportURL"

-- | @Selector@ for @builtInPlugInsURL@
builtInPlugInsURLSelector :: Selector '[] (Id NSURL)
builtInPlugInsURLSelector = mkSelector "builtInPlugInsURL"

-- | @Selector@ for @appStoreReceiptURL@
appStoreReceiptURLSelector :: Selector '[] (Id NSURL)
appStoreReceiptURLSelector = mkSelector "appStoreReceiptURL"

-- | @Selector@ for @bundlePath@
bundlePathSelector :: Selector '[] (Id NSString)
bundlePathSelector = mkSelector "bundlePath"

-- | @Selector@ for @resourcePath@
resourcePathSelector :: Selector '[] (Id NSString)
resourcePathSelector = mkSelector "resourcePath"

-- | @Selector@ for @executablePath@
executablePathSelector :: Selector '[] (Id NSString)
executablePathSelector = mkSelector "executablePath"

-- | @Selector@ for @privateFrameworksPath@
privateFrameworksPathSelector :: Selector '[] (Id NSString)
privateFrameworksPathSelector = mkSelector "privateFrameworksPath"

-- | @Selector@ for @sharedFrameworksPath@
sharedFrameworksPathSelector :: Selector '[] (Id NSString)
sharedFrameworksPathSelector = mkSelector "sharedFrameworksPath"

-- | @Selector@ for @sharedSupportPath@
sharedSupportPathSelector :: Selector '[] (Id NSString)
sharedSupportPathSelector = mkSelector "sharedSupportPath"

-- | @Selector@ for @builtInPlugInsPath@
builtInPlugInsPathSelector :: Selector '[] (Id NSString)
builtInPlugInsPathSelector = mkSelector "builtInPlugInsPath"

-- | @Selector@ for @bundleIdentifier@
bundleIdentifierSelector :: Selector '[] (Id NSString)
bundleIdentifierSelector = mkSelector "bundleIdentifier"

-- | @Selector@ for @infoDictionary@
infoDictionarySelector :: Selector '[] (Id NSDictionary)
infoDictionarySelector = mkSelector "infoDictionary"

-- | @Selector@ for @localizedInfoDictionary@
localizedInfoDictionarySelector :: Selector '[] (Id NSDictionary)
localizedInfoDictionarySelector = mkSelector "localizedInfoDictionary"

-- | @Selector@ for @principalClass@
principalClassSelector :: Selector '[] Class
principalClassSelector = mkSelector "principalClass"

-- | @Selector@ for @preferredLocalizations@
preferredLocalizationsSelector :: Selector '[] (Id NSArray)
preferredLocalizationsSelector = mkSelector "preferredLocalizations"

-- | @Selector@ for @localizations@
localizationsSelector :: Selector '[] (Id NSArray)
localizationsSelector = mkSelector "localizations"

-- | @Selector@ for @developmentLocalization@
developmentLocalizationSelector :: Selector '[] (Id NSString)
developmentLocalizationSelector = mkSelector "developmentLocalization"

-- | @Selector@ for @executableArchitectures@
executableArchitecturesSelector :: Selector '[] (Id NSArray)
executableArchitecturesSelector = mkSelector "executableArchitectures"

