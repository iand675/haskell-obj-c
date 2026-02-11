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
  , bundleWithPathSelector
  , initWithPathSelector
  , bundleWithURLSelector
  , initWithURLSelector
  , bundleForClassSelector
  , bundleWithIdentifierSelector
  , loadSelector
  , unloadSelector
  , preflightAndReturnErrorSelector
  , loadAndReturnErrorSelector
  , urlForAuxiliaryExecutableSelector
  , pathForAuxiliaryExecutableSelector
  , urlForResource_withExtension_subdirectory_inBundleWithURLSelector
  , urLsForResourcesWithExtension_subdirectory_inBundleWithURLSelector
  , urlForResource_withExtensionSelector
  , urlForResource_withExtension_subdirectorySelector
  , urlForResource_withExtension_subdirectory_localizationSelector
  , urLsForResourcesWithExtension_subdirectorySelector
  , urLsForResourcesWithExtension_subdirectory_localizationSelector
  , pathForResource_ofType_inDirectorySelector
  , pathsForResourcesOfType_inDirectorySelector
  , pathForResource_ofTypeSelector
  , pathForResource_ofType_inDirectory_forLocalizationSelector
  , pathsForResourcesOfType_inDirectory_forLocalizationSelector
  , localizedStringForKey_value_tableSelector
  , localizedAttributedStringForKey_value_tableSelector
  , localizedStringForKey_value_table_localizationsSelector
  , objectForInfoDictionaryKeySelector
  , classNamedSelector
  , preferredLocalizationsFromArraySelector
  , preferredLocalizationsFromArray_forPreferencesSelector
  , setPreservationPriority_forTagsSelector
  , preservationPriorityForTagSelector
  , mainBundleSelector
  , allBundlesSelector
  , allFrameworksSelector
  , loadedSelector
  , bundleURLSelector
  , resourceURLSelector
  , executableURLSelector
  , privateFrameworksURLSelector
  , sharedFrameworksURLSelector
  , sharedSupportURLSelector
  , builtInPlugInsURLSelector
  , appStoreReceiptURLSelector
  , bundlePathSelector
  , resourcePathSelector
  , executablePathSelector
  , privateFrameworksPathSelector
  , sharedFrameworksPathSelector
  , sharedSupportPathSelector
  , builtInPlugInsPathSelector
  , bundleIdentifierSelector
  , infoDictionarySelector
  , localizedInfoDictionarySelector
  , principalClassSelector
  , preferredLocalizationsSelector
  , localizationsSelector
  , developmentLocalizationSelector
  , executableArchitecturesSelector


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

-- | @+ bundleWithPath:@
bundleWithPath :: IsNSString path => path -> IO (Id NSBundle)
bundleWithPath path =
  do
    cls' <- getRequiredClass "NSBundle"
    withObjCPtr path $ \raw_path ->
      sendClassMsg cls' (mkSelector "bundleWithPath:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithPath:@
initWithPath :: (IsNSBundle nsBundle, IsNSString path) => nsBundle -> path -> IO (Id NSBundle)
initWithPath nsBundle  path =
withObjCPtr path $ \raw_path ->
    sendMsg nsBundle (mkSelector "initWithPath:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= ownedObject . castPtr

-- | @+ bundleWithURL:@
bundleWithURL :: IsNSURL url => url -> IO (Id NSBundle)
bundleWithURL url =
  do
    cls' <- getRequiredClass "NSBundle"
    withObjCPtr url $ \raw_url ->
      sendClassMsg cls' (mkSelector "bundleWithURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithURL:@
initWithURL :: (IsNSBundle nsBundle, IsNSURL url) => nsBundle -> url -> IO (Id NSBundle)
initWithURL nsBundle  url =
withObjCPtr url $ \raw_url ->
    sendMsg nsBundle (mkSelector "initWithURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= ownedObject . castPtr

-- | @+ bundleForClass:@
bundleForClass :: Class -> IO (Id NSBundle)
bundleForClass aClass =
  do
    cls' <- getRequiredClass "NSBundle"
    sendClassMsg cls' (mkSelector "bundleForClass:") (retPtr retVoid) [argPtr (unClass aClass)] >>= retainedObject . castPtr

-- | @+ bundleWithIdentifier:@
bundleWithIdentifier :: IsNSString identifier => identifier -> IO (Id NSBundle)
bundleWithIdentifier identifier =
  do
    cls' <- getRequiredClass "NSBundle"
    withObjCPtr identifier $ \raw_identifier ->
      sendClassMsg cls' (mkSelector "bundleWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= retainedObject . castPtr

-- | @- load@
load :: IsNSBundle nsBundle => nsBundle -> IO Bool
load nsBundle  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsBundle (mkSelector "load") retCULong []

-- | @- unload@
unload :: IsNSBundle nsBundle => nsBundle -> IO Bool
unload nsBundle  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsBundle (mkSelector "unload") retCULong []

-- | @- preflightAndReturnError:@
preflightAndReturnError :: (IsNSBundle nsBundle, IsNSError error_) => nsBundle -> error_ -> IO Bool
preflightAndReturnError nsBundle  error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsBundle (mkSelector "preflightAndReturnError:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- loadAndReturnError:@
loadAndReturnError :: (IsNSBundle nsBundle, IsNSError error_) => nsBundle -> error_ -> IO Bool
loadAndReturnError nsBundle  error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsBundle (mkSelector "loadAndReturnError:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- URLForAuxiliaryExecutable:@
urlForAuxiliaryExecutable :: (IsNSBundle nsBundle, IsNSString executableName) => nsBundle -> executableName -> IO (Id NSURL)
urlForAuxiliaryExecutable nsBundle  executableName =
withObjCPtr executableName $ \raw_executableName ->
    sendMsg nsBundle (mkSelector "URLForAuxiliaryExecutable:") (retPtr retVoid) [argPtr (castPtr raw_executableName :: Ptr ())] >>= retainedObject . castPtr

-- | @- pathForAuxiliaryExecutable:@
pathForAuxiliaryExecutable :: (IsNSBundle nsBundle, IsNSString executableName) => nsBundle -> executableName -> IO (Id NSString)
pathForAuxiliaryExecutable nsBundle  executableName =
withObjCPtr executableName $ \raw_executableName ->
    sendMsg nsBundle (mkSelector "pathForAuxiliaryExecutable:") (retPtr retVoid) [argPtr (castPtr raw_executableName :: Ptr ())] >>= retainedObject . castPtr

-- | @+ URLForResource:withExtension:subdirectory:inBundleWithURL:@
urlForResource_withExtension_subdirectory_inBundleWithURL :: (IsNSString name, IsNSString ext, IsNSString subpath, IsNSURL bundleURL) => name -> ext -> subpath -> bundleURL -> IO (Id NSURL)
urlForResource_withExtension_subdirectory_inBundleWithURL name ext subpath bundleURL =
  do
    cls' <- getRequiredClass "NSBundle"
    withObjCPtr name $ \raw_name ->
      withObjCPtr ext $ \raw_ext ->
        withObjCPtr subpath $ \raw_subpath ->
          withObjCPtr bundleURL $ \raw_bundleURL ->
            sendClassMsg cls' (mkSelector "URLForResource:withExtension:subdirectory:inBundleWithURL:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_ext :: Ptr ()), argPtr (castPtr raw_subpath :: Ptr ()), argPtr (castPtr raw_bundleURL :: Ptr ())] >>= retainedObject . castPtr

-- | @+ URLsForResourcesWithExtension:subdirectory:inBundleWithURL:@
urLsForResourcesWithExtension_subdirectory_inBundleWithURL :: (IsNSString ext, IsNSString subpath, IsNSURL bundleURL) => ext -> subpath -> bundleURL -> IO (Id NSArray)
urLsForResourcesWithExtension_subdirectory_inBundleWithURL ext subpath bundleURL =
  do
    cls' <- getRequiredClass "NSBundle"
    withObjCPtr ext $ \raw_ext ->
      withObjCPtr subpath $ \raw_subpath ->
        withObjCPtr bundleURL $ \raw_bundleURL ->
          sendClassMsg cls' (mkSelector "URLsForResourcesWithExtension:subdirectory:inBundleWithURL:") (retPtr retVoid) [argPtr (castPtr raw_ext :: Ptr ()), argPtr (castPtr raw_subpath :: Ptr ()), argPtr (castPtr raw_bundleURL :: Ptr ())] >>= retainedObject . castPtr

-- | @- URLForResource:withExtension:@
urlForResource_withExtension :: (IsNSBundle nsBundle, IsNSString name, IsNSString ext) => nsBundle -> name -> ext -> IO (Id NSURL)
urlForResource_withExtension nsBundle  name ext =
withObjCPtr name $ \raw_name ->
  withObjCPtr ext $ \raw_ext ->
      sendMsg nsBundle (mkSelector "URLForResource:withExtension:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_ext :: Ptr ())] >>= retainedObject . castPtr

-- | @- URLForResource:withExtension:subdirectory:@
urlForResource_withExtension_subdirectory :: (IsNSBundle nsBundle, IsNSString name, IsNSString ext, IsNSString subpath) => nsBundle -> name -> ext -> subpath -> IO (Id NSURL)
urlForResource_withExtension_subdirectory nsBundle  name ext subpath =
withObjCPtr name $ \raw_name ->
  withObjCPtr ext $ \raw_ext ->
    withObjCPtr subpath $ \raw_subpath ->
        sendMsg nsBundle (mkSelector "URLForResource:withExtension:subdirectory:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_ext :: Ptr ()), argPtr (castPtr raw_subpath :: Ptr ())] >>= retainedObject . castPtr

-- | @- URLForResource:withExtension:subdirectory:localization:@
urlForResource_withExtension_subdirectory_localization :: (IsNSBundle nsBundle, IsNSString name, IsNSString ext, IsNSString subpath, IsNSString localizationName) => nsBundle -> name -> ext -> subpath -> localizationName -> IO (Id NSURL)
urlForResource_withExtension_subdirectory_localization nsBundle  name ext subpath localizationName =
withObjCPtr name $ \raw_name ->
  withObjCPtr ext $ \raw_ext ->
    withObjCPtr subpath $ \raw_subpath ->
      withObjCPtr localizationName $ \raw_localizationName ->
          sendMsg nsBundle (mkSelector "URLForResource:withExtension:subdirectory:localization:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_ext :: Ptr ()), argPtr (castPtr raw_subpath :: Ptr ()), argPtr (castPtr raw_localizationName :: Ptr ())] >>= retainedObject . castPtr

-- | @- URLsForResourcesWithExtension:subdirectory:@
urLsForResourcesWithExtension_subdirectory :: (IsNSBundle nsBundle, IsNSString ext, IsNSString subpath) => nsBundle -> ext -> subpath -> IO (Id NSArray)
urLsForResourcesWithExtension_subdirectory nsBundle  ext subpath =
withObjCPtr ext $ \raw_ext ->
  withObjCPtr subpath $ \raw_subpath ->
      sendMsg nsBundle (mkSelector "URLsForResourcesWithExtension:subdirectory:") (retPtr retVoid) [argPtr (castPtr raw_ext :: Ptr ()), argPtr (castPtr raw_subpath :: Ptr ())] >>= retainedObject . castPtr

-- | @- URLsForResourcesWithExtension:subdirectory:localization:@
urLsForResourcesWithExtension_subdirectory_localization :: (IsNSBundle nsBundle, IsNSString ext, IsNSString subpath, IsNSString localizationName) => nsBundle -> ext -> subpath -> localizationName -> IO (Id NSArray)
urLsForResourcesWithExtension_subdirectory_localization nsBundle  ext subpath localizationName =
withObjCPtr ext $ \raw_ext ->
  withObjCPtr subpath $ \raw_subpath ->
    withObjCPtr localizationName $ \raw_localizationName ->
        sendMsg nsBundle (mkSelector "URLsForResourcesWithExtension:subdirectory:localization:") (retPtr retVoid) [argPtr (castPtr raw_ext :: Ptr ()), argPtr (castPtr raw_subpath :: Ptr ()), argPtr (castPtr raw_localizationName :: Ptr ())] >>= retainedObject . castPtr

-- | @+ pathForResource:ofType:inDirectory:@
nsBundlePathForResource_ofType_inDirectory :: (IsNSString name, IsNSString ext, IsNSString bundlePath) => name -> ext -> bundlePath -> IO (Id NSString)
nsBundlePathForResource_ofType_inDirectory name ext bundlePath =
  do
    cls' <- getRequiredClass "NSBundle"
    withObjCPtr name $ \raw_name ->
      withObjCPtr ext $ \raw_ext ->
        withObjCPtr bundlePath $ \raw_bundlePath ->
          sendClassMsg cls' (mkSelector "pathForResource:ofType:inDirectory:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_ext :: Ptr ()), argPtr (castPtr raw_bundlePath :: Ptr ())] >>= retainedObject . castPtr

-- | @+ pathsForResourcesOfType:inDirectory:@
nsBundlePathsForResourcesOfType_inDirectory :: (IsNSString ext, IsNSString bundlePath) => ext -> bundlePath -> IO (Id NSArray)
nsBundlePathsForResourcesOfType_inDirectory ext bundlePath =
  do
    cls' <- getRequiredClass "NSBundle"
    withObjCPtr ext $ \raw_ext ->
      withObjCPtr bundlePath $ \raw_bundlePath ->
        sendClassMsg cls' (mkSelector "pathsForResourcesOfType:inDirectory:") (retPtr retVoid) [argPtr (castPtr raw_ext :: Ptr ()), argPtr (castPtr raw_bundlePath :: Ptr ())] >>= retainedObject . castPtr

-- | @- pathForResource:ofType:@
pathForResource_ofType :: (IsNSBundle nsBundle, IsNSString name, IsNSString ext) => nsBundle -> name -> ext -> IO (Id NSString)
pathForResource_ofType nsBundle  name ext =
withObjCPtr name $ \raw_name ->
  withObjCPtr ext $ \raw_ext ->
      sendMsg nsBundle (mkSelector "pathForResource:ofType:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_ext :: Ptr ())] >>= retainedObject . castPtr

-- | @- pathForResource:ofType:inDirectory:@
pathForResource_ofType_inDirectory :: (IsNSBundle nsBundle, IsNSString name, IsNSString ext, IsNSString subpath) => nsBundle -> name -> ext -> subpath -> IO (Id NSString)
pathForResource_ofType_inDirectory nsBundle  name ext subpath =
withObjCPtr name $ \raw_name ->
  withObjCPtr ext $ \raw_ext ->
    withObjCPtr subpath $ \raw_subpath ->
        sendMsg nsBundle (mkSelector "pathForResource:ofType:inDirectory:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_ext :: Ptr ()), argPtr (castPtr raw_subpath :: Ptr ())] >>= retainedObject . castPtr

-- | @- pathForResource:ofType:inDirectory:forLocalization:@
pathForResource_ofType_inDirectory_forLocalization :: (IsNSBundle nsBundle, IsNSString name, IsNSString ext, IsNSString subpath, IsNSString localizationName) => nsBundle -> name -> ext -> subpath -> localizationName -> IO (Id NSString)
pathForResource_ofType_inDirectory_forLocalization nsBundle  name ext subpath localizationName =
withObjCPtr name $ \raw_name ->
  withObjCPtr ext $ \raw_ext ->
    withObjCPtr subpath $ \raw_subpath ->
      withObjCPtr localizationName $ \raw_localizationName ->
          sendMsg nsBundle (mkSelector "pathForResource:ofType:inDirectory:forLocalization:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_ext :: Ptr ()), argPtr (castPtr raw_subpath :: Ptr ()), argPtr (castPtr raw_localizationName :: Ptr ())] >>= retainedObject . castPtr

-- | @- pathsForResourcesOfType:inDirectory:@
pathsForResourcesOfType_inDirectory :: (IsNSBundle nsBundle, IsNSString ext, IsNSString subpath) => nsBundle -> ext -> subpath -> IO (Id NSArray)
pathsForResourcesOfType_inDirectory nsBundle  ext subpath =
withObjCPtr ext $ \raw_ext ->
  withObjCPtr subpath $ \raw_subpath ->
      sendMsg nsBundle (mkSelector "pathsForResourcesOfType:inDirectory:") (retPtr retVoid) [argPtr (castPtr raw_ext :: Ptr ()), argPtr (castPtr raw_subpath :: Ptr ())] >>= retainedObject . castPtr

-- | @- pathsForResourcesOfType:inDirectory:forLocalization:@
pathsForResourcesOfType_inDirectory_forLocalization :: (IsNSBundle nsBundle, IsNSString ext, IsNSString subpath, IsNSString localizationName) => nsBundle -> ext -> subpath -> localizationName -> IO (Id NSArray)
pathsForResourcesOfType_inDirectory_forLocalization nsBundle  ext subpath localizationName =
withObjCPtr ext $ \raw_ext ->
  withObjCPtr subpath $ \raw_subpath ->
    withObjCPtr localizationName $ \raw_localizationName ->
        sendMsg nsBundle (mkSelector "pathsForResourcesOfType:inDirectory:forLocalization:") (retPtr retVoid) [argPtr (castPtr raw_ext :: Ptr ()), argPtr (castPtr raw_subpath :: Ptr ()), argPtr (castPtr raw_localizationName :: Ptr ())] >>= retainedObject . castPtr

-- | @- localizedStringForKey:value:table:@
localizedStringForKey_value_table :: (IsNSBundle nsBundle, IsNSString key, IsNSString value, IsNSString tableName) => nsBundle -> key -> value -> tableName -> IO (Id NSString)
localizedStringForKey_value_table nsBundle  key value tableName =
withObjCPtr key $ \raw_key ->
  withObjCPtr value $ \raw_value ->
    withObjCPtr tableName $ \raw_tableName ->
        sendMsg nsBundle (mkSelector "localizedStringForKey:value:table:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_tableName :: Ptr ())] >>= retainedObject . castPtr

-- | @- localizedAttributedStringForKey:value:table:@
localizedAttributedStringForKey_value_table :: (IsNSBundle nsBundle, IsNSString key, IsNSString value, IsNSString tableName) => nsBundle -> key -> value -> tableName -> IO (Id NSAttributedString)
localizedAttributedStringForKey_value_table nsBundle  key value tableName =
withObjCPtr key $ \raw_key ->
  withObjCPtr value $ \raw_value ->
    withObjCPtr tableName $ \raw_tableName ->
        sendMsg nsBundle (mkSelector "localizedAttributedStringForKey:value:table:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_tableName :: Ptr ())] >>= retainedObject . castPtr

-- | Look up a localized string given a list of available localizations. - Parameters:   - key: The key for the localized string to retrieve.   - value: A default value to return if a localized string for ``key`` cannot be found.   - tableName: The name of the strings file to search. If @nil@, the method uses tables in @Localizable.strings@.   - localizations: An array of BCP 47 language codes corresponding to available localizations. Bundle compares the array against its available localizations, and uses the best result to retrieve the localized string. If empty, we treat it as no localization is available, and may return a fallback. - Returns: A localized version of the string designated by ``key`` in table ``tableName``.
--
-- ObjC selector: @- localizedStringForKey:value:table:localizations:@
localizedStringForKey_value_table_localizations :: (IsNSBundle nsBundle, IsNSString key, IsNSString value, IsNSString tableName, IsNSArray localizations) => nsBundle -> key -> value -> tableName -> localizations -> IO (Id NSString)
localizedStringForKey_value_table_localizations nsBundle  key value tableName localizations =
withObjCPtr key $ \raw_key ->
  withObjCPtr value $ \raw_value ->
    withObjCPtr tableName $ \raw_tableName ->
      withObjCPtr localizations $ \raw_localizations ->
          sendMsg nsBundle (mkSelector "localizedStringForKey:value:table:localizations:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_tableName :: Ptr ()), argPtr (castPtr raw_localizations :: Ptr ())] >>= retainedObject . castPtr

-- | @- objectForInfoDictionaryKey:@
objectForInfoDictionaryKey :: (IsNSBundle nsBundle, IsNSString key) => nsBundle -> key -> IO RawId
objectForInfoDictionaryKey nsBundle  key =
withObjCPtr key $ \raw_key ->
    fmap (RawId . castPtr) $ sendMsg nsBundle (mkSelector "objectForInfoDictionaryKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())]

-- | @- classNamed:@
classNamed :: (IsNSBundle nsBundle, IsNSString className) => nsBundle -> className -> IO Class
classNamed nsBundle  className =
withObjCPtr className $ \raw_className ->
    fmap (Class . castPtr) $ sendMsg nsBundle (mkSelector "classNamed:") (retPtr retVoid) [argPtr (castPtr raw_className :: Ptr ())]

-- | @+ preferredLocalizationsFromArray:@
preferredLocalizationsFromArray :: IsNSArray localizationsArray => localizationsArray -> IO (Id NSArray)
preferredLocalizationsFromArray localizationsArray =
  do
    cls' <- getRequiredClass "NSBundle"
    withObjCPtr localizationsArray $ \raw_localizationsArray ->
      sendClassMsg cls' (mkSelector "preferredLocalizationsFromArray:") (retPtr retVoid) [argPtr (castPtr raw_localizationsArray :: Ptr ())] >>= retainedObject . castPtr

-- | @+ preferredLocalizationsFromArray:forPreferences:@
preferredLocalizationsFromArray_forPreferences :: (IsNSArray localizationsArray, IsNSArray preferencesArray) => localizationsArray -> preferencesArray -> IO (Id NSArray)
preferredLocalizationsFromArray_forPreferences localizationsArray preferencesArray =
  do
    cls' <- getRequiredClass "NSBundle"
    withObjCPtr localizationsArray $ \raw_localizationsArray ->
      withObjCPtr preferencesArray $ \raw_preferencesArray ->
        sendClassMsg cls' (mkSelector "preferredLocalizationsFromArray:forPreferences:") (retPtr retVoid) [argPtr (castPtr raw_localizationsArray :: Ptr ()), argPtr (castPtr raw_preferencesArray :: Ptr ())] >>= retainedObject . castPtr

-- | @- setPreservationPriority:forTags:@
setPreservationPriority_forTags :: (IsNSBundle nsBundle, IsNSSet tags) => nsBundle -> CDouble -> tags -> IO ()
setPreservationPriority_forTags nsBundle  priority tags =
withObjCPtr tags $ \raw_tags ->
    sendMsg nsBundle (mkSelector "setPreservationPriority:forTags:") retVoid [argCDouble (fromIntegral priority), argPtr (castPtr raw_tags :: Ptr ())]

-- | @- preservationPriorityForTag:@
preservationPriorityForTag :: (IsNSBundle nsBundle, IsNSString tag) => nsBundle -> tag -> IO CDouble
preservationPriorityForTag nsBundle  tag =
withObjCPtr tag $ \raw_tag ->
    sendMsg nsBundle (mkSelector "preservationPriorityForTag:") retCDouble [argPtr (castPtr raw_tag :: Ptr ())]

-- | @+ mainBundle@
mainBundle :: IO (Id NSBundle)
mainBundle  =
  do
    cls' <- getRequiredClass "NSBundle"
    sendClassMsg cls' (mkSelector "mainBundle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ allBundles@
allBundles :: IO (Id NSArray)
allBundles  =
  do
    cls' <- getRequiredClass "NSBundle"
    sendClassMsg cls' (mkSelector "allBundles") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ allFrameworks@
allFrameworks :: IO (Id NSArray)
allFrameworks  =
  do
    cls' <- getRequiredClass "NSBundle"
    sendClassMsg cls' (mkSelector "allFrameworks") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- loaded@
loaded :: IsNSBundle nsBundle => nsBundle -> IO Bool
loaded nsBundle  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsBundle (mkSelector "loaded") retCULong []

-- | @- bundleURL@
bundleURL :: IsNSBundle nsBundle => nsBundle -> IO (Id NSURL)
bundleURL nsBundle  =
  sendMsg nsBundle (mkSelector "bundleURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- resourceURL@
resourceURL :: IsNSBundle nsBundle => nsBundle -> IO (Id NSURL)
resourceURL nsBundle  =
  sendMsg nsBundle (mkSelector "resourceURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- executableURL@
executableURL :: IsNSBundle nsBundle => nsBundle -> IO (Id NSURL)
executableURL nsBundle  =
  sendMsg nsBundle (mkSelector "executableURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- privateFrameworksURL@
privateFrameworksURL :: IsNSBundle nsBundle => nsBundle -> IO (Id NSURL)
privateFrameworksURL nsBundle  =
  sendMsg nsBundle (mkSelector "privateFrameworksURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sharedFrameworksURL@
sharedFrameworksURL :: IsNSBundle nsBundle => nsBundle -> IO (Id NSURL)
sharedFrameworksURL nsBundle  =
  sendMsg nsBundle (mkSelector "sharedFrameworksURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sharedSupportURL@
sharedSupportURL :: IsNSBundle nsBundle => nsBundle -> IO (Id NSURL)
sharedSupportURL nsBundle  =
  sendMsg nsBundle (mkSelector "sharedSupportURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- builtInPlugInsURL@
builtInPlugInsURL :: IsNSBundle nsBundle => nsBundle -> IO (Id NSURL)
builtInPlugInsURL nsBundle  =
  sendMsg nsBundle (mkSelector "builtInPlugInsURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- appStoreReceiptURL@
appStoreReceiptURL :: IsNSBundle nsBundle => nsBundle -> IO (Id NSURL)
appStoreReceiptURL nsBundle  =
  sendMsg nsBundle (mkSelector "appStoreReceiptURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- bundlePath@
bundlePath :: IsNSBundle nsBundle => nsBundle -> IO (Id NSString)
bundlePath nsBundle  =
  sendMsg nsBundle (mkSelector "bundlePath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- resourcePath@
resourcePath :: IsNSBundle nsBundle => nsBundle -> IO (Id NSString)
resourcePath nsBundle  =
  sendMsg nsBundle (mkSelector "resourcePath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- executablePath@
executablePath :: IsNSBundle nsBundle => nsBundle -> IO (Id NSString)
executablePath nsBundle  =
  sendMsg nsBundle (mkSelector "executablePath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- privateFrameworksPath@
privateFrameworksPath :: IsNSBundle nsBundle => nsBundle -> IO (Id NSString)
privateFrameworksPath nsBundle  =
  sendMsg nsBundle (mkSelector "privateFrameworksPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sharedFrameworksPath@
sharedFrameworksPath :: IsNSBundle nsBundle => nsBundle -> IO (Id NSString)
sharedFrameworksPath nsBundle  =
  sendMsg nsBundle (mkSelector "sharedFrameworksPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sharedSupportPath@
sharedSupportPath :: IsNSBundle nsBundle => nsBundle -> IO (Id NSString)
sharedSupportPath nsBundle  =
  sendMsg nsBundle (mkSelector "sharedSupportPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- builtInPlugInsPath@
builtInPlugInsPath :: IsNSBundle nsBundle => nsBundle -> IO (Id NSString)
builtInPlugInsPath nsBundle  =
  sendMsg nsBundle (mkSelector "builtInPlugInsPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- bundleIdentifier@
bundleIdentifier :: IsNSBundle nsBundle => nsBundle -> IO (Id NSString)
bundleIdentifier nsBundle  =
  sendMsg nsBundle (mkSelector "bundleIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- infoDictionary@
infoDictionary :: IsNSBundle nsBundle => nsBundle -> IO (Id NSDictionary)
infoDictionary nsBundle  =
  sendMsg nsBundle (mkSelector "infoDictionary") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- localizedInfoDictionary@
localizedInfoDictionary :: IsNSBundle nsBundle => nsBundle -> IO (Id NSDictionary)
localizedInfoDictionary nsBundle  =
  sendMsg nsBundle (mkSelector "localizedInfoDictionary") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- principalClass@
principalClass :: IsNSBundle nsBundle => nsBundle -> IO Class
principalClass nsBundle  =
  fmap (Class . castPtr) $ sendMsg nsBundle (mkSelector "principalClass") (retPtr retVoid) []

-- | @- preferredLocalizations@
preferredLocalizations :: IsNSBundle nsBundle => nsBundle -> IO (Id NSArray)
preferredLocalizations nsBundle  =
  sendMsg nsBundle (mkSelector "preferredLocalizations") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- localizations@
localizations :: IsNSBundle nsBundle => nsBundle -> IO (Id NSArray)
localizations nsBundle  =
  sendMsg nsBundle (mkSelector "localizations") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- developmentLocalization@
developmentLocalization :: IsNSBundle nsBundle => nsBundle -> IO (Id NSString)
developmentLocalization nsBundle  =
  sendMsg nsBundle (mkSelector "developmentLocalization") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- executableArchitectures@
executableArchitectures :: IsNSBundle nsBundle => nsBundle -> IO (Id NSArray)
executableArchitectures nsBundle  =
  sendMsg nsBundle (mkSelector "executableArchitectures") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bundleWithPath:@
bundleWithPathSelector :: Selector
bundleWithPathSelector = mkSelector "bundleWithPath:"

-- | @Selector@ for @initWithPath:@
initWithPathSelector :: Selector
initWithPathSelector = mkSelector "initWithPath:"

-- | @Selector@ for @bundleWithURL:@
bundleWithURLSelector :: Selector
bundleWithURLSelector = mkSelector "bundleWithURL:"

-- | @Selector@ for @initWithURL:@
initWithURLSelector :: Selector
initWithURLSelector = mkSelector "initWithURL:"

-- | @Selector@ for @bundleForClass:@
bundleForClassSelector :: Selector
bundleForClassSelector = mkSelector "bundleForClass:"

-- | @Selector@ for @bundleWithIdentifier:@
bundleWithIdentifierSelector :: Selector
bundleWithIdentifierSelector = mkSelector "bundleWithIdentifier:"

-- | @Selector@ for @load@
loadSelector :: Selector
loadSelector = mkSelector "load"

-- | @Selector@ for @unload@
unloadSelector :: Selector
unloadSelector = mkSelector "unload"

-- | @Selector@ for @preflightAndReturnError:@
preflightAndReturnErrorSelector :: Selector
preflightAndReturnErrorSelector = mkSelector "preflightAndReturnError:"

-- | @Selector@ for @loadAndReturnError:@
loadAndReturnErrorSelector :: Selector
loadAndReturnErrorSelector = mkSelector "loadAndReturnError:"

-- | @Selector@ for @URLForAuxiliaryExecutable:@
urlForAuxiliaryExecutableSelector :: Selector
urlForAuxiliaryExecutableSelector = mkSelector "URLForAuxiliaryExecutable:"

-- | @Selector@ for @pathForAuxiliaryExecutable:@
pathForAuxiliaryExecutableSelector :: Selector
pathForAuxiliaryExecutableSelector = mkSelector "pathForAuxiliaryExecutable:"

-- | @Selector@ for @URLForResource:withExtension:subdirectory:inBundleWithURL:@
urlForResource_withExtension_subdirectory_inBundleWithURLSelector :: Selector
urlForResource_withExtension_subdirectory_inBundleWithURLSelector = mkSelector "URLForResource:withExtension:subdirectory:inBundleWithURL:"

-- | @Selector@ for @URLsForResourcesWithExtension:subdirectory:inBundleWithURL:@
urLsForResourcesWithExtension_subdirectory_inBundleWithURLSelector :: Selector
urLsForResourcesWithExtension_subdirectory_inBundleWithURLSelector = mkSelector "URLsForResourcesWithExtension:subdirectory:inBundleWithURL:"

-- | @Selector@ for @URLForResource:withExtension:@
urlForResource_withExtensionSelector :: Selector
urlForResource_withExtensionSelector = mkSelector "URLForResource:withExtension:"

-- | @Selector@ for @URLForResource:withExtension:subdirectory:@
urlForResource_withExtension_subdirectorySelector :: Selector
urlForResource_withExtension_subdirectorySelector = mkSelector "URLForResource:withExtension:subdirectory:"

-- | @Selector@ for @URLForResource:withExtension:subdirectory:localization:@
urlForResource_withExtension_subdirectory_localizationSelector :: Selector
urlForResource_withExtension_subdirectory_localizationSelector = mkSelector "URLForResource:withExtension:subdirectory:localization:"

-- | @Selector@ for @URLsForResourcesWithExtension:subdirectory:@
urLsForResourcesWithExtension_subdirectorySelector :: Selector
urLsForResourcesWithExtension_subdirectorySelector = mkSelector "URLsForResourcesWithExtension:subdirectory:"

-- | @Selector@ for @URLsForResourcesWithExtension:subdirectory:localization:@
urLsForResourcesWithExtension_subdirectory_localizationSelector :: Selector
urLsForResourcesWithExtension_subdirectory_localizationSelector = mkSelector "URLsForResourcesWithExtension:subdirectory:localization:"

-- | @Selector@ for @pathForResource:ofType:inDirectory:@
pathForResource_ofType_inDirectorySelector :: Selector
pathForResource_ofType_inDirectorySelector = mkSelector "pathForResource:ofType:inDirectory:"

-- | @Selector@ for @pathsForResourcesOfType:inDirectory:@
pathsForResourcesOfType_inDirectorySelector :: Selector
pathsForResourcesOfType_inDirectorySelector = mkSelector "pathsForResourcesOfType:inDirectory:"

-- | @Selector@ for @pathForResource:ofType:@
pathForResource_ofTypeSelector :: Selector
pathForResource_ofTypeSelector = mkSelector "pathForResource:ofType:"

-- | @Selector@ for @pathForResource:ofType:inDirectory:forLocalization:@
pathForResource_ofType_inDirectory_forLocalizationSelector :: Selector
pathForResource_ofType_inDirectory_forLocalizationSelector = mkSelector "pathForResource:ofType:inDirectory:forLocalization:"

-- | @Selector@ for @pathsForResourcesOfType:inDirectory:forLocalization:@
pathsForResourcesOfType_inDirectory_forLocalizationSelector :: Selector
pathsForResourcesOfType_inDirectory_forLocalizationSelector = mkSelector "pathsForResourcesOfType:inDirectory:forLocalization:"

-- | @Selector@ for @localizedStringForKey:value:table:@
localizedStringForKey_value_tableSelector :: Selector
localizedStringForKey_value_tableSelector = mkSelector "localizedStringForKey:value:table:"

-- | @Selector@ for @localizedAttributedStringForKey:value:table:@
localizedAttributedStringForKey_value_tableSelector :: Selector
localizedAttributedStringForKey_value_tableSelector = mkSelector "localizedAttributedStringForKey:value:table:"

-- | @Selector@ for @localizedStringForKey:value:table:localizations:@
localizedStringForKey_value_table_localizationsSelector :: Selector
localizedStringForKey_value_table_localizationsSelector = mkSelector "localizedStringForKey:value:table:localizations:"

-- | @Selector@ for @objectForInfoDictionaryKey:@
objectForInfoDictionaryKeySelector :: Selector
objectForInfoDictionaryKeySelector = mkSelector "objectForInfoDictionaryKey:"

-- | @Selector@ for @classNamed:@
classNamedSelector :: Selector
classNamedSelector = mkSelector "classNamed:"

-- | @Selector@ for @preferredLocalizationsFromArray:@
preferredLocalizationsFromArraySelector :: Selector
preferredLocalizationsFromArraySelector = mkSelector "preferredLocalizationsFromArray:"

-- | @Selector@ for @preferredLocalizationsFromArray:forPreferences:@
preferredLocalizationsFromArray_forPreferencesSelector :: Selector
preferredLocalizationsFromArray_forPreferencesSelector = mkSelector "preferredLocalizationsFromArray:forPreferences:"

-- | @Selector@ for @setPreservationPriority:forTags:@
setPreservationPriority_forTagsSelector :: Selector
setPreservationPriority_forTagsSelector = mkSelector "setPreservationPriority:forTags:"

-- | @Selector@ for @preservationPriorityForTag:@
preservationPriorityForTagSelector :: Selector
preservationPriorityForTagSelector = mkSelector "preservationPriorityForTag:"

-- | @Selector@ for @mainBundle@
mainBundleSelector :: Selector
mainBundleSelector = mkSelector "mainBundle"

-- | @Selector@ for @allBundles@
allBundlesSelector :: Selector
allBundlesSelector = mkSelector "allBundles"

-- | @Selector@ for @allFrameworks@
allFrameworksSelector :: Selector
allFrameworksSelector = mkSelector "allFrameworks"

-- | @Selector@ for @loaded@
loadedSelector :: Selector
loadedSelector = mkSelector "loaded"

-- | @Selector@ for @bundleURL@
bundleURLSelector :: Selector
bundleURLSelector = mkSelector "bundleURL"

-- | @Selector@ for @resourceURL@
resourceURLSelector :: Selector
resourceURLSelector = mkSelector "resourceURL"

-- | @Selector@ for @executableURL@
executableURLSelector :: Selector
executableURLSelector = mkSelector "executableURL"

-- | @Selector@ for @privateFrameworksURL@
privateFrameworksURLSelector :: Selector
privateFrameworksURLSelector = mkSelector "privateFrameworksURL"

-- | @Selector@ for @sharedFrameworksURL@
sharedFrameworksURLSelector :: Selector
sharedFrameworksURLSelector = mkSelector "sharedFrameworksURL"

-- | @Selector@ for @sharedSupportURL@
sharedSupportURLSelector :: Selector
sharedSupportURLSelector = mkSelector "sharedSupportURL"

-- | @Selector@ for @builtInPlugInsURL@
builtInPlugInsURLSelector :: Selector
builtInPlugInsURLSelector = mkSelector "builtInPlugInsURL"

-- | @Selector@ for @appStoreReceiptURL@
appStoreReceiptURLSelector :: Selector
appStoreReceiptURLSelector = mkSelector "appStoreReceiptURL"

-- | @Selector@ for @bundlePath@
bundlePathSelector :: Selector
bundlePathSelector = mkSelector "bundlePath"

-- | @Selector@ for @resourcePath@
resourcePathSelector :: Selector
resourcePathSelector = mkSelector "resourcePath"

-- | @Selector@ for @executablePath@
executablePathSelector :: Selector
executablePathSelector = mkSelector "executablePath"

-- | @Selector@ for @privateFrameworksPath@
privateFrameworksPathSelector :: Selector
privateFrameworksPathSelector = mkSelector "privateFrameworksPath"

-- | @Selector@ for @sharedFrameworksPath@
sharedFrameworksPathSelector :: Selector
sharedFrameworksPathSelector = mkSelector "sharedFrameworksPath"

-- | @Selector@ for @sharedSupportPath@
sharedSupportPathSelector :: Selector
sharedSupportPathSelector = mkSelector "sharedSupportPath"

-- | @Selector@ for @builtInPlugInsPath@
builtInPlugInsPathSelector :: Selector
builtInPlugInsPathSelector = mkSelector "builtInPlugInsPath"

-- | @Selector@ for @bundleIdentifier@
bundleIdentifierSelector :: Selector
bundleIdentifierSelector = mkSelector "bundleIdentifier"

-- | @Selector@ for @infoDictionary@
infoDictionarySelector :: Selector
infoDictionarySelector = mkSelector "infoDictionary"

-- | @Selector@ for @localizedInfoDictionary@
localizedInfoDictionarySelector :: Selector
localizedInfoDictionarySelector = mkSelector "localizedInfoDictionary"

-- | @Selector@ for @principalClass@
principalClassSelector :: Selector
principalClassSelector = mkSelector "principalClass"

-- | @Selector@ for @preferredLocalizations@
preferredLocalizationsSelector :: Selector
preferredLocalizationsSelector = mkSelector "preferredLocalizations"

-- | @Selector@ for @localizations@
localizationsSelector :: Selector
localizationsSelector = mkSelector "localizations"

-- | @Selector@ for @developmentLocalization@
developmentLocalizationSelector :: Selector
developmentLocalizationSelector = mkSelector "developmentLocalization"

-- | @Selector@ for @executableArchitectures@
executableArchitecturesSelector :: Selector
executableArchitecturesSelector = mkSelector "executableArchitectures"

