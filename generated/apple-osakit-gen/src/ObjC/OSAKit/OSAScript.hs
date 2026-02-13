{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @OSAScript@.
module ObjC.OSAKit.OSAScript
  ( OSAScript
  , IsOSAScript(..)
  , scriptDataDescriptorWithContentsOfURL
  , initWithSource
  , initWithSource_language
  , initWithSource_fromURL_languageInstance_usingStorageOptions
  , initWithContentsOfURL_error
  , initWithContentsOfURL_language_error
  , initWithContentsOfURL_languageInstance_usingStorageOptions_error
  , initWithCompiledData_error
  , initWithCompiledData_fromURL_usingStorageOptions_error
  , initWithScriptDataDescriptor_fromURL_languageInstance_usingStorageOptions_error
  , compileAndReturnError
  , executeAndReturnError
  , executeAppleEvent_error
  , executeAndReturnDisplayValue_error
  , executeHandlerWithName_arguments_error
  , richTextFromDescriptor
  , writeToURL_ofType_error
  , writeToURL_ofType_usingStorageOptions_error
  , compiledDataForType_usingStorageOptions_error
  , source
  , url
  , language
  , setLanguage
  , languageInstance
  , setLanguageInstance
  , compiled
  , richTextSource
  , compileAndReturnErrorSelector
  , compiledDataForType_usingStorageOptions_errorSelector
  , compiledSelector
  , executeAndReturnDisplayValue_errorSelector
  , executeAndReturnErrorSelector
  , executeAppleEvent_errorSelector
  , executeHandlerWithName_arguments_errorSelector
  , initWithCompiledData_errorSelector
  , initWithCompiledData_fromURL_usingStorageOptions_errorSelector
  , initWithContentsOfURL_errorSelector
  , initWithContentsOfURL_languageInstance_usingStorageOptions_errorSelector
  , initWithContentsOfURL_language_errorSelector
  , initWithScriptDataDescriptor_fromURL_languageInstance_usingStorageOptions_errorSelector
  , initWithSourceSelector
  , initWithSource_fromURL_languageInstance_usingStorageOptionsSelector
  , initWithSource_languageSelector
  , languageInstanceSelector
  , languageSelector
  , richTextFromDescriptorSelector
  , richTextSourceSelector
  , scriptDataDescriptorWithContentsOfURLSelector
  , setLanguageInstanceSelector
  , setLanguageSelector
  , sourceSelector
  , urlSelector
  , writeToURL_ofType_errorSelector
  , writeToURL_ofType_usingStorageOptions_errorSelector

  -- * Enum types
  , OSAStorageOptions(OSAStorageOptions)
  , pattern OSANull
  , pattern OSAPreventGetSource
  , pattern OSACompileIntoContext
  , pattern OSADontSetScriptLocation
  , pattern OSAStayOpenApplet
  , pattern OSAShowStartupScreen

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.OSAKit.Internal.Classes
import ObjC.OSAKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ scriptDataDescriptorWithContentsOfURL:@
scriptDataDescriptorWithContentsOfURL :: IsNSURL url => url -> IO (Id NSAppleEventDescriptor)
scriptDataDescriptorWithContentsOfURL url =
  do
    cls' <- getRequiredClass "OSAScript"
    sendClassMessage cls' scriptDataDescriptorWithContentsOfURLSelector (toNSURL url)

-- | @- initWithSource:@
initWithSource :: (IsOSAScript osaScript, IsNSString source) => osaScript -> source -> IO (Id OSAScript)
initWithSource osaScript source =
  sendOwnedMessage osaScript initWithSourceSelector (toNSString source)

-- | @- initWithSource:language:@
initWithSource_language :: (IsOSAScript osaScript, IsNSString source, IsOSALanguage language) => osaScript -> source -> language -> IO (Id OSAScript)
initWithSource_language osaScript source language =
  sendOwnedMessage osaScript initWithSource_languageSelector (toNSString source) (toOSALanguage language)

-- | @- initWithSource:fromURL:languageInstance:usingStorageOptions:@
initWithSource_fromURL_languageInstance_usingStorageOptions :: (IsOSAScript osaScript, IsNSString source, IsNSURL url, IsOSALanguageInstance instance_) => osaScript -> source -> url -> instance_ -> OSAStorageOptions -> IO (Id OSAScript)
initWithSource_fromURL_languageInstance_usingStorageOptions osaScript source url instance_ storageOptions =
  sendOwnedMessage osaScript initWithSource_fromURL_languageInstance_usingStorageOptionsSelector (toNSString source) (toNSURL url) (toOSALanguageInstance instance_) storageOptions

-- | @- initWithContentsOfURL:error:@
initWithContentsOfURL_error :: (IsOSAScript osaScript, IsNSURL url, IsNSDictionary errorInfo) => osaScript -> url -> errorInfo -> IO (Id OSAScript)
initWithContentsOfURL_error osaScript url errorInfo =
  sendOwnedMessage osaScript initWithContentsOfURL_errorSelector (toNSURL url) (toNSDictionary errorInfo)

-- | @- initWithContentsOfURL:language:error:@
initWithContentsOfURL_language_error :: (IsOSAScript osaScript, IsNSURL url, IsOSALanguage language, IsNSDictionary errorInfo) => osaScript -> url -> language -> errorInfo -> IO RawId
initWithContentsOfURL_language_error osaScript url language errorInfo =
  sendOwnedMessage osaScript initWithContentsOfURL_language_errorSelector (toNSURL url) (toOSALanguage language) (toNSDictionary errorInfo)

-- | @- initWithContentsOfURL:languageInstance:usingStorageOptions:error:@
initWithContentsOfURL_languageInstance_usingStorageOptions_error :: (IsOSAScript osaScript, IsNSURL url, IsOSALanguageInstance instance_, IsNSError errorInfo) => osaScript -> url -> instance_ -> OSAStorageOptions -> errorInfo -> IO (Id OSAScript)
initWithContentsOfURL_languageInstance_usingStorageOptions_error osaScript url instance_ storageOptions errorInfo =
  sendOwnedMessage osaScript initWithContentsOfURL_languageInstance_usingStorageOptions_errorSelector (toNSURL url) (toOSALanguageInstance instance_) storageOptions (toNSError errorInfo)

-- | @- initWithCompiledData:error:@
initWithCompiledData_error :: (IsOSAScript osaScript, IsNSData data_, IsNSDictionary errorInfo) => osaScript -> data_ -> errorInfo -> IO RawId
initWithCompiledData_error osaScript data_ errorInfo =
  sendOwnedMessage osaScript initWithCompiledData_errorSelector (toNSData data_) (toNSDictionary errorInfo)

-- | @- initWithCompiledData:fromURL:usingStorageOptions:error:@
initWithCompiledData_fromURL_usingStorageOptions_error :: (IsOSAScript osaScript, IsNSData data_, IsNSURL url, IsNSError errorInfo) => osaScript -> data_ -> url -> OSAStorageOptions -> errorInfo -> IO (Id OSAScript)
initWithCompiledData_fromURL_usingStorageOptions_error osaScript data_ url storageOptions errorInfo =
  sendOwnedMessage osaScript initWithCompiledData_fromURL_usingStorageOptions_errorSelector (toNSData data_) (toNSURL url) storageOptions (toNSError errorInfo)

-- | @- initWithScriptDataDescriptor:fromURL:languageInstance:usingStorageOptions:error:@
initWithScriptDataDescriptor_fromURL_languageInstance_usingStorageOptions_error :: (IsOSAScript osaScript, IsNSAppleEventDescriptor data_, IsNSURL url, IsOSALanguageInstance instance_, IsNSError errorInfo) => osaScript -> data_ -> url -> instance_ -> OSAStorageOptions -> errorInfo -> IO (Id OSAScript)
initWithScriptDataDescriptor_fromURL_languageInstance_usingStorageOptions_error osaScript data_ url instance_ storageOptions errorInfo =
  sendOwnedMessage osaScript initWithScriptDataDescriptor_fromURL_languageInstance_usingStorageOptions_errorSelector (toNSAppleEventDescriptor data_) (toNSURL url) (toOSALanguageInstance instance_) storageOptions (toNSError errorInfo)

-- | @- compileAndReturnError:@
compileAndReturnError :: (IsOSAScript osaScript, IsNSDictionary errorInfo) => osaScript -> errorInfo -> IO Bool
compileAndReturnError osaScript errorInfo =
  sendMessage osaScript compileAndReturnErrorSelector (toNSDictionary errorInfo)

-- | @- executeAndReturnError:@
executeAndReturnError :: (IsOSAScript osaScript, IsNSDictionary errorInfo) => osaScript -> errorInfo -> IO (Id NSAppleEventDescriptor)
executeAndReturnError osaScript errorInfo =
  sendMessage osaScript executeAndReturnErrorSelector (toNSDictionary errorInfo)

-- | @- executeAppleEvent:error:@
executeAppleEvent_error :: (IsOSAScript osaScript, IsNSAppleEventDescriptor event, IsNSDictionary errorInfo) => osaScript -> event -> errorInfo -> IO (Id NSAppleEventDescriptor)
executeAppleEvent_error osaScript event errorInfo =
  sendMessage osaScript executeAppleEvent_errorSelector (toNSAppleEventDescriptor event) (toNSDictionary errorInfo)

-- | @- executeAndReturnDisplayValue:error:@
executeAndReturnDisplayValue_error :: (IsOSAScript osaScript, IsNSAttributedString displayValue, IsNSDictionary errorInfo) => osaScript -> displayValue -> errorInfo -> IO (Id NSAppleEventDescriptor)
executeAndReturnDisplayValue_error osaScript displayValue errorInfo =
  sendMessage osaScript executeAndReturnDisplayValue_errorSelector (toNSAttributedString displayValue) (toNSDictionary errorInfo)

-- | @- executeHandlerWithName:arguments:error:@
executeHandlerWithName_arguments_error :: (IsOSAScript osaScript, IsNSString name, IsNSArray arguments, IsNSDictionary errorInfo) => osaScript -> name -> arguments -> errorInfo -> IO (Id NSAppleEventDescriptor)
executeHandlerWithName_arguments_error osaScript name arguments errorInfo =
  sendMessage osaScript executeHandlerWithName_arguments_errorSelector (toNSString name) (toNSArray arguments) (toNSDictionary errorInfo)

-- | @- richTextFromDescriptor:@
richTextFromDescriptor :: (IsOSAScript osaScript, IsNSAppleEventDescriptor descriptor) => osaScript -> descriptor -> IO (Id NSAttributedString)
richTextFromDescriptor osaScript descriptor =
  sendMessage osaScript richTextFromDescriptorSelector (toNSAppleEventDescriptor descriptor)

-- | @- writeToURL:ofType:error:@
writeToURL_ofType_error :: (IsOSAScript osaScript, IsNSURL url, IsNSString type_, IsNSDictionary errorInfo) => osaScript -> url -> type_ -> errorInfo -> IO Bool
writeToURL_ofType_error osaScript url type_ errorInfo =
  sendMessage osaScript writeToURL_ofType_errorSelector (toNSURL url) (toNSString type_) (toNSDictionary errorInfo)

-- | @- writeToURL:ofType:usingStorageOptions:error:@
writeToURL_ofType_usingStorageOptions_error :: (IsOSAScript osaScript, IsNSURL url, IsNSString type_, IsNSDictionary errorInfo) => osaScript -> url -> type_ -> OSAStorageOptions -> errorInfo -> IO Bool
writeToURL_ofType_usingStorageOptions_error osaScript url type_ storageOptions errorInfo =
  sendMessage osaScript writeToURL_ofType_usingStorageOptions_errorSelector (toNSURL url) (toNSString type_) storageOptions (toNSDictionary errorInfo)

-- | @- compiledDataForType:usingStorageOptions:error:@
compiledDataForType_usingStorageOptions_error :: (IsOSAScript osaScript, IsNSString type_, IsNSDictionary errorInfo) => osaScript -> type_ -> OSAStorageOptions -> errorInfo -> IO (Id NSData)
compiledDataForType_usingStorageOptions_error osaScript type_ storageOptions errorInfo =
  sendMessage osaScript compiledDataForType_usingStorageOptions_errorSelector (toNSString type_) storageOptions (toNSDictionary errorInfo)

-- | @- source@
source :: IsOSAScript osaScript => osaScript -> IO (Id NSString)
source osaScript =
  sendMessage osaScript sourceSelector

-- | @- url@
url :: IsOSAScript osaScript => osaScript -> IO (Id NSURL)
url osaScript =
  sendMessage osaScript urlSelector

-- | @- language@
language :: IsOSAScript osaScript => osaScript -> IO (Id OSALanguage)
language osaScript =
  sendMessage osaScript languageSelector

-- | @- setLanguage:@
setLanguage :: (IsOSAScript osaScript, IsOSALanguage value) => osaScript -> value -> IO ()
setLanguage osaScript value =
  sendMessage osaScript setLanguageSelector (toOSALanguage value)

-- | @- languageInstance@
languageInstance :: IsOSAScript osaScript => osaScript -> IO RawId
languageInstance osaScript =
  sendMessage osaScript languageInstanceSelector

-- | @- setLanguageInstance:@
setLanguageInstance :: IsOSAScript osaScript => osaScript -> RawId -> IO ()
setLanguageInstance osaScript value =
  sendMessage osaScript setLanguageInstanceSelector value

-- | @- compiled@
compiled :: IsOSAScript osaScript => osaScript -> IO Bool
compiled osaScript =
  sendMessage osaScript compiledSelector

-- | @- richTextSource@
richTextSource :: IsOSAScript osaScript => osaScript -> IO (Id NSAttributedString)
richTextSource osaScript =
  sendMessage osaScript richTextSourceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @scriptDataDescriptorWithContentsOfURL:@
scriptDataDescriptorWithContentsOfURLSelector :: Selector '[Id NSURL] (Id NSAppleEventDescriptor)
scriptDataDescriptorWithContentsOfURLSelector = mkSelector "scriptDataDescriptorWithContentsOfURL:"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector '[Id NSString] (Id OSAScript)
initWithSourceSelector = mkSelector "initWithSource:"

-- | @Selector@ for @initWithSource:language:@
initWithSource_languageSelector :: Selector '[Id NSString, Id OSALanguage] (Id OSAScript)
initWithSource_languageSelector = mkSelector "initWithSource:language:"

-- | @Selector@ for @initWithSource:fromURL:languageInstance:usingStorageOptions:@
initWithSource_fromURL_languageInstance_usingStorageOptionsSelector :: Selector '[Id NSString, Id NSURL, Id OSALanguageInstance, OSAStorageOptions] (Id OSAScript)
initWithSource_fromURL_languageInstance_usingStorageOptionsSelector = mkSelector "initWithSource:fromURL:languageInstance:usingStorageOptions:"

-- | @Selector@ for @initWithContentsOfURL:error:@
initWithContentsOfURL_errorSelector :: Selector '[Id NSURL, Id NSDictionary] (Id OSAScript)
initWithContentsOfURL_errorSelector = mkSelector "initWithContentsOfURL:error:"

-- | @Selector@ for @initWithContentsOfURL:language:error:@
initWithContentsOfURL_language_errorSelector :: Selector '[Id NSURL, Id OSALanguage, Id NSDictionary] RawId
initWithContentsOfURL_language_errorSelector = mkSelector "initWithContentsOfURL:language:error:"

-- | @Selector@ for @initWithContentsOfURL:languageInstance:usingStorageOptions:error:@
initWithContentsOfURL_languageInstance_usingStorageOptions_errorSelector :: Selector '[Id NSURL, Id OSALanguageInstance, OSAStorageOptions, Id NSError] (Id OSAScript)
initWithContentsOfURL_languageInstance_usingStorageOptions_errorSelector = mkSelector "initWithContentsOfURL:languageInstance:usingStorageOptions:error:"

-- | @Selector@ for @initWithCompiledData:error:@
initWithCompiledData_errorSelector :: Selector '[Id NSData, Id NSDictionary] RawId
initWithCompiledData_errorSelector = mkSelector "initWithCompiledData:error:"

-- | @Selector@ for @initWithCompiledData:fromURL:usingStorageOptions:error:@
initWithCompiledData_fromURL_usingStorageOptions_errorSelector :: Selector '[Id NSData, Id NSURL, OSAStorageOptions, Id NSError] (Id OSAScript)
initWithCompiledData_fromURL_usingStorageOptions_errorSelector = mkSelector "initWithCompiledData:fromURL:usingStorageOptions:error:"

-- | @Selector@ for @initWithScriptDataDescriptor:fromURL:languageInstance:usingStorageOptions:error:@
initWithScriptDataDescriptor_fromURL_languageInstance_usingStorageOptions_errorSelector :: Selector '[Id NSAppleEventDescriptor, Id NSURL, Id OSALanguageInstance, OSAStorageOptions, Id NSError] (Id OSAScript)
initWithScriptDataDescriptor_fromURL_languageInstance_usingStorageOptions_errorSelector = mkSelector "initWithScriptDataDescriptor:fromURL:languageInstance:usingStorageOptions:error:"

-- | @Selector@ for @compileAndReturnError:@
compileAndReturnErrorSelector :: Selector '[Id NSDictionary] Bool
compileAndReturnErrorSelector = mkSelector "compileAndReturnError:"

-- | @Selector@ for @executeAndReturnError:@
executeAndReturnErrorSelector :: Selector '[Id NSDictionary] (Id NSAppleEventDescriptor)
executeAndReturnErrorSelector = mkSelector "executeAndReturnError:"

-- | @Selector@ for @executeAppleEvent:error:@
executeAppleEvent_errorSelector :: Selector '[Id NSAppleEventDescriptor, Id NSDictionary] (Id NSAppleEventDescriptor)
executeAppleEvent_errorSelector = mkSelector "executeAppleEvent:error:"

-- | @Selector@ for @executeAndReturnDisplayValue:error:@
executeAndReturnDisplayValue_errorSelector :: Selector '[Id NSAttributedString, Id NSDictionary] (Id NSAppleEventDescriptor)
executeAndReturnDisplayValue_errorSelector = mkSelector "executeAndReturnDisplayValue:error:"

-- | @Selector@ for @executeHandlerWithName:arguments:error:@
executeHandlerWithName_arguments_errorSelector :: Selector '[Id NSString, Id NSArray, Id NSDictionary] (Id NSAppleEventDescriptor)
executeHandlerWithName_arguments_errorSelector = mkSelector "executeHandlerWithName:arguments:error:"

-- | @Selector@ for @richTextFromDescriptor:@
richTextFromDescriptorSelector :: Selector '[Id NSAppleEventDescriptor] (Id NSAttributedString)
richTextFromDescriptorSelector = mkSelector "richTextFromDescriptor:"

-- | @Selector@ for @writeToURL:ofType:error:@
writeToURL_ofType_errorSelector :: Selector '[Id NSURL, Id NSString, Id NSDictionary] Bool
writeToURL_ofType_errorSelector = mkSelector "writeToURL:ofType:error:"

-- | @Selector@ for @writeToURL:ofType:usingStorageOptions:error:@
writeToURL_ofType_usingStorageOptions_errorSelector :: Selector '[Id NSURL, Id NSString, OSAStorageOptions, Id NSDictionary] Bool
writeToURL_ofType_usingStorageOptions_errorSelector = mkSelector "writeToURL:ofType:usingStorageOptions:error:"

-- | @Selector@ for @compiledDataForType:usingStorageOptions:error:@
compiledDataForType_usingStorageOptions_errorSelector :: Selector '[Id NSString, OSAStorageOptions, Id NSDictionary] (Id NSData)
compiledDataForType_usingStorageOptions_errorSelector = mkSelector "compiledDataForType:usingStorageOptions:error:"

-- | @Selector@ for @source@
sourceSelector :: Selector '[] (Id NSString)
sourceSelector = mkSelector "source"

-- | @Selector@ for @url@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "url"

-- | @Selector@ for @language@
languageSelector :: Selector '[] (Id OSALanguage)
languageSelector = mkSelector "language"

-- | @Selector@ for @setLanguage:@
setLanguageSelector :: Selector '[Id OSALanguage] ()
setLanguageSelector = mkSelector "setLanguage:"

-- | @Selector@ for @languageInstance@
languageInstanceSelector :: Selector '[] RawId
languageInstanceSelector = mkSelector "languageInstance"

-- | @Selector@ for @setLanguageInstance:@
setLanguageInstanceSelector :: Selector '[RawId] ()
setLanguageInstanceSelector = mkSelector "setLanguageInstance:"

-- | @Selector@ for @compiled@
compiledSelector :: Selector '[] Bool
compiledSelector = mkSelector "compiled"

-- | @Selector@ for @richTextSource@
richTextSourceSelector :: Selector '[] (Id NSAttributedString)
richTextSourceSelector = mkSelector "richTextSource"

