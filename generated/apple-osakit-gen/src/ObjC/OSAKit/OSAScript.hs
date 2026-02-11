{-# LANGUAGE PatternSynonyms #-}
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
  , scriptDataDescriptorWithContentsOfURLSelector
  , initWithSourceSelector
  , initWithSource_languageSelector
  , initWithSource_fromURL_languageInstance_usingStorageOptionsSelector
  , initWithContentsOfURL_errorSelector
  , initWithContentsOfURL_language_errorSelector
  , initWithContentsOfURL_languageInstance_usingStorageOptions_errorSelector
  , initWithCompiledData_errorSelector
  , initWithCompiledData_fromURL_usingStorageOptions_errorSelector
  , initWithScriptDataDescriptor_fromURL_languageInstance_usingStorageOptions_errorSelector
  , compileAndReturnErrorSelector
  , executeAndReturnErrorSelector
  , executeAppleEvent_errorSelector
  , executeAndReturnDisplayValue_errorSelector
  , executeHandlerWithName_arguments_errorSelector
  , richTextFromDescriptorSelector
  , writeToURL_ofType_errorSelector
  , writeToURL_ofType_usingStorageOptions_errorSelector
  , compiledDataForType_usingStorageOptions_errorSelector
  , sourceSelector
  , urlSelector
  , languageSelector
  , setLanguageSelector
  , languageInstanceSelector
  , setLanguageInstanceSelector
  , compiledSelector
  , richTextSourceSelector

  -- * Enum types
  , OSAStorageOptions(OSAStorageOptions)
  , pattern OSANull
  , pattern OSAPreventGetSource
  , pattern OSACompileIntoContext
  , pattern OSADontSetScriptLocation
  , pattern OSAStayOpenApplet
  , pattern OSAShowStartupScreen

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

import ObjC.OSAKit.Internal.Classes
import ObjC.OSAKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ scriptDataDescriptorWithContentsOfURL:@
scriptDataDescriptorWithContentsOfURL :: IsNSURL url => url -> IO (Id NSAppleEventDescriptor)
scriptDataDescriptorWithContentsOfURL url =
  do
    cls' <- getRequiredClass "OSAScript"
    withObjCPtr url $ \raw_url ->
      sendClassMsg cls' (mkSelector "scriptDataDescriptorWithContentsOfURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithSource:@
initWithSource :: (IsOSAScript osaScript, IsNSString source) => osaScript -> source -> IO (Id OSAScript)
initWithSource osaScript  source =
  withObjCPtr source $ \raw_source ->
      sendMsg osaScript (mkSelector "initWithSource:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithSource:language:@
initWithSource_language :: (IsOSAScript osaScript, IsNSString source, IsOSALanguage language) => osaScript -> source -> language -> IO (Id OSAScript)
initWithSource_language osaScript  source language =
  withObjCPtr source $ \raw_source ->
    withObjCPtr language $ \raw_language ->
        sendMsg osaScript (mkSelector "initWithSource:language:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ()), argPtr (castPtr raw_language :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithSource:fromURL:languageInstance:usingStorageOptions:@
initWithSource_fromURL_languageInstance_usingStorageOptions :: (IsOSAScript osaScript, IsNSString source, IsNSURL url, IsOSALanguageInstance instance_) => osaScript -> source -> url -> instance_ -> OSAStorageOptions -> IO (Id OSAScript)
initWithSource_fromURL_languageInstance_usingStorageOptions osaScript  source url instance_ storageOptions =
  withObjCPtr source $ \raw_source ->
    withObjCPtr url $ \raw_url ->
      withObjCPtr instance_ $ \raw_instance_ ->
          sendMsg osaScript (mkSelector "initWithSource:fromURL:languageInstance:usingStorageOptions:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ()), argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_instance_ :: Ptr ()), argCULong (coerce storageOptions)] >>= ownedObject . castPtr

-- | @- initWithContentsOfURL:error:@
initWithContentsOfURL_error :: (IsOSAScript osaScript, IsNSURL url, IsNSDictionary errorInfo) => osaScript -> url -> errorInfo -> IO (Id OSAScript)
initWithContentsOfURL_error osaScript  url errorInfo =
  withObjCPtr url $ \raw_url ->
    withObjCPtr errorInfo $ \raw_errorInfo ->
        sendMsg osaScript (mkSelector "initWithContentsOfURL:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_errorInfo :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithContentsOfURL:language:error:@
initWithContentsOfURL_language_error :: (IsOSAScript osaScript, IsNSURL url, IsOSALanguage language, IsNSDictionary errorInfo) => osaScript -> url -> language -> errorInfo -> IO RawId
initWithContentsOfURL_language_error osaScript  url language errorInfo =
  withObjCPtr url $ \raw_url ->
    withObjCPtr language $ \raw_language ->
      withObjCPtr errorInfo $ \raw_errorInfo ->
          fmap (RawId . castPtr) $ sendMsg osaScript (mkSelector "initWithContentsOfURL:language:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_language :: Ptr ()), argPtr (castPtr raw_errorInfo :: Ptr ())]

-- | @- initWithContentsOfURL:languageInstance:usingStorageOptions:error:@
initWithContentsOfURL_languageInstance_usingStorageOptions_error :: (IsOSAScript osaScript, IsNSURL url, IsOSALanguageInstance instance_, IsNSError errorInfo) => osaScript -> url -> instance_ -> OSAStorageOptions -> errorInfo -> IO (Id OSAScript)
initWithContentsOfURL_languageInstance_usingStorageOptions_error osaScript  url instance_ storageOptions errorInfo =
  withObjCPtr url $ \raw_url ->
    withObjCPtr instance_ $ \raw_instance_ ->
      withObjCPtr errorInfo $ \raw_errorInfo ->
          sendMsg osaScript (mkSelector "initWithContentsOfURL:languageInstance:usingStorageOptions:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_instance_ :: Ptr ()), argCULong (coerce storageOptions), argPtr (castPtr raw_errorInfo :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCompiledData:error:@
initWithCompiledData_error :: (IsOSAScript osaScript, IsNSData data_, IsNSDictionary errorInfo) => osaScript -> data_ -> errorInfo -> IO RawId
initWithCompiledData_error osaScript  data_ errorInfo =
  withObjCPtr data_ $ \raw_data_ ->
    withObjCPtr errorInfo $ \raw_errorInfo ->
        fmap (RawId . castPtr) $ sendMsg osaScript (mkSelector "initWithCompiledData:error:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_errorInfo :: Ptr ())]

-- | @- initWithCompiledData:fromURL:usingStorageOptions:error:@
initWithCompiledData_fromURL_usingStorageOptions_error :: (IsOSAScript osaScript, IsNSData data_, IsNSURL url, IsNSError errorInfo) => osaScript -> data_ -> url -> OSAStorageOptions -> errorInfo -> IO (Id OSAScript)
initWithCompiledData_fromURL_usingStorageOptions_error osaScript  data_ url storageOptions errorInfo =
  withObjCPtr data_ $ \raw_data_ ->
    withObjCPtr url $ \raw_url ->
      withObjCPtr errorInfo $ \raw_errorInfo ->
          sendMsg osaScript (mkSelector "initWithCompiledData:fromURL:usingStorageOptions:error:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_url :: Ptr ()), argCULong (coerce storageOptions), argPtr (castPtr raw_errorInfo :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithScriptDataDescriptor:fromURL:languageInstance:usingStorageOptions:error:@
initWithScriptDataDescriptor_fromURL_languageInstance_usingStorageOptions_error :: (IsOSAScript osaScript, IsNSAppleEventDescriptor data_, IsNSURL url, IsOSALanguageInstance instance_, IsNSError errorInfo) => osaScript -> data_ -> url -> instance_ -> OSAStorageOptions -> errorInfo -> IO (Id OSAScript)
initWithScriptDataDescriptor_fromURL_languageInstance_usingStorageOptions_error osaScript  data_ url instance_ storageOptions errorInfo =
  withObjCPtr data_ $ \raw_data_ ->
    withObjCPtr url $ \raw_url ->
      withObjCPtr instance_ $ \raw_instance_ ->
        withObjCPtr errorInfo $ \raw_errorInfo ->
            sendMsg osaScript (mkSelector "initWithScriptDataDescriptor:fromURL:languageInstance:usingStorageOptions:error:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_instance_ :: Ptr ()), argCULong (coerce storageOptions), argPtr (castPtr raw_errorInfo :: Ptr ())] >>= ownedObject . castPtr

-- | @- compileAndReturnError:@
compileAndReturnError :: (IsOSAScript osaScript, IsNSDictionary errorInfo) => osaScript -> errorInfo -> IO Bool
compileAndReturnError osaScript  errorInfo =
  withObjCPtr errorInfo $ \raw_errorInfo ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg osaScript (mkSelector "compileAndReturnError:") retCULong [argPtr (castPtr raw_errorInfo :: Ptr ())]

-- | @- executeAndReturnError:@
executeAndReturnError :: (IsOSAScript osaScript, IsNSDictionary errorInfo) => osaScript -> errorInfo -> IO (Id NSAppleEventDescriptor)
executeAndReturnError osaScript  errorInfo =
  withObjCPtr errorInfo $ \raw_errorInfo ->
      sendMsg osaScript (mkSelector "executeAndReturnError:") (retPtr retVoid) [argPtr (castPtr raw_errorInfo :: Ptr ())] >>= retainedObject . castPtr

-- | @- executeAppleEvent:error:@
executeAppleEvent_error :: (IsOSAScript osaScript, IsNSAppleEventDescriptor event, IsNSDictionary errorInfo) => osaScript -> event -> errorInfo -> IO (Id NSAppleEventDescriptor)
executeAppleEvent_error osaScript  event errorInfo =
  withObjCPtr event $ \raw_event ->
    withObjCPtr errorInfo $ \raw_errorInfo ->
        sendMsg osaScript (mkSelector "executeAppleEvent:error:") (retPtr retVoid) [argPtr (castPtr raw_event :: Ptr ()), argPtr (castPtr raw_errorInfo :: Ptr ())] >>= retainedObject . castPtr

-- | @- executeAndReturnDisplayValue:error:@
executeAndReturnDisplayValue_error :: (IsOSAScript osaScript, IsNSAttributedString displayValue, IsNSDictionary errorInfo) => osaScript -> displayValue -> errorInfo -> IO (Id NSAppleEventDescriptor)
executeAndReturnDisplayValue_error osaScript  displayValue errorInfo =
  withObjCPtr displayValue $ \raw_displayValue ->
    withObjCPtr errorInfo $ \raw_errorInfo ->
        sendMsg osaScript (mkSelector "executeAndReturnDisplayValue:error:") (retPtr retVoid) [argPtr (castPtr raw_displayValue :: Ptr ()), argPtr (castPtr raw_errorInfo :: Ptr ())] >>= retainedObject . castPtr

-- | @- executeHandlerWithName:arguments:error:@
executeHandlerWithName_arguments_error :: (IsOSAScript osaScript, IsNSString name, IsNSArray arguments, IsNSDictionary errorInfo) => osaScript -> name -> arguments -> errorInfo -> IO (Id NSAppleEventDescriptor)
executeHandlerWithName_arguments_error osaScript  name arguments errorInfo =
  withObjCPtr name $ \raw_name ->
    withObjCPtr arguments $ \raw_arguments ->
      withObjCPtr errorInfo $ \raw_errorInfo ->
          sendMsg osaScript (mkSelector "executeHandlerWithName:arguments:error:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_arguments :: Ptr ()), argPtr (castPtr raw_errorInfo :: Ptr ())] >>= retainedObject . castPtr

-- | @- richTextFromDescriptor:@
richTextFromDescriptor :: (IsOSAScript osaScript, IsNSAppleEventDescriptor descriptor) => osaScript -> descriptor -> IO (Id NSAttributedString)
richTextFromDescriptor osaScript  descriptor =
  withObjCPtr descriptor $ \raw_descriptor ->
      sendMsg osaScript (mkSelector "richTextFromDescriptor:") (retPtr retVoid) [argPtr (castPtr raw_descriptor :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeToURL:ofType:error:@
writeToURL_ofType_error :: (IsOSAScript osaScript, IsNSURL url, IsNSString type_, IsNSDictionary errorInfo) => osaScript -> url -> type_ -> errorInfo -> IO Bool
writeToURL_ofType_error osaScript  url type_ errorInfo =
  withObjCPtr url $ \raw_url ->
    withObjCPtr type_ $ \raw_type_ ->
      withObjCPtr errorInfo $ \raw_errorInfo ->
          fmap ((/= 0) :: CULong -> Bool) $ sendMsg osaScript (mkSelector "writeToURL:ofType:error:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_type_ :: Ptr ()), argPtr (castPtr raw_errorInfo :: Ptr ())]

-- | @- writeToURL:ofType:usingStorageOptions:error:@
writeToURL_ofType_usingStorageOptions_error :: (IsOSAScript osaScript, IsNSURL url, IsNSString type_, IsNSDictionary errorInfo) => osaScript -> url -> type_ -> OSAStorageOptions -> errorInfo -> IO Bool
writeToURL_ofType_usingStorageOptions_error osaScript  url type_ storageOptions errorInfo =
  withObjCPtr url $ \raw_url ->
    withObjCPtr type_ $ \raw_type_ ->
      withObjCPtr errorInfo $ \raw_errorInfo ->
          fmap ((/= 0) :: CULong -> Bool) $ sendMsg osaScript (mkSelector "writeToURL:ofType:usingStorageOptions:error:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_type_ :: Ptr ()), argCULong (coerce storageOptions), argPtr (castPtr raw_errorInfo :: Ptr ())]

-- | @- compiledDataForType:usingStorageOptions:error:@
compiledDataForType_usingStorageOptions_error :: (IsOSAScript osaScript, IsNSString type_, IsNSDictionary errorInfo) => osaScript -> type_ -> OSAStorageOptions -> errorInfo -> IO (Id NSData)
compiledDataForType_usingStorageOptions_error osaScript  type_ storageOptions errorInfo =
  withObjCPtr type_ $ \raw_type_ ->
    withObjCPtr errorInfo $ \raw_errorInfo ->
        sendMsg osaScript (mkSelector "compiledDataForType:usingStorageOptions:error:") (retPtr retVoid) [argPtr (castPtr raw_type_ :: Ptr ()), argCULong (coerce storageOptions), argPtr (castPtr raw_errorInfo :: Ptr ())] >>= retainedObject . castPtr

-- | @- source@
source :: IsOSAScript osaScript => osaScript -> IO (Id NSString)
source osaScript  =
    sendMsg osaScript (mkSelector "source") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- url@
url :: IsOSAScript osaScript => osaScript -> IO (Id NSURL)
url osaScript  =
    sendMsg osaScript (mkSelector "url") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- language@
language :: IsOSAScript osaScript => osaScript -> IO (Id OSALanguage)
language osaScript  =
    sendMsg osaScript (mkSelector "language") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLanguage:@
setLanguage :: (IsOSAScript osaScript, IsOSALanguage value) => osaScript -> value -> IO ()
setLanguage osaScript  value =
  withObjCPtr value $ \raw_value ->
      sendMsg osaScript (mkSelector "setLanguage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- languageInstance@
languageInstance :: IsOSAScript osaScript => osaScript -> IO RawId
languageInstance osaScript  =
    fmap (RawId . castPtr) $ sendMsg osaScript (mkSelector "languageInstance") (retPtr retVoid) []

-- | @- setLanguageInstance:@
setLanguageInstance :: IsOSAScript osaScript => osaScript -> RawId -> IO ()
setLanguageInstance osaScript  value =
    sendMsg osaScript (mkSelector "setLanguageInstance:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- compiled@
compiled :: IsOSAScript osaScript => osaScript -> IO Bool
compiled osaScript  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg osaScript (mkSelector "compiled") retCULong []

-- | @- richTextSource@
richTextSource :: IsOSAScript osaScript => osaScript -> IO (Id NSAttributedString)
richTextSource osaScript  =
    sendMsg osaScript (mkSelector "richTextSource") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @scriptDataDescriptorWithContentsOfURL:@
scriptDataDescriptorWithContentsOfURLSelector :: Selector
scriptDataDescriptorWithContentsOfURLSelector = mkSelector "scriptDataDescriptorWithContentsOfURL:"

-- | @Selector@ for @initWithSource:@
initWithSourceSelector :: Selector
initWithSourceSelector = mkSelector "initWithSource:"

-- | @Selector@ for @initWithSource:language:@
initWithSource_languageSelector :: Selector
initWithSource_languageSelector = mkSelector "initWithSource:language:"

-- | @Selector@ for @initWithSource:fromURL:languageInstance:usingStorageOptions:@
initWithSource_fromURL_languageInstance_usingStorageOptionsSelector :: Selector
initWithSource_fromURL_languageInstance_usingStorageOptionsSelector = mkSelector "initWithSource:fromURL:languageInstance:usingStorageOptions:"

-- | @Selector@ for @initWithContentsOfURL:error:@
initWithContentsOfURL_errorSelector :: Selector
initWithContentsOfURL_errorSelector = mkSelector "initWithContentsOfURL:error:"

-- | @Selector@ for @initWithContentsOfURL:language:error:@
initWithContentsOfURL_language_errorSelector :: Selector
initWithContentsOfURL_language_errorSelector = mkSelector "initWithContentsOfURL:language:error:"

-- | @Selector@ for @initWithContentsOfURL:languageInstance:usingStorageOptions:error:@
initWithContentsOfURL_languageInstance_usingStorageOptions_errorSelector :: Selector
initWithContentsOfURL_languageInstance_usingStorageOptions_errorSelector = mkSelector "initWithContentsOfURL:languageInstance:usingStorageOptions:error:"

-- | @Selector@ for @initWithCompiledData:error:@
initWithCompiledData_errorSelector :: Selector
initWithCompiledData_errorSelector = mkSelector "initWithCompiledData:error:"

-- | @Selector@ for @initWithCompiledData:fromURL:usingStorageOptions:error:@
initWithCompiledData_fromURL_usingStorageOptions_errorSelector :: Selector
initWithCompiledData_fromURL_usingStorageOptions_errorSelector = mkSelector "initWithCompiledData:fromURL:usingStorageOptions:error:"

-- | @Selector@ for @initWithScriptDataDescriptor:fromURL:languageInstance:usingStorageOptions:error:@
initWithScriptDataDescriptor_fromURL_languageInstance_usingStorageOptions_errorSelector :: Selector
initWithScriptDataDescriptor_fromURL_languageInstance_usingStorageOptions_errorSelector = mkSelector "initWithScriptDataDescriptor:fromURL:languageInstance:usingStorageOptions:error:"

-- | @Selector@ for @compileAndReturnError:@
compileAndReturnErrorSelector :: Selector
compileAndReturnErrorSelector = mkSelector "compileAndReturnError:"

-- | @Selector@ for @executeAndReturnError:@
executeAndReturnErrorSelector :: Selector
executeAndReturnErrorSelector = mkSelector "executeAndReturnError:"

-- | @Selector@ for @executeAppleEvent:error:@
executeAppleEvent_errorSelector :: Selector
executeAppleEvent_errorSelector = mkSelector "executeAppleEvent:error:"

-- | @Selector@ for @executeAndReturnDisplayValue:error:@
executeAndReturnDisplayValue_errorSelector :: Selector
executeAndReturnDisplayValue_errorSelector = mkSelector "executeAndReturnDisplayValue:error:"

-- | @Selector@ for @executeHandlerWithName:arguments:error:@
executeHandlerWithName_arguments_errorSelector :: Selector
executeHandlerWithName_arguments_errorSelector = mkSelector "executeHandlerWithName:arguments:error:"

-- | @Selector@ for @richTextFromDescriptor:@
richTextFromDescriptorSelector :: Selector
richTextFromDescriptorSelector = mkSelector "richTextFromDescriptor:"

-- | @Selector@ for @writeToURL:ofType:error:@
writeToURL_ofType_errorSelector :: Selector
writeToURL_ofType_errorSelector = mkSelector "writeToURL:ofType:error:"

-- | @Selector@ for @writeToURL:ofType:usingStorageOptions:error:@
writeToURL_ofType_usingStorageOptions_errorSelector :: Selector
writeToURL_ofType_usingStorageOptions_errorSelector = mkSelector "writeToURL:ofType:usingStorageOptions:error:"

-- | @Selector@ for @compiledDataForType:usingStorageOptions:error:@
compiledDataForType_usingStorageOptions_errorSelector :: Selector
compiledDataForType_usingStorageOptions_errorSelector = mkSelector "compiledDataForType:usingStorageOptions:error:"

-- | @Selector@ for @source@
sourceSelector :: Selector
sourceSelector = mkSelector "source"

-- | @Selector@ for @url@
urlSelector :: Selector
urlSelector = mkSelector "url"

-- | @Selector@ for @language@
languageSelector :: Selector
languageSelector = mkSelector "language"

-- | @Selector@ for @setLanguage:@
setLanguageSelector :: Selector
setLanguageSelector = mkSelector "setLanguage:"

-- | @Selector@ for @languageInstance@
languageInstanceSelector :: Selector
languageInstanceSelector = mkSelector "languageInstance"

-- | @Selector@ for @setLanguageInstance:@
setLanguageInstanceSelector :: Selector
setLanguageInstanceSelector = mkSelector "setLanguageInstance:"

-- | @Selector@ for @compiled@
compiledSelector :: Selector
compiledSelector = mkSelector "compiled"

-- | @Selector@ for @richTextSource@
richTextSourceSelector :: Selector
richTextSourceSelector = mkSelector "richTextSource"

