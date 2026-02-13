{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @OSALanguage@.
module ObjC.OSAKit.OSALanguage
  ( OSALanguage
  , IsOSALanguage(..)
  , availableLanguages
  , languageForName
  , languageForScriptDataDescriptor
  , defaultLanguage
  , setDefaultLanguage
  , initWithComponent
  , sharedLanguageInstance
  , componentInstance
  , name
  , info
  , version
  , type_
  , subType
  , manufacturer
  , features
  , threadSafe
  , availableLanguagesSelector
  , componentInstanceSelector
  , defaultLanguageSelector
  , featuresSelector
  , infoSelector
  , initWithComponentSelector
  , languageForNameSelector
  , languageForScriptDataDescriptorSelector
  , manufacturerSelector
  , nameSelector
  , setDefaultLanguageSelector
  , sharedLanguageInstanceSelector
  , subTypeSelector
  , threadSafeSelector
  , typeSelector
  , versionSelector

  -- * Enum types
  , OSALanguageFeatures(OSALanguageFeatures)
  , pattern OSASupportsCompiling
  , pattern OSASupportsGetSource
  , pattern OSASupportsAECoercion
  , pattern OSASupportsAESending
  , pattern OSASupportsRecording
  , pattern OSASupportsConvenience
  , pattern OSASupportsDialects
  , pattern OSASupportsEventHandling

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

-- | @+ availableLanguages@
availableLanguages :: IO (Id NSArray)
availableLanguages  =
  do
    cls' <- getRequiredClass "OSALanguage"
    sendClassMessage cls' availableLanguagesSelector

-- | @+ languageForName:@
languageForName :: IsNSString name => name -> IO (Id OSALanguage)
languageForName name =
  do
    cls' <- getRequiredClass "OSALanguage"
    sendClassMessage cls' languageForNameSelector (toNSString name)

-- | @+ languageForScriptDataDescriptor:@
languageForScriptDataDescriptor :: IsNSAppleEventDescriptor descriptor => descriptor -> IO (Id OSALanguage)
languageForScriptDataDescriptor descriptor =
  do
    cls' <- getRequiredClass "OSALanguage"
    sendClassMessage cls' languageForScriptDataDescriptorSelector (toNSAppleEventDescriptor descriptor)

-- | @+ defaultLanguage@
defaultLanguage :: IO (Id OSALanguage)
defaultLanguage  =
  do
    cls' <- getRequiredClass "OSALanguage"
    sendClassMessage cls' defaultLanguageSelector

-- | @+ setDefaultLanguage:@
setDefaultLanguage :: IsOSALanguage defaultLanguage => defaultLanguage -> IO ()
setDefaultLanguage defaultLanguage =
  do
    cls' <- getRequiredClass "OSALanguage"
    sendClassMessage cls' setDefaultLanguageSelector (toOSALanguage defaultLanguage)

-- | @- initWithComponent:@
initWithComponent :: IsOSALanguage osaLanguage => osaLanguage -> RawId -> IO (Id OSALanguage)
initWithComponent osaLanguage component =
  sendOwnedMessage osaLanguage initWithComponentSelector component

-- | @- sharedLanguageInstance@
sharedLanguageInstance :: IsOSALanguage osaLanguage => osaLanguage -> IO (Id OSALanguageInstance)
sharedLanguageInstance osaLanguage =
  sendMessage osaLanguage sharedLanguageInstanceSelector

-- | @- componentInstance@
componentInstance :: IsOSALanguage osaLanguage => osaLanguage -> IO RawId
componentInstance osaLanguage =
  sendMessage osaLanguage componentInstanceSelector

-- | @- name@
name :: IsOSALanguage osaLanguage => osaLanguage -> IO (Id NSString)
name osaLanguage =
  sendMessage osaLanguage nameSelector

-- | @- info@
info :: IsOSALanguage osaLanguage => osaLanguage -> IO (Id NSString)
info osaLanguage =
  sendMessage osaLanguage infoSelector

-- | @- version@
version :: IsOSALanguage osaLanguage => osaLanguage -> IO (Id NSString)
version osaLanguage =
  sendMessage osaLanguage versionSelector

-- | @- type@
type_ :: IsOSALanguage osaLanguage => osaLanguage -> IO CUInt
type_ osaLanguage =
  sendMessage osaLanguage typeSelector

-- | @- subType@
subType :: IsOSALanguage osaLanguage => osaLanguage -> IO CUInt
subType osaLanguage =
  sendMessage osaLanguage subTypeSelector

-- | @- manufacturer@
manufacturer :: IsOSALanguage osaLanguage => osaLanguage -> IO CUInt
manufacturer osaLanguage =
  sendMessage osaLanguage manufacturerSelector

-- | @- features@
features :: IsOSALanguage osaLanguage => osaLanguage -> IO OSALanguageFeatures
features osaLanguage =
  sendMessage osaLanguage featuresSelector

-- | @- threadSafe@
threadSafe :: IsOSALanguage osaLanguage => osaLanguage -> IO Bool
threadSafe osaLanguage =
  sendMessage osaLanguage threadSafeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @availableLanguages@
availableLanguagesSelector :: Selector '[] (Id NSArray)
availableLanguagesSelector = mkSelector "availableLanguages"

-- | @Selector@ for @languageForName:@
languageForNameSelector :: Selector '[Id NSString] (Id OSALanguage)
languageForNameSelector = mkSelector "languageForName:"

-- | @Selector@ for @languageForScriptDataDescriptor:@
languageForScriptDataDescriptorSelector :: Selector '[Id NSAppleEventDescriptor] (Id OSALanguage)
languageForScriptDataDescriptorSelector = mkSelector "languageForScriptDataDescriptor:"

-- | @Selector@ for @defaultLanguage@
defaultLanguageSelector :: Selector '[] (Id OSALanguage)
defaultLanguageSelector = mkSelector "defaultLanguage"

-- | @Selector@ for @setDefaultLanguage:@
setDefaultLanguageSelector :: Selector '[Id OSALanguage] ()
setDefaultLanguageSelector = mkSelector "setDefaultLanguage:"

-- | @Selector@ for @initWithComponent:@
initWithComponentSelector :: Selector '[RawId] (Id OSALanguage)
initWithComponentSelector = mkSelector "initWithComponent:"

-- | @Selector@ for @sharedLanguageInstance@
sharedLanguageInstanceSelector :: Selector '[] (Id OSALanguageInstance)
sharedLanguageInstanceSelector = mkSelector "sharedLanguageInstance"

-- | @Selector@ for @componentInstance@
componentInstanceSelector :: Selector '[] RawId
componentInstanceSelector = mkSelector "componentInstance"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @info@
infoSelector :: Selector '[] (Id NSString)
infoSelector = mkSelector "info"

-- | @Selector@ for @version@
versionSelector :: Selector '[] (Id NSString)
versionSelector = mkSelector "version"

-- | @Selector@ for @type@
typeSelector :: Selector '[] CUInt
typeSelector = mkSelector "type"

-- | @Selector@ for @subType@
subTypeSelector :: Selector '[] CUInt
subTypeSelector = mkSelector "subType"

-- | @Selector@ for @manufacturer@
manufacturerSelector :: Selector '[] CUInt
manufacturerSelector = mkSelector "manufacturer"

-- | @Selector@ for @features@
featuresSelector :: Selector '[] OSALanguageFeatures
featuresSelector = mkSelector "features"

-- | @Selector@ for @threadSafe@
threadSafeSelector :: Selector '[] Bool
threadSafeSelector = mkSelector "threadSafe"

