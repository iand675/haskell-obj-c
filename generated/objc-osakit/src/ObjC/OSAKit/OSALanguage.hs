{-# LANGUAGE PatternSynonyms #-}
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
  , languageForNameSelector
  , languageForScriptDataDescriptorSelector
  , defaultLanguageSelector
  , setDefaultLanguageSelector
  , initWithComponentSelector
  , sharedLanguageInstanceSelector
  , componentInstanceSelector
  , nameSelector
  , infoSelector
  , versionSelector
  , typeSelector
  , subTypeSelector
  , manufacturerSelector
  , featuresSelector
  , threadSafeSelector

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

-- | @+ availableLanguages@
availableLanguages :: IO (Id NSArray)
availableLanguages  =
  do
    cls' <- getRequiredClass "OSALanguage"
    sendClassMsg cls' (mkSelector "availableLanguages") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ languageForName:@
languageForName :: IsNSString name => name -> IO (Id OSALanguage)
languageForName name =
  do
    cls' <- getRequiredClass "OSALanguage"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "languageForName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @+ languageForScriptDataDescriptor:@
languageForScriptDataDescriptor :: IsNSAppleEventDescriptor descriptor => descriptor -> IO (Id OSALanguage)
languageForScriptDataDescriptor descriptor =
  do
    cls' <- getRequiredClass "OSALanguage"
    withObjCPtr descriptor $ \raw_descriptor ->
      sendClassMsg cls' (mkSelector "languageForScriptDataDescriptor:") (retPtr retVoid) [argPtr (castPtr raw_descriptor :: Ptr ())] >>= retainedObject . castPtr

-- | @+ defaultLanguage@
defaultLanguage :: IO (Id OSALanguage)
defaultLanguage  =
  do
    cls' <- getRequiredClass "OSALanguage"
    sendClassMsg cls' (mkSelector "defaultLanguage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ setDefaultLanguage:@
setDefaultLanguage :: IsOSALanguage defaultLanguage => defaultLanguage -> IO ()
setDefaultLanguage defaultLanguage =
  do
    cls' <- getRequiredClass "OSALanguage"
    withObjCPtr defaultLanguage $ \raw_defaultLanguage ->
      sendClassMsg cls' (mkSelector "setDefaultLanguage:") retVoid [argPtr (castPtr raw_defaultLanguage :: Ptr ())]

-- | @- initWithComponent:@
initWithComponent :: IsOSALanguage osaLanguage => osaLanguage -> RawId -> IO (Id OSALanguage)
initWithComponent osaLanguage  component =
  sendMsg osaLanguage (mkSelector "initWithComponent:") (retPtr retVoid) [argPtr (castPtr (unRawId component) :: Ptr ())] >>= ownedObject . castPtr

-- | @- sharedLanguageInstance@
sharedLanguageInstance :: IsOSALanguage osaLanguage => osaLanguage -> IO (Id OSALanguageInstance)
sharedLanguageInstance osaLanguage  =
  sendMsg osaLanguage (mkSelector "sharedLanguageInstance") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- componentInstance@
componentInstance :: IsOSALanguage osaLanguage => osaLanguage -> IO RawId
componentInstance osaLanguage  =
  fmap (RawId . castPtr) $ sendMsg osaLanguage (mkSelector "componentInstance") (retPtr retVoid) []

-- | @- name@
name :: IsOSALanguage osaLanguage => osaLanguage -> IO (Id NSString)
name osaLanguage  =
  sendMsg osaLanguage (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- info@
info :: IsOSALanguage osaLanguage => osaLanguage -> IO (Id NSString)
info osaLanguage  =
  sendMsg osaLanguage (mkSelector "info") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- version@
version :: IsOSALanguage osaLanguage => osaLanguage -> IO (Id NSString)
version osaLanguage  =
  sendMsg osaLanguage (mkSelector "version") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- type@
type_ :: IsOSALanguage osaLanguage => osaLanguage -> IO CUInt
type_ osaLanguage  =
  sendMsg osaLanguage (mkSelector "type") retCUInt []

-- | @- subType@
subType :: IsOSALanguage osaLanguage => osaLanguage -> IO CUInt
subType osaLanguage  =
  sendMsg osaLanguage (mkSelector "subType") retCUInt []

-- | @- manufacturer@
manufacturer :: IsOSALanguage osaLanguage => osaLanguage -> IO CUInt
manufacturer osaLanguage  =
  sendMsg osaLanguage (mkSelector "manufacturer") retCUInt []

-- | @- features@
features :: IsOSALanguage osaLanguage => osaLanguage -> IO OSALanguageFeatures
features osaLanguage  =
  fmap (coerce :: CULong -> OSALanguageFeatures) $ sendMsg osaLanguage (mkSelector "features") retCULong []

-- | @- threadSafe@
threadSafe :: IsOSALanguage osaLanguage => osaLanguage -> IO Bool
threadSafe osaLanguage  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg osaLanguage (mkSelector "threadSafe") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @availableLanguages@
availableLanguagesSelector :: Selector
availableLanguagesSelector = mkSelector "availableLanguages"

-- | @Selector@ for @languageForName:@
languageForNameSelector :: Selector
languageForNameSelector = mkSelector "languageForName:"

-- | @Selector@ for @languageForScriptDataDescriptor:@
languageForScriptDataDescriptorSelector :: Selector
languageForScriptDataDescriptorSelector = mkSelector "languageForScriptDataDescriptor:"

-- | @Selector@ for @defaultLanguage@
defaultLanguageSelector :: Selector
defaultLanguageSelector = mkSelector "defaultLanguage"

-- | @Selector@ for @setDefaultLanguage:@
setDefaultLanguageSelector :: Selector
setDefaultLanguageSelector = mkSelector "setDefaultLanguage:"

-- | @Selector@ for @initWithComponent:@
initWithComponentSelector :: Selector
initWithComponentSelector = mkSelector "initWithComponent:"

-- | @Selector@ for @sharedLanguageInstance@
sharedLanguageInstanceSelector :: Selector
sharedLanguageInstanceSelector = mkSelector "sharedLanguageInstance"

-- | @Selector@ for @componentInstance@
componentInstanceSelector :: Selector
componentInstanceSelector = mkSelector "componentInstance"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @info@
infoSelector :: Selector
infoSelector = mkSelector "info"

-- | @Selector@ for @version@
versionSelector :: Selector
versionSelector = mkSelector "version"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @subType@
subTypeSelector :: Selector
subTypeSelector = mkSelector "subType"

-- | @Selector@ for @manufacturer@
manufacturerSelector :: Selector
manufacturerSelector = mkSelector "manufacturer"

-- | @Selector@ for @features@
featuresSelector :: Selector
featuresSelector = mkSelector "features"

-- | @Selector@ for @threadSafe@
threadSafeSelector :: Selector
threadSafeSelector = mkSelector "threadSafe"

