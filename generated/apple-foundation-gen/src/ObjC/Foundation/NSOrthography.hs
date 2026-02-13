{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSOrthography@.
module ObjC.Foundation.NSOrthography
  ( NSOrthography
  , IsNSOrthography(..)
  , initWithDominantScript_languageMap
  , initWithCoder
  , orthographyWithDominantScript_languageMap
  , languagesForScript
  , dominantLanguageForScript
  , defaultOrthographyForLanguage
  , dominantScript
  , languageMap
  , dominantLanguage
  , allScripts
  , allLanguages
  , allLanguagesSelector
  , allScriptsSelector
  , defaultOrthographyForLanguageSelector
  , dominantLanguageForScriptSelector
  , dominantLanguageSelector
  , dominantScriptSelector
  , initWithCoderSelector
  , initWithDominantScript_languageMapSelector
  , languageMapSelector
  , languagesForScriptSelector
  , orthographyWithDominantScript_languageMapSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- initWithDominantScript:languageMap:@
initWithDominantScript_languageMap :: (IsNSOrthography nsOrthography, IsNSString script, IsNSDictionary map_) => nsOrthography -> script -> map_ -> IO (Id NSOrthography)
initWithDominantScript_languageMap nsOrthography script map_ =
  sendOwnedMessage nsOrthography initWithDominantScript_languageMapSelector (toNSString script) (toNSDictionary map_)

-- | @- initWithCoder:@
initWithCoder :: (IsNSOrthography nsOrthography, IsNSCoder coder) => nsOrthography -> coder -> IO (Id NSOrthography)
initWithCoder nsOrthography coder =
  sendOwnedMessage nsOrthography initWithCoderSelector (toNSCoder coder)

-- | @+ orthographyWithDominantScript:languageMap:@
orthographyWithDominantScript_languageMap :: (IsNSString script, IsNSDictionary map_) => script -> map_ -> IO (Id NSOrthography)
orthographyWithDominantScript_languageMap script map_ =
  do
    cls' <- getRequiredClass "NSOrthography"
    sendClassMessage cls' orthographyWithDominantScript_languageMapSelector (toNSString script) (toNSDictionary map_)

-- | @- languagesForScript:@
languagesForScript :: (IsNSOrthography nsOrthography, IsNSString script) => nsOrthography -> script -> IO (Id NSArray)
languagesForScript nsOrthography script =
  sendMessage nsOrthography languagesForScriptSelector (toNSString script)

-- | @- dominantLanguageForScript:@
dominantLanguageForScript :: (IsNSOrthography nsOrthography, IsNSString script) => nsOrthography -> script -> IO (Id NSString)
dominantLanguageForScript nsOrthography script =
  sendMessage nsOrthography dominantLanguageForScriptSelector (toNSString script)

-- | @+ defaultOrthographyForLanguage:@
defaultOrthographyForLanguage :: IsNSString language => language -> IO (Id NSOrthography)
defaultOrthographyForLanguage language =
  do
    cls' <- getRequiredClass "NSOrthography"
    sendClassMessage cls' defaultOrthographyForLanguageSelector (toNSString language)

-- | @- dominantScript@
dominantScript :: IsNSOrthography nsOrthography => nsOrthography -> IO (Id NSString)
dominantScript nsOrthography =
  sendMessage nsOrthography dominantScriptSelector

-- | @- languageMap@
languageMap :: IsNSOrthography nsOrthography => nsOrthography -> IO (Id NSDictionary)
languageMap nsOrthography =
  sendMessage nsOrthography languageMapSelector

-- | @- dominantLanguage@
dominantLanguage :: IsNSOrthography nsOrthography => nsOrthography -> IO (Id NSString)
dominantLanguage nsOrthography =
  sendMessage nsOrthography dominantLanguageSelector

-- | @- allScripts@
allScripts :: IsNSOrthography nsOrthography => nsOrthography -> IO (Id NSArray)
allScripts nsOrthography =
  sendMessage nsOrthography allScriptsSelector

-- | @- allLanguages@
allLanguages :: IsNSOrthography nsOrthography => nsOrthography -> IO (Id NSArray)
allLanguages nsOrthography =
  sendMessage nsOrthography allLanguagesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDominantScript:languageMap:@
initWithDominantScript_languageMapSelector :: Selector '[Id NSString, Id NSDictionary] (Id NSOrthography)
initWithDominantScript_languageMapSelector = mkSelector "initWithDominantScript:languageMap:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSOrthography)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @orthographyWithDominantScript:languageMap:@
orthographyWithDominantScript_languageMapSelector :: Selector '[Id NSString, Id NSDictionary] (Id NSOrthography)
orthographyWithDominantScript_languageMapSelector = mkSelector "orthographyWithDominantScript:languageMap:"

-- | @Selector@ for @languagesForScript:@
languagesForScriptSelector :: Selector '[Id NSString] (Id NSArray)
languagesForScriptSelector = mkSelector "languagesForScript:"

-- | @Selector@ for @dominantLanguageForScript:@
dominantLanguageForScriptSelector :: Selector '[Id NSString] (Id NSString)
dominantLanguageForScriptSelector = mkSelector "dominantLanguageForScript:"

-- | @Selector@ for @defaultOrthographyForLanguage:@
defaultOrthographyForLanguageSelector :: Selector '[Id NSString] (Id NSOrthography)
defaultOrthographyForLanguageSelector = mkSelector "defaultOrthographyForLanguage:"

-- | @Selector@ for @dominantScript@
dominantScriptSelector :: Selector '[] (Id NSString)
dominantScriptSelector = mkSelector "dominantScript"

-- | @Selector@ for @languageMap@
languageMapSelector :: Selector '[] (Id NSDictionary)
languageMapSelector = mkSelector "languageMap"

-- | @Selector@ for @dominantLanguage@
dominantLanguageSelector :: Selector '[] (Id NSString)
dominantLanguageSelector = mkSelector "dominantLanguage"

-- | @Selector@ for @allScripts@
allScriptsSelector :: Selector '[] (Id NSArray)
allScriptsSelector = mkSelector "allScripts"

-- | @Selector@ for @allLanguages@
allLanguagesSelector :: Selector '[] (Id NSArray)
allLanguagesSelector = mkSelector "allLanguages"

