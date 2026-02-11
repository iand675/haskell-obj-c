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
  , initWithDominantScript_languageMapSelector
  , initWithCoderSelector
  , orthographyWithDominantScript_languageMapSelector
  , languagesForScriptSelector
  , dominantLanguageForScriptSelector
  , defaultOrthographyForLanguageSelector
  , dominantScriptSelector
  , languageMapSelector
  , dominantLanguageSelector
  , allScriptsSelector
  , allLanguagesSelector


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

-- | @- initWithDominantScript:languageMap:@
initWithDominantScript_languageMap :: (IsNSOrthography nsOrthography, IsNSString script, IsNSDictionary map_) => nsOrthography -> script -> map_ -> IO (Id NSOrthography)
initWithDominantScript_languageMap nsOrthography  script map_ =
withObjCPtr script $ \raw_script ->
  withObjCPtr map_ $ \raw_map_ ->
      sendMsg nsOrthography (mkSelector "initWithDominantScript:languageMap:") (retPtr retVoid) [argPtr (castPtr raw_script :: Ptr ()), argPtr (castPtr raw_map_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSOrthography nsOrthography, IsNSCoder coder) => nsOrthography -> coder -> IO (Id NSOrthography)
initWithCoder nsOrthography  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsOrthography (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @+ orthographyWithDominantScript:languageMap:@
orthographyWithDominantScript_languageMap :: (IsNSString script, IsNSDictionary map_) => script -> map_ -> IO (Id NSOrthography)
orthographyWithDominantScript_languageMap script map_ =
  do
    cls' <- getRequiredClass "NSOrthography"
    withObjCPtr script $ \raw_script ->
      withObjCPtr map_ $ \raw_map_ ->
        sendClassMsg cls' (mkSelector "orthographyWithDominantScript:languageMap:") (retPtr retVoid) [argPtr (castPtr raw_script :: Ptr ()), argPtr (castPtr raw_map_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- languagesForScript:@
languagesForScript :: (IsNSOrthography nsOrthography, IsNSString script) => nsOrthography -> script -> IO (Id NSArray)
languagesForScript nsOrthography  script =
withObjCPtr script $ \raw_script ->
    sendMsg nsOrthography (mkSelector "languagesForScript:") (retPtr retVoid) [argPtr (castPtr raw_script :: Ptr ())] >>= retainedObject . castPtr

-- | @- dominantLanguageForScript:@
dominantLanguageForScript :: (IsNSOrthography nsOrthography, IsNSString script) => nsOrthography -> script -> IO (Id NSString)
dominantLanguageForScript nsOrthography  script =
withObjCPtr script $ \raw_script ->
    sendMsg nsOrthography (mkSelector "dominantLanguageForScript:") (retPtr retVoid) [argPtr (castPtr raw_script :: Ptr ())] >>= retainedObject . castPtr

-- | @+ defaultOrthographyForLanguage:@
defaultOrthographyForLanguage :: IsNSString language => language -> IO (Id NSOrthography)
defaultOrthographyForLanguage language =
  do
    cls' <- getRequiredClass "NSOrthography"
    withObjCPtr language $ \raw_language ->
      sendClassMsg cls' (mkSelector "defaultOrthographyForLanguage:") (retPtr retVoid) [argPtr (castPtr raw_language :: Ptr ())] >>= retainedObject . castPtr

-- | @- dominantScript@
dominantScript :: IsNSOrthography nsOrthography => nsOrthography -> IO (Id NSString)
dominantScript nsOrthography  =
  sendMsg nsOrthography (mkSelector "dominantScript") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- languageMap@
languageMap :: IsNSOrthography nsOrthography => nsOrthography -> IO (Id NSDictionary)
languageMap nsOrthography  =
  sendMsg nsOrthography (mkSelector "languageMap") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- dominantLanguage@
dominantLanguage :: IsNSOrthography nsOrthography => nsOrthography -> IO (Id NSString)
dominantLanguage nsOrthography  =
  sendMsg nsOrthography (mkSelector "dominantLanguage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- allScripts@
allScripts :: IsNSOrthography nsOrthography => nsOrthography -> IO (Id NSArray)
allScripts nsOrthography  =
  sendMsg nsOrthography (mkSelector "allScripts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- allLanguages@
allLanguages :: IsNSOrthography nsOrthography => nsOrthography -> IO (Id NSArray)
allLanguages nsOrthography  =
  sendMsg nsOrthography (mkSelector "allLanguages") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDominantScript:languageMap:@
initWithDominantScript_languageMapSelector :: Selector
initWithDominantScript_languageMapSelector = mkSelector "initWithDominantScript:languageMap:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @orthographyWithDominantScript:languageMap:@
orthographyWithDominantScript_languageMapSelector :: Selector
orthographyWithDominantScript_languageMapSelector = mkSelector "orthographyWithDominantScript:languageMap:"

-- | @Selector@ for @languagesForScript:@
languagesForScriptSelector :: Selector
languagesForScriptSelector = mkSelector "languagesForScript:"

-- | @Selector@ for @dominantLanguageForScript:@
dominantLanguageForScriptSelector :: Selector
dominantLanguageForScriptSelector = mkSelector "dominantLanguageForScript:"

-- | @Selector@ for @defaultOrthographyForLanguage:@
defaultOrthographyForLanguageSelector :: Selector
defaultOrthographyForLanguageSelector = mkSelector "defaultOrthographyForLanguage:"

-- | @Selector@ for @dominantScript@
dominantScriptSelector :: Selector
dominantScriptSelector = mkSelector "dominantScript"

-- | @Selector@ for @languageMap@
languageMapSelector :: Selector
languageMapSelector = mkSelector "languageMap"

-- | @Selector@ for @dominantLanguage@
dominantLanguageSelector :: Selector
dominantLanguageSelector = mkSelector "dominantLanguage"

-- | @Selector@ for @allScripts@
allScriptsSelector :: Selector
allScriptsSelector = mkSelector "allScripts"

-- | @Selector@ for @allLanguages@
allLanguagesSelector :: Selector
allLanguagesSelector = mkSelector "allLanguages"

