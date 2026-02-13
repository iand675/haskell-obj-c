{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @OSALanguageInstance@.
module ObjC.OSAKit.OSALanguageInstance
  ( OSALanguageInstance
  , IsOSALanguageInstance(..)
  , languageInstanceWithLanguage
  , initWithLanguage
  , richTextFromDescriptor
  , language
  , componentInstance
  , defaultTarget
  , setDefaultTarget
  , componentInstanceSelector
  , defaultTargetSelector
  , initWithLanguageSelector
  , languageInstanceWithLanguageSelector
  , languageSelector
  , richTextFromDescriptorSelector
  , setDefaultTargetSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.OSAKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ languageInstanceWithLanguage:@
languageInstanceWithLanguage :: IsOSALanguage language => language -> IO (Id OSALanguageInstance)
languageInstanceWithLanguage language =
  do
    cls' <- getRequiredClass "OSALanguageInstance"
    sendClassMessage cls' languageInstanceWithLanguageSelector (toOSALanguage language)

-- | @- initWithLanguage:@
initWithLanguage :: (IsOSALanguageInstance osaLanguageInstance, IsOSALanguage language) => osaLanguageInstance -> language -> IO (Id OSALanguageInstance)
initWithLanguage osaLanguageInstance language =
  sendOwnedMessage osaLanguageInstance initWithLanguageSelector (toOSALanguage language)

-- | @- richTextFromDescriptor:@
richTextFromDescriptor :: (IsOSALanguageInstance osaLanguageInstance, IsNSAppleEventDescriptor descriptor) => osaLanguageInstance -> descriptor -> IO (Id NSAttributedString)
richTextFromDescriptor osaLanguageInstance descriptor =
  sendMessage osaLanguageInstance richTextFromDescriptorSelector (toNSAppleEventDescriptor descriptor)

-- | @- language@
language :: IsOSALanguageInstance osaLanguageInstance => osaLanguageInstance -> IO (Id OSALanguage)
language osaLanguageInstance =
  sendMessage osaLanguageInstance languageSelector

-- | @- componentInstance@
componentInstance :: IsOSALanguageInstance osaLanguageInstance => osaLanguageInstance -> IO RawId
componentInstance osaLanguageInstance =
  sendMessage osaLanguageInstance componentInstanceSelector

-- | @- defaultTarget@
defaultTarget :: IsOSALanguageInstance osaLanguageInstance => osaLanguageInstance -> IO RawId
defaultTarget osaLanguageInstance =
  sendMessage osaLanguageInstance defaultTargetSelector

-- | @- setDefaultTarget:@
setDefaultTarget :: IsOSALanguageInstance osaLanguageInstance => osaLanguageInstance -> RawId -> IO ()
setDefaultTarget osaLanguageInstance value =
  sendMessage osaLanguageInstance setDefaultTargetSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @languageInstanceWithLanguage:@
languageInstanceWithLanguageSelector :: Selector '[Id OSALanguage] (Id OSALanguageInstance)
languageInstanceWithLanguageSelector = mkSelector "languageInstanceWithLanguage:"

-- | @Selector@ for @initWithLanguage:@
initWithLanguageSelector :: Selector '[Id OSALanguage] (Id OSALanguageInstance)
initWithLanguageSelector = mkSelector "initWithLanguage:"

-- | @Selector@ for @richTextFromDescriptor:@
richTextFromDescriptorSelector :: Selector '[Id NSAppleEventDescriptor] (Id NSAttributedString)
richTextFromDescriptorSelector = mkSelector "richTextFromDescriptor:"

-- | @Selector@ for @language@
languageSelector :: Selector '[] (Id OSALanguage)
languageSelector = mkSelector "language"

-- | @Selector@ for @componentInstance@
componentInstanceSelector :: Selector '[] RawId
componentInstanceSelector = mkSelector "componentInstance"

-- | @Selector@ for @defaultTarget@
defaultTargetSelector :: Selector '[] RawId
defaultTargetSelector = mkSelector "defaultTarget"

-- | @Selector@ for @setDefaultTarget:@
setDefaultTargetSelector :: Selector '[RawId] ()
setDefaultTargetSelector = mkSelector "setDefaultTarget:"

