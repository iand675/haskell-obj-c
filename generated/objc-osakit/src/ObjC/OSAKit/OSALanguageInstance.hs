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
  , languageInstanceWithLanguageSelector
  , initWithLanguageSelector
  , richTextFromDescriptorSelector
  , languageSelector
  , componentInstanceSelector


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
import ObjC.Foundation.Internal.Classes

-- | @+ languageInstanceWithLanguage:@
languageInstanceWithLanguage :: IsOSALanguage language => language -> IO (Id OSALanguageInstance)
languageInstanceWithLanguage language =
  do
    cls' <- getRequiredClass "OSALanguageInstance"
    withObjCPtr language $ \raw_language ->
      sendClassMsg cls' (mkSelector "languageInstanceWithLanguage:") (retPtr retVoid) [argPtr (castPtr raw_language :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithLanguage:@
initWithLanguage :: (IsOSALanguageInstance osaLanguageInstance, IsOSALanguage language) => osaLanguageInstance -> language -> IO (Id OSALanguageInstance)
initWithLanguage osaLanguageInstance  language =
withObjCPtr language $ \raw_language ->
    sendMsg osaLanguageInstance (mkSelector "initWithLanguage:") (retPtr retVoid) [argPtr (castPtr raw_language :: Ptr ())] >>= ownedObject . castPtr

-- | @- richTextFromDescriptor:@
richTextFromDescriptor :: (IsOSALanguageInstance osaLanguageInstance, IsNSAppleEventDescriptor descriptor) => osaLanguageInstance -> descriptor -> IO (Id NSAttributedString)
richTextFromDescriptor osaLanguageInstance  descriptor =
withObjCPtr descriptor $ \raw_descriptor ->
    sendMsg osaLanguageInstance (mkSelector "richTextFromDescriptor:") (retPtr retVoid) [argPtr (castPtr raw_descriptor :: Ptr ())] >>= retainedObject . castPtr

-- | @- language@
language :: IsOSALanguageInstance osaLanguageInstance => osaLanguageInstance -> IO (Id OSALanguage)
language osaLanguageInstance  =
  sendMsg osaLanguageInstance (mkSelector "language") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- componentInstance@
componentInstance :: IsOSALanguageInstance osaLanguageInstance => osaLanguageInstance -> IO RawId
componentInstance osaLanguageInstance  =
  fmap (RawId . castPtr) $ sendMsg osaLanguageInstance (mkSelector "componentInstance") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @languageInstanceWithLanguage:@
languageInstanceWithLanguageSelector :: Selector
languageInstanceWithLanguageSelector = mkSelector "languageInstanceWithLanguage:"

-- | @Selector@ for @initWithLanguage:@
initWithLanguageSelector :: Selector
initWithLanguageSelector = mkSelector "initWithLanguage:"

-- | @Selector@ for @richTextFromDescriptor:@
richTextFromDescriptorSelector :: Selector
richTextFromDescriptorSelector = mkSelector "richTextFromDescriptor:"

-- | @Selector@ for @language@
languageSelector :: Selector
languageSelector = mkSelector "language"

-- | @Selector@ for @componentInstance@
componentInstanceSelector :: Selector
componentInstanceSelector = mkSelector "componentInstance"

