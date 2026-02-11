{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTermOfAddress@.
module ObjC.Foundation.NSTermOfAddress
  ( NSTermOfAddress
  , IsNSTermOfAddress(..)
  , neutral
  , feminine
  , masculine
  , currentUser
  , localizedForLanguageIdentifier_withPronouns
  , new
  , init_
  , languageIdentifier
  , pronouns
  , neutralSelector
  , feminineSelector
  , masculineSelector
  , currentUserSelector
  , localizedForLanguageIdentifier_withPronounsSelector
  , newSelector
  , initSelector
  , languageIdentifierSelector
  , pronounsSelector


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

-- | Term of address that uses gender-neutral pronouns (e.g. they/them/theirs in English), and an epicene grammatical gender when inflecting verbs and  adjectives referring to the person
--
-- ObjC selector: @+ neutral@
neutral :: IO (Id NSTermOfAddress)
neutral  =
  do
    cls' <- getRequiredClass "NSTermOfAddress"
    sendClassMsg cls' (mkSelector "neutral") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Term of address that uses feminine pronouns (e.g. she/her/hers in English), and a feminine grammatical gender when inflecting verbs and adjectives referring to the person
--
-- ObjC selector: @+ feminine@
feminine :: IO (Id NSTermOfAddress)
feminine  =
  do
    cls' <- getRequiredClass "NSTermOfAddress"
    sendClassMsg cls' (mkSelector "feminine") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Term of address that uses masculine pronouns (e.g. he/him/his in English), and a masculine grammatical gender when inflecting verbs and adjectives referring to the person
--
-- ObjC selector: @+ masculine@
masculine :: IO (Id NSTermOfAddress)
masculine  =
  do
    cls' <- getRequiredClass "NSTermOfAddress"
    sendClassMsg cls' (mkSelector "masculine") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The term of address that should be used for addressing the user
--
-- This term of address will only compare equal to another @+[NSTermOfAddress currentUser]@
--
-- ObjC selector: @+ currentUser@
currentUser :: IO (Id NSTermOfAddress)
currentUser  =
  do
    cls' <- getRequiredClass "NSTermOfAddress"
    sendClassMsg cls' (mkSelector "currentUser") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A term of address restricted to a given language
--
-- @language@ — ISO language code identifier for the language
--
-- @pronouns@ — A list of pronouns in the target language that can be used to                 refer to the person.
--
-- ObjC selector: @+ localizedForLanguageIdentifier:withPronouns:@
localizedForLanguageIdentifier_withPronouns :: (IsNSString language, IsNSArray pronouns) => language -> pronouns -> IO (Id NSTermOfAddress)
localizedForLanguageIdentifier_withPronouns language pronouns =
  do
    cls' <- getRequiredClass "NSTermOfAddress"
    withObjCPtr language $ \raw_language ->
      withObjCPtr pronouns $ \raw_pronouns ->
        sendClassMsg cls' (mkSelector "localizedForLanguageIdentifier:withPronouns:") (retPtr retVoid) [argPtr (castPtr raw_language :: Ptr ()), argPtr (castPtr raw_pronouns :: Ptr ())] >>= retainedObject . castPtr

-- | @+ new@
new :: IO (Id NSTermOfAddress)
new  =
  do
    cls' <- getRequiredClass "NSTermOfAddress"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsNSTermOfAddress nsTermOfAddress => nsTermOfAddress -> IO (Id NSTermOfAddress)
init_ nsTermOfAddress  =
  sendMsg nsTermOfAddress (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The ISO language code if this is a localized term of address
--
-- ObjC selector: @- languageIdentifier@
languageIdentifier :: IsNSTermOfAddress nsTermOfAddress => nsTermOfAddress -> IO (Id NSString)
languageIdentifier nsTermOfAddress  =
  sendMsg nsTermOfAddress (mkSelector "languageIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A list of pronouns for a localized term of address
--
-- ObjC selector: @- pronouns@
pronouns :: IsNSTermOfAddress nsTermOfAddress => nsTermOfAddress -> IO (Id NSArray)
pronouns nsTermOfAddress  =
  sendMsg nsTermOfAddress (mkSelector "pronouns") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @neutral@
neutralSelector :: Selector
neutralSelector = mkSelector "neutral"

-- | @Selector@ for @feminine@
feminineSelector :: Selector
feminineSelector = mkSelector "feminine"

-- | @Selector@ for @masculine@
masculineSelector :: Selector
masculineSelector = mkSelector "masculine"

-- | @Selector@ for @currentUser@
currentUserSelector :: Selector
currentUserSelector = mkSelector "currentUser"

-- | @Selector@ for @localizedForLanguageIdentifier:withPronouns:@
localizedForLanguageIdentifier_withPronounsSelector :: Selector
localizedForLanguageIdentifier_withPronounsSelector = mkSelector "localizedForLanguageIdentifier:withPronouns:"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @languageIdentifier@
languageIdentifierSelector :: Selector
languageIdentifierSelector = mkSelector "languageIdentifier"

-- | @Selector@ for @pronouns@
pronounsSelector :: Selector
pronounsSelector = mkSelector "pronouns"

