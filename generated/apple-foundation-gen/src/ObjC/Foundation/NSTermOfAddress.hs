{-# LANGUAGE DataKinds #-}
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
  , currentUserSelector
  , feminineSelector
  , initSelector
  , languageIdentifierSelector
  , localizedForLanguageIdentifier_withPronounsSelector
  , masculineSelector
  , neutralSelector
  , newSelector
  , pronounsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' neutralSelector

-- | Term of address that uses feminine pronouns (e.g. she/her/hers in English), and a feminine grammatical gender when inflecting verbs and adjectives referring to the person
--
-- ObjC selector: @+ feminine@
feminine :: IO (Id NSTermOfAddress)
feminine  =
  do
    cls' <- getRequiredClass "NSTermOfAddress"
    sendClassMessage cls' feminineSelector

-- | Term of address that uses masculine pronouns (e.g. he/him/his in English), and a masculine grammatical gender when inflecting verbs and adjectives referring to the person
--
-- ObjC selector: @+ masculine@
masculine :: IO (Id NSTermOfAddress)
masculine  =
  do
    cls' <- getRequiredClass "NSTermOfAddress"
    sendClassMessage cls' masculineSelector

-- | The term of address that should be used for addressing the user
--
-- This term of address will only compare equal to another @+[NSTermOfAddress currentUser]@
--
-- ObjC selector: @+ currentUser@
currentUser :: IO (Id NSTermOfAddress)
currentUser  =
  do
    cls' <- getRequiredClass "NSTermOfAddress"
    sendClassMessage cls' currentUserSelector

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
    sendClassMessage cls' localizedForLanguageIdentifier_withPronounsSelector (toNSString language) (toNSArray pronouns)

-- | @+ new@
new :: IO (Id NSTermOfAddress)
new  =
  do
    cls' <- getRequiredClass "NSTermOfAddress"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsNSTermOfAddress nsTermOfAddress => nsTermOfAddress -> IO (Id NSTermOfAddress)
init_ nsTermOfAddress =
  sendOwnedMessage nsTermOfAddress initSelector

-- | The ISO language code if this is a localized term of address
--
-- ObjC selector: @- languageIdentifier@
languageIdentifier :: IsNSTermOfAddress nsTermOfAddress => nsTermOfAddress -> IO (Id NSString)
languageIdentifier nsTermOfAddress =
  sendMessage nsTermOfAddress languageIdentifierSelector

-- | A list of pronouns for a localized term of address
--
-- ObjC selector: @- pronouns@
pronouns :: IsNSTermOfAddress nsTermOfAddress => nsTermOfAddress -> IO (Id NSArray)
pronouns nsTermOfAddress =
  sendMessage nsTermOfAddress pronounsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @neutral@
neutralSelector :: Selector '[] (Id NSTermOfAddress)
neutralSelector = mkSelector "neutral"

-- | @Selector@ for @feminine@
feminineSelector :: Selector '[] (Id NSTermOfAddress)
feminineSelector = mkSelector "feminine"

-- | @Selector@ for @masculine@
masculineSelector :: Selector '[] (Id NSTermOfAddress)
masculineSelector = mkSelector "masculine"

-- | @Selector@ for @currentUser@
currentUserSelector :: Selector '[] (Id NSTermOfAddress)
currentUserSelector = mkSelector "currentUser"

-- | @Selector@ for @localizedForLanguageIdentifier:withPronouns:@
localizedForLanguageIdentifier_withPronounsSelector :: Selector '[Id NSString, Id NSArray] (Id NSTermOfAddress)
localizedForLanguageIdentifier_withPronounsSelector = mkSelector "localizedForLanguageIdentifier:withPronouns:"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSTermOfAddress)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSTermOfAddress)
initSelector = mkSelector "init"

-- | @Selector@ for @languageIdentifier@
languageIdentifierSelector :: Selector '[] (Id NSString)
languageIdentifierSelector = mkSelector "languageIdentifier"

-- | @Selector@ for @pronouns@
pronounsSelector :: Selector '[] (Id NSArray)
pronounsSelector = mkSelector "pronouns"

