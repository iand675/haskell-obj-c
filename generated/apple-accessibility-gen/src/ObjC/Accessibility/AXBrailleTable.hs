{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A rule for translating print text to Braille, and back-translating Braille to print text.
--
-- Generated bindings for @AXBrailleTable@.
module ObjC.Accessibility.AXBrailleTable
  ( AXBrailleTable
  , IsAXBrailleTable(..)
  , supportedLocales
  , defaultTableForLocale
  , tablesForLocale
  , languageAgnosticTables
  , initWithIdentifier
  , init_
  , new
  , identifier
  , localizedName
  , providerIdentifier
  , localizedProviderName
  , language
  , locales
  , isEightDot
  , defaultTableForLocaleSelector
  , identifierSelector
  , initSelector
  , initWithIdentifierSelector
  , isEightDotSelector
  , languageAgnosticTablesSelector
  , languageSelector
  , localesSelector
  , localizedNameSelector
  , localizedProviderNameSelector
  , newSelector
  , providerIdentifierSelector
  , supportedLocalesSelector
  , tablesForLocaleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Accessibility.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | All locales supported by existing tables.
--
-- ObjC selector: @+ supportedLocales@
supportedLocales :: IO (Id NSSet)
supportedLocales  =
  do
    cls' <- getRequiredClass "AXBrailleTable"
    sendClassMessage cls' supportedLocalesSelector

-- | The default table that provides translations for the given locale's language. Returns nil if there is none.
--
-- ObjC selector: @+ defaultTableForLocale:@
defaultTableForLocale :: IsNSLocale locale => locale -> IO (Id AXBrailleTable)
defaultTableForLocale locale =
  do
    cls' <- getRequiredClass "AXBrailleTable"
    sendClassMessage cls' defaultTableForLocaleSelector (toNSLocale locale)

-- | All tables that provide translations for the given locale's language.
--
-- ObjC selector: @+ tablesForLocale:@
tablesForLocale :: IsNSLocale locale => locale -> IO (Id NSSet)
tablesForLocale locale =
  do
    cls' <- getRequiredClass "AXBrailleTable"
    sendClassMessage cls' tablesForLocaleSelector (toNSLocale locale)

-- | All tables that are not specific to any language.
--
-- ObjC selector: @+ languageAgnosticTables@
languageAgnosticTables :: IO (Id NSSet)
languageAgnosticTables  =
  do
    cls' <- getRequiredClass "AXBrailleTable"
    sendClassMessage cls' languageAgnosticTablesSelector

-- | Returns nil if there is no table with the given identifier.
--
-- ObjC selector: @- initWithIdentifier:@
initWithIdentifier :: (IsAXBrailleTable axBrailleTable, IsNSString identifier) => axBrailleTable -> identifier -> IO (Id AXBrailleTable)
initWithIdentifier axBrailleTable identifier =
  sendOwnedMessage axBrailleTable initWithIdentifierSelector (toNSString identifier)

-- | @- init@
init_ :: IsAXBrailleTable axBrailleTable => axBrailleTable -> IO (Id AXBrailleTable)
init_ axBrailleTable =
  sendOwnedMessage axBrailleTable initSelector

-- | @+ new@
new :: IO (Id AXBrailleTable)
new  =
  do
    cls' <- getRequiredClass "AXBrailleTable"
    sendOwnedClassMessage cls' newSelector

-- | A unique string that identifies this table.
--
-- ObjC selector: @- identifier@
identifier :: IsAXBrailleTable axBrailleTable => axBrailleTable -> IO (Id NSString)
identifier axBrailleTable =
  sendMessage axBrailleTable identifierSelector

-- | The localized name of this table for user display.
--
-- ObjC selector: @- localizedName@
localizedName :: IsAXBrailleTable axBrailleTable => axBrailleTable -> IO (Id NSString)
localizedName axBrailleTable =
  sendMessage axBrailleTable localizedNameSelector

-- | The identifier of the provider of this table.
--
-- ObjC selector: @- providerIdentifier@
providerIdentifier :: IsAXBrailleTable axBrailleTable => axBrailleTable -> IO (Id NSString)
providerIdentifier axBrailleTable =
  sendMessage axBrailleTable providerIdentifierSelector

-- | The localized name of the provider of this table for user display.
--
-- ObjC selector: @- localizedProviderName@
localizedProviderName :: IsAXBrailleTable axBrailleTable => axBrailleTable -> IO (Id NSString)
localizedProviderName axBrailleTable =
  sendMessage axBrailleTable localizedProviderNameSelector

-- | The 3-character code from ISO 639-2 for the language this Braille table pertains to.
--
-- ObjC selector: @- language@
language :: IsAXBrailleTable axBrailleTable => axBrailleTable -> IO (Id NSString)
language axBrailleTable =
  sendMessage axBrailleTable languageSelector

-- | All locales this table supports.
--
-- ObjC selector: @- locales@
locales :: IsAXBrailleTable axBrailleTable => axBrailleTable -> IO (Id NSSet)
locales axBrailleTable =
  sendMessage axBrailleTable localesSelector

-- | Returns true if this table makes use of eight dots as opposed to six dots.
--
-- ObjC selector: @- isEightDot@
isEightDot :: IsAXBrailleTable axBrailleTable => axBrailleTable -> IO Bool
isEightDot axBrailleTable =
  sendMessage axBrailleTable isEightDotSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @supportedLocales@
supportedLocalesSelector :: Selector '[] (Id NSSet)
supportedLocalesSelector = mkSelector "supportedLocales"

-- | @Selector@ for @defaultTableForLocale:@
defaultTableForLocaleSelector :: Selector '[Id NSLocale] (Id AXBrailleTable)
defaultTableForLocaleSelector = mkSelector "defaultTableForLocale:"

-- | @Selector@ for @tablesForLocale:@
tablesForLocaleSelector :: Selector '[Id NSLocale] (Id NSSet)
tablesForLocaleSelector = mkSelector "tablesForLocale:"

-- | @Selector@ for @languageAgnosticTables@
languageAgnosticTablesSelector :: Selector '[] (Id NSSet)
languageAgnosticTablesSelector = mkSelector "languageAgnosticTables"

-- | @Selector@ for @initWithIdentifier:@
initWithIdentifierSelector :: Selector '[Id NSString] (Id AXBrailleTable)
initWithIdentifierSelector = mkSelector "initWithIdentifier:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AXBrailleTable)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AXBrailleTable)
newSelector = mkSelector "new"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @localizedName@
localizedNameSelector :: Selector '[] (Id NSString)
localizedNameSelector = mkSelector "localizedName"

-- | @Selector@ for @providerIdentifier@
providerIdentifierSelector :: Selector '[] (Id NSString)
providerIdentifierSelector = mkSelector "providerIdentifier"

-- | @Selector@ for @localizedProviderName@
localizedProviderNameSelector :: Selector '[] (Id NSString)
localizedProviderNameSelector = mkSelector "localizedProviderName"

-- | @Selector@ for @language@
languageSelector :: Selector '[] (Id NSString)
languageSelector = mkSelector "language"

-- | @Selector@ for @locales@
localesSelector :: Selector '[] (Id NSSet)
localesSelector = mkSelector "locales"

-- | @Selector@ for @isEightDot@
isEightDotSelector :: Selector '[] Bool
isEightDotSelector = mkSelector "isEightDot"

