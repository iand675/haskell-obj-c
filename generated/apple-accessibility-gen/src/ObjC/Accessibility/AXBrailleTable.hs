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
  , supportedLocalesSelector
  , defaultTableForLocaleSelector
  , tablesForLocaleSelector
  , languageAgnosticTablesSelector
  , initWithIdentifierSelector
  , initSelector
  , newSelector
  , identifierSelector
  , localizedNameSelector
  , providerIdentifierSelector
  , localizedProviderNameSelector
  , languageSelector
  , localesSelector
  , isEightDotSelector


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

import ObjC.Accessibility.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | All locales supported by existing tables.
--
-- ObjC selector: @+ supportedLocales@
supportedLocales :: IO (Id NSSet)
supportedLocales  =
  do
    cls' <- getRequiredClass "AXBrailleTable"
    sendClassMsg cls' (mkSelector "supportedLocales") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The default table that provides translations for the given locale's language. Returns nil if there is none.
--
-- ObjC selector: @+ defaultTableForLocale:@
defaultTableForLocale :: IsNSLocale locale => locale -> IO (Id AXBrailleTable)
defaultTableForLocale locale =
  do
    cls' <- getRequiredClass "AXBrailleTable"
    withObjCPtr locale $ \raw_locale ->
      sendClassMsg cls' (mkSelector "defaultTableForLocale:") (retPtr retVoid) [argPtr (castPtr raw_locale :: Ptr ())] >>= retainedObject . castPtr

-- | All tables that provide translations for the given locale's language.
--
-- ObjC selector: @+ tablesForLocale:@
tablesForLocale :: IsNSLocale locale => locale -> IO (Id NSSet)
tablesForLocale locale =
  do
    cls' <- getRequiredClass "AXBrailleTable"
    withObjCPtr locale $ \raw_locale ->
      sendClassMsg cls' (mkSelector "tablesForLocale:") (retPtr retVoid) [argPtr (castPtr raw_locale :: Ptr ())] >>= retainedObject . castPtr

-- | All tables that are not specific to any language.
--
-- ObjC selector: @+ languageAgnosticTables@
languageAgnosticTables :: IO (Id NSSet)
languageAgnosticTables  =
  do
    cls' <- getRequiredClass "AXBrailleTable"
    sendClassMsg cls' (mkSelector "languageAgnosticTables") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns nil if there is no table with the given identifier.
--
-- ObjC selector: @- initWithIdentifier:@
initWithIdentifier :: (IsAXBrailleTable axBrailleTable, IsNSString identifier) => axBrailleTable -> identifier -> IO (Id AXBrailleTable)
initWithIdentifier axBrailleTable  identifier =
  withObjCPtr identifier $ \raw_identifier ->
      sendMsg axBrailleTable (mkSelector "initWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsAXBrailleTable axBrailleTable => axBrailleTable -> IO (Id AXBrailleTable)
init_ axBrailleTable  =
    sendMsg axBrailleTable (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AXBrailleTable)
new  =
  do
    cls' <- getRequiredClass "AXBrailleTable"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | A unique string that identifies this table.
--
-- ObjC selector: @- identifier@
identifier :: IsAXBrailleTable axBrailleTable => axBrailleTable -> IO (Id NSString)
identifier axBrailleTable  =
    sendMsg axBrailleTable (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The localized name of this table for user display.
--
-- ObjC selector: @- localizedName@
localizedName :: IsAXBrailleTable axBrailleTable => axBrailleTable -> IO (Id NSString)
localizedName axBrailleTable  =
    sendMsg axBrailleTable (mkSelector "localizedName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The identifier of the provider of this table.
--
-- ObjC selector: @- providerIdentifier@
providerIdentifier :: IsAXBrailleTable axBrailleTable => axBrailleTable -> IO (Id NSString)
providerIdentifier axBrailleTable  =
    sendMsg axBrailleTable (mkSelector "providerIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The localized name of the provider of this table for user display.
--
-- ObjC selector: @- localizedProviderName@
localizedProviderName :: IsAXBrailleTable axBrailleTable => axBrailleTable -> IO (Id NSString)
localizedProviderName axBrailleTable  =
    sendMsg axBrailleTable (mkSelector "localizedProviderName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The 3-character code from ISO 639-2 for the language this Braille table pertains to.
--
-- ObjC selector: @- language@
language :: IsAXBrailleTable axBrailleTable => axBrailleTable -> IO (Id NSString)
language axBrailleTable  =
    sendMsg axBrailleTable (mkSelector "language") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | All locales this table supports.
--
-- ObjC selector: @- locales@
locales :: IsAXBrailleTable axBrailleTable => axBrailleTable -> IO (Id NSSet)
locales axBrailleTable  =
    sendMsg axBrailleTable (mkSelector "locales") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns true if this table makes use of eight dots as opposed to six dots.
--
-- ObjC selector: @- isEightDot@
isEightDot :: IsAXBrailleTable axBrailleTable => axBrailleTable -> IO Bool
isEightDot axBrailleTable  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg axBrailleTable (mkSelector "isEightDot") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @supportedLocales@
supportedLocalesSelector :: Selector
supportedLocalesSelector = mkSelector "supportedLocales"

-- | @Selector@ for @defaultTableForLocale:@
defaultTableForLocaleSelector :: Selector
defaultTableForLocaleSelector = mkSelector "defaultTableForLocale:"

-- | @Selector@ for @tablesForLocale:@
tablesForLocaleSelector :: Selector
tablesForLocaleSelector = mkSelector "tablesForLocale:"

-- | @Selector@ for @languageAgnosticTables@
languageAgnosticTablesSelector :: Selector
languageAgnosticTablesSelector = mkSelector "languageAgnosticTables"

-- | @Selector@ for @initWithIdentifier:@
initWithIdentifierSelector :: Selector
initWithIdentifierSelector = mkSelector "initWithIdentifier:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @localizedName@
localizedNameSelector :: Selector
localizedNameSelector = mkSelector "localizedName"

-- | @Selector@ for @providerIdentifier@
providerIdentifierSelector :: Selector
providerIdentifierSelector = mkSelector "providerIdentifier"

-- | @Selector@ for @localizedProviderName@
localizedProviderNameSelector :: Selector
localizedProviderNameSelector = mkSelector "localizedProviderName"

-- | @Selector@ for @language@
languageSelector :: Selector
languageSelector = mkSelector "language"

-- | @Selector@ for @locales@
localesSelector :: Selector
localesSelector = mkSelector "locales"

-- | @Selector@ for @isEightDot@
isEightDotSelector :: Selector
isEightDotSelector = mkSelector "isEightDot"

