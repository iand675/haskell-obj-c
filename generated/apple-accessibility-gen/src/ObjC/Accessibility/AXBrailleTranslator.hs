{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Translates print text to Braille and Braille to print text according to the given Braille table.
--
-- Generated bindings for @AXBrailleTranslator@.
module ObjC.Accessibility.AXBrailleTranslator
  ( AXBrailleTranslator
  , IsAXBrailleTranslator(..)
  , initWithBrailleTable
  , translatePrintText
  , backTranslateBraille
  , init_
  , new
  , backTranslateBrailleSelector
  , initSelector
  , initWithBrailleTableSelector
  , newSelector
  , translatePrintTextSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Accessibility.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithBrailleTable:@
initWithBrailleTable :: (IsAXBrailleTranslator axBrailleTranslator, IsAXBrailleTable brailleTable) => axBrailleTranslator -> brailleTable -> IO (Id AXBrailleTranslator)
initWithBrailleTable axBrailleTranslator brailleTable =
  sendOwnedMessage axBrailleTranslator initWithBrailleTableSelector (toAXBrailleTable brailleTable)

-- | Output Braille uses the unicode Braille characters (0x2800-0x28FF).
--
-- ObjC selector: @- translatePrintText:@
translatePrintText :: (IsAXBrailleTranslator axBrailleTranslator, IsNSString printText) => axBrailleTranslator -> printText -> IO (Id AXBrailleTranslationResult)
translatePrintText axBrailleTranslator printText =
  sendMessage axBrailleTranslator translatePrintTextSelector (toNSString printText)

-- | Input Braille should use the unicode Braille characters (0x2800-0x28FF).
--
-- ObjC selector: @- backTranslateBraille:@
backTranslateBraille :: (IsAXBrailleTranslator axBrailleTranslator, IsNSString braille) => axBrailleTranslator -> braille -> IO (Id AXBrailleTranslationResult)
backTranslateBraille axBrailleTranslator braille =
  sendMessage axBrailleTranslator backTranslateBrailleSelector (toNSString braille)

-- | @- init@
init_ :: IsAXBrailleTranslator axBrailleTranslator => axBrailleTranslator -> IO (Id AXBrailleTranslator)
init_ axBrailleTranslator =
  sendOwnedMessage axBrailleTranslator initSelector

-- | @+ new@
new :: IO (Id AXBrailleTranslator)
new  =
  do
    cls' <- getRequiredClass "AXBrailleTranslator"
    sendOwnedClassMessage cls' newSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithBrailleTable:@
initWithBrailleTableSelector :: Selector '[Id AXBrailleTable] (Id AXBrailleTranslator)
initWithBrailleTableSelector = mkSelector "initWithBrailleTable:"

-- | @Selector@ for @translatePrintText:@
translatePrintTextSelector :: Selector '[Id NSString] (Id AXBrailleTranslationResult)
translatePrintTextSelector = mkSelector "translatePrintText:"

-- | @Selector@ for @backTranslateBraille:@
backTranslateBrailleSelector :: Selector '[Id NSString] (Id AXBrailleTranslationResult)
backTranslateBrailleSelector = mkSelector "backTranslateBraille:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AXBrailleTranslator)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AXBrailleTranslator)
newSelector = mkSelector "new"

