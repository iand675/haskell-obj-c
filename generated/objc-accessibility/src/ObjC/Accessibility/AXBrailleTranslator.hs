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
  , initWithBrailleTableSelector
  , translatePrintTextSelector
  , backTranslateBrailleSelector
  , initSelector
  , newSelector


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

-- | @- initWithBrailleTable:@
initWithBrailleTable :: (IsAXBrailleTranslator axBrailleTranslator, IsAXBrailleTable brailleTable) => axBrailleTranslator -> brailleTable -> IO (Id AXBrailleTranslator)
initWithBrailleTable axBrailleTranslator  brailleTable =
withObjCPtr brailleTable $ \raw_brailleTable ->
    sendMsg axBrailleTranslator (mkSelector "initWithBrailleTable:") (retPtr retVoid) [argPtr (castPtr raw_brailleTable :: Ptr ())] >>= ownedObject . castPtr

-- | Output Braille uses the unicode Braille characters (0x2800-0x28FF).
--
-- ObjC selector: @- translatePrintText:@
translatePrintText :: (IsAXBrailleTranslator axBrailleTranslator, IsNSString printText) => axBrailleTranslator -> printText -> IO (Id AXBrailleTranslationResult)
translatePrintText axBrailleTranslator  printText =
withObjCPtr printText $ \raw_printText ->
    sendMsg axBrailleTranslator (mkSelector "translatePrintText:") (retPtr retVoid) [argPtr (castPtr raw_printText :: Ptr ())] >>= retainedObject . castPtr

-- | Input Braille should use the unicode Braille characters (0x2800-0x28FF).
--
-- ObjC selector: @- backTranslateBraille:@
backTranslateBraille :: (IsAXBrailleTranslator axBrailleTranslator, IsNSString braille) => axBrailleTranslator -> braille -> IO (Id AXBrailleTranslationResult)
backTranslateBraille axBrailleTranslator  braille =
withObjCPtr braille $ \raw_braille ->
    sendMsg axBrailleTranslator (mkSelector "backTranslateBraille:") (retPtr retVoid) [argPtr (castPtr raw_braille :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsAXBrailleTranslator axBrailleTranslator => axBrailleTranslator -> IO (Id AXBrailleTranslator)
init_ axBrailleTranslator  =
  sendMsg axBrailleTranslator (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AXBrailleTranslator)
new  =
  do
    cls' <- getRequiredClass "AXBrailleTranslator"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithBrailleTable:@
initWithBrailleTableSelector :: Selector
initWithBrailleTableSelector = mkSelector "initWithBrailleTable:"

-- | @Selector@ for @translatePrintText:@
translatePrintTextSelector :: Selector
translatePrintTextSelector = mkSelector "translatePrintText:"

-- | @Selector@ for @backTranslateBraille:@
backTranslateBrailleSelector :: Selector
backTranslateBrailleSelector = mkSelector "backTranslateBraille:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

