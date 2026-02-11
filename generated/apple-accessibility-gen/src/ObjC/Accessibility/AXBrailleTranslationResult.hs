{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The result of translation or back-translation.
--
-- Generated bindings for @AXBrailleTranslationResult@.
module ObjC.Accessibility.AXBrailleTranslationResult
  ( AXBrailleTranslationResult
  , IsAXBrailleTranslationResult(..)
  , init_
  , new
  , resultString
  , locationMap
  , initSelector
  , newSelector
  , resultStringSelector
  , locationMapSelector


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

-- | @- init@
init_ :: IsAXBrailleTranslationResult axBrailleTranslationResult => axBrailleTranslationResult -> IO (Id AXBrailleTranslationResult)
init_ axBrailleTranslationResult  =
    sendMsg axBrailleTranslationResult (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AXBrailleTranslationResult)
new  =
  do
    cls' <- getRequiredClass "AXBrailleTranslationResult"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The resulting string after translation or back-translation.
--
-- ObjC selector: @- resultString@
resultString :: IsAXBrailleTranslationResult axBrailleTranslationResult => axBrailleTranslationResult -> IO (Id NSString)
resultString axBrailleTranslationResult  =
    sendMsg axBrailleTranslationResult (mkSelector "resultString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An array of integers that has the same length as the resultString. locationMap[i]-th character in the input string corresponds to resultString[i].
--
-- ObjC selector: @- locationMap@
locationMap :: IsAXBrailleTranslationResult axBrailleTranslationResult => axBrailleTranslationResult -> IO (Id NSArray)
locationMap axBrailleTranslationResult  =
    sendMsg axBrailleTranslationResult (mkSelector "locationMap") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @resultString@
resultStringSelector :: Selector
resultStringSelector = mkSelector "resultString"

-- | @Selector@ for @locationMap@
locationMapSelector :: Selector
locationMapSelector = mkSelector "locationMap"

