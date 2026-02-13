{-# LANGUAGE DataKinds #-}
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
  , locationMapSelector
  , newSelector
  , resultStringSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Accessibility.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAXBrailleTranslationResult axBrailleTranslationResult => axBrailleTranslationResult -> IO (Id AXBrailleTranslationResult)
init_ axBrailleTranslationResult =
  sendOwnedMessage axBrailleTranslationResult initSelector

-- | @+ new@
new :: IO (Id AXBrailleTranslationResult)
new  =
  do
    cls' <- getRequiredClass "AXBrailleTranslationResult"
    sendOwnedClassMessage cls' newSelector

-- | The resulting string after translation or back-translation.
--
-- ObjC selector: @- resultString@
resultString :: IsAXBrailleTranslationResult axBrailleTranslationResult => axBrailleTranslationResult -> IO (Id NSString)
resultString axBrailleTranslationResult =
  sendMessage axBrailleTranslationResult resultStringSelector

-- | An array of integers that has the same length as the resultString. locationMap[i]-th character in the input string corresponds to resultString[i].
--
-- ObjC selector: @- locationMap@
locationMap :: IsAXBrailleTranslationResult axBrailleTranslationResult => axBrailleTranslationResult -> IO (Id NSArray)
locationMap axBrailleTranslationResult =
  sendMessage axBrailleTranslationResult locationMapSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AXBrailleTranslationResult)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AXBrailleTranslationResult)
newSelector = mkSelector "new"

-- | @Selector@ for @resultString@
resultStringSelector :: Selector '[] (Id NSString)
resultStringSelector = mkSelector "resultString"

-- | @Selector@ for @locationMap@
locationMapSelector :: Selector '[] (Id NSArray)
locationMapSelector = mkSelector "locationMap"

