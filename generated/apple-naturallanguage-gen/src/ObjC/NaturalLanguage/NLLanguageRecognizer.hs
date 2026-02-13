{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NLLanguageRecognizer@.
module ObjC.NaturalLanguage.NLLanguageRecognizer
  ( NLLanguageRecognizer
  , IsNLLanguageRecognizer(..)
  , dominantLanguageForString
  , init_
  , processString
  , reset
  , languageHypothesesWithMaximum
  , dominantLanguage
  , languageHints
  , setLanguageHints
  , languageConstraints
  , setLanguageConstraints
  , dominantLanguageForStringSelector
  , dominantLanguageSelector
  , initSelector
  , languageConstraintsSelector
  , languageHintsSelector
  , languageHypothesesWithMaximumSelector
  , processStringSelector
  , resetSelector
  , setLanguageConstraintsSelector
  , setLanguageHintsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NaturalLanguage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ dominantLanguageForString:@
dominantLanguageForString :: IsNSString string => string -> IO (Id NSString)
dominantLanguageForString string =
  do
    cls' <- getRequiredClass "NLLanguageRecognizer"
    sendClassMessage cls' dominantLanguageForStringSelector (toNSString string)

-- | @- init@
init_ :: IsNLLanguageRecognizer nlLanguageRecognizer => nlLanguageRecognizer -> IO (Id NLLanguageRecognizer)
init_ nlLanguageRecognizer =
  sendOwnedMessage nlLanguageRecognizer initSelector

-- | @- processString:@
processString :: (IsNLLanguageRecognizer nlLanguageRecognizer, IsNSString string) => nlLanguageRecognizer -> string -> IO ()
processString nlLanguageRecognizer string =
  sendMessage nlLanguageRecognizer processStringSelector (toNSString string)

-- | @- reset@
reset :: IsNLLanguageRecognizer nlLanguageRecognizer => nlLanguageRecognizer -> IO ()
reset nlLanguageRecognizer =
  sendMessage nlLanguageRecognizer resetSelector

-- | @- languageHypothesesWithMaximum:@
languageHypothesesWithMaximum :: IsNLLanguageRecognizer nlLanguageRecognizer => nlLanguageRecognizer -> CULong -> IO (Id NSDictionary)
languageHypothesesWithMaximum nlLanguageRecognizer maxHypotheses =
  sendMessage nlLanguageRecognizer languageHypothesesWithMaximumSelector maxHypotheses

-- | @- dominantLanguage@
dominantLanguage :: IsNLLanguageRecognizer nlLanguageRecognizer => nlLanguageRecognizer -> IO (Id NSString)
dominantLanguage nlLanguageRecognizer =
  sendMessage nlLanguageRecognizer dominantLanguageSelector

-- | @- languageHints@
languageHints :: IsNLLanguageRecognizer nlLanguageRecognizer => nlLanguageRecognizer -> IO (Id NSDictionary)
languageHints nlLanguageRecognizer =
  sendMessage nlLanguageRecognizer languageHintsSelector

-- | @- setLanguageHints:@
setLanguageHints :: (IsNLLanguageRecognizer nlLanguageRecognizer, IsNSDictionary value) => nlLanguageRecognizer -> value -> IO ()
setLanguageHints nlLanguageRecognizer value =
  sendMessage nlLanguageRecognizer setLanguageHintsSelector (toNSDictionary value)

-- | @- languageConstraints@
languageConstraints :: IsNLLanguageRecognizer nlLanguageRecognizer => nlLanguageRecognizer -> IO (Id NSArray)
languageConstraints nlLanguageRecognizer =
  sendMessage nlLanguageRecognizer languageConstraintsSelector

-- | @- setLanguageConstraints:@
setLanguageConstraints :: (IsNLLanguageRecognizer nlLanguageRecognizer, IsNSArray value) => nlLanguageRecognizer -> value -> IO ()
setLanguageConstraints nlLanguageRecognizer value =
  sendMessage nlLanguageRecognizer setLanguageConstraintsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dominantLanguageForString:@
dominantLanguageForStringSelector :: Selector '[Id NSString] (Id NSString)
dominantLanguageForStringSelector = mkSelector "dominantLanguageForString:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NLLanguageRecognizer)
initSelector = mkSelector "init"

-- | @Selector@ for @processString:@
processStringSelector :: Selector '[Id NSString] ()
processStringSelector = mkSelector "processString:"

-- | @Selector@ for @reset@
resetSelector :: Selector '[] ()
resetSelector = mkSelector "reset"

-- | @Selector@ for @languageHypothesesWithMaximum:@
languageHypothesesWithMaximumSelector :: Selector '[CULong] (Id NSDictionary)
languageHypothesesWithMaximumSelector = mkSelector "languageHypothesesWithMaximum:"

-- | @Selector@ for @dominantLanguage@
dominantLanguageSelector :: Selector '[] (Id NSString)
dominantLanguageSelector = mkSelector "dominantLanguage"

-- | @Selector@ for @languageHints@
languageHintsSelector :: Selector '[] (Id NSDictionary)
languageHintsSelector = mkSelector "languageHints"

-- | @Selector@ for @setLanguageHints:@
setLanguageHintsSelector :: Selector '[Id NSDictionary] ()
setLanguageHintsSelector = mkSelector "setLanguageHints:"

-- | @Selector@ for @languageConstraints@
languageConstraintsSelector :: Selector '[] (Id NSArray)
languageConstraintsSelector = mkSelector "languageConstraints"

-- | @Selector@ for @setLanguageConstraints:@
setLanguageConstraintsSelector :: Selector '[Id NSArray] ()
setLanguageConstraintsSelector = mkSelector "setLanguageConstraints:"

