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
  , languageConstraints
  , setLanguageConstraints
  , dominantLanguageForStringSelector
  , initSelector
  , processStringSelector
  , resetSelector
  , languageHypothesesWithMaximumSelector
  , dominantLanguageSelector
  , languageConstraintsSelector
  , setLanguageConstraintsSelector


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

import ObjC.NaturalLanguage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ dominantLanguageForString:@
dominantLanguageForString :: IsNSString string => string -> IO (Id NSString)
dominantLanguageForString string =
  do
    cls' <- getRequiredClass "NLLanguageRecognizer"
    withObjCPtr string $ \raw_string ->
      sendClassMsg cls' (mkSelector "dominantLanguageForString:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsNLLanguageRecognizer nlLanguageRecognizer => nlLanguageRecognizer -> IO (Id NLLanguageRecognizer)
init_ nlLanguageRecognizer  =
  sendMsg nlLanguageRecognizer (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- processString:@
processString :: (IsNLLanguageRecognizer nlLanguageRecognizer, IsNSString string) => nlLanguageRecognizer -> string -> IO ()
processString nlLanguageRecognizer  string =
withObjCPtr string $ \raw_string ->
    sendMsg nlLanguageRecognizer (mkSelector "processString:") retVoid [argPtr (castPtr raw_string :: Ptr ())]

-- | @- reset@
reset :: IsNLLanguageRecognizer nlLanguageRecognizer => nlLanguageRecognizer -> IO ()
reset nlLanguageRecognizer  =
  sendMsg nlLanguageRecognizer (mkSelector "reset") retVoid []

-- | @- languageHypothesesWithMaximum:@
languageHypothesesWithMaximum :: IsNLLanguageRecognizer nlLanguageRecognizer => nlLanguageRecognizer -> CULong -> IO (Id NSDictionary)
languageHypothesesWithMaximum nlLanguageRecognizer  maxHypotheses =
  sendMsg nlLanguageRecognizer (mkSelector "languageHypothesesWithMaximum:") (retPtr retVoid) [argCULong (fromIntegral maxHypotheses)] >>= retainedObject . castPtr

-- | @- dominantLanguage@
dominantLanguage :: IsNLLanguageRecognizer nlLanguageRecognizer => nlLanguageRecognizer -> IO (Id NSString)
dominantLanguage nlLanguageRecognizer  =
  sendMsg nlLanguageRecognizer (mkSelector "dominantLanguage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- languageConstraints@
languageConstraints :: IsNLLanguageRecognizer nlLanguageRecognizer => nlLanguageRecognizer -> IO (Id NSArray)
languageConstraints nlLanguageRecognizer  =
  sendMsg nlLanguageRecognizer (mkSelector "languageConstraints") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLanguageConstraints:@
setLanguageConstraints :: (IsNLLanguageRecognizer nlLanguageRecognizer, IsNSArray value) => nlLanguageRecognizer -> value -> IO ()
setLanguageConstraints nlLanguageRecognizer  value =
withObjCPtr value $ \raw_value ->
    sendMsg nlLanguageRecognizer (mkSelector "setLanguageConstraints:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dominantLanguageForString:@
dominantLanguageForStringSelector :: Selector
dominantLanguageForStringSelector = mkSelector "dominantLanguageForString:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @processString:@
processStringSelector :: Selector
processStringSelector = mkSelector "processString:"

-- | @Selector@ for @reset@
resetSelector :: Selector
resetSelector = mkSelector "reset"

-- | @Selector@ for @languageHypothesesWithMaximum:@
languageHypothesesWithMaximumSelector :: Selector
languageHypothesesWithMaximumSelector = mkSelector "languageHypothesesWithMaximum:"

-- | @Selector@ for @dominantLanguage@
dominantLanguageSelector :: Selector
dominantLanguageSelector = mkSelector "dominantLanguage"

-- | @Selector@ for @languageConstraints@
languageConstraintsSelector :: Selector
languageConstraintsSelector = mkSelector "languageConstraints"

-- | @Selector@ for @setLanguageConstraints:@
setLanguageConstraintsSelector :: Selector
setLanguageConstraintsSelector = mkSelector "setLanguageConstraints:"

