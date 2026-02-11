{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMorphology@.
module ObjC.Foundation.NSMorphology
  ( NSMorphology
  , IsNSMorphology(..)
  , customPronounForLanguage
  , setCustomPronoun_forLanguage_error
  , grammaticalGender
  , setGrammaticalGender
  , partOfSpeech
  , setPartOfSpeech
  , number
  , setNumber
  , grammaticalCase
  , setGrammaticalCase
  , determination
  , setDetermination
  , grammaticalPerson
  , setGrammaticalPerson
  , pronounType
  , setPronounType
  , definiteness
  , setDefiniteness
  , unspecified
  , userMorphology
  , customPronounForLanguageSelector
  , setCustomPronoun_forLanguage_errorSelector
  , grammaticalGenderSelector
  , setGrammaticalGenderSelector
  , partOfSpeechSelector
  , setPartOfSpeechSelector
  , numberSelector
  , setNumberSelector
  , grammaticalCaseSelector
  , setGrammaticalCaseSelector
  , determinationSelector
  , setDeterminationSelector
  , grammaticalPersonSelector
  , setGrammaticalPersonSelector
  , pronounTypeSelector
  , setPronounTypeSelector
  , definitenessSelector
  , setDefinitenessSelector
  , unspecifiedSelector
  , userMorphologySelector

  -- * Enum types
  , NSGrammaticalCase(NSGrammaticalCase)
  , pattern NSGrammaticalCaseNotSet
  , pattern NSGrammaticalCaseNominative
  , pattern NSGrammaticalCaseAccusative
  , pattern NSGrammaticalCaseDative
  , pattern NSGrammaticalCaseGenitive
  , pattern NSGrammaticalCasePrepositional
  , pattern NSGrammaticalCaseAblative
  , pattern NSGrammaticalCaseAdessive
  , pattern NSGrammaticalCaseAllative
  , pattern NSGrammaticalCaseElative
  , pattern NSGrammaticalCaseIllative
  , pattern NSGrammaticalCaseEssive
  , pattern NSGrammaticalCaseInessive
  , pattern NSGrammaticalCaseLocative
  , pattern NSGrammaticalCaseTranslative
  , NSGrammaticalDefiniteness(NSGrammaticalDefiniteness)
  , pattern NSGrammaticalDefinitenessNotSet
  , pattern NSGrammaticalDefinitenessIndefinite
  , pattern NSGrammaticalDefinitenessDefinite
  , NSGrammaticalDetermination(NSGrammaticalDetermination)
  , pattern NSGrammaticalDeterminationNotSet
  , pattern NSGrammaticalDeterminationIndependent
  , pattern NSGrammaticalDeterminationDependent
  , NSGrammaticalGender(NSGrammaticalGender)
  , pattern NSGrammaticalGenderNotSet
  , pattern NSGrammaticalGenderFeminine
  , pattern NSGrammaticalGenderMasculine
  , pattern NSGrammaticalGenderNeuter
  , NSGrammaticalNumber(NSGrammaticalNumber)
  , pattern NSGrammaticalNumberNotSet
  , pattern NSGrammaticalNumberSingular
  , pattern NSGrammaticalNumberZero
  , pattern NSGrammaticalNumberPlural
  , pattern NSGrammaticalNumberPluralTwo
  , pattern NSGrammaticalNumberPluralFew
  , pattern NSGrammaticalNumberPluralMany
  , NSGrammaticalPartOfSpeech(NSGrammaticalPartOfSpeech)
  , pattern NSGrammaticalPartOfSpeechNotSet
  , pattern NSGrammaticalPartOfSpeechDeterminer
  , pattern NSGrammaticalPartOfSpeechPronoun
  , pattern NSGrammaticalPartOfSpeechLetter
  , pattern NSGrammaticalPartOfSpeechAdverb
  , pattern NSGrammaticalPartOfSpeechParticle
  , pattern NSGrammaticalPartOfSpeechAdjective
  , pattern NSGrammaticalPartOfSpeechAdposition
  , pattern NSGrammaticalPartOfSpeechVerb
  , pattern NSGrammaticalPartOfSpeechNoun
  , pattern NSGrammaticalPartOfSpeechConjunction
  , pattern NSGrammaticalPartOfSpeechNumeral
  , pattern NSGrammaticalPartOfSpeechInterjection
  , pattern NSGrammaticalPartOfSpeechPreposition
  , pattern NSGrammaticalPartOfSpeechAbbreviation
  , NSGrammaticalPerson(NSGrammaticalPerson)
  , pattern NSGrammaticalPersonNotSet
  , pattern NSGrammaticalPersonFirst
  , pattern NSGrammaticalPersonSecond
  , pattern NSGrammaticalPersonThird
  , NSGrammaticalPronounType(NSGrammaticalPronounType)
  , pattern NSGrammaticalPronounTypeNotSet
  , pattern NSGrammaticalPronounTypePersonal
  , pattern NSGrammaticalPronounTypeReflexive
  , pattern NSGrammaticalPronounTypePossessive

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
import ObjC.Foundation.Internal.Enums

-- | @- customPronounForLanguage:@
customPronounForLanguage :: (IsNSMorphology nsMorphology, IsNSString language) => nsMorphology -> language -> IO (Id NSMorphologyCustomPronoun)
customPronounForLanguage nsMorphology  language =
withObjCPtr language $ \raw_language ->
    sendMsg nsMorphology (mkSelector "customPronounForLanguage:") (retPtr retVoid) [argPtr (castPtr raw_language :: Ptr ())] >>= retainedObject . castPtr

-- | @- setCustomPronoun:forLanguage:error:@
setCustomPronoun_forLanguage_error :: (IsNSMorphology nsMorphology, IsNSMorphologyCustomPronoun features, IsNSString language, IsNSError error_) => nsMorphology -> features -> language -> error_ -> IO Bool
setCustomPronoun_forLanguage_error nsMorphology  features language error_ =
withObjCPtr features $ \raw_features ->
  withObjCPtr language $ \raw_language ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMorphology (mkSelector "setCustomPronoun:forLanguage:error:") retCULong [argPtr (castPtr raw_features :: Ptr ()), argPtr (castPtr raw_language :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- grammaticalGender@
grammaticalGender :: IsNSMorphology nsMorphology => nsMorphology -> IO NSGrammaticalGender
grammaticalGender nsMorphology  =
  fmap (coerce :: CLong -> NSGrammaticalGender) $ sendMsg nsMorphology (mkSelector "grammaticalGender") retCLong []

-- | @- setGrammaticalGender:@
setGrammaticalGender :: IsNSMorphology nsMorphology => nsMorphology -> NSGrammaticalGender -> IO ()
setGrammaticalGender nsMorphology  value =
  sendMsg nsMorphology (mkSelector "setGrammaticalGender:") retVoid [argCLong (coerce value)]

-- | @- partOfSpeech@
partOfSpeech :: IsNSMorphology nsMorphology => nsMorphology -> IO NSGrammaticalPartOfSpeech
partOfSpeech nsMorphology  =
  fmap (coerce :: CLong -> NSGrammaticalPartOfSpeech) $ sendMsg nsMorphology (mkSelector "partOfSpeech") retCLong []

-- | @- setPartOfSpeech:@
setPartOfSpeech :: IsNSMorphology nsMorphology => nsMorphology -> NSGrammaticalPartOfSpeech -> IO ()
setPartOfSpeech nsMorphology  value =
  sendMsg nsMorphology (mkSelector "setPartOfSpeech:") retVoid [argCLong (coerce value)]

-- | @- number@
number :: IsNSMorphology nsMorphology => nsMorphology -> IO NSGrammaticalNumber
number nsMorphology  =
  fmap (coerce :: CLong -> NSGrammaticalNumber) $ sendMsg nsMorphology (mkSelector "number") retCLong []

-- | @- setNumber:@
setNumber :: IsNSMorphology nsMorphology => nsMorphology -> NSGrammaticalNumber -> IO ()
setNumber nsMorphology  value =
  sendMsg nsMorphology (mkSelector "setNumber:") retVoid [argCLong (coerce value)]

-- | @- grammaticalCase@
grammaticalCase :: IsNSMorphology nsMorphology => nsMorphology -> IO NSGrammaticalCase
grammaticalCase nsMorphology  =
  fmap (coerce :: CLong -> NSGrammaticalCase) $ sendMsg nsMorphology (mkSelector "grammaticalCase") retCLong []

-- | @- setGrammaticalCase:@
setGrammaticalCase :: IsNSMorphology nsMorphology => nsMorphology -> NSGrammaticalCase -> IO ()
setGrammaticalCase nsMorphology  value =
  sendMsg nsMorphology (mkSelector "setGrammaticalCase:") retVoid [argCLong (coerce value)]

-- | @- determination@
determination :: IsNSMorphology nsMorphology => nsMorphology -> IO NSGrammaticalDetermination
determination nsMorphology  =
  fmap (coerce :: CLong -> NSGrammaticalDetermination) $ sendMsg nsMorphology (mkSelector "determination") retCLong []

-- | @- setDetermination:@
setDetermination :: IsNSMorphology nsMorphology => nsMorphology -> NSGrammaticalDetermination -> IO ()
setDetermination nsMorphology  value =
  sendMsg nsMorphology (mkSelector "setDetermination:") retVoid [argCLong (coerce value)]

-- | @- grammaticalPerson@
grammaticalPerson :: IsNSMorphology nsMorphology => nsMorphology -> IO NSGrammaticalPerson
grammaticalPerson nsMorphology  =
  fmap (coerce :: CLong -> NSGrammaticalPerson) $ sendMsg nsMorphology (mkSelector "grammaticalPerson") retCLong []

-- | @- setGrammaticalPerson:@
setGrammaticalPerson :: IsNSMorphology nsMorphology => nsMorphology -> NSGrammaticalPerson -> IO ()
setGrammaticalPerson nsMorphology  value =
  sendMsg nsMorphology (mkSelector "setGrammaticalPerson:") retVoid [argCLong (coerce value)]

-- | @- pronounType@
pronounType :: IsNSMorphology nsMorphology => nsMorphology -> IO NSGrammaticalPronounType
pronounType nsMorphology  =
  fmap (coerce :: CLong -> NSGrammaticalPronounType) $ sendMsg nsMorphology (mkSelector "pronounType") retCLong []

-- | @- setPronounType:@
setPronounType :: IsNSMorphology nsMorphology => nsMorphology -> NSGrammaticalPronounType -> IO ()
setPronounType nsMorphology  value =
  sendMsg nsMorphology (mkSelector "setPronounType:") retVoid [argCLong (coerce value)]

-- | @- definiteness@
definiteness :: IsNSMorphology nsMorphology => nsMorphology -> IO NSGrammaticalDefiniteness
definiteness nsMorphology  =
  fmap (coerce :: CLong -> NSGrammaticalDefiniteness) $ sendMsg nsMorphology (mkSelector "definiteness") retCLong []

-- | @- setDefiniteness:@
setDefiniteness :: IsNSMorphology nsMorphology => nsMorphology -> NSGrammaticalDefiniteness -> IO ()
setDefiniteness nsMorphology  value =
  sendMsg nsMorphology (mkSelector "setDefiniteness:") retVoid [argCLong (coerce value)]

-- | @- unspecified@
unspecified :: IsNSMorphology nsMorphology => nsMorphology -> IO Bool
unspecified nsMorphology  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMorphology (mkSelector "unspecified") retCULong []

-- | @+ userMorphology@
userMorphology :: IO (Id NSMorphology)
userMorphology  =
  do
    cls' <- getRequiredClass "NSMorphology"
    sendClassMsg cls' (mkSelector "userMorphology") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @customPronounForLanguage:@
customPronounForLanguageSelector :: Selector
customPronounForLanguageSelector = mkSelector "customPronounForLanguage:"

-- | @Selector@ for @setCustomPronoun:forLanguage:error:@
setCustomPronoun_forLanguage_errorSelector :: Selector
setCustomPronoun_forLanguage_errorSelector = mkSelector "setCustomPronoun:forLanguage:error:"

-- | @Selector@ for @grammaticalGender@
grammaticalGenderSelector :: Selector
grammaticalGenderSelector = mkSelector "grammaticalGender"

-- | @Selector@ for @setGrammaticalGender:@
setGrammaticalGenderSelector :: Selector
setGrammaticalGenderSelector = mkSelector "setGrammaticalGender:"

-- | @Selector@ for @partOfSpeech@
partOfSpeechSelector :: Selector
partOfSpeechSelector = mkSelector "partOfSpeech"

-- | @Selector@ for @setPartOfSpeech:@
setPartOfSpeechSelector :: Selector
setPartOfSpeechSelector = mkSelector "setPartOfSpeech:"

-- | @Selector@ for @number@
numberSelector :: Selector
numberSelector = mkSelector "number"

-- | @Selector@ for @setNumber:@
setNumberSelector :: Selector
setNumberSelector = mkSelector "setNumber:"

-- | @Selector@ for @grammaticalCase@
grammaticalCaseSelector :: Selector
grammaticalCaseSelector = mkSelector "grammaticalCase"

-- | @Selector@ for @setGrammaticalCase:@
setGrammaticalCaseSelector :: Selector
setGrammaticalCaseSelector = mkSelector "setGrammaticalCase:"

-- | @Selector@ for @determination@
determinationSelector :: Selector
determinationSelector = mkSelector "determination"

-- | @Selector@ for @setDetermination:@
setDeterminationSelector :: Selector
setDeterminationSelector = mkSelector "setDetermination:"

-- | @Selector@ for @grammaticalPerson@
grammaticalPersonSelector :: Selector
grammaticalPersonSelector = mkSelector "grammaticalPerson"

-- | @Selector@ for @setGrammaticalPerson:@
setGrammaticalPersonSelector :: Selector
setGrammaticalPersonSelector = mkSelector "setGrammaticalPerson:"

-- | @Selector@ for @pronounType@
pronounTypeSelector :: Selector
pronounTypeSelector = mkSelector "pronounType"

-- | @Selector@ for @setPronounType:@
setPronounTypeSelector :: Selector
setPronounTypeSelector = mkSelector "setPronounType:"

-- | @Selector@ for @definiteness@
definitenessSelector :: Selector
definitenessSelector = mkSelector "definiteness"

-- | @Selector@ for @setDefiniteness:@
setDefinitenessSelector :: Selector
setDefinitenessSelector = mkSelector "setDefiniteness:"

-- | @Selector@ for @unspecified@
unspecifiedSelector :: Selector
unspecifiedSelector = mkSelector "unspecified"

-- | @Selector@ for @userMorphology@
userMorphologySelector :: Selector
userMorphologySelector = mkSelector "userMorphology"

