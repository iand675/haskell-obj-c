{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , definitenessSelector
  , determinationSelector
  , grammaticalCaseSelector
  , grammaticalGenderSelector
  , grammaticalPersonSelector
  , numberSelector
  , partOfSpeechSelector
  , pronounTypeSelector
  , setCustomPronoun_forLanguage_errorSelector
  , setDefinitenessSelector
  , setDeterminationSelector
  , setGrammaticalCaseSelector
  , setGrammaticalGenderSelector
  , setGrammaticalPersonSelector
  , setNumberSelector
  , setPartOfSpeechSelector
  , setPronounTypeSelector
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- customPronounForLanguage:@
customPronounForLanguage :: (IsNSMorphology nsMorphology, IsNSString language) => nsMorphology -> language -> IO (Id NSMorphologyCustomPronoun)
customPronounForLanguage nsMorphology language =
  sendMessage nsMorphology customPronounForLanguageSelector (toNSString language)

-- | @- setCustomPronoun:forLanguage:error:@
setCustomPronoun_forLanguage_error :: (IsNSMorphology nsMorphology, IsNSMorphologyCustomPronoun features, IsNSString language, IsNSError error_) => nsMorphology -> features -> language -> error_ -> IO Bool
setCustomPronoun_forLanguage_error nsMorphology features language error_ =
  sendMessage nsMorphology setCustomPronoun_forLanguage_errorSelector (toNSMorphologyCustomPronoun features) (toNSString language) (toNSError error_)

-- | @- grammaticalGender@
grammaticalGender :: IsNSMorphology nsMorphology => nsMorphology -> IO NSGrammaticalGender
grammaticalGender nsMorphology =
  sendMessage nsMorphology grammaticalGenderSelector

-- | @- setGrammaticalGender:@
setGrammaticalGender :: IsNSMorphology nsMorphology => nsMorphology -> NSGrammaticalGender -> IO ()
setGrammaticalGender nsMorphology value =
  sendMessage nsMorphology setGrammaticalGenderSelector value

-- | @- partOfSpeech@
partOfSpeech :: IsNSMorphology nsMorphology => nsMorphology -> IO NSGrammaticalPartOfSpeech
partOfSpeech nsMorphology =
  sendMessage nsMorphology partOfSpeechSelector

-- | @- setPartOfSpeech:@
setPartOfSpeech :: IsNSMorphology nsMorphology => nsMorphology -> NSGrammaticalPartOfSpeech -> IO ()
setPartOfSpeech nsMorphology value =
  sendMessage nsMorphology setPartOfSpeechSelector value

-- | @- number@
number :: IsNSMorphology nsMorphology => nsMorphology -> IO NSGrammaticalNumber
number nsMorphology =
  sendMessage nsMorphology numberSelector

-- | @- setNumber:@
setNumber :: IsNSMorphology nsMorphology => nsMorphology -> NSGrammaticalNumber -> IO ()
setNumber nsMorphology value =
  sendMessage nsMorphology setNumberSelector value

-- | @- grammaticalCase@
grammaticalCase :: IsNSMorphology nsMorphology => nsMorphology -> IO NSGrammaticalCase
grammaticalCase nsMorphology =
  sendMessage nsMorphology grammaticalCaseSelector

-- | @- setGrammaticalCase:@
setGrammaticalCase :: IsNSMorphology nsMorphology => nsMorphology -> NSGrammaticalCase -> IO ()
setGrammaticalCase nsMorphology value =
  sendMessage nsMorphology setGrammaticalCaseSelector value

-- | @- determination@
determination :: IsNSMorphology nsMorphology => nsMorphology -> IO NSGrammaticalDetermination
determination nsMorphology =
  sendMessage nsMorphology determinationSelector

-- | @- setDetermination:@
setDetermination :: IsNSMorphology nsMorphology => nsMorphology -> NSGrammaticalDetermination -> IO ()
setDetermination nsMorphology value =
  sendMessage nsMorphology setDeterminationSelector value

-- | @- grammaticalPerson@
grammaticalPerson :: IsNSMorphology nsMorphology => nsMorphology -> IO NSGrammaticalPerson
grammaticalPerson nsMorphology =
  sendMessage nsMorphology grammaticalPersonSelector

-- | @- setGrammaticalPerson:@
setGrammaticalPerson :: IsNSMorphology nsMorphology => nsMorphology -> NSGrammaticalPerson -> IO ()
setGrammaticalPerson nsMorphology value =
  sendMessage nsMorphology setGrammaticalPersonSelector value

-- | @- pronounType@
pronounType :: IsNSMorphology nsMorphology => nsMorphology -> IO NSGrammaticalPronounType
pronounType nsMorphology =
  sendMessage nsMorphology pronounTypeSelector

-- | @- setPronounType:@
setPronounType :: IsNSMorphology nsMorphology => nsMorphology -> NSGrammaticalPronounType -> IO ()
setPronounType nsMorphology value =
  sendMessage nsMorphology setPronounTypeSelector value

-- | @- definiteness@
definiteness :: IsNSMorphology nsMorphology => nsMorphology -> IO NSGrammaticalDefiniteness
definiteness nsMorphology =
  sendMessage nsMorphology definitenessSelector

-- | @- setDefiniteness:@
setDefiniteness :: IsNSMorphology nsMorphology => nsMorphology -> NSGrammaticalDefiniteness -> IO ()
setDefiniteness nsMorphology value =
  sendMessage nsMorphology setDefinitenessSelector value

-- | @- unspecified@
unspecified :: IsNSMorphology nsMorphology => nsMorphology -> IO Bool
unspecified nsMorphology =
  sendMessage nsMorphology unspecifiedSelector

-- | @+ userMorphology@
userMorphology :: IO (Id NSMorphology)
userMorphology  =
  do
    cls' <- getRequiredClass "NSMorphology"
    sendClassMessage cls' userMorphologySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @customPronounForLanguage:@
customPronounForLanguageSelector :: Selector '[Id NSString] (Id NSMorphologyCustomPronoun)
customPronounForLanguageSelector = mkSelector "customPronounForLanguage:"

-- | @Selector@ for @setCustomPronoun:forLanguage:error:@
setCustomPronoun_forLanguage_errorSelector :: Selector '[Id NSMorphologyCustomPronoun, Id NSString, Id NSError] Bool
setCustomPronoun_forLanguage_errorSelector = mkSelector "setCustomPronoun:forLanguage:error:"

-- | @Selector@ for @grammaticalGender@
grammaticalGenderSelector :: Selector '[] NSGrammaticalGender
grammaticalGenderSelector = mkSelector "grammaticalGender"

-- | @Selector@ for @setGrammaticalGender:@
setGrammaticalGenderSelector :: Selector '[NSGrammaticalGender] ()
setGrammaticalGenderSelector = mkSelector "setGrammaticalGender:"

-- | @Selector@ for @partOfSpeech@
partOfSpeechSelector :: Selector '[] NSGrammaticalPartOfSpeech
partOfSpeechSelector = mkSelector "partOfSpeech"

-- | @Selector@ for @setPartOfSpeech:@
setPartOfSpeechSelector :: Selector '[NSGrammaticalPartOfSpeech] ()
setPartOfSpeechSelector = mkSelector "setPartOfSpeech:"

-- | @Selector@ for @number@
numberSelector :: Selector '[] NSGrammaticalNumber
numberSelector = mkSelector "number"

-- | @Selector@ for @setNumber:@
setNumberSelector :: Selector '[NSGrammaticalNumber] ()
setNumberSelector = mkSelector "setNumber:"

-- | @Selector@ for @grammaticalCase@
grammaticalCaseSelector :: Selector '[] NSGrammaticalCase
grammaticalCaseSelector = mkSelector "grammaticalCase"

-- | @Selector@ for @setGrammaticalCase:@
setGrammaticalCaseSelector :: Selector '[NSGrammaticalCase] ()
setGrammaticalCaseSelector = mkSelector "setGrammaticalCase:"

-- | @Selector@ for @determination@
determinationSelector :: Selector '[] NSGrammaticalDetermination
determinationSelector = mkSelector "determination"

-- | @Selector@ for @setDetermination:@
setDeterminationSelector :: Selector '[NSGrammaticalDetermination] ()
setDeterminationSelector = mkSelector "setDetermination:"

-- | @Selector@ for @grammaticalPerson@
grammaticalPersonSelector :: Selector '[] NSGrammaticalPerson
grammaticalPersonSelector = mkSelector "grammaticalPerson"

-- | @Selector@ for @setGrammaticalPerson:@
setGrammaticalPersonSelector :: Selector '[NSGrammaticalPerson] ()
setGrammaticalPersonSelector = mkSelector "setGrammaticalPerson:"

-- | @Selector@ for @pronounType@
pronounTypeSelector :: Selector '[] NSGrammaticalPronounType
pronounTypeSelector = mkSelector "pronounType"

-- | @Selector@ for @setPronounType:@
setPronounTypeSelector :: Selector '[NSGrammaticalPronounType] ()
setPronounTypeSelector = mkSelector "setPronounType:"

-- | @Selector@ for @definiteness@
definitenessSelector :: Selector '[] NSGrammaticalDefiniteness
definitenessSelector = mkSelector "definiteness"

-- | @Selector@ for @setDefiniteness:@
setDefinitenessSelector :: Selector '[NSGrammaticalDefiniteness] ()
setDefinitenessSelector = mkSelector "setDefiniteness:"

-- | @Selector@ for @unspecified@
unspecifiedSelector :: Selector '[] Bool
unspecifiedSelector = mkSelector "unspecified"

-- | @Selector@ for @userMorphology@
userMorphologySelector :: Selector '[] (Id NSMorphology)
userMorphologySelector = mkSelector "userMorphology"

