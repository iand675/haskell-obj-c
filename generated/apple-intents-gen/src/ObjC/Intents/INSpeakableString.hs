{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSpeakableString@.
module ObjC.Intents.INSpeakableString
  ( INSpeakableString
  , IsINSpeakableString(..)
  , init_
  , initWithVocabularyIdentifier_spokenPhrase_pronunciationHint
  , initWithIdentifier_spokenPhrase_pronunciationHint
  , initWithSpokenPhrase
  , initSelector
  , initWithIdentifier_spokenPhrase_pronunciationHintSelector
  , initWithSpokenPhraseSelector
  , initWithVocabularyIdentifier_spokenPhrase_pronunciationHintSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINSpeakableString inSpeakableString => inSpeakableString -> IO (Id INSpeakableString)
init_ inSpeakableString =
  sendOwnedMessage inSpeakableString initSelector

-- | @- initWithVocabularyIdentifier:spokenPhrase:pronunciationHint:@
initWithVocabularyIdentifier_spokenPhrase_pronunciationHint :: (IsINSpeakableString inSpeakableString, IsNSString vocabularyIdentifier, IsNSString spokenPhrase, IsNSString pronunciationHint) => inSpeakableString -> vocabularyIdentifier -> spokenPhrase -> pronunciationHint -> IO (Id INSpeakableString)
initWithVocabularyIdentifier_spokenPhrase_pronunciationHint inSpeakableString vocabularyIdentifier spokenPhrase pronunciationHint =
  sendOwnedMessage inSpeakableString initWithVocabularyIdentifier_spokenPhrase_pronunciationHintSelector (toNSString vocabularyIdentifier) (toNSString spokenPhrase) (toNSString pronunciationHint)

-- | @- initWithIdentifier:spokenPhrase:pronunciationHint:@
initWithIdentifier_spokenPhrase_pronunciationHint :: (IsINSpeakableString inSpeakableString, IsNSString identifier, IsNSString spokenPhrase, IsNSString pronunciationHint) => inSpeakableString -> identifier -> spokenPhrase -> pronunciationHint -> IO (Id INSpeakableString)
initWithIdentifier_spokenPhrase_pronunciationHint inSpeakableString identifier spokenPhrase pronunciationHint =
  sendOwnedMessage inSpeakableString initWithIdentifier_spokenPhrase_pronunciationHintSelector (toNSString identifier) (toNSString spokenPhrase) (toNSString pronunciationHint)

-- | @- initWithSpokenPhrase:@
initWithSpokenPhrase :: (IsINSpeakableString inSpeakableString, IsNSString spokenPhrase) => inSpeakableString -> spokenPhrase -> IO (Id INSpeakableString)
initWithSpokenPhrase inSpeakableString spokenPhrase =
  sendOwnedMessage inSpeakableString initWithSpokenPhraseSelector (toNSString spokenPhrase)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INSpeakableString)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithVocabularyIdentifier:spokenPhrase:pronunciationHint:@
initWithVocabularyIdentifier_spokenPhrase_pronunciationHintSelector :: Selector '[Id NSString, Id NSString, Id NSString] (Id INSpeakableString)
initWithVocabularyIdentifier_spokenPhrase_pronunciationHintSelector = mkSelector "initWithVocabularyIdentifier:spokenPhrase:pronunciationHint:"

-- | @Selector@ for @initWithIdentifier:spokenPhrase:pronunciationHint:@
initWithIdentifier_spokenPhrase_pronunciationHintSelector :: Selector '[Id NSString, Id NSString, Id NSString] (Id INSpeakableString)
initWithIdentifier_spokenPhrase_pronunciationHintSelector = mkSelector "initWithIdentifier:spokenPhrase:pronunciationHint:"

-- | @Selector@ for @initWithSpokenPhrase:@
initWithSpokenPhraseSelector :: Selector '[Id NSString] (Id INSpeakableString)
initWithSpokenPhraseSelector = mkSelector "initWithSpokenPhrase:"

