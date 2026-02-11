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
  , initWithVocabularyIdentifier_spokenPhrase_pronunciationHintSelector
  , initWithIdentifier_spokenPhrase_pronunciationHintSelector
  , initWithSpokenPhraseSelector


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

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINSpeakableString inSpeakableString => inSpeakableString -> IO (Id INSpeakableString)
init_ inSpeakableString  =
  sendMsg inSpeakableString (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithVocabularyIdentifier:spokenPhrase:pronunciationHint:@
initWithVocabularyIdentifier_spokenPhrase_pronunciationHint :: (IsINSpeakableString inSpeakableString, IsNSString vocabularyIdentifier, IsNSString spokenPhrase, IsNSString pronunciationHint) => inSpeakableString -> vocabularyIdentifier -> spokenPhrase -> pronunciationHint -> IO (Id INSpeakableString)
initWithVocabularyIdentifier_spokenPhrase_pronunciationHint inSpeakableString  vocabularyIdentifier spokenPhrase pronunciationHint =
withObjCPtr vocabularyIdentifier $ \raw_vocabularyIdentifier ->
  withObjCPtr spokenPhrase $ \raw_spokenPhrase ->
    withObjCPtr pronunciationHint $ \raw_pronunciationHint ->
        sendMsg inSpeakableString (mkSelector "initWithVocabularyIdentifier:spokenPhrase:pronunciationHint:") (retPtr retVoid) [argPtr (castPtr raw_vocabularyIdentifier :: Ptr ()), argPtr (castPtr raw_spokenPhrase :: Ptr ()), argPtr (castPtr raw_pronunciationHint :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithIdentifier:spokenPhrase:pronunciationHint:@
initWithIdentifier_spokenPhrase_pronunciationHint :: (IsINSpeakableString inSpeakableString, IsNSString identifier, IsNSString spokenPhrase, IsNSString pronunciationHint) => inSpeakableString -> identifier -> spokenPhrase -> pronunciationHint -> IO (Id INSpeakableString)
initWithIdentifier_spokenPhrase_pronunciationHint inSpeakableString  identifier spokenPhrase pronunciationHint =
withObjCPtr identifier $ \raw_identifier ->
  withObjCPtr spokenPhrase $ \raw_spokenPhrase ->
    withObjCPtr pronunciationHint $ \raw_pronunciationHint ->
        sendMsg inSpeakableString (mkSelector "initWithIdentifier:spokenPhrase:pronunciationHint:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_spokenPhrase :: Ptr ()), argPtr (castPtr raw_pronunciationHint :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithSpokenPhrase:@
initWithSpokenPhrase :: (IsINSpeakableString inSpeakableString, IsNSString spokenPhrase) => inSpeakableString -> spokenPhrase -> IO (Id INSpeakableString)
initWithSpokenPhrase inSpeakableString  spokenPhrase =
withObjCPtr spokenPhrase $ \raw_spokenPhrase ->
    sendMsg inSpeakableString (mkSelector "initWithSpokenPhrase:") (retPtr retVoid) [argPtr (castPtr raw_spokenPhrase :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithVocabularyIdentifier:spokenPhrase:pronunciationHint:@
initWithVocabularyIdentifier_spokenPhrase_pronunciationHintSelector :: Selector
initWithVocabularyIdentifier_spokenPhrase_pronunciationHintSelector = mkSelector "initWithVocabularyIdentifier:spokenPhrase:pronunciationHint:"

-- | @Selector@ for @initWithIdentifier:spokenPhrase:pronunciationHint:@
initWithIdentifier_spokenPhrase_pronunciationHintSelector :: Selector
initWithIdentifier_spokenPhrase_pronunciationHintSelector = mkSelector "initWithIdentifier:spokenPhrase:pronunciationHint:"

-- | @Selector@ for @initWithSpokenPhrase:@
initWithSpokenPhraseSelector :: Selector
initWithSpokenPhraseSelector = mkSelector "initWithSpokenPhrase:"

