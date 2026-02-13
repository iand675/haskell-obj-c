{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSSpeechSynthesizer@.
module ObjC.AppKit.NSSpeechSynthesizer
  ( NSSpeechSynthesizer
  , IsNSSpeechSynthesizer(..)
  , initWithVoice
  , startSpeakingString
  , startSpeakingString_toURL
  , stopSpeaking
  , stopSpeakingAtBoundary
  , pauseSpeakingAtBoundary
  , continueSpeaking
  , voice
  , setVoice
  , addSpeechDictionary
  , phonemesFromText
  , objectForProperty_error
  , setObject_forProperty_error
  , attributesForVoice
  , speaking
  , delegate
  , setDelegate
  , rate
  , setRate
  , volume
  , setVolume
  , usesFeedbackWindow
  , setUsesFeedbackWindow
  , anyApplicationSpeaking
  , defaultVoice
  , availableVoices
  , addSpeechDictionarySelector
  , anyApplicationSpeakingSelector
  , attributesForVoiceSelector
  , availableVoicesSelector
  , continueSpeakingSelector
  , defaultVoiceSelector
  , delegateSelector
  , initWithVoiceSelector
  , objectForProperty_errorSelector
  , pauseSpeakingAtBoundarySelector
  , phonemesFromTextSelector
  , rateSelector
  , setDelegateSelector
  , setObject_forProperty_errorSelector
  , setRateSelector
  , setUsesFeedbackWindowSelector
  , setVoiceSelector
  , setVolumeSelector
  , speakingSelector
  , startSpeakingStringSelector
  , startSpeakingString_toURLSelector
  , stopSpeakingAtBoundarySelector
  , stopSpeakingSelector
  , usesFeedbackWindowSelector
  , voiceSelector
  , volumeSelector

  -- * Enum types
  , NSSpeechBoundary(NSSpeechBoundary)
  , pattern NSSpeechImmediateBoundary
  , pattern NSSpeechWordBoundary
  , pattern NSSpeechSentenceBoundary

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithVoice:@
initWithVoice :: (IsNSSpeechSynthesizer nsSpeechSynthesizer, IsNSString voice) => nsSpeechSynthesizer -> voice -> IO (Id NSSpeechSynthesizer)
initWithVoice nsSpeechSynthesizer voice =
  sendOwnedMessage nsSpeechSynthesizer initWithVoiceSelector (toNSString voice)

-- | @- startSpeakingString:@
startSpeakingString :: (IsNSSpeechSynthesizer nsSpeechSynthesizer, IsNSString string) => nsSpeechSynthesizer -> string -> IO Bool
startSpeakingString nsSpeechSynthesizer string =
  sendMessage nsSpeechSynthesizer startSpeakingStringSelector (toNSString string)

-- | @- startSpeakingString:toURL:@
startSpeakingString_toURL :: (IsNSSpeechSynthesizer nsSpeechSynthesizer, IsNSString string, IsNSURL url) => nsSpeechSynthesizer -> string -> url -> IO Bool
startSpeakingString_toURL nsSpeechSynthesizer string url =
  sendMessage nsSpeechSynthesizer startSpeakingString_toURLSelector (toNSString string) (toNSURL url)

-- | @- stopSpeaking@
stopSpeaking :: IsNSSpeechSynthesizer nsSpeechSynthesizer => nsSpeechSynthesizer -> IO ()
stopSpeaking nsSpeechSynthesizer =
  sendMessage nsSpeechSynthesizer stopSpeakingSelector

-- | @- stopSpeakingAtBoundary:@
stopSpeakingAtBoundary :: IsNSSpeechSynthesizer nsSpeechSynthesizer => nsSpeechSynthesizer -> NSSpeechBoundary -> IO ()
stopSpeakingAtBoundary nsSpeechSynthesizer boundary =
  sendMessage nsSpeechSynthesizer stopSpeakingAtBoundarySelector boundary

-- | @- pauseSpeakingAtBoundary:@
pauseSpeakingAtBoundary :: IsNSSpeechSynthesizer nsSpeechSynthesizer => nsSpeechSynthesizer -> NSSpeechBoundary -> IO ()
pauseSpeakingAtBoundary nsSpeechSynthesizer boundary =
  sendMessage nsSpeechSynthesizer pauseSpeakingAtBoundarySelector boundary

-- | @- continueSpeaking@
continueSpeaking :: IsNSSpeechSynthesizer nsSpeechSynthesizer => nsSpeechSynthesizer -> IO ()
continueSpeaking nsSpeechSynthesizer =
  sendMessage nsSpeechSynthesizer continueSpeakingSelector

-- | @- voice@
voice :: IsNSSpeechSynthesizer nsSpeechSynthesizer => nsSpeechSynthesizer -> IO (Id NSString)
voice nsSpeechSynthesizer =
  sendMessage nsSpeechSynthesizer voiceSelector

-- | @- setVoice:@
setVoice :: (IsNSSpeechSynthesizer nsSpeechSynthesizer, IsNSString voice) => nsSpeechSynthesizer -> voice -> IO Bool
setVoice nsSpeechSynthesizer voice =
  sendMessage nsSpeechSynthesizer setVoiceSelector (toNSString voice)

-- | @- addSpeechDictionary:@
addSpeechDictionary :: (IsNSSpeechSynthesizer nsSpeechSynthesizer, IsNSDictionary speechDictionary) => nsSpeechSynthesizer -> speechDictionary -> IO ()
addSpeechDictionary nsSpeechSynthesizer speechDictionary =
  sendMessage nsSpeechSynthesizer addSpeechDictionarySelector (toNSDictionary speechDictionary)

-- | @- phonemesFromText:@
phonemesFromText :: (IsNSSpeechSynthesizer nsSpeechSynthesizer, IsNSString text) => nsSpeechSynthesizer -> text -> IO (Id NSString)
phonemesFromText nsSpeechSynthesizer text =
  sendMessage nsSpeechSynthesizer phonemesFromTextSelector (toNSString text)

-- | @- objectForProperty:error:@
objectForProperty_error :: (IsNSSpeechSynthesizer nsSpeechSynthesizer, IsNSString property, IsNSError outError) => nsSpeechSynthesizer -> property -> outError -> IO RawId
objectForProperty_error nsSpeechSynthesizer property outError =
  sendMessage nsSpeechSynthesizer objectForProperty_errorSelector (toNSString property) (toNSError outError)

-- | @- setObject:forProperty:error:@
setObject_forProperty_error :: (IsNSSpeechSynthesizer nsSpeechSynthesizer, IsNSString property, IsNSError outError) => nsSpeechSynthesizer -> RawId -> property -> outError -> IO Bool
setObject_forProperty_error nsSpeechSynthesizer object property outError =
  sendMessage nsSpeechSynthesizer setObject_forProperty_errorSelector object (toNSString property) (toNSError outError)

-- | @+ attributesForVoice:@
attributesForVoice :: IsNSString voice => voice -> IO (Id NSDictionary)
attributesForVoice voice =
  do
    cls' <- getRequiredClass "NSSpeechSynthesizer"
    sendClassMessage cls' attributesForVoiceSelector (toNSString voice)

-- | @- speaking@
speaking :: IsNSSpeechSynthesizer nsSpeechSynthesizer => nsSpeechSynthesizer -> IO Bool
speaking nsSpeechSynthesizer =
  sendMessage nsSpeechSynthesizer speakingSelector

-- | @- delegate@
delegate :: IsNSSpeechSynthesizer nsSpeechSynthesizer => nsSpeechSynthesizer -> IO RawId
delegate nsSpeechSynthesizer =
  sendMessage nsSpeechSynthesizer delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSSpeechSynthesizer nsSpeechSynthesizer => nsSpeechSynthesizer -> RawId -> IO ()
setDelegate nsSpeechSynthesizer value =
  sendMessage nsSpeechSynthesizer setDelegateSelector value

-- | @- rate@
rate :: IsNSSpeechSynthesizer nsSpeechSynthesizer => nsSpeechSynthesizer -> IO CFloat
rate nsSpeechSynthesizer =
  sendMessage nsSpeechSynthesizer rateSelector

-- | @- setRate:@
setRate :: IsNSSpeechSynthesizer nsSpeechSynthesizer => nsSpeechSynthesizer -> CFloat -> IO ()
setRate nsSpeechSynthesizer value =
  sendMessage nsSpeechSynthesizer setRateSelector value

-- | @- volume@
volume :: IsNSSpeechSynthesizer nsSpeechSynthesizer => nsSpeechSynthesizer -> IO CFloat
volume nsSpeechSynthesizer =
  sendMessage nsSpeechSynthesizer volumeSelector

-- | @- setVolume:@
setVolume :: IsNSSpeechSynthesizer nsSpeechSynthesizer => nsSpeechSynthesizer -> CFloat -> IO ()
setVolume nsSpeechSynthesizer value =
  sendMessage nsSpeechSynthesizer setVolumeSelector value

-- | @- usesFeedbackWindow@
usesFeedbackWindow :: IsNSSpeechSynthesizer nsSpeechSynthesizer => nsSpeechSynthesizer -> IO Bool
usesFeedbackWindow nsSpeechSynthesizer =
  sendMessage nsSpeechSynthesizer usesFeedbackWindowSelector

-- | @- setUsesFeedbackWindow:@
setUsesFeedbackWindow :: IsNSSpeechSynthesizer nsSpeechSynthesizer => nsSpeechSynthesizer -> Bool -> IO ()
setUsesFeedbackWindow nsSpeechSynthesizer value =
  sendMessage nsSpeechSynthesizer setUsesFeedbackWindowSelector value

-- | @+ anyApplicationSpeaking@
anyApplicationSpeaking :: IO Bool
anyApplicationSpeaking  =
  do
    cls' <- getRequiredClass "NSSpeechSynthesizer"
    sendClassMessage cls' anyApplicationSpeakingSelector

-- | @+ defaultVoice@
defaultVoice :: IO (Id NSString)
defaultVoice  =
  do
    cls' <- getRequiredClass "NSSpeechSynthesizer"
    sendClassMessage cls' defaultVoiceSelector

-- | @+ availableVoices@
availableVoices :: IO (Id NSArray)
availableVoices  =
  do
    cls' <- getRequiredClass "NSSpeechSynthesizer"
    sendClassMessage cls' availableVoicesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithVoice:@
initWithVoiceSelector :: Selector '[Id NSString] (Id NSSpeechSynthesizer)
initWithVoiceSelector = mkSelector "initWithVoice:"

-- | @Selector@ for @startSpeakingString:@
startSpeakingStringSelector :: Selector '[Id NSString] Bool
startSpeakingStringSelector = mkSelector "startSpeakingString:"

-- | @Selector@ for @startSpeakingString:toURL:@
startSpeakingString_toURLSelector :: Selector '[Id NSString, Id NSURL] Bool
startSpeakingString_toURLSelector = mkSelector "startSpeakingString:toURL:"

-- | @Selector@ for @stopSpeaking@
stopSpeakingSelector :: Selector '[] ()
stopSpeakingSelector = mkSelector "stopSpeaking"

-- | @Selector@ for @stopSpeakingAtBoundary:@
stopSpeakingAtBoundarySelector :: Selector '[NSSpeechBoundary] ()
stopSpeakingAtBoundarySelector = mkSelector "stopSpeakingAtBoundary:"

-- | @Selector@ for @pauseSpeakingAtBoundary:@
pauseSpeakingAtBoundarySelector :: Selector '[NSSpeechBoundary] ()
pauseSpeakingAtBoundarySelector = mkSelector "pauseSpeakingAtBoundary:"

-- | @Selector@ for @continueSpeaking@
continueSpeakingSelector :: Selector '[] ()
continueSpeakingSelector = mkSelector "continueSpeaking"

-- | @Selector@ for @voice@
voiceSelector :: Selector '[] (Id NSString)
voiceSelector = mkSelector "voice"

-- | @Selector@ for @setVoice:@
setVoiceSelector :: Selector '[Id NSString] Bool
setVoiceSelector = mkSelector "setVoice:"

-- | @Selector@ for @addSpeechDictionary:@
addSpeechDictionarySelector :: Selector '[Id NSDictionary] ()
addSpeechDictionarySelector = mkSelector "addSpeechDictionary:"

-- | @Selector@ for @phonemesFromText:@
phonemesFromTextSelector :: Selector '[Id NSString] (Id NSString)
phonemesFromTextSelector = mkSelector "phonemesFromText:"

-- | @Selector@ for @objectForProperty:error:@
objectForProperty_errorSelector :: Selector '[Id NSString, Id NSError] RawId
objectForProperty_errorSelector = mkSelector "objectForProperty:error:"

-- | @Selector@ for @setObject:forProperty:error:@
setObject_forProperty_errorSelector :: Selector '[RawId, Id NSString, Id NSError] Bool
setObject_forProperty_errorSelector = mkSelector "setObject:forProperty:error:"

-- | @Selector@ for @attributesForVoice:@
attributesForVoiceSelector :: Selector '[Id NSString] (Id NSDictionary)
attributesForVoiceSelector = mkSelector "attributesForVoice:"

-- | @Selector@ for @speaking@
speakingSelector :: Selector '[] Bool
speakingSelector = mkSelector "speaking"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @rate@
rateSelector :: Selector '[] CFloat
rateSelector = mkSelector "rate"

-- | @Selector@ for @setRate:@
setRateSelector :: Selector '[CFloat] ()
setRateSelector = mkSelector "setRate:"

-- | @Selector@ for @volume@
volumeSelector :: Selector '[] CFloat
volumeSelector = mkSelector "volume"

-- | @Selector@ for @setVolume:@
setVolumeSelector :: Selector '[CFloat] ()
setVolumeSelector = mkSelector "setVolume:"

-- | @Selector@ for @usesFeedbackWindow@
usesFeedbackWindowSelector :: Selector '[] Bool
usesFeedbackWindowSelector = mkSelector "usesFeedbackWindow"

-- | @Selector@ for @setUsesFeedbackWindow:@
setUsesFeedbackWindowSelector :: Selector '[Bool] ()
setUsesFeedbackWindowSelector = mkSelector "setUsesFeedbackWindow:"

-- | @Selector@ for @anyApplicationSpeaking@
anyApplicationSpeakingSelector :: Selector '[] Bool
anyApplicationSpeakingSelector = mkSelector "anyApplicationSpeaking"

-- | @Selector@ for @defaultVoice@
defaultVoiceSelector :: Selector '[] (Id NSString)
defaultVoiceSelector = mkSelector "defaultVoice"

-- | @Selector@ for @availableVoices@
availableVoicesSelector :: Selector '[] (Id NSArray)
availableVoicesSelector = mkSelector "availableVoices"

