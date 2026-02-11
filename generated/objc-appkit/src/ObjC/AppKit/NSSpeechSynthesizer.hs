{-# LANGUAGE PatternSynonyms #-}
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
  , rate
  , setRate
  , volume
  , setVolume
  , usesFeedbackWindow
  , setUsesFeedbackWindow
  , anyApplicationSpeaking
  , defaultVoice
  , availableVoices
  , initWithVoiceSelector
  , startSpeakingStringSelector
  , startSpeakingString_toURLSelector
  , stopSpeakingSelector
  , stopSpeakingAtBoundarySelector
  , pauseSpeakingAtBoundarySelector
  , continueSpeakingSelector
  , voiceSelector
  , setVoiceSelector
  , addSpeechDictionarySelector
  , phonemesFromTextSelector
  , objectForProperty_errorSelector
  , setObject_forProperty_errorSelector
  , attributesForVoiceSelector
  , speakingSelector
  , rateSelector
  , setRateSelector
  , volumeSelector
  , setVolumeSelector
  , usesFeedbackWindowSelector
  , setUsesFeedbackWindowSelector
  , anyApplicationSpeakingSelector
  , defaultVoiceSelector
  , availableVoicesSelector

  -- * Enum types
  , NSSpeechBoundary(NSSpeechBoundary)
  , pattern NSSpeechImmediateBoundary
  , pattern NSSpeechWordBoundary
  , pattern NSSpeechSentenceBoundary

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

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithVoice:@
initWithVoice :: (IsNSSpeechSynthesizer nsSpeechSynthesizer, IsNSString voice) => nsSpeechSynthesizer -> voice -> IO (Id NSSpeechSynthesizer)
initWithVoice nsSpeechSynthesizer  voice =
withObjCPtr voice $ \raw_voice ->
    sendMsg nsSpeechSynthesizer (mkSelector "initWithVoice:") (retPtr retVoid) [argPtr (castPtr raw_voice :: Ptr ())] >>= ownedObject . castPtr

-- | @- startSpeakingString:@
startSpeakingString :: (IsNSSpeechSynthesizer nsSpeechSynthesizer, IsNSString string) => nsSpeechSynthesizer -> string -> IO Bool
startSpeakingString nsSpeechSynthesizer  string =
withObjCPtr string $ \raw_string ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSpeechSynthesizer (mkSelector "startSpeakingString:") retCULong [argPtr (castPtr raw_string :: Ptr ())]

-- | @- startSpeakingString:toURL:@
startSpeakingString_toURL :: (IsNSSpeechSynthesizer nsSpeechSynthesizer, IsNSString string, IsNSURL url) => nsSpeechSynthesizer -> string -> url -> IO Bool
startSpeakingString_toURL nsSpeechSynthesizer  string url =
withObjCPtr string $ \raw_string ->
  withObjCPtr url $ \raw_url ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSpeechSynthesizer (mkSelector "startSpeakingString:toURL:") retCULong [argPtr (castPtr raw_string :: Ptr ()), argPtr (castPtr raw_url :: Ptr ())]

-- | @- stopSpeaking@
stopSpeaking :: IsNSSpeechSynthesizer nsSpeechSynthesizer => nsSpeechSynthesizer -> IO ()
stopSpeaking nsSpeechSynthesizer  =
  sendMsg nsSpeechSynthesizer (mkSelector "stopSpeaking") retVoid []

-- | @- stopSpeakingAtBoundary:@
stopSpeakingAtBoundary :: IsNSSpeechSynthesizer nsSpeechSynthesizer => nsSpeechSynthesizer -> NSSpeechBoundary -> IO ()
stopSpeakingAtBoundary nsSpeechSynthesizer  boundary =
  sendMsg nsSpeechSynthesizer (mkSelector "stopSpeakingAtBoundary:") retVoid [argCULong (coerce boundary)]

-- | @- pauseSpeakingAtBoundary:@
pauseSpeakingAtBoundary :: IsNSSpeechSynthesizer nsSpeechSynthesizer => nsSpeechSynthesizer -> NSSpeechBoundary -> IO ()
pauseSpeakingAtBoundary nsSpeechSynthesizer  boundary =
  sendMsg nsSpeechSynthesizer (mkSelector "pauseSpeakingAtBoundary:") retVoid [argCULong (coerce boundary)]

-- | @- continueSpeaking@
continueSpeaking :: IsNSSpeechSynthesizer nsSpeechSynthesizer => nsSpeechSynthesizer -> IO ()
continueSpeaking nsSpeechSynthesizer  =
  sendMsg nsSpeechSynthesizer (mkSelector "continueSpeaking") retVoid []

-- | @- voice@
voice :: IsNSSpeechSynthesizer nsSpeechSynthesizer => nsSpeechSynthesizer -> IO (Id NSString)
voice nsSpeechSynthesizer  =
  sendMsg nsSpeechSynthesizer (mkSelector "voice") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVoice:@
setVoice :: (IsNSSpeechSynthesizer nsSpeechSynthesizer, IsNSString voice) => nsSpeechSynthesizer -> voice -> IO Bool
setVoice nsSpeechSynthesizer  voice =
withObjCPtr voice $ \raw_voice ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSpeechSynthesizer (mkSelector "setVoice:") retCULong [argPtr (castPtr raw_voice :: Ptr ())]

-- | @- addSpeechDictionary:@
addSpeechDictionary :: (IsNSSpeechSynthesizer nsSpeechSynthesizer, IsNSDictionary speechDictionary) => nsSpeechSynthesizer -> speechDictionary -> IO ()
addSpeechDictionary nsSpeechSynthesizer  speechDictionary =
withObjCPtr speechDictionary $ \raw_speechDictionary ->
    sendMsg nsSpeechSynthesizer (mkSelector "addSpeechDictionary:") retVoid [argPtr (castPtr raw_speechDictionary :: Ptr ())]

-- | @- phonemesFromText:@
phonemesFromText :: (IsNSSpeechSynthesizer nsSpeechSynthesizer, IsNSString text) => nsSpeechSynthesizer -> text -> IO (Id NSString)
phonemesFromText nsSpeechSynthesizer  text =
withObjCPtr text $ \raw_text ->
    sendMsg nsSpeechSynthesizer (mkSelector "phonemesFromText:") (retPtr retVoid) [argPtr (castPtr raw_text :: Ptr ())] >>= retainedObject . castPtr

-- | @- objectForProperty:error:@
objectForProperty_error :: (IsNSSpeechSynthesizer nsSpeechSynthesizer, IsNSString property, IsNSError outError) => nsSpeechSynthesizer -> property -> outError -> IO RawId
objectForProperty_error nsSpeechSynthesizer  property outError =
withObjCPtr property $ \raw_property ->
  withObjCPtr outError $ \raw_outError ->
      fmap (RawId . castPtr) $ sendMsg nsSpeechSynthesizer (mkSelector "objectForProperty:error:") (retPtr retVoid) [argPtr (castPtr raw_property :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | @- setObject:forProperty:error:@
setObject_forProperty_error :: (IsNSSpeechSynthesizer nsSpeechSynthesizer, IsNSString property, IsNSError outError) => nsSpeechSynthesizer -> RawId -> property -> outError -> IO Bool
setObject_forProperty_error nsSpeechSynthesizer  object property outError =
withObjCPtr property $ \raw_property ->
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSpeechSynthesizer (mkSelector "setObject:forProperty:error:") retCULong [argPtr (castPtr (unRawId object) :: Ptr ()), argPtr (castPtr raw_property :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | @+ attributesForVoice:@
attributesForVoice :: IsNSString voice => voice -> IO (Id NSDictionary)
attributesForVoice voice =
  do
    cls' <- getRequiredClass "NSSpeechSynthesizer"
    withObjCPtr voice $ \raw_voice ->
      sendClassMsg cls' (mkSelector "attributesForVoice:") (retPtr retVoid) [argPtr (castPtr raw_voice :: Ptr ())] >>= retainedObject . castPtr

-- | @- speaking@
speaking :: IsNSSpeechSynthesizer nsSpeechSynthesizer => nsSpeechSynthesizer -> IO Bool
speaking nsSpeechSynthesizer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSpeechSynthesizer (mkSelector "speaking") retCULong []

-- | @- rate@
rate :: IsNSSpeechSynthesizer nsSpeechSynthesizer => nsSpeechSynthesizer -> IO CFloat
rate nsSpeechSynthesizer  =
  sendMsg nsSpeechSynthesizer (mkSelector "rate") retCFloat []

-- | @- setRate:@
setRate :: IsNSSpeechSynthesizer nsSpeechSynthesizer => nsSpeechSynthesizer -> CFloat -> IO ()
setRate nsSpeechSynthesizer  value =
  sendMsg nsSpeechSynthesizer (mkSelector "setRate:") retVoid [argCFloat (fromIntegral value)]

-- | @- volume@
volume :: IsNSSpeechSynthesizer nsSpeechSynthesizer => nsSpeechSynthesizer -> IO CFloat
volume nsSpeechSynthesizer  =
  sendMsg nsSpeechSynthesizer (mkSelector "volume") retCFloat []

-- | @- setVolume:@
setVolume :: IsNSSpeechSynthesizer nsSpeechSynthesizer => nsSpeechSynthesizer -> CFloat -> IO ()
setVolume nsSpeechSynthesizer  value =
  sendMsg nsSpeechSynthesizer (mkSelector "setVolume:") retVoid [argCFloat (fromIntegral value)]

-- | @- usesFeedbackWindow@
usesFeedbackWindow :: IsNSSpeechSynthesizer nsSpeechSynthesizer => nsSpeechSynthesizer -> IO Bool
usesFeedbackWindow nsSpeechSynthesizer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSpeechSynthesizer (mkSelector "usesFeedbackWindow") retCULong []

-- | @- setUsesFeedbackWindow:@
setUsesFeedbackWindow :: IsNSSpeechSynthesizer nsSpeechSynthesizer => nsSpeechSynthesizer -> Bool -> IO ()
setUsesFeedbackWindow nsSpeechSynthesizer  value =
  sendMsg nsSpeechSynthesizer (mkSelector "setUsesFeedbackWindow:") retVoid [argCULong (if value then 1 else 0)]

-- | @+ anyApplicationSpeaking@
anyApplicationSpeaking :: IO Bool
anyApplicationSpeaking  =
  do
    cls' <- getRequiredClass "NSSpeechSynthesizer"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "anyApplicationSpeaking") retCULong []

-- | @+ defaultVoice@
defaultVoice :: IO (Id NSString)
defaultVoice  =
  do
    cls' <- getRequiredClass "NSSpeechSynthesizer"
    sendClassMsg cls' (mkSelector "defaultVoice") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ availableVoices@
availableVoices :: IO (Id NSArray)
availableVoices  =
  do
    cls' <- getRequiredClass "NSSpeechSynthesizer"
    sendClassMsg cls' (mkSelector "availableVoices") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithVoice:@
initWithVoiceSelector :: Selector
initWithVoiceSelector = mkSelector "initWithVoice:"

-- | @Selector@ for @startSpeakingString:@
startSpeakingStringSelector :: Selector
startSpeakingStringSelector = mkSelector "startSpeakingString:"

-- | @Selector@ for @startSpeakingString:toURL:@
startSpeakingString_toURLSelector :: Selector
startSpeakingString_toURLSelector = mkSelector "startSpeakingString:toURL:"

-- | @Selector@ for @stopSpeaking@
stopSpeakingSelector :: Selector
stopSpeakingSelector = mkSelector "stopSpeaking"

-- | @Selector@ for @stopSpeakingAtBoundary:@
stopSpeakingAtBoundarySelector :: Selector
stopSpeakingAtBoundarySelector = mkSelector "stopSpeakingAtBoundary:"

-- | @Selector@ for @pauseSpeakingAtBoundary:@
pauseSpeakingAtBoundarySelector :: Selector
pauseSpeakingAtBoundarySelector = mkSelector "pauseSpeakingAtBoundary:"

-- | @Selector@ for @continueSpeaking@
continueSpeakingSelector :: Selector
continueSpeakingSelector = mkSelector "continueSpeaking"

-- | @Selector@ for @voice@
voiceSelector :: Selector
voiceSelector = mkSelector "voice"

-- | @Selector@ for @setVoice:@
setVoiceSelector :: Selector
setVoiceSelector = mkSelector "setVoice:"

-- | @Selector@ for @addSpeechDictionary:@
addSpeechDictionarySelector :: Selector
addSpeechDictionarySelector = mkSelector "addSpeechDictionary:"

-- | @Selector@ for @phonemesFromText:@
phonemesFromTextSelector :: Selector
phonemesFromTextSelector = mkSelector "phonemesFromText:"

-- | @Selector@ for @objectForProperty:error:@
objectForProperty_errorSelector :: Selector
objectForProperty_errorSelector = mkSelector "objectForProperty:error:"

-- | @Selector@ for @setObject:forProperty:error:@
setObject_forProperty_errorSelector :: Selector
setObject_forProperty_errorSelector = mkSelector "setObject:forProperty:error:"

-- | @Selector@ for @attributesForVoice:@
attributesForVoiceSelector :: Selector
attributesForVoiceSelector = mkSelector "attributesForVoice:"

-- | @Selector@ for @speaking@
speakingSelector :: Selector
speakingSelector = mkSelector "speaking"

-- | @Selector@ for @rate@
rateSelector :: Selector
rateSelector = mkSelector "rate"

-- | @Selector@ for @setRate:@
setRateSelector :: Selector
setRateSelector = mkSelector "setRate:"

-- | @Selector@ for @volume@
volumeSelector :: Selector
volumeSelector = mkSelector "volume"

-- | @Selector@ for @setVolume:@
setVolumeSelector :: Selector
setVolumeSelector = mkSelector "setVolume:"

-- | @Selector@ for @usesFeedbackWindow@
usesFeedbackWindowSelector :: Selector
usesFeedbackWindowSelector = mkSelector "usesFeedbackWindow"

-- | @Selector@ for @setUsesFeedbackWindow:@
setUsesFeedbackWindowSelector :: Selector
setUsesFeedbackWindowSelector = mkSelector "setUsesFeedbackWindow:"

-- | @Selector@ for @anyApplicationSpeaking@
anyApplicationSpeakingSelector :: Selector
anyApplicationSpeakingSelector = mkSelector "anyApplicationSpeaking"

-- | @Selector@ for @defaultVoice@
defaultVoiceSelector :: Selector
defaultVoiceSelector = mkSelector "defaultVoice"

-- | @Selector@ for @availableVoices@
availableVoicesSelector :: Selector
availableVoicesSelector = mkSelector "availableVoices"

