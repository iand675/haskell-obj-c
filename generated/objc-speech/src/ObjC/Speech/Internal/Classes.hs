{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.Speech.Internal.Classes (
    module ObjC.Speech.Internal.Classes,
    module ObjC.AVFAudio.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- SFAcousticFeature ----------

-- | The value of a voice analysis metric.
-- 
-- Phantom type for @SFAcousticFeature@.
data SFAcousticFeature

instance IsObjCObject (Id SFAcousticFeature) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SFAcousticFeature"

class IsNSObject a => IsSFAcousticFeature a where
  toSFAcousticFeature :: a -> Id SFAcousticFeature

instance IsSFAcousticFeature (Id SFAcousticFeature) where
  toSFAcousticFeature = unsafeCastId

instance IsNSObject (Id SFAcousticFeature) where
  toNSObject = unsafeCastId

-- ---------- SFSpeechLanguageModel ----------

-- | A language model built from custom training data.
--
-- Create this object using ``SFSpeechLanguageModel/prepareCustomLanguageModelForUrl:configuration:completion:`` or ``SFSpeechLanguageModel/prepareCustomLanguageModelForUrl:configuration:ignoresCache:completion:``.
-- 
-- Phantom type for @SFSpeechLanguageModel@.
data SFSpeechLanguageModel

instance IsObjCObject (Id SFSpeechLanguageModel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SFSpeechLanguageModel"

class IsNSObject a => IsSFSpeechLanguageModel a where
  toSFSpeechLanguageModel :: a -> Id SFSpeechLanguageModel

instance IsSFSpeechLanguageModel (Id SFSpeechLanguageModel) where
  toSFSpeechLanguageModel = unsafeCastId

instance IsNSObject (Id SFSpeechLanguageModel) where
  toNSObject = unsafeCastId

-- ---------- SFSpeechLanguageModelConfiguration ----------

-- | An object describing the location of a custom language model and specialized vocabulary.
--
-- Pass this object to ``SFSpeechLanguageModel/prepareCustomLanguageModelForUrl:configuration:completion:`` to indicate where that method should create the custom language model file, and to ``SFSpeechRecognitionRequest/customizedLanguageModel`` or ``DictationTranscriber/ContentHint/customizedLanguage(modelConfiguration:)`` to indicate where the system should find that model to use.
-- 
-- Phantom type for @SFSpeechLanguageModelConfiguration@.
data SFSpeechLanguageModelConfiguration

instance IsObjCObject (Id SFSpeechLanguageModelConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SFSpeechLanguageModelConfiguration"

class IsNSObject a => IsSFSpeechLanguageModelConfiguration a where
  toSFSpeechLanguageModelConfiguration :: a -> Id SFSpeechLanguageModelConfiguration

instance IsSFSpeechLanguageModelConfiguration (Id SFSpeechLanguageModelConfiguration) where
  toSFSpeechLanguageModelConfiguration = unsafeCastId

instance IsNSObject (Id SFSpeechLanguageModelConfiguration) where
  toNSObject = unsafeCastId

-- ---------- SFSpeechRecognitionMetadata ----------

-- | The metadata of speech in the audio of a speech recognition request.
-- 
-- Phantom type for @SFSpeechRecognitionMetadata@.
data SFSpeechRecognitionMetadata

instance IsObjCObject (Id SFSpeechRecognitionMetadata) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SFSpeechRecognitionMetadata"

class IsNSObject a => IsSFSpeechRecognitionMetadata a where
  toSFSpeechRecognitionMetadata :: a -> Id SFSpeechRecognitionMetadata

instance IsSFSpeechRecognitionMetadata (Id SFSpeechRecognitionMetadata) where
  toSFSpeechRecognitionMetadata = unsafeCastId

instance IsNSObject (Id SFSpeechRecognitionMetadata) where
  toNSObject = unsafeCastId

-- ---------- SFSpeechRecognitionRequest ----------

-- | An abstract class that represents a request to recognize speech from an audio source.
--
-- Don't create ``SFSpeechRecognitionRequest`` objects directly. Create an ``SFSpeechURLRecognitionRequest`` or ``SFSpeechAudioBufferRecognitionRequest`` object instead. Use the properties of this class to configure various aspects of your request object before you start the speech recognition process. For example, use the ``shouldReportPartialResults`` property to specify whether you want partial results or only the final result of speech recognition.
-- 
-- Phantom type for @SFSpeechRecognitionRequest@.
data SFSpeechRecognitionRequest

instance IsObjCObject (Id SFSpeechRecognitionRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SFSpeechRecognitionRequest"

class IsNSObject a => IsSFSpeechRecognitionRequest a where
  toSFSpeechRecognitionRequest :: a -> Id SFSpeechRecognitionRequest

instance IsSFSpeechRecognitionRequest (Id SFSpeechRecognitionRequest) where
  toSFSpeechRecognitionRequest = unsafeCastId

instance IsNSObject (Id SFSpeechRecognitionRequest) where
  toNSObject = unsafeCastId

-- ---------- SFSpeechRecognitionResult ----------

-- | An object that contains the partial or final results of a speech recognition request.
--
-- Use an @SFSpeechRecognitionResult@ object to retrieve the results of a speech recognition request. You don't create these objects directly. Instead, the Speech framework creates them and passes them to the handler block or delegate object you specified when starting your speech recognition task.
--
-- A speech recognition result object contains one or more ``transcriptions`` of the current utterance. Each transcription has a confidence rating indicating how likely it is to be correct. You can also get the transcription with the highest rating directly from the ``bestTranscription`` property.
--
-- If you requested partial results from the speech recognizer, the transcriptions may represent only part of the total audio content. Use the ``isFinal`` property to determine if the request contains partial or final results.
-- 
-- Phantom type for @SFSpeechRecognitionResult@.
data SFSpeechRecognitionResult

instance IsObjCObject (Id SFSpeechRecognitionResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SFSpeechRecognitionResult"

class IsNSObject a => IsSFSpeechRecognitionResult a where
  toSFSpeechRecognitionResult :: a -> Id SFSpeechRecognitionResult

instance IsSFSpeechRecognitionResult (Id SFSpeechRecognitionResult) where
  toSFSpeechRecognitionResult = unsafeCastId

instance IsNSObject (Id SFSpeechRecognitionResult) where
  toNSObject = unsafeCastId

-- ---------- SFSpeechRecognitionTask ----------

-- | A task object for monitoring the speech recognition progress.
--
-- Use an @SFSpeechRecognitionTask@ object to determine the state of a speech recognition task, to cancel an ongoing task, or to signal the end of the task.
--
-- You don't create speech recognition task objects directly. Instead, you receive one of these objects after calling ``SFSpeechRecognizer/recognitionTask(with:resultHandler:)`` or ``SFSpeechRecognizer/recognitionTask(with:delegate:)`` on your ``SFSpeechRecognizer`` object.
-- 
-- Phantom type for @SFSpeechRecognitionTask@.
data SFSpeechRecognitionTask

instance IsObjCObject (Id SFSpeechRecognitionTask) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SFSpeechRecognitionTask"

class IsNSObject a => IsSFSpeechRecognitionTask a where
  toSFSpeechRecognitionTask :: a -> Id SFSpeechRecognitionTask

instance IsSFSpeechRecognitionTask (Id SFSpeechRecognitionTask) where
  toSFSpeechRecognitionTask = unsafeCastId

instance IsNSObject (Id SFSpeechRecognitionTask) where
  toNSObject = unsafeCastId

-- ---------- SFSpeechRecognizer ----------

-- | An object you use to check for the availability of the speech recognition service, and to initiate the speech recognition process.
--
-- An ``SFSpeechRecognizer`` object is the central object for managing the speech recognizer process. Use this object to:
--
-- - Request authorization to use speech recognition services. - Specify the language to use during the recognition process. - Initiate new speech recognition tasks.
--
-- ### Set up speech recognition
--
-- Each speech recognizer supports only one language, which you specify at creation time. The successful creation of a speech recognizer does not guarantee that speech recognition services are available. For some languages, the recognizer might require an Internet connection. Use the ``isAvailable`` property to find out if speech recognition services are available for the current language.
--
-- To initiate the speech recognition process, do the following:
--
-- 1. Request authorization to use speech recognition. See <doc:asking-permission-to-use-speech-recognition>. 2. Create an ``SFSpeechRecognizer`` object. 3. Verify the availability of services using the ``isAvailable`` property of your speech recognizer object. 4. Prepare your audio content. 5. Create a recognition request object—an object that descends from ``SFSpeechRecognitionRequest``. 6. Call the ``recognitionTask(with:delegate:)`` or ``recognitionTask(with:resultHandler:)`` method to begin the recognition process.
--
-- The type of recognition request object you create depends on whether you are processing an existing audio file or an incoming stream of audio. For existing audio files, create a ``SFSpeechURLRecognitionRequest`` object. For audio streams, create a ``SFSpeechAudioBufferRecognitionRequest`` object.
--
-- ### Create a great user experience for speech recognition
--
-- Here are some tips to consider when adding speech recognition support to your app.
--
-- - **Be prepared to handle failures caused by speech recognition limits.** Because speech recognition is a network-based service, limits are enforced so that the service can remain freely available to all apps. Individual devices may be limited in the number of recognitions that can be performed per day, and each app may be throttled globally based on the number of requests it makes per day. If a recognition request fails quickly (within a second or two of starting), check to see if the recognition service became unavailable. If it is, you may want to ask users to try again later. - **Plan for a one-minute limit on audio duration.** Speech recognition places a relatively high burden on battery life and network usage. To minimize this burden, the framework stops speech recognition tasks that last longer than one minute. This limit is similar to the one for keyboard-related dictation. - **Remind the user when your app is recording.** For example, display a visual indicator and play sounds at the beginning and end of speech recognition to help users understand that they're being actively recorded. You can also display speech as it is being recognized so that users understand what your app is doing and see any mistakes made during the recognition process. - **Do not perform speech recognition on private or sensitive information.** Some speech is not appropriate for recognition. Don't send passwords, health or financial data, and other sensitive speech for recognition.
-- 
-- Phantom type for @SFSpeechRecognizer@.
data SFSpeechRecognizer

instance IsObjCObject (Id SFSpeechRecognizer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SFSpeechRecognizer"

class IsNSObject a => IsSFSpeechRecognizer a where
  toSFSpeechRecognizer :: a -> Id SFSpeechRecognizer

instance IsSFSpeechRecognizer (Id SFSpeechRecognizer) where
  toSFSpeechRecognizer = unsafeCastId

instance IsNSObject (Id SFSpeechRecognizer) where
  toNSObject = unsafeCastId

-- ---------- SFTranscription ----------

-- | A textual representation of the specified speech in its entirety, as recognized by the speech recognizer.
--
-- Use @SFTranscription@ to obtain all the recognized utterances from your audio content. An _utterance_ is a vocalized word or group of words that represent a single meaning to the speech recognizer (``SFSpeechRecognizer``).
--
-- Use the ``formattedString`` property to retrieve the entire transcription of utterances, or use the ``segments`` property to retrieve an individual utterance (``SFTranscriptionSegment``).
--
-- You don't create an @SFTranscription@ directly. Instead, you retrieve it from an ``SFSpeechRecognitionResult`` instance. The speech recognizer sends a speech recognition result to your app in one of two ways, depending on how your app started a speech recognition task.
--
-- You can start a speech recognition task by using the speech recognizer's ``SFSpeechRecognizer/recognitionTask(with:resultHandler:)`` method. When the task is complete, the speech recognizer sends an ``SFSpeechRecognitionResult`` instance to your @resultHandler@ closure. Alternatively, you can use the speech recognizer's ``SFSpeechRecognizer/recognitionTask(with:delegate:)`` method to start a speech recognition task. When the task is complete, the speech recognizer uses your ``SFSpeechRecognitionTaskDelegate`` to send an ``SFSpeechRecognitionResult`` by using the delegate's ``SFSpeechRecognitionTaskDelegate/speechRecognitionTask(_:didFinishRecognition:)`` method.
--
-- An @SFTranscription@ represents only a potential version of the speech. It might not be an accurate representation of the utterances.
-- 
-- Phantom type for @SFTranscription@.
data SFTranscription

instance IsObjCObject (Id SFTranscription) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SFTranscription"

class IsNSObject a => IsSFTranscription a where
  toSFTranscription :: a -> Id SFTranscription

instance IsSFTranscription (Id SFTranscription) where
  toSFTranscription = unsafeCastId

instance IsNSObject (Id SFTranscription) where
  toNSObject = unsafeCastId

-- ---------- SFTranscriptionSegment ----------

-- | A discrete part of an entire transcription, as identified by the speech recognizer.
--
-- Use ``SFTranscriptionSegment`` to get details about a part of an overall ``SFTranscription``. An ``SFTranscriptionSegment`` represents an utterance, which is a vocalized word or group of words that represent a single meaning to the speech recognizer (``SFSpeechRecognizer``).
--
-- You don't create transcription object segments directly. Instead, you access them from a transcription's ``SFTranscription/segments`` property.
--
-- A transcription segment includes the following information:
--
-- - The text of the utterance, plus any alternative interpretations of the spoken word. - The character range of the segment within the ``SFTranscription/formattedString`` of its parent ``SFTranscription``. - A ``confidence`` value, indicating how likely it is that the specified string matches the audible speech. - A ``timestamp`` and ``duration`` value, indicating the position of the segment within the provided audio stream.
-- 
-- Phantom type for @SFTranscriptionSegment@.
data SFTranscriptionSegment

instance IsObjCObject (Id SFTranscriptionSegment) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SFTranscriptionSegment"

class IsNSObject a => IsSFTranscriptionSegment a where
  toSFTranscriptionSegment :: a -> Id SFTranscriptionSegment

instance IsSFTranscriptionSegment (Id SFTranscriptionSegment) where
  toSFTranscriptionSegment = unsafeCastId

instance IsNSObject (Id SFTranscriptionSegment) where
  toNSObject = unsafeCastId

-- ---------- SFVoiceAnalytics ----------

-- | A collection of vocal analysis metrics.
--
-- Use an ``SFAcousticFeature`` object to access the @SFVoiceAnalytics@ insights. Voice analytics include the following features:
--
-- - Use ``jitter`` to measure how pitch varies in audio. - Use ``shimmer`` to measure how amplitude varies in audio. - Use ``pitch`` to measure the highness and lowness of the tone. - Use ``voicing`` to identify voiced regions in speech.
--
-- These results are part of the ``SFTranscriptionSegment`` object and are available when the system sends the ``SFSpeechRecognitionResult/isFinal`` flag.
-- 
-- Phantom type for @SFVoiceAnalytics@.
data SFVoiceAnalytics

instance IsObjCObject (Id SFVoiceAnalytics) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SFVoiceAnalytics"

class IsNSObject a => IsSFVoiceAnalytics a where
  toSFVoiceAnalytics :: a -> Id SFVoiceAnalytics

instance IsSFVoiceAnalytics (Id SFVoiceAnalytics) where
  toSFVoiceAnalytics = unsafeCastId

instance IsNSObject (Id SFVoiceAnalytics) where
  toNSObject = unsafeCastId

-- ---------- SFSpeechAudioBufferRecognitionRequest ----------

-- | A request to recognize speech from captured audio content, such as audio from the device's microphone.
--
-- Use an ``SFSpeechAudioBufferRecognitionRequest`` object to perform speech recognition on live audio, or on a set of existing audio buffers. For example, use this request object to route audio from a device's microphone to the speech recognizer.
--
-- The request object contains no audio initially. As you capture audio, call ``append(_:)`` or ``appendAudioSampleBuffer(_:)`` to add audio samples to the request object. The speech recognizer continuously analyzes the audio you appended, stopping only when you call the ``endAudio()`` method. You must call ``endAudio()`` explicitly to stop the speech recognition process.
--
-- For a complete example of how to use audio buffers with speech recognition, see [SpeakToMe: Using Speech Recognition with AVAudioEngine](https://developer.apple.com/library/archive/samplecode/SpeakToMe/Introduction/Intro.html#//apple_ref/doc/uid/TP40017110).
-- 
-- Phantom type for @SFSpeechAudioBufferRecognitionRequest@.
data SFSpeechAudioBufferRecognitionRequest

instance IsObjCObject (Id SFSpeechAudioBufferRecognitionRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SFSpeechAudioBufferRecognitionRequest"

class IsSFSpeechRecognitionRequest a => IsSFSpeechAudioBufferRecognitionRequest a where
  toSFSpeechAudioBufferRecognitionRequest :: a -> Id SFSpeechAudioBufferRecognitionRequest

instance IsSFSpeechAudioBufferRecognitionRequest (Id SFSpeechAudioBufferRecognitionRequest) where
  toSFSpeechAudioBufferRecognitionRequest = unsafeCastId

instance IsNSObject (Id SFSpeechAudioBufferRecognitionRequest) where
  toNSObject = unsafeCastId

instance IsSFSpeechRecognitionRequest (Id SFSpeechAudioBufferRecognitionRequest) where
  toSFSpeechRecognitionRequest = unsafeCastId

-- ---------- SFSpeechURLRecognitionRequest ----------

-- | A request to recognize speech in a recorded audio file.
--
-- Use this object to perform speech recognition on the contents of an audio file.
--
-- The following example shows a method that performs recognition on an audio file based on the user's default language and prints out the transcription.
--
-- Listing 1. Getting a speech recognizer and making a recognition request
--
-- ```swift func recognizeFile(url: URL) {     // Create a speech recognizer associated with the user's default language.     guard let myRecognizer = SFSpeechRecognizer() else {         // The system doesn't support the user's default language.         return     }
--
-- guard myRecognizer.isAvailable else {         // The recognizer isn't available.         return     }
--
-- // Create and execute a speech recognition request for the audio file at the URL.     let request = SFSpeechURLRecognitionRequest(url: url)     myRecognizer.recognitionTask(with: request) { (result, error) in         guard let result else {             // Recognition failed, so check the error for details and handle it.             return         }
--
-- // Print the speech transcription with the highest confidence that the         // system recognized.         if result.isFinal {             print(result.bestTranscription.formattedString)         }     } } ```
-- 
-- Phantom type for @SFSpeechURLRecognitionRequest@.
data SFSpeechURLRecognitionRequest

instance IsObjCObject (Id SFSpeechURLRecognitionRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SFSpeechURLRecognitionRequest"

class IsSFSpeechRecognitionRequest a => IsSFSpeechURLRecognitionRequest a where
  toSFSpeechURLRecognitionRequest :: a -> Id SFSpeechURLRecognitionRequest

instance IsSFSpeechURLRecognitionRequest (Id SFSpeechURLRecognitionRequest) where
  toSFSpeechURLRecognitionRequest = unsafeCastId

instance IsNSObject (Id SFSpeechURLRecognitionRequest) where
  toNSObject = unsafeCastId

instance IsSFSpeechRecognitionRequest (Id SFSpeechURLRecognitionRequest) where
  toSFSpeechRecognitionRequest = unsafeCastId
