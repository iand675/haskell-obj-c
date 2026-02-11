{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

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
-- Generated bindings for @SFSpeechRecognizer@.
module ObjC.Speech.SFSpeechRecognizer
  ( SFSpeechRecognizer
  , IsSFSpeechRecognizer(..)
  , supportedLocales
  , authorizationStatus
  , requestAuthorization
  , init_
  , initWithLocale
  , recognitionTaskWithRequest_resultHandler
  , recognitionTaskWithRequest_delegate
  , available
  , locale
  , supportsOnDeviceRecognition
  , setSupportsOnDeviceRecognition
  , defaultTaskHint
  , setDefaultTaskHint
  , queue
  , setQueue
  , supportedLocalesSelector
  , authorizationStatusSelector
  , requestAuthorizationSelector
  , initSelector
  , initWithLocaleSelector
  , recognitionTaskWithRequest_resultHandlerSelector
  , recognitionTaskWithRequest_delegateSelector
  , availableSelector
  , localeSelector
  , supportsOnDeviceRecognitionSelector
  , setSupportsOnDeviceRecognitionSelector
  , defaultTaskHintSelector
  , setDefaultTaskHintSelector
  , queueSelector
  , setQueueSelector

  -- * Enum types
  , SFSpeechRecognitionTaskHint(SFSpeechRecognitionTaskHint)
  , pattern SFSpeechRecognitionTaskHintUnspecified
  , pattern SFSpeechRecognitionTaskHintDictation
  , pattern SFSpeechRecognitionTaskHintSearch
  , pattern SFSpeechRecognitionTaskHintConfirmation
  , SFSpeechRecognizerAuthorizationStatus(SFSpeechRecognizerAuthorizationStatus)
  , pattern SFSpeechRecognizerAuthorizationStatusNotDetermined
  , pattern SFSpeechRecognizerAuthorizationStatusDenied
  , pattern SFSpeechRecognizerAuthorizationStatusRestricted
  , pattern SFSpeechRecognizerAuthorizationStatusAuthorized

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

import ObjC.Speech.Internal.Classes
import ObjC.Speech.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Returns the set of locales that are supported by the speech recognizer.
--
-- This method returns the locales for which speech recognition is supported. Support for a locale does not guarantee that speech recognition is currently possible for that locale. For some locales, the speech recognizer requires an active Internet connection to communicate with Apple's servers. If the speech recognizer is currently unable to process requests,   ``isAvailable`` returns @false@.
--
-- Speech recognition supports the same locales that are supported by the keyboard's dictation feature. For a list of these locales, see [QuickType Keyboard: Dictation](https://www.apple.com/ios/feature-availability/#quicktype-keyboard-dictation).
--
-- - Returns: A set of locales that support speech recognition.
--
-- ObjC selector: @+ supportedLocales@
supportedLocales :: IO (Id NSSet)
supportedLocales  =
  do
    cls' <- getRequiredClass "SFSpeechRecognizer"
    sendClassMsg cls' (mkSelector "supportedLocales") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns your app's current authorization to perform speech recognition.
--
-- The user can reject your app's request to perform speech recognition, but your request can also be denied if speech recognition is not supported on the device. The app can also change your app's authorization status at any time from the Settings app.
--
-- - Returns: The app's current authorization status value. For a list of values, see ``SFSpeechRecognizerAuthorizationStatus``.
--
-- ObjC selector: @+ authorizationStatus@
authorizationStatus :: IO SFSpeechRecognizerAuthorizationStatus
authorizationStatus  =
  do
    cls' <- getRequiredClass "SFSpeechRecognizer"
    fmap (coerce :: CLong -> SFSpeechRecognizerAuthorizationStatus) $ sendClassMsg cls' (mkSelector "authorizationStatus") retCLong []

-- | Asks the user to allow your app to perform speech recognition.
--
-- Call this method before performing any other tasks associated with speech recognition. This method executes asynchronously, returning shortly after you call it. At some point later, the system calls the provided @handler@ block with the results.
--
-- When your app's authorization status is ``SFSpeechRecognizerAuthorizationStatus/notDetermined``, this method causes the system to prompt the user to grant or deny permission for your app to use speech recognition. The prompt includes the custom message you specify in the @NSSpeechRecognitionUsageDescription@ key of your app's @Info.plist@ file. The user's response is saved so that future calls to this method do not prompt the user again.
--
-- > Important: > Your app's @Info.plist@ file must contain the @NSSpeechRecognitionUsageDescription@ key with a valid usage description. If this key is not present, your app will crash when you call this method.
--
-- For more information about requesting authorization, see <doc:asking-permission-to-use-speech-recognition>.
--
-- - Parameters:   - handler: The block to execute when your app's authorization status is known. The status parameter of the block contains your app's authorization status. The system does not guarantee the execution of this block on your app's main dispatch queue.
--
-- ObjC selector: @+ requestAuthorization:@
requestAuthorization :: Ptr () -> IO ()
requestAuthorization handler =
  do
    cls' <- getRequiredClass "SFSpeechRecognizer"
    sendClassMsg cls' (mkSelector "requestAuthorization:") retVoid [argPtr (castPtr handler :: Ptr ())]

-- | Creates a speech recognizer associated with the user's default language settings.
--
-- If the user's default language is not supported for speech recognition, this method attempts to fall back to the language used by the keyboard for dictation. If that fails, this method returns @nil@.
--
-- Even if this method returns a valid speech recognizer object, the speech recognition services may be temporarily unavailable. To determine whether speech recognition services are available, check the ``isAvailable`` property.
--
-- - Returns: An initialized speech recognizer object, or @nil@ if there was a problem creating the object.
--
-- ObjC selector: @- init@
init_ :: IsSFSpeechRecognizer sfSpeechRecognizer => sfSpeechRecognizer -> IO (Id SFSpeechRecognizer)
init_ sfSpeechRecognizer  =
  sendMsg sfSpeechRecognizer (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Creates a speech recognizer associated with the specified locale.
--
-- If you specify a language that is not supported by the speech recognizer, this method attempts to fall back to the language used by the keyboard for dictation. If that fails, this method returns @nil@.
--
-- Even if this method returns a valid speech recognizer object, the speech recognition services may be temporarily unavailable. To determine whether speech recognition services are available, check the ``isAvailable`` property.
--
-- - Parameters:   - locale: The locale object representing the language you want to use for speech recognition. For a list of languages supported by the speech recognizer, see ``supportedLocales()``.
--
-- - Returns: An initialized speech recognizer object, or @nil@ if the specified language was not supported.
--
-- ObjC selector: @- initWithLocale:@
initWithLocale :: (IsSFSpeechRecognizer sfSpeechRecognizer, IsNSLocale locale) => sfSpeechRecognizer -> locale -> IO (Id SFSpeechRecognizer)
initWithLocale sfSpeechRecognizer  locale =
withObjCPtr locale $ \raw_locale ->
    sendMsg sfSpeechRecognizer (mkSelector "initWithLocale:") (retPtr retVoid) [argPtr (castPtr raw_locale :: Ptr ())] >>= ownedObject . castPtr

-- | Executes the speech recognition request and delivers the results to the specified handler block.
--
-- Use this method to initiate the speech recognition process on the audio contained in the request object. This method executes asynchronously and returns a ``SFSpeechRecognitionTask`` object that you can use to cancel or finalize the recognition process later. As results become available, the method calls the block in the @resultHandler@ parameter.
--
-- - Parameters:   - request: A request (in an ``SFSpeechRecognitionRequest`` object) to recognize speech from an audio source.   - resultHandler: The block to call when partial or final results are available, or when an error occurs. If the ``SFSpeechRecognitionRequest/shouldReportPartialResults`` property is @true@, this block may be called multiple times to deliver the partial and final results. The block has no return value and takes the following parameters:
--
-- - term result: A ``SFSpeechRecognitionResult`` containing the partial or final transcriptions of the audio content.     - term error: An error object if a problem occurred. This parameter is @nil@ if speech recognition was successful.
--
-- - Returns: The task object you can use to manage an in-progress recognition request.
--
-- ObjC selector: @- recognitionTaskWithRequest:resultHandler:@
recognitionTaskWithRequest_resultHandler :: (IsSFSpeechRecognizer sfSpeechRecognizer, IsSFSpeechRecognitionRequest request) => sfSpeechRecognizer -> request -> Ptr () -> IO (Id SFSpeechRecognitionTask)
recognitionTaskWithRequest_resultHandler sfSpeechRecognizer  request resultHandler =
withObjCPtr request $ \raw_request ->
    sendMsg sfSpeechRecognizer (mkSelector "recognitionTaskWithRequest:resultHandler:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr resultHandler :: Ptr ())] >>= retainedObject . castPtr

-- | Recognizes speech from the audio source associated with the specified request, using the specified delegate to manage the results.
--
-- Use this method to initiate the speech recognition process on the audio contained in the request object. This method executes asynchronously and returns a ``SFSpeechRecognitionTask`` object that you can use to cancel or finalize the recognition process later. As results become available, the method calls the methods of the provided @delegate@ object.
--
-- Note that the ``SFSpeechRecognitionTask`` object returned by this method does not retain your delegate object. You must maintain a strong reference to your delegate while speech recognition is in progress.
--
-- - Parameters:   - request: A request (encapsulated in an ``SFSpeechRecognitionRequest`` object) to recognize speech from an audio source.   - delegate: An object that can handle results from the speech recognition task. This object must conform to the ``SFSpeechRecognitionTaskDelegate`` protocol.
--
-- - Returns: The task object you can use to manage an in-progress recognition request.
--
-- ObjC selector: @- recognitionTaskWithRequest:delegate:@
recognitionTaskWithRequest_delegate :: (IsSFSpeechRecognizer sfSpeechRecognizer, IsSFSpeechRecognitionRequest request) => sfSpeechRecognizer -> request -> RawId -> IO (Id SFSpeechRecognitionTask)
recognitionTaskWithRequest_delegate sfSpeechRecognizer  request delegate =
withObjCPtr request $ \raw_request ->
    sendMsg sfSpeechRecognizer (mkSelector "recognitionTaskWithRequest:delegate:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ())] >>= retainedObject . castPtr

-- | A Boolean value that indicates whether the speech recognizer is currently available.
--
-- When the value of this property is @true@, you may create new speech recognition tasks. When value of this property is @false@, speech recognition services are not available.
--
-- ObjC selector: @- available@
available :: IsSFSpeechRecognizer sfSpeechRecognizer => sfSpeechRecognizer -> IO Bool
available sfSpeechRecognizer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg sfSpeechRecognizer (mkSelector "available") retCULong []

-- | The locale of the speech recognizer.
--
-- The locale of the speech recognizer is an @NSLocale@ object. The default value of this property is the system locale (that is, @+[NSLocale systemLocale]@).
--
-- ObjC selector: @- locale@
locale :: IsSFSpeechRecognizer sfSpeechRecognizer => sfSpeechRecognizer -> IO (Id NSLocale)
locale sfSpeechRecognizer  =
  sendMsg sfSpeechRecognizer (mkSelector "locale") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A Boolean value that indicates whether the speech recognizer can operate without network access.
--
-- An ``SFSpeechRecognitionRequest`` can only honor its ``SFSpeechRecognitionRequest/requiresOnDeviceRecognition`` property if ``supportsOnDeviceRecognition`` is @true@. If ``supportsOnDeviceRecognition`` is @false@, the ``SFSpeechRecognizer`` requires a network in order to recognize speech.
--
-- ObjC selector: @- supportsOnDeviceRecognition@
supportsOnDeviceRecognition :: IsSFSpeechRecognizer sfSpeechRecognizer => sfSpeechRecognizer -> IO Bool
supportsOnDeviceRecognition sfSpeechRecognizer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg sfSpeechRecognizer (mkSelector "supportsOnDeviceRecognition") retCULong []

-- | A Boolean value that indicates whether the speech recognizer can operate without network access.
--
-- An ``SFSpeechRecognitionRequest`` can only honor its ``SFSpeechRecognitionRequest/requiresOnDeviceRecognition`` property if ``supportsOnDeviceRecognition`` is @true@. If ``supportsOnDeviceRecognition`` is @false@, the ``SFSpeechRecognizer`` requires a network in order to recognize speech.
--
-- ObjC selector: @- setSupportsOnDeviceRecognition:@
setSupportsOnDeviceRecognition :: IsSFSpeechRecognizer sfSpeechRecognizer => sfSpeechRecognizer -> Bool -> IO ()
setSupportsOnDeviceRecognition sfSpeechRecognizer  value =
  sendMsg sfSpeechRecognizer (mkSelector "setSupportsOnDeviceRecognition:") retVoid [argCULong (if value then 1 else 0)]

-- | A hint that indicates the type of speech recognition being requested.
--
-- By default, the value of this property overrides the ``SFSpeechRecognitionTaskHint/unspecified`` value for requests. For possible values, see ``SFSpeechRecognitionTaskHint``.
--
-- ObjC selector: @- defaultTaskHint@
defaultTaskHint :: IsSFSpeechRecognizer sfSpeechRecognizer => sfSpeechRecognizer -> IO SFSpeechRecognitionTaskHint
defaultTaskHint sfSpeechRecognizer  =
  fmap (coerce :: CLong -> SFSpeechRecognitionTaskHint) $ sendMsg sfSpeechRecognizer (mkSelector "defaultTaskHint") retCLong []

-- | A hint that indicates the type of speech recognition being requested.
--
-- By default, the value of this property overrides the ``SFSpeechRecognitionTaskHint/unspecified`` value for requests. For possible values, see ``SFSpeechRecognitionTaskHint``.
--
-- ObjC selector: @- setDefaultTaskHint:@
setDefaultTaskHint :: IsSFSpeechRecognizer sfSpeechRecognizer => sfSpeechRecognizer -> SFSpeechRecognitionTaskHint -> IO ()
setDefaultTaskHint sfSpeechRecognizer  value =
  sendMsg sfSpeechRecognizer (mkSelector "setDefaultTaskHint:") retVoid [argCLong (coerce value)]

-- | The queue on which to execute recognition task handlers and delegate methods.
--
-- The default value of this property is the app's main queue. Assign a different queue if you want delegate methods and handlers to be executed on a background queue.
--
-- The handler you pass to the ``requestAuthorization(_:)`` method does not use this queue.
--
-- ObjC selector: @- queue@
queue :: IsSFSpeechRecognizer sfSpeechRecognizer => sfSpeechRecognizer -> IO (Id NSOperationQueue)
queue sfSpeechRecognizer  =
  sendMsg sfSpeechRecognizer (mkSelector "queue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The queue on which to execute recognition task handlers and delegate methods.
--
-- The default value of this property is the app's main queue. Assign a different queue if you want delegate methods and handlers to be executed on a background queue.
--
-- The handler you pass to the ``requestAuthorization(_:)`` method does not use this queue.
--
-- ObjC selector: @- setQueue:@
setQueue :: (IsSFSpeechRecognizer sfSpeechRecognizer, IsNSOperationQueue value) => sfSpeechRecognizer -> value -> IO ()
setQueue sfSpeechRecognizer  value =
withObjCPtr value $ \raw_value ->
    sendMsg sfSpeechRecognizer (mkSelector "setQueue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @supportedLocales@
supportedLocalesSelector :: Selector
supportedLocalesSelector = mkSelector "supportedLocales"

-- | @Selector@ for @authorizationStatus@
authorizationStatusSelector :: Selector
authorizationStatusSelector = mkSelector "authorizationStatus"

-- | @Selector@ for @requestAuthorization:@
requestAuthorizationSelector :: Selector
requestAuthorizationSelector = mkSelector "requestAuthorization:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithLocale:@
initWithLocaleSelector :: Selector
initWithLocaleSelector = mkSelector "initWithLocale:"

-- | @Selector@ for @recognitionTaskWithRequest:resultHandler:@
recognitionTaskWithRequest_resultHandlerSelector :: Selector
recognitionTaskWithRequest_resultHandlerSelector = mkSelector "recognitionTaskWithRequest:resultHandler:"

-- | @Selector@ for @recognitionTaskWithRequest:delegate:@
recognitionTaskWithRequest_delegateSelector :: Selector
recognitionTaskWithRequest_delegateSelector = mkSelector "recognitionTaskWithRequest:delegate:"

-- | @Selector@ for @available@
availableSelector :: Selector
availableSelector = mkSelector "available"

-- | @Selector@ for @locale@
localeSelector :: Selector
localeSelector = mkSelector "locale"

-- | @Selector@ for @supportsOnDeviceRecognition@
supportsOnDeviceRecognitionSelector :: Selector
supportsOnDeviceRecognitionSelector = mkSelector "supportsOnDeviceRecognition"

-- | @Selector@ for @setSupportsOnDeviceRecognition:@
setSupportsOnDeviceRecognitionSelector :: Selector
setSupportsOnDeviceRecognitionSelector = mkSelector "setSupportsOnDeviceRecognition:"

-- | @Selector@ for @defaultTaskHint@
defaultTaskHintSelector :: Selector
defaultTaskHintSelector = mkSelector "defaultTaskHint"

-- | @Selector@ for @setDefaultTaskHint:@
setDefaultTaskHintSelector :: Selector
setDefaultTaskHintSelector = mkSelector "setDefaultTaskHint:"

-- | @Selector@ for @queue@
queueSelector :: Selector
queueSelector = mkSelector "queue"

-- | @Selector@ for @setQueue:@
setQueueSelector :: Selector
setQueueSelector = mkSelector "setQueue:"

