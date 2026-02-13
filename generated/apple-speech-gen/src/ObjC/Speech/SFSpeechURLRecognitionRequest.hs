{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

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
-- Generated bindings for @SFSpeechURLRecognitionRequest@.
module ObjC.Speech.SFSpeechURLRecognitionRequest
  ( SFSpeechURLRecognitionRequest
  , IsSFSpeechURLRecognitionRequest(..)
  , init_
  , initWithURL
  , url
  , initSelector
  , initWithURLSelector
  , urlSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Speech.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSFSpeechURLRecognitionRequest sfSpeechURLRecognitionRequest => sfSpeechURLRecognitionRequest -> IO (Id SFSpeechURLRecognitionRequest)
init_ sfSpeechURLRecognitionRequest =
  sendOwnedMessage sfSpeechURLRecognitionRequest initSelector

-- | Creates a speech recognition request, initialized with the specified URL.
--
-- Use this method to create a request to recognize speech in a recorded audio file that resides at the specified URL. Pass the request to the recognizer's ``SFSpeechRecognizer/recognitionTask(with:delegate:)`` method to start recognition.
--
-- ObjC selector: @- initWithURL:@
initWithURL :: (IsSFSpeechURLRecognitionRequest sfSpeechURLRecognitionRequest, IsNSURL url) => sfSpeechURLRecognitionRequest -> url -> IO (Id SFSpeechURLRecognitionRequest)
initWithURL sfSpeechURLRecognitionRequest url =
  sendOwnedMessage sfSpeechURLRecognitionRequest initWithURLSelector (toNSURL url)

-- | The URL of the audio file.
--
-- ObjC selector: @- URL@
url :: IsSFSpeechURLRecognitionRequest sfSpeechURLRecognitionRequest => sfSpeechURLRecognitionRequest -> IO (Id NSURL)
url sfSpeechURLRecognitionRequest =
  sendMessage sfSpeechURLRecognitionRequest urlSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SFSpeechURLRecognitionRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithURL:@
initWithURLSelector :: Selector '[Id NSURL] (Id SFSpeechURLRecognitionRequest)
initWithURLSelector = mkSelector "initWithURL:"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

