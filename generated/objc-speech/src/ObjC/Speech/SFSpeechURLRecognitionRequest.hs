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
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSFSpeechURLRecognitionRequest sfSpeechURLRecognitionRequest => sfSpeechURLRecognitionRequest -> IO (Id SFSpeechURLRecognitionRequest)
init_ sfSpeechURLRecognitionRequest  =
  sendMsg sfSpeechURLRecognitionRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Creates a speech recognition request, initialized with the specified URL.
--
-- Use this method to create a request to recognize speech in a recorded audio file that resides at the specified URL. Pass the request to the recognizer's ``SFSpeechRecognizer/recognitionTask(with:delegate:)`` method to start recognition.
--
-- ObjC selector: @- initWithURL:@
initWithURL :: (IsSFSpeechURLRecognitionRequest sfSpeechURLRecognitionRequest, IsNSURL url) => sfSpeechURLRecognitionRequest -> url -> IO (Id SFSpeechURLRecognitionRequest)
initWithURL sfSpeechURLRecognitionRequest  url =
withObjCPtr url $ \raw_url ->
    sendMsg sfSpeechURLRecognitionRequest (mkSelector "initWithURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= ownedObject . castPtr

-- | The URL of the audio file.
--
-- ObjC selector: @- URL@
url :: IsSFSpeechURLRecognitionRequest sfSpeechURLRecognitionRequest => sfSpeechURLRecognitionRequest -> IO (Id NSURL)
url sfSpeechURLRecognitionRequest  =
  sendMsg sfSpeechURLRecognitionRequest (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithURL:@
initWithURLSelector :: Selector
initWithURLSelector = mkSelector "initWithURL:"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

