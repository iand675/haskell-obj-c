{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A task object for monitoring the speech recognition progress.
--
-- Use an @SFSpeechRecognitionTask@ object to determine the state of a speech recognition task, to cancel an ongoing task, or to signal the end of the task.
--
-- You don't create speech recognition task objects directly. Instead, you receive one of these objects after calling ``SFSpeechRecognizer/recognitionTask(with:resultHandler:)`` or ``SFSpeechRecognizer/recognitionTask(with:delegate:)`` on your ``SFSpeechRecognizer`` object.
--
-- Generated bindings for @SFSpeechRecognitionTask@.
module ObjC.Speech.SFSpeechRecognitionTask
  ( SFSpeechRecognitionTask
  , IsSFSpeechRecognitionTask(..)
  , finish
  , cancel
  , state
  , finishing
  , cancelled
  , error_
  , finishSelector
  , cancelSelector
  , stateSelector
  , finishingSelector
  , cancelledSelector
  , errorSelector

  -- * Enum types
  , SFSpeechRecognitionTaskState(SFSpeechRecognitionTaskState)
  , pattern SFSpeechRecognitionTaskStateStarting
  , pattern SFSpeechRecognitionTaskStateRunning
  , pattern SFSpeechRecognitionTaskStateFinishing
  , pattern SFSpeechRecognitionTaskStateCanceling
  , pattern SFSpeechRecognitionTaskStateCompleted

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

-- | Stops accepting new audio and finishes processing on the audio input that has already been accepted.
--
-- For audio buffer–based recognition, recognition does not finish until this method is called, so be sure to call it when the audio source is exhausted.
--
-- ObjC selector: @- finish@
finish :: IsSFSpeechRecognitionTask sfSpeechRecognitionTask => sfSpeechRecognitionTask -> IO ()
finish sfSpeechRecognitionTask  =
  sendMsg sfSpeechRecognitionTask (mkSelector "finish") retVoid []

-- | Cancels the current speech recognition task.
--
-- You can cancel recognition tasks for both prerecorded and live audio input. For example, you might cancel a task in response to a user action or because the recording was interrupted.
--
-- When canceling a task, be sure to release any resources associated with the task, such as the audio input resources you are using to capture audio samples.
--
-- ObjC selector: @- cancel@
cancel :: IsSFSpeechRecognitionTask sfSpeechRecognitionTask => sfSpeechRecognitionTask -> IO ()
cancel sfSpeechRecognitionTask  =
  sendMsg sfSpeechRecognitionTask (mkSelector "cancel") retVoid []

-- | The current state of the speech recognition task.
--
-- Check the value of this property to get the state of the in-progress speech recognition session. For valid values, see ``SFSpeechRecognitionTaskState``.
--
-- ObjC selector: @- state@
state :: IsSFSpeechRecognitionTask sfSpeechRecognitionTask => sfSpeechRecognitionTask -> IO SFSpeechRecognitionTaskState
state sfSpeechRecognitionTask  =
  fmap (coerce :: CLong -> SFSpeechRecognitionTaskState) $ sendMsg sfSpeechRecognitionTask (mkSelector "state") retCLong []

-- | A Boolean value that indicates whether audio input has stopped.
--
-- By default, the value of this property is @false@.
--
-- ObjC selector: @- finishing@
finishing :: IsSFSpeechRecognitionTask sfSpeechRecognitionTask => sfSpeechRecognitionTask -> IO Bool
finishing sfSpeechRecognitionTask  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg sfSpeechRecognitionTask (mkSelector "finishing") retCULong []

-- | A Boolean value that indicates whether the speech recognition task was canceled.
--
-- By default, the value of this property is @false@.
--
-- ObjC selector: @- cancelled@
cancelled :: IsSFSpeechRecognitionTask sfSpeechRecognitionTask => sfSpeechRecognitionTask -> IO Bool
cancelled sfSpeechRecognitionTask  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg sfSpeechRecognitionTask (mkSelector "cancelled") retCULong []

-- | An error object that specifies the error that occurred during a speech recognition task.
--
-- The system may return one of the errors listed in the table below.
--
-- | Error Code | Error Domain | Description | |---|---|---| | @102@ | @kLSRErrorDomain@ | Assets are not installed. | | @201@ | @kLSRErrorDomain@ | Siri or Dictation is disabled. | | @300@ | @kLSRErrorDomain@ | Failed to initialize recognizer. | | @301@ | @kLSRErrorDomain@ | Request was canceled. | | @203@ | @kAFAssistantErrorDomain@ | Failure occurred during speech recognition. | | @1100@ | @kAFAssistantErrorDomain@ | Trying to start recognition while an earlier instance is still active. | | @1101@ | @kAFAssistantErrorDomain@ | Connection to speech process was invalidated. | | @1107@ | @kAFAssistantErrorDomain@ | Connection to speech process was interrupted. | | @1110@ | @kAFAssistantErrorDomain@ | Failed to recognize any speech. | | @1700@ | @kAFAssistantErrorDomain@ | Request is not authorized. |
--
-- ObjC selector: @- error@
error_ :: IsSFSpeechRecognitionTask sfSpeechRecognitionTask => sfSpeechRecognitionTask -> IO (Id NSError)
error_ sfSpeechRecognitionTask  =
  sendMsg sfSpeechRecognitionTask (mkSelector "error") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @finish@
finishSelector :: Selector
finishSelector = mkSelector "finish"

-- | @Selector@ for @cancel@
cancelSelector :: Selector
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @finishing@
finishingSelector :: Selector
finishingSelector = mkSelector "finishing"

-- | @Selector@ for @cancelled@
cancelledSelector :: Selector
cancelledSelector = mkSelector "cancelled"

-- | @Selector@ for @error@
errorSelector :: Selector
errorSelector = mkSelector "error"

