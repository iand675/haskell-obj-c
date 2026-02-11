{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Configuration information for an assessment session.
--
-- Create a configuration instance and pass it to the ``AEAssessmentSession/init(configuration:)`` initializer of an ``AEAssessmentSession`` instance to create a new assessment session. Before using the configuration, indicate which exceptions you want to allow for the assessment session's restrictions by setting values on the configuration instance. For example, you can set values to allow dictation and certain aspects of autocorrect:
--
-- ```swift let config = AEAssessmentConfiguration()
--
-- #if os(iOS) // These exceptions available only on iOS and iPadOS. config.allowsDictation = true config.autocorrectMode = [.punctuation, .spelling] #endif
--
-- let session = AEAssessmentSession(configuration: config) ```
--
-- While you provide a configuration instance when creating a session on iOS, iPadOS, and macOS, specific exceptions apply only to certain platforms. In particular, on macOS, you can selectively make specific apps besides your own available during an assessment — for example, to allow users to access a calculator or a dictionary. All other exceptions apply only to iOS and iPadOS.
--
-- ## Topics
--
-- ### Allowing access to other apps
--
-- - ``setConfiguration(_:for:)`` - ``configurationsByApplication`` - ``remove(_:)`` - ``mainParticipantConfiguration`` - ``AEAssessmentApplication`` - ``AEAssessmentParticipantConfiguration``
--
-- ### Allowing accessibility
--
-- - ``allowsAccessibilityKeyboard`` - ``allowsAccessibilityLiveCaptions`` - ``allowsAccessibilityReader`` - ``allowsAccessibilitySpeech`` - ``allowsAccessibilityTypingFeedback`` - ``allowsDictation``
--
-- ### Allowing typing assistance
--
-- - ``allowsContinuousPathKeyboard`` - ``allowsKeyboardShortcuts`` - ``allowsPredictiveKeyboard`` - ``allowsPasswordAutoFill``
--
-- ### Allowing corrections
--
-- - ``allowsSpellCheck`` - ``autocorrectMode-swift.property`` - ``AutocorrectMode-swift.struct``
--
-- ### Allowing handoff
--
-- - ``allowsActivityContinuation``
--
-- ### Allowing content capture
--
-- - ``allowsScreenshots``
--
-- Generated bindings for @AEAssessmentConfiguration@.
module ObjC.AutomaticAssessmentConfiguration.AEAssessmentConfiguration
  ( AEAssessmentConfiguration
  , IsAEAssessmentConfiguration(..)
  , setConfiguration_forApplication
  , removeApplication
  , autocorrectMode
  , setAutocorrectMode
  , allowsSpellCheck
  , setAllowsSpellCheck
  , allowsPredictiveKeyboard
  , setAllowsPredictiveKeyboard
  , allowsKeyboardShortcuts
  , setAllowsKeyboardShortcuts
  , allowsActivityContinuation
  , setAllowsActivityContinuation
  , allowsDictation
  , setAllowsDictation
  , allowsAccessibilityKeyboard
  , setAllowsAccessibilityKeyboard
  , allowsAccessibilityLiveCaptions
  , setAllowsAccessibilityLiveCaptions
  , allowsAccessibilityReader
  , setAllowsAccessibilityReader
  , allowsAccessibilitySpeech
  , setAllowsAccessibilitySpeech
  , allowsAccessibilityTypingFeedback
  , setAllowsAccessibilityTypingFeedback
  , allowsPasswordAutoFill
  , setAllowsPasswordAutoFill
  , allowsContinuousPathKeyboard
  , setAllowsContinuousPathKeyboard
  , allowsScreenshots
  , setAllowsScreenshots
  , mainParticipantConfiguration
  , configurationsByApplication
  , setConfiguration_forApplicationSelector
  , removeApplicationSelector
  , autocorrectModeSelector
  , setAutocorrectModeSelector
  , allowsSpellCheckSelector
  , setAllowsSpellCheckSelector
  , allowsPredictiveKeyboardSelector
  , setAllowsPredictiveKeyboardSelector
  , allowsKeyboardShortcutsSelector
  , setAllowsKeyboardShortcutsSelector
  , allowsActivityContinuationSelector
  , setAllowsActivityContinuationSelector
  , allowsDictationSelector
  , setAllowsDictationSelector
  , allowsAccessibilityKeyboardSelector
  , setAllowsAccessibilityKeyboardSelector
  , allowsAccessibilityLiveCaptionsSelector
  , setAllowsAccessibilityLiveCaptionsSelector
  , allowsAccessibilityReaderSelector
  , setAllowsAccessibilityReaderSelector
  , allowsAccessibilitySpeechSelector
  , setAllowsAccessibilitySpeechSelector
  , allowsAccessibilityTypingFeedbackSelector
  , setAllowsAccessibilityTypingFeedbackSelector
  , allowsPasswordAutoFillSelector
  , setAllowsPasswordAutoFillSelector
  , allowsContinuousPathKeyboardSelector
  , setAllowsContinuousPathKeyboardSelector
  , allowsScreenshotsSelector
  , setAllowsScreenshotsSelector
  , mainParticipantConfigurationSelector
  , configurationsByApplicationSelector

  -- * Enum types
  , AEAutocorrectMode(AEAutocorrectMode)
  , pattern AEAutocorrectModeNone
  , pattern AEAutocorrectModeSpelling
  , pattern AEAutocorrectModePunctuation

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

import ObjC.AutomaticAssessmentConfiguration.Internal.Classes
import ObjC.AutomaticAssessmentConfiguration.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Adds an app to the list of apps available during an assessment.
--
-- Use this method to make an app besides your own available during an assessment. Create a representation of the app that you want to allow as an ``AEAssessmentApplication`` instance, and the configuration for that app using an ``AEAssessmentParticipantConfiguration`` instance:
--
-- ```swift let calculator = AEAssessmentApplication(bundleIdentifier: "com.apple.calculator") let calculatorConfig = AEAssessmentParticipantConfiguration() calculatorConfig.allowsNetworkAccess = false // Calculator doesn't need the network. ```
--
-- Use the app and its configuration to create an assessment configuration, and either create an assessment session with that, or update an existing session as shown below:
--
-- ```swift let configuration = AEAssessmentConfiguration() configuration.setConfiguration(calculatorConfig, for: calculator) session.update(to: configuration) ```
--
-- You can get a list of the currently allowed apps by accessing the ``AEAssessmentConfiguration/configurationsByApplication`` property. You can disallow a previously allowed app by using the ``AEAssessmentConfiguration/remove(_:)`` method.
--
-- - Parameters:   - configuration: The configuration of the secondary app.   - application: The app that you want to configure.
--
-- ObjC selector: @- setConfiguration:forApplication:@
setConfiguration_forApplication :: (IsAEAssessmentConfiguration aeAssessmentConfiguration, IsAEAssessmentParticipantConfiguration configuration, IsAEAssessmentApplication application) => aeAssessmentConfiguration -> configuration -> application -> IO ()
setConfiguration_forApplication aeAssessmentConfiguration  configuration application =
withObjCPtr configuration $ \raw_configuration ->
  withObjCPtr application $ \raw_application ->
      sendMsg aeAssessmentConfiguration (mkSelector "setConfiguration:forApplication:") retVoid [argPtr (castPtr raw_configuration :: Ptr ()), argPtr (castPtr raw_application :: Ptr ())]

-- | Removes the availability of a previously allowed app.
--
-- Use this method to remove apps that you previously added to the list of apps that are available during an assessment with the ``AEAssessmentConfiguration/setConfiguration(_:for:)`` method. You can get the list of currently allowed apps by accessing the configuration's ``AEAssessmentConfiguration/configurationsByApplication`` property.
--
-- - Parameters:   - application: The app that you want to remove from the list of allowed secondary apps.
--
-- ObjC selector: @- removeApplication:@
removeApplication :: (IsAEAssessmentConfiguration aeAssessmentConfiguration, IsAEAssessmentApplication application) => aeAssessmentConfiguration -> application -> IO ()
removeApplication aeAssessmentConfiguration  application =
withObjCPtr application $ \raw_application ->
    sendMsg aeAssessmentConfiguration (mkSelector "removeApplication:") retVoid [argPtr (castPtr raw_application :: Ptr ())]

-- | The autocorrect mode that specifies which autocorrect features to allow during an assessment.
--
-- Users can turn on autocorrect in the Settings app (General > Keyboard > Auto-Correction). An assessment session disables this feature by default, but you can allow it by setting ``AEAssessmentConfiguration/autocorrectMode-swift.property`` in the ``AEAssessmentConfiguration`` instance that you use to initialize a session. Set the mode's value to some combination of the the values from the ``AEAssessmentConfiguration/AutocorrectMode-swift.struct`` structure.
--
-- ObjC selector: @- autocorrectMode@
autocorrectMode :: IsAEAssessmentConfiguration aeAssessmentConfiguration => aeAssessmentConfiguration -> IO AEAutocorrectMode
autocorrectMode aeAssessmentConfiguration  =
  fmap (coerce :: CULong -> AEAutocorrectMode) $ sendMsg aeAssessmentConfiguration (mkSelector "autocorrectMode") retCULong []

-- | The autocorrect mode that specifies which autocorrect features to allow during an assessment.
--
-- Users can turn on autocorrect in the Settings app (General > Keyboard > Auto-Correction). An assessment session disables this feature by default, but you can allow it by setting ``AEAssessmentConfiguration/autocorrectMode-swift.property`` in the ``AEAssessmentConfiguration`` instance that you use to initialize a session. Set the mode's value to some combination of the the values from the ``AEAssessmentConfiguration/AutocorrectMode-swift.struct`` structure.
--
-- ObjC selector: @- setAutocorrectMode:@
setAutocorrectMode :: IsAEAssessmentConfiguration aeAssessmentConfiguration => aeAssessmentConfiguration -> AEAutocorrectMode -> IO ()
setAutocorrectMode aeAssessmentConfiguration  value =
  sendMsg aeAssessmentConfiguration (mkSelector "setAutocorrectMode:") retVoid [argCULong (coerce value)]

-- | A Boolean value that indicates whether to allow spell check during an assessment.
--
-- Users can activate the spell checker by turning on the Check Spelling feature in the Settings app (General > Keyboard). An assessment session disables spell checking by default, but you can allow it by setting ``AEAssessmentConfiguration/allowsSpellCheck`` to @true@ in the ``AEAssessmentConfiguration`` instance that you use to initialize a session.
--
-- ObjC selector: @- allowsSpellCheck@
allowsSpellCheck :: IsAEAssessmentConfiguration aeAssessmentConfiguration => aeAssessmentConfiguration -> IO Bool
allowsSpellCheck aeAssessmentConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg aeAssessmentConfiguration (mkSelector "allowsSpellCheck") retCULong []

-- | A Boolean value that indicates whether to allow spell check during an assessment.
--
-- Users can activate the spell checker by turning on the Check Spelling feature in the Settings app (General > Keyboard). An assessment session disables spell checking by default, but you can allow it by setting ``AEAssessmentConfiguration/allowsSpellCheck`` to @true@ in the ``AEAssessmentConfiguration`` instance that you use to initialize a session.
--
-- ObjC selector: @- setAllowsSpellCheck:@
setAllowsSpellCheck :: IsAEAssessmentConfiguration aeAssessmentConfiguration => aeAssessmentConfiguration -> Bool -> IO ()
setAllowsSpellCheck aeAssessmentConfiguration  value =
  sendMsg aeAssessmentConfiguration (mkSelector "setAllowsSpellCheck:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean value that indicates whether to enable the predictive keyboard during an assessment.
--
-- Users can turn on the Predictive Keyboard feature in the Settings app (General > Keyboard). An assessment session disables this feature by default, but you can allow it by setting ``AEAssessmentConfiguration/allowsPredictiveKeyboard`` to @true@ in the ``AEAssessmentConfiguration`` instance that you use to initialize a session.
--
-- ObjC selector: @- allowsPredictiveKeyboard@
allowsPredictiveKeyboard :: IsAEAssessmentConfiguration aeAssessmentConfiguration => aeAssessmentConfiguration -> IO Bool
allowsPredictiveKeyboard aeAssessmentConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg aeAssessmentConfiguration (mkSelector "allowsPredictiveKeyboard") retCULong []

-- | A Boolean value that indicates whether to enable the predictive keyboard during an assessment.
--
-- Users can turn on the Predictive Keyboard feature in the Settings app (General > Keyboard). An assessment session disables this feature by default, but you can allow it by setting ``AEAssessmentConfiguration/allowsPredictiveKeyboard`` to @true@ in the ``AEAssessmentConfiguration`` instance that you use to initialize a session.
--
-- ObjC selector: @- setAllowsPredictiveKeyboard:@
setAllowsPredictiveKeyboard :: IsAEAssessmentConfiguration aeAssessmentConfiguration => aeAssessmentConfiguration -> Bool -> IO ()
setAllowsPredictiveKeyboard aeAssessmentConfiguration  value =
  sendMsg aeAssessmentConfiguration (mkSelector "setAllowsPredictiveKeyboard:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean value that indicates whether to allow keyboard shortcuts during an assessment.
--
-- Users can add Keyboard Shortcuts in the Settings app (General > Keyboard > Text Replacement). An assessment session disables the use of keyboard shortcuts by default, but you can allow them by setting ``AEAssessmentConfiguration/allowsKeyboardShortcuts`` to @true@ in the ``AEAssessmentConfiguration`` instance that you use to initialize a session.
--
-- ObjC selector: @- allowsKeyboardShortcuts@
allowsKeyboardShortcuts :: IsAEAssessmentConfiguration aeAssessmentConfiguration => aeAssessmentConfiguration -> IO Bool
allowsKeyboardShortcuts aeAssessmentConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg aeAssessmentConfiguration (mkSelector "allowsKeyboardShortcuts") retCULong []

-- | A Boolean value that indicates whether to allow keyboard shortcuts during an assessment.
--
-- Users can add Keyboard Shortcuts in the Settings app (General > Keyboard > Text Replacement). An assessment session disables the use of keyboard shortcuts by default, but you can allow them by setting ``AEAssessmentConfiguration/allowsKeyboardShortcuts`` to @true@ in the ``AEAssessmentConfiguration`` instance that you use to initialize a session.
--
-- ObjC selector: @- setAllowsKeyboardShortcuts:@
setAllowsKeyboardShortcuts :: IsAEAssessmentConfiguration aeAssessmentConfiguration => aeAssessmentConfiguration -> Bool -> IO ()
setAllowsKeyboardShortcuts aeAssessmentConfiguration  value =
  sendMsg aeAssessmentConfiguration (mkSelector "setAllowsKeyboardShortcuts:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean value that indicates whether to allow Handoff during an assessment.
--
-- Handoff lets users start an activity on one device and seamlessly resume the activity on another. Users control whether a device participates in Handoff by turning the feature on or off in the Settings app (General > AirPlay & Handoff > Handoff). An assessment disables this feature by default, but you can allow users undergoing an assessment to continue to use Handoff by setting ``AEAssessmentConfiguration/allowsActivityContinuation`` to @true@.
--
-- ObjC selector: @- allowsActivityContinuation@
allowsActivityContinuation :: IsAEAssessmentConfiguration aeAssessmentConfiguration => aeAssessmentConfiguration -> IO Bool
allowsActivityContinuation aeAssessmentConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg aeAssessmentConfiguration (mkSelector "allowsActivityContinuation") retCULong []

-- | A Boolean value that indicates whether to allow Handoff during an assessment.
--
-- Handoff lets users start an activity on one device and seamlessly resume the activity on another. Users control whether a device participates in Handoff by turning the feature on or off in the Settings app (General > AirPlay & Handoff > Handoff). An assessment disables this feature by default, but you can allow users undergoing an assessment to continue to use Handoff by setting ``AEAssessmentConfiguration/allowsActivityContinuation`` to @true@.
--
-- ObjC selector: @- setAllowsActivityContinuation:@
setAllowsActivityContinuation :: IsAEAssessmentConfiguration aeAssessmentConfiguration => aeAssessmentConfiguration -> Bool -> IO ()
setAllowsActivityContinuation aeAssessmentConfiguration  value =
  sendMsg aeAssessmentConfiguration (mkSelector "setAllowsActivityContinuation:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean value that indicates whether to allow the use of dictation during an assessment.
--
-- By turning on Enable Dictation (General > Keyboard in the Settings app on iOS and iPadOS), users can speak into their device and have the words they speak converted to text. An assessment session disables this feature by default, but you can allow it by setting ``AEAssessmentConfiguration/allowsDictation`` to @true@ in the ``AEAssessmentConfiguration`` instance that you use to initialize a session.
--
-- ObjC selector: @- allowsDictation@
allowsDictation :: IsAEAssessmentConfiguration aeAssessmentConfiguration => aeAssessmentConfiguration -> IO Bool
allowsDictation aeAssessmentConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg aeAssessmentConfiguration (mkSelector "allowsDictation") retCULong []

-- | A Boolean value that indicates whether to allow the use of dictation during an assessment.
--
-- By turning on Enable Dictation (General > Keyboard in the Settings app on iOS and iPadOS), users can speak into their device and have the words they speak converted to text. An assessment session disables this feature by default, but you can allow it by setting ``AEAssessmentConfiguration/allowsDictation`` to @true@ in the ``AEAssessmentConfiguration`` instance that you use to initialize a session.
--
-- ObjC selector: @- setAllowsDictation:@
setAllowsDictation :: IsAEAssessmentConfiguration aeAssessmentConfiguration => aeAssessmentConfiguration -> Bool -> IO ()
setAllowsDictation aeAssessmentConfiguration  value =
  sendMsg aeAssessmentConfiguration (mkSelector "setAllowsDictation:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean value that indicates whether to allow alternative input methods in the Accessibility Keyboard during an assessment.
--
-- Users can enable the Accessibility Keyboard in the Settings app (Accessibility > Keyboard > Accessibility Keyboard) to access an on-screen keyboard with alternative input methods. An assessment session disables alternative input methods in the Accessibility Keyboard by default, but you can allow them by setting ``AEAssessmentConfiguration/allowsAccessibilityKeyboard`` to @true@ in the ``AEAssessmentConfiguration`` instance that you use to initialize a session.
--
-- ObjC selector: @- allowsAccessibilityKeyboard@
allowsAccessibilityKeyboard :: IsAEAssessmentConfiguration aeAssessmentConfiguration => aeAssessmentConfiguration -> IO Bool
allowsAccessibilityKeyboard aeAssessmentConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg aeAssessmentConfiguration (mkSelector "allowsAccessibilityKeyboard") retCULong []

-- | A Boolean value that indicates whether to allow alternative input methods in the Accessibility Keyboard during an assessment.
--
-- Users can enable the Accessibility Keyboard in the Settings app (Accessibility > Keyboard > Accessibility Keyboard) to access an on-screen keyboard with alternative input methods. An assessment session disables alternative input methods in the Accessibility Keyboard by default, but you can allow them by setting ``AEAssessmentConfiguration/allowsAccessibilityKeyboard`` to @true@ in the ``AEAssessmentConfiguration`` instance that you use to initialize a session.
--
-- ObjC selector: @- setAllowsAccessibilityKeyboard:@
setAllowsAccessibilityKeyboard :: IsAEAssessmentConfiguration aeAssessmentConfiguration => aeAssessmentConfiguration -> Bool -> IO ()
setAllowsAccessibilityKeyboard aeAssessmentConfiguration  value =
  sendMsg aeAssessmentConfiguration (mkSelector "setAllowsAccessibilityKeyboard:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean value that indicates whether to allow Live Captions during an assessment.
--
-- Users can enable Live Captions in the Settings app (Accessibility > Live Captions) to receive real-time transcription of spoken audio as text on screen. An assessment session disables Live Captions by default, but you can allow it by setting ``AEAssessmentConfiguration/allowsAccessibilityLiveCaptions`` to @true@ in the ``AEAssessmentConfiguration`` instance that you use to initialize a session.
--
-- ObjC selector: @- allowsAccessibilityLiveCaptions@
allowsAccessibilityLiveCaptions :: IsAEAssessmentConfiguration aeAssessmentConfiguration => aeAssessmentConfiguration -> IO Bool
allowsAccessibilityLiveCaptions aeAssessmentConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg aeAssessmentConfiguration (mkSelector "allowsAccessibilityLiveCaptions") retCULong []

-- | A Boolean value that indicates whether to allow Live Captions during an assessment.
--
-- Users can enable Live Captions in the Settings app (Accessibility > Live Captions) to receive real-time transcription of spoken audio as text on screen. An assessment session disables Live Captions by default, but you can allow it by setting ``AEAssessmentConfiguration/allowsAccessibilityLiveCaptions`` to @true@ in the ``AEAssessmentConfiguration`` instance that you use to initialize a session.
--
-- ObjC selector: @- setAllowsAccessibilityLiveCaptions:@
setAllowsAccessibilityLiveCaptions :: IsAEAssessmentConfiguration aeAssessmentConfiguration => aeAssessmentConfiguration -> Bool -> IO ()
setAllowsAccessibilityLiveCaptions aeAssessmentConfiguration  value =
  sendMsg aeAssessmentConfiguration (mkSelector "setAllowsAccessibilityLiveCaptions:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean value that indicates whether to allow the Accessibility Reader during an assessment.
--
-- Users can enable the Accessibility Reader in the Settings app (Accessibility > Read & Speak > Accessibility Reader) to have text content formatted or read aloud. An assessment session disables the Accessibility Reader by default, but you can allow it by setting ``AEAssessmentConfiguration/allowsAccessibilityReader`` to @true@ in the ``AEAssessmentConfiguration`` instance that you use to initialize a session.
--
-- ObjC selector: @- allowsAccessibilityReader@
allowsAccessibilityReader :: IsAEAssessmentConfiguration aeAssessmentConfiguration => aeAssessmentConfiguration -> IO Bool
allowsAccessibilityReader aeAssessmentConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg aeAssessmentConfiguration (mkSelector "allowsAccessibilityReader") retCULong []

-- | A Boolean value that indicates whether to allow the Accessibility Reader during an assessment.
--
-- Users can enable the Accessibility Reader in the Settings app (Accessibility > Read & Speak > Accessibility Reader) to have text content formatted or read aloud. An assessment session disables the Accessibility Reader by default, but you can allow it by setting ``AEAssessmentConfiguration/allowsAccessibilityReader`` to @true@ in the ``AEAssessmentConfiguration`` instance that you use to initialize a session.
--
-- ObjC selector: @- setAllowsAccessibilityReader:@
setAllowsAccessibilityReader :: IsAEAssessmentConfiguration aeAssessmentConfiguration => aeAssessmentConfiguration -> Bool -> IO ()
setAllowsAccessibilityReader aeAssessmentConfiguration  value =
  sendMsg aeAssessmentConfiguration (mkSelector "setAllowsAccessibilityReader:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean value that indicates whether to allow the speech-related accessibility features during an assessment.
--
-- A device reads text aloud for users who need it. In particular, users can enable the following features from Accessibility > Spoken Content in the Settings app on iOS and iPadOS: - Speak Selection - Speak Screen - Typing Feedback > Speak Words
--
-- An assessment session disables these features by default, but you can allow them by setting ``AEAssessmentConfiguration/allowsAccessibilitySpeech`` to @true@ in the ``AEAssessmentConfiguration`` instance that you use to initialize a session.
--
-- ObjC selector: @- allowsAccessibilitySpeech@
allowsAccessibilitySpeech :: IsAEAssessmentConfiguration aeAssessmentConfiguration => aeAssessmentConfiguration -> IO Bool
allowsAccessibilitySpeech aeAssessmentConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg aeAssessmentConfiguration (mkSelector "allowsAccessibilitySpeech") retCULong []

-- | A Boolean value that indicates whether to allow the speech-related accessibility features during an assessment.
--
-- A device reads text aloud for users who need it. In particular, users can enable the following features from Accessibility > Spoken Content in the Settings app on iOS and iPadOS: - Speak Selection - Speak Screen - Typing Feedback > Speak Words
--
-- An assessment session disables these features by default, but you can allow them by setting ``AEAssessmentConfiguration/allowsAccessibilitySpeech`` to @true@ in the ``AEAssessmentConfiguration`` instance that you use to initialize a session.
--
-- ObjC selector: @- setAllowsAccessibilitySpeech:@
setAllowsAccessibilitySpeech :: IsAEAssessmentConfiguration aeAssessmentConfiguration => aeAssessmentConfiguration -> Bool -> IO ()
setAllowsAccessibilitySpeech aeAssessmentConfiguration  value =
  sendMsg aeAssessmentConfiguration (mkSelector "setAllowsAccessibilitySpeech:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean value that indicates whether to allow accessibility typing feedback during an assessment.
--
-- Users can enable typing feedback features in the Settings app (Accessibility > Keyboards & Typing > Typing Feedback)  to receive audio feedback when typing. An assessment session disables these accessibility typing feedback features by default, but you can allow them by setting ``AEAssessmentConfiguration/allowsAccessibilityTypingFeedback`` to @true@ in the ``AEAssessmentConfiguration`` instance that you use to initialize a session.
--
-- ObjC selector: @- allowsAccessibilityTypingFeedback@
allowsAccessibilityTypingFeedback :: IsAEAssessmentConfiguration aeAssessmentConfiguration => aeAssessmentConfiguration -> IO Bool
allowsAccessibilityTypingFeedback aeAssessmentConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg aeAssessmentConfiguration (mkSelector "allowsAccessibilityTypingFeedback") retCULong []

-- | A Boolean value that indicates whether to allow accessibility typing feedback during an assessment.
--
-- Users can enable typing feedback features in the Settings app (Accessibility > Keyboards & Typing > Typing Feedback)  to receive audio feedback when typing. An assessment session disables these accessibility typing feedback features by default, but you can allow them by setting ``AEAssessmentConfiguration/allowsAccessibilityTypingFeedback`` to @true@ in the ``AEAssessmentConfiguration`` instance that you use to initialize a session.
--
-- ObjC selector: @- setAllowsAccessibilityTypingFeedback:@
setAllowsAccessibilityTypingFeedback :: IsAEAssessmentConfiguration aeAssessmentConfiguration => aeAssessmentConfiguration -> Bool -> IO ()
setAllowsAccessibilityTypingFeedback aeAssessmentConfiguration  value =
  sendMsg aeAssessmentConfiguration (mkSelector "setAllowsAccessibilityTypingFeedback:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean value that indicates whether to allow password autofill during an assessment.
--
-- Users can store passwords for use with Password Autofill by turning on the feature in the Settings app (General > Passwords > AutoFill Passwords). An assessment session disables Password Autofill by default, but you can allow it by setting ``AEAssessmentConfiguration/allowsPasswordAutoFill`` to @true@ in the ``AEAssessmentConfiguration`` instance that you use to initialize a session.
--
-- ObjC selector: @- allowsPasswordAutoFill@
allowsPasswordAutoFill :: IsAEAssessmentConfiguration aeAssessmentConfiguration => aeAssessmentConfiguration -> IO Bool
allowsPasswordAutoFill aeAssessmentConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg aeAssessmentConfiguration (mkSelector "allowsPasswordAutoFill") retCULong []

-- | A Boolean value that indicates whether to allow password autofill during an assessment.
--
-- Users can store passwords for use with Password Autofill by turning on the feature in the Settings app (General > Passwords > AutoFill Passwords). An assessment session disables Password Autofill by default, but you can allow it by setting ``AEAssessmentConfiguration/allowsPasswordAutoFill`` to @true@ in the ``AEAssessmentConfiguration`` instance that you use to initialize a session.
--
-- ObjC selector: @- setAllowsPasswordAutoFill:@
setAllowsPasswordAutoFill :: IsAEAssessmentConfiguration aeAssessmentConfiguration => aeAssessmentConfiguration -> Bool -> IO ()
setAllowsPasswordAutoFill aeAssessmentConfiguration  value =
  sendMsg aeAssessmentConfiguration (mkSelector "setAllowsPasswordAutoFill:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean value that indicates whether to allow Slide to Type to operate during an assessment.
--
-- Users can turn on Slide to Type in the Settings app (General > Keyboard). An assessment session disables this feature by default, but you can allow it by setting ``AEAssessmentConfiguration/allowsContinuousPathKeyboard`` to @true@ in the ``AEAssessmentConfiguration`` instance that you use to initialize a session.
--
-- ObjC selector: @- allowsContinuousPathKeyboard@
allowsContinuousPathKeyboard :: IsAEAssessmentConfiguration aeAssessmentConfiguration => aeAssessmentConfiguration -> IO Bool
allowsContinuousPathKeyboard aeAssessmentConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg aeAssessmentConfiguration (mkSelector "allowsContinuousPathKeyboard") retCULong []

-- | A Boolean value that indicates whether to allow Slide to Type to operate during an assessment.
--
-- Users can turn on Slide to Type in the Settings app (General > Keyboard). An assessment session disables this feature by default, but you can allow it by setting ``AEAssessmentConfiguration/allowsContinuousPathKeyboard`` to @true@ in the ``AEAssessmentConfiguration`` instance that you use to initialize a session.
--
-- ObjC selector: @- setAllowsContinuousPathKeyboard:@
setAllowsContinuousPathKeyboard :: IsAEAssessmentConfiguration aeAssessmentConfiguration => aeAssessmentConfiguration -> Bool -> IO ()
setAllowsContinuousPathKeyboard aeAssessmentConfiguration  value =
  sendMsg aeAssessmentConfiguration (mkSelector "setAllowsContinuousPathKeyboard:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean value that indicates whether to allow screenshots copied to the clipboard during an assessment.
--
-- An assessment session disables the ability to take screenshots by default to maintain assessment integrity. This property specifically applies to screenshots that are copied to the clipboard, typically those taken using the Command+Control+Shift+3 and Command+Control+Shift+4 keyboard shortcuts. You can allow clipboard screenshots by setting @allowsScreenshots@ to @true@.
--
-- - Note: The clipboard is cleared before the assessment session ends to prevent exporting captured content.
--
-- ObjC selector: @- allowsScreenshots@
allowsScreenshots :: IsAEAssessmentConfiguration aeAssessmentConfiguration => aeAssessmentConfiguration -> IO Bool
allowsScreenshots aeAssessmentConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg aeAssessmentConfiguration (mkSelector "allowsScreenshots") retCULong []

-- | A Boolean value that indicates whether to allow screenshots copied to the clipboard during an assessment.
--
-- An assessment session disables the ability to take screenshots by default to maintain assessment integrity. This property specifically applies to screenshots that are copied to the clipboard, typically those taken using the Command+Control+Shift+3 and Command+Control+Shift+4 keyboard shortcuts. You can allow clipboard screenshots by setting @allowsScreenshots@ to @true@.
--
-- - Note: The clipboard is cleared before the assessment session ends to prevent exporting captured content.
--
-- ObjC selector: @- setAllowsScreenshots:@
setAllowsScreenshots :: IsAEAssessmentConfiguration aeAssessmentConfiguration => aeAssessmentConfiguration -> Bool -> IO ()
setAllowsScreenshots aeAssessmentConfiguration  value =
  sendMsg aeAssessmentConfiguration (mkSelector "setAllowsScreenshots:") retVoid [argCULong (if value then 1 else 0)]

-- | The app-specific configuration for the app that invokes the assessment.
--
-- Use this property to get and customize the app-specific configuration that's applied to your own app. For example, you can set the @allowsNetworkAccess@ property for your own app:
--
-- ```swift let config = AEAssessmentConfiguration() config.mainParticipantConfiguration.allowsNetworkAccess = false ```
--
-- ObjC selector: @- mainParticipantConfiguration@
mainParticipantConfiguration :: IsAEAssessmentConfiguration aeAssessmentConfiguration => aeAssessmentConfiguration -> IO (Id AEAssessmentParticipantConfiguration)
mainParticipantConfiguration aeAssessmentConfiguration  =
  sendMsg aeAssessmentConfiguration (mkSelector "mainParticipantConfiguration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The collection of apps available during an assessment, along with their associated configurations.
--
-- Access this property to get a list of the currently allowed secondary apps and their individual configurations. Add apps to the list by calling the ``AEAssessmentConfiguration/setConfiguration(_:for:)`` method. Remove them from the list by calling the ``AEAssessmentConfiguration/remove(_:)`` method.
--
-- ObjC selector: @- configurationsByApplication@
configurationsByApplication :: IsAEAssessmentConfiguration aeAssessmentConfiguration => aeAssessmentConfiguration -> IO (Id NSDictionary)
configurationsByApplication aeAssessmentConfiguration  =
  sendMsg aeAssessmentConfiguration (mkSelector "configurationsByApplication") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setConfiguration:forApplication:@
setConfiguration_forApplicationSelector :: Selector
setConfiguration_forApplicationSelector = mkSelector "setConfiguration:forApplication:"

-- | @Selector@ for @removeApplication:@
removeApplicationSelector :: Selector
removeApplicationSelector = mkSelector "removeApplication:"

-- | @Selector@ for @autocorrectMode@
autocorrectModeSelector :: Selector
autocorrectModeSelector = mkSelector "autocorrectMode"

-- | @Selector@ for @setAutocorrectMode:@
setAutocorrectModeSelector :: Selector
setAutocorrectModeSelector = mkSelector "setAutocorrectMode:"

-- | @Selector@ for @allowsSpellCheck@
allowsSpellCheckSelector :: Selector
allowsSpellCheckSelector = mkSelector "allowsSpellCheck"

-- | @Selector@ for @setAllowsSpellCheck:@
setAllowsSpellCheckSelector :: Selector
setAllowsSpellCheckSelector = mkSelector "setAllowsSpellCheck:"

-- | @Selector@ for @allowsPredictiveKeyboard@
allowsPredictiveKeyboardSelector :: Selector
allowsPredictiveKeyboardSelector = mkSelector "allowsPredictiveKeyboard"

-- | @Selector@ for @setAllowsPredictiveKeyboard:@
setAllowsPredictiveKeyboardSelector :: Selector
setAllowsPredictiveKeyboardSelector = mkSelector "setAllowsPredictiveKeyboard:"

-- | @Selector@ for @allowsKeyboardShortcuts@
allowsKeyboardShortcutsSelector :: Selector
allowsKeyboardShortcutsSelector = mkSelector "allowsKeyboardShortcuts"

-- | @Selector@ for @setAllowsKeyboardShortcuts:@
setAllowsKeyboardShortcutsSelector :: Selector
setAllowsKeyboardShortcutsSelector = mkSelector "setAllowsKeyboardShortcuts:"

-- | @Selector@ for @allowsActivityContinuation@
allowsActivityContinuationSelector :: Selector
allowsActivityContinuationSelector = mkSelector "allowsActivityContinuation"

-- | @Selector@ for @setAllowsActivityContinuation:@
setAllowsActivityContinuationSelector :: Selector
setAllowsActivityContinuationSelector = mkSelector "setAllowsActivityContinuation:"

-- | @Selector@ for @allowsDictation@
allowsDictationSelector :: Selector
allowsDictationSelector = mkSelector "allowsDictation"

-- | @Selector@ for @setAllowsDictation:@
setAllowsDictationSelector :: Selector
setAllowsDictationSelector = mkSelector "setAllowsDictation:"

-- | @Selector@ for @allowsAccessibilityKeyboard@
allowsAccessibilityKeyboardSelector :: Selector
allowsAccessibilityKeyboardSelector = mkSelector "allowsAccessibilityKeyboard"

-- | @Selector@ for @setAllowsAccessibilityKeyboard:@
setAllowsAccessibilityKeyboardSelector :: Selector
setAllowsAccessibilityKeyboardSelector = mkSelector "setAllowsAccessibilityKeyboard:"

-- | @Selector@ for @allowsAccessibilityLiveCaptions@
allowsAccessibilityLiveCaptionsSelector :: Selector
allowsAccessibilityLiveCaptionsSelector = mkSelector "allowsAccessibilityLiveCaptions"

-- | @Selector@ for @setAllowsAccessibilityLiveCaptions:@
setAllowsAccessibilityLiveCaptionsSelector :: Selector
setAllowsAccessibilityLiveCaptionsSelector = mkSelector "setAllowsAccessibilityLiveCaptions:"

-- | @Selector@ for @allowsAccessibilityReader@
allowsAccessibilityReaderSelector :: Selector
allowsAccessibilityReaderSelector = mkSelector "allowsAccessibilityReader"

-- | @Selector@ for @setAllowsAccessibilityReader:@
setAllowsAccessibilityReaderSelector :: Selector
setAllowsAccessibilityReaderSelector = mkSelector "setAllowsAccessibilityReader:"

-- | @Selector@ for @allowsAccessibilitySpeech@
allowsAccessibilitySpeechSelector :: Selector
allowsAccessibilitySpeechSelector = mkSelector "allowsAccessibilitySpeech"

-- | @Selector@ for @setAllowsAccessibilitySpeech:@
setAllowsAccessibilitySpeechSelector :: Selector
setAllowsAccessibilitySpeechSelector = mkSelector "setAllowsAccessibilitySpeech:"

-- | @Selector@ for @allowsAccessibilityTypingFeedback@
allowsAccessibilityTypingFeedbackSelector :: Selector
allowsAccessibilityTypingFeedbackSelector = mkSelector "allowsAccessibilityTypingFeedback"

-- | @Selector@ for @setAllowsAccessibilityTypingFeedback:@
setAllowsAccessibilityTypingFeedbackSelector :: Selector
setAllowsAccessibilityTypingFeedbackSelector = mkSelector "setAllowsAccessibilityTypingFeedback:"

-- | @Selector@ for @allowsPasswordAutoFill@
allowsPasswordAutoFillSelector :: Selector
allowsPasswordAutoFillSelector = mkSelector "allowsPasswordAutoFill"

-- | @Selector@ for @setAllowsPasswordAutoFill:@
setAllowsPasswordAutoFillSelector :: Selector
setAllowsPasswordAutoFillSelector = mkSelector "setAllowsPasswordAutoFill:"

-- | @Selector@ for @allowsContinuousPathKeyboard@
allowsContinuousPathKeyboardSelector :: Selector
allowsContinuousPathKeyboardSelector = mkSelector "allowsContinuousPathKeyboard"

-- | @Selector@ for @setAllowsContinuousPathKeyboard:@
setAllowsContinuousPathKeyboardSelector :: Selector
setAllowsContinuousPathKeyboardSelector = mkSelector "setAllowsContinuousPathKeyboard:"

-- | @Selector@ for @allowsScreenshots@
allowsScreenshotsSelector :: Selector
allowsScreenshotsSelector = mkSelector "allowsScreenshots"

-- | @Selector@ for @setAllowsScreenshots:@
setAllowsScreenshotsSelector :: Selector
setAllowsScreenshotsSelector = mkSelector "setAllowsScreenshots:"

-- | @Selector@ for @mainParticipantConfiguration@
mainParticipantConfigurationSelector :: Selector
mainParticipantConfigurationSelector = mkSelector "mainParticipantConfiguration"

-- | @Selector@ for @configurationsByApplication@
configurationsByApplicationSelector :: Selector
configurationsByApplicationSelector = mkSelector "configurationsByApplication"

