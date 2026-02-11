{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.AutomaticAssessmentConfiguration.Internal.Classes (
    module ObjC.AutomaticAssessmentConfiguration.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- AEAssessmentApplication ----------

-- | Phantom type for @AEAssessmentApplication@.
data AEAssessmentApplication

instance IsObjCObject (Id AEAssessmentApplication) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AEAssessmentApplication"

class IsNSObject a => IsAEAssessmentApplication a where
  toAEAssessmentApplication :: a -> Id AEAssessmentApplication

instance IsAEAssessmentApplication (Id AEAssessmentApplication) where
  toAEAssessmentApplication = unsafeCastId

instance IsNSObject (Id AEAssessmentApplication) where
  toNSObject = unsafeCastId

-- ---------- AEAssessmentConfiguration ----------

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
-- Phantom type for @AEAssessmentConfiguration@.
data AEAssessmentConfiguration

instance IsObjCObject (Id AEAssessmentConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AEAssessmentConfiguration"

class IsNSObject a => IsAEAssessmentConfiguration a where
  toAEAssessmentConfiguration :: a -> Id AEAssessmentConfiguration

instance IsAEAssessmentConfiguration (Id AEAssessmentConfiguration) where
  toAEAssessmentConfiguration = unsafeCastId

instance IsNSObject (Id AEAssessmentConfiguration) where
  toNSObject = unsafeCastId

-- ---------- AEAssessmentParticipantConfiguration ----------

-- | Phantom type for @AEAssessmentParticipantConfiguration@.
data AEAssessmentParticipantConfiguration

instance IsObjCObject (Id AEAssessmentParticipantConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AEAssessmentParticipantConfiguration"

class IsNSObject a => IsAEAssessmentParticipantConfiguration a where
  toAEAssessmentParticipantConfiguration :: a -> Id AEAssessmentParticipantConfiguration

instance IsAEAssessmentParticipantConfiguration (Id AEAssessmentParticipantConfiguration) where
  toAEAssessmentParticipantConfiguration = unsafeCastId

instance IsNSObject (Id AEAssessmentParticipantConfiguration) where
  toNSObject = unsafeCastId

-- ---------- AEAssessmentSession ----------

-- | Phantom type for @AEAssessmentSession@.
data AEAssessmentSession

instance IsObjCObject (Id AEAssessmentSession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "AEAssessmentSession"

class IsNSObject a => IsAEAssessmentSession a where
  toAEAssessmentSession :: a -> Id AEAssessmentSession

instance IsAEAssessmentSession (Id AEAssessmentSession) where
  toAEAssessmentSession = unsafeCastId

instance IsNSObject (Id AEAssessmentSession) where
  toNSObject = unsafeCastId
