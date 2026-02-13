{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | For content that has been authored with the express intent of offering an alternative selection interface for AVMediaSelectionOptions, AVMediaPresentationSetting represents a selectable setting for controlling the presentation of the media.
--
-- Each selectable setting is associated with a media characteristic that one or more of the AVMediaSelectionOptions in the AVMediaSelectionGroup possesses. By selecting a setting in a user interface that offers AVMediaPresentationSettings, users are essentially indicating a preference for the media characteristic of the selected setting. Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVMediaPresentationSetting@.
module ObjC.AVFoundation.AVMediaPresentationSetting
  ( AVMediaPresentationSetting
  , IsAVMediaPresentationSetting(..)
  , displayNameForLocaleIdentifier
  , mediaCharacteristic
  , displayNameForLocaleIdentifierSelector
  , mediaCharacteristicSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Returns the display name for the selectable setting that best matches the specified locale identifier.
--
-- ObjC selector: @- displayNameForLocaleIdentifier:@
displayNameForLocaleIdentifier :: (IsAVMediaPresentationSetting avMediaPresentationSetting, IsNSString localeIdentifier) => avMediaPresentationSetting -> localeIdentifier -> IO (Id NSString)
displayNameForLocaleIdentifier avMediaPresentationSetting localeIdentifier =
  sendMessage avMediaPresentationSetting displayNameForLocaleIdentifierSelector (toNSString localeIdentifier)

-- | Provides the media characteristic that corresponds to the selectable setting.
--
-- ObjC selector: @- mediaCharacteristic@
mediaCharacteristic :: IsAVMediaPresentationSetting avMediaPresentationSetting => avMediaPresentationSetting -> IO (Id NSString)
mediaCharacteristic avMediaPresentationSetting =
  sendMessage avMediaPresentationSetting mediaCharacteristicSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @displayNameForLocaleIdentifier:@
displayNameForLocaleIdentifierSelector :: Selector '[Id NSString] (Id NSString)
displayNameForLocaleIdentifierSelector = mkSelector "displayNameForLocaleIdentifier:"

-- | @Selector@ for @mediaCharacteristic@
mediaCharacteristicSelector :: Selector '[] (Id NSString)
mediaCharacteristicSelector = mkSelector "mediaCharacteristic"

