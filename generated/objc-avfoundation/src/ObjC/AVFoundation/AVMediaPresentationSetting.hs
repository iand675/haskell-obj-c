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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Returns the display name for the selectable setting that best matches the specified locale identifier.
--
-- ObjC selector: @- displayNameForLocaleIdentifier:@
displayNameForLocaleIdentifier :: (IsAVMediaPresentationSetting avMediaPresentationSetting, IsNSString localeIdentifier) => avMediaPresentationSetting -> localeIdentifier -> IO (Id NSString)
displayNameForLocaleIdentifier avMediaPresentationSetting  localeIdentifier =
withObjCPtr localeIdentifier $ \raw_localeIdentifier ->
    sendMsg avMediaPresentationSetting (mkSelector "displayNameForLocaleIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_localeIdentifier :: Ptr ())] >>= retainedObject . castPtr

-- | Provides the media characteristic that corresponds to the selectable setting.
--
-- ObjC selector: @- mediaCharacteristic@
mediaCharacteristic :: IsAVMediaPresentationSetting avMediaPresentationSetting => avMediaPresentationSetting -> IO (Id NSString)
mediaCharacteristic avMediaPresentationSetting  =
  sendMsg avMediaPresentationSetting (mkSelector "mediaCharacteristic") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @displayNameForLocaleIdentifier:@
displayNameForLocaleIdentifierSelector :: Selector
displayNameForLocaleIdentifierSelector = mkSelector "displayNameForLocaleIdentifier:"

-- | @Selector@ for @mediaCharacteristic@
mediaCharacteristicSelector :: Selector
mediaCharacteristicSelector = mkSelector "mediaCharacteristic"

