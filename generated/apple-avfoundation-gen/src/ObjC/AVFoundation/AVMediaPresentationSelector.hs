{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | For content that has been authored with the express intent of offering an alternative selection interface for AVMediaSelectionOptions, AVMediaPresentationSelector represents a collection of mutually exclusive settings.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVMediaPresentationSelector@.
module ObjC.AVFoundation.AVMediaPresentationSelector
  ( AVMediaPresentationSelector
  , IsAVMediaPresentationSelector(..)
  , displayNameForLocaleIdentifier
  , identifier
  , settings
  , displayNameForLocaleIdentifierSelector
  , identifierSelector
  , settingsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Returns the display name for the selector that best matches the specified locale identifier.
--
-- ObjC selector: @- displayNameForLocaleIdentifier:@
displayNameForLocaleIdentifier :: (IsAVMediaPresentationSelector avMediaPresentationSelector, IsNSString localeIdentifier) => avMediaPresentationSelector -> localeIdentifier -> IO (Id NSString)
displayNameForLocaleIdentifier avMediaPresentationSelector localeIdentifier =
  sendMessage avMediaPresentationSelector displayNameForLocaleIdentifierSelector (toNSString localeIdentifier)

-- | Provides the authored identifier for the selector.
--
-- ObjC selector: @- identifier@
identifier :: IsAVMediaPresentationSelector avMediaPresentationSelector => avMediaPresentationSelector -> IO (Id NSString)
identifier avMediaPresentationSelector =
  sendMessage avMediaPresentationSelector identifierSelector

-- | Provides selectable mutually exclusive settings for the selector.
--
-- ObjC selector: @- settings@
settings :: IsAVMediaPresentationSelector avMediaPresentationSelector => avMediaPresentationSelector -> IO (Id NSArray)
settings avMediaPresentationSelector =
  sendMessage avMediaPresentationSelector settingsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @displayNameForLocaleIdentifier:@
displayNameForLocaleIdentifierSelector :: Selector '[Id NSString] (Id NSString)
displayNameForLocaleIdentifierSelector = mkSelector "displayNameForLocaleIdentifier:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @settings@
settingsSelector :: Selector '[] (Id NSArray)
settingsSelector = mkSelector "settings"

