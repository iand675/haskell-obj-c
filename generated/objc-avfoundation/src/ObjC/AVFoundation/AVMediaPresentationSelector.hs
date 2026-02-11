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

-- | Returns the display name for the selector that best matches the specified locale identifier.
--
-- ObjC selector: @- displayNameForLocaleIdentifier:@
displayNameForLocaleIdentifier :: (IsAVMediaPresentationSelector avMediaPresentationSelector, IsNSString localeIdentifier) => avMediaPresentationSelector -> localeIdentifier -> IO (Id NSString)
displayNameForLocaleIdentifier avMediaPresentationSelector  localeIdentifier =
withObjCPtr localeIdentifier $ \raw_localeIdentifier ->
    sendMsg avMediaPresentationSelector (mkSelector "displayNameForLocaleIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_localeIdentifier :: Ptr ())] >>= retainedObject . castPtr

-- | Provides the authored identifier for the selector.
--
-- ObjC selector: @- identifier@
identifier :: IsAVMediaPresentationSelector avMediaPresentationSelector => avMediaPresentationSelector -> IO (Id NSString)
identifier avMediaPresentationSelector  =
  sendMsg avMediaPresentationSelector (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Provides selectable mutually exclusive settings for the selector.
--
-- ObjC selector: @- settings@
settings :: IsAVMediaPresentationSelector avMediaPresentationSelector => avMediaPresentationSelector -> IO (Id NSArray)
settings avMediaPresentationSelector  =
  sendMsg avMediaPresentationSelector (mkSelector "settings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @displayNameForLocaleIdentifier:@
displayNameForLocaleIdentifierSelector :: Selector
displayNameForLocaleIdentifierSelector = mkSelector "displayNameForLocaleIdentifier:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @settings@
settingsSelector :: Selector
settingsSelector = mkSelector "settings"

