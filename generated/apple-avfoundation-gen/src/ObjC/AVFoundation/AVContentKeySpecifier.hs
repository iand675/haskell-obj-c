{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVContentKeySpecifier@.
module ObjC.AVFoundation.AVContentKeySpecifier
  ( AVContentKeySpecifier
  , IsAVContentKeySpecifier(..)
  , contentKeySpecifierForKeySystem_identifier_options
  , initForKeySystem_identifier_options
  , keySystem
  , identifier
  , options
  , contentKeySpecifierForKeySystem_identifier_optionsSelector
  , identifierSelector
  , initForKeySystem_identifier_optionsSelector
  , keySystemSelector
  , optionsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a new instance of AVContentKeySpecifier.
--
-- This method returns an AVContentKeySpecifier instance that represents a content key in a specific content key system.
--
-- - Parameter keySystem: A valid key system for content keys. - Parameter contentKeyIdentifier: Container and protocol-specific key identifier. - Parameter options: Additional information necessary to obtain the key, can be empty if none needed.
--
-- - Returns: A new AVContentKeySpecifier
--
-- ObjC selector: @+ contentKeySpecifierForKeySystem:identifier:options:@
contentKeySpecifierForKeySystem_identifier_options :: (IsNSString keySystem, IsNSDictionary options) => keySystem -> RawId -> options -> IO (Id AVContentKeySpecifier)
contentKeySpecifierForKeySystem_identifier_options keySystem contentKeyIdentifier options =
  do
    cls' <- getRequiredClass "AVContentKeySpecifier"
    sendClassMessage cls' contentKeySpecifierForKeySystem_identifier_optionsSelector (toNSString keySystem) contentKeyIdentifier (toNSDictionary options)

-- | Initialize an instance of AVContentKeySpecifier.
--
-- This method returns an AVContentKeySpecifier instance that represents a content key in a specific content key system.
--
-- - Parameter keySystem: A valid key system for content keys. - Parameter contentKeyIdentifier: Container and protocol-specific key identifier. - Parameter options: Additional information necessary to obtain the key, can be empty if none needed.
--
-- - Returns: An instance of AVContentKeySpecifier
--
-- ObjC selector: @- initForKeySystem:identifier:options:@
initForKeySystem_identifier_options :: (IsAVContentKeySpecifier avContentKeySpecifier, IsNSString keySystem, IsNSDictionary options) => avContentKeySpecifier -> keySystem -> RawId -> options -> IO (Id AVContentKeySpecifier)
initForKeySystem_identifier_options avContentKeySpecifier keySystem contentKeyIdentifier options =
  sendOwnedMessage avContentKeySpecifier initForKeySystem_identifier_optionsSelector (toNSString keySystem) contentKeyIdentifier (toNSDictionary options)

-- | A valid key system for content keys.
--
-- ObjC selector: @- keySystem@
keySystem :: IsAVContentKeySpecifier avContentKeySpecifier => avContentKeySpecifier -> IO (Id NSString)
keySystem avContentKeySpecifier =
  sendMessage avContentKeySpecifier keySystemSelector

-- | Container and protocol-specific key identifier.
--
-- ObjC selector: @- identifier@
identifier :: IsAVContentKeySpecifier avContentKeySpecifier => avContentKeySpecifier -> IO RawId
identifier avContentKeySpecifier =
  sendMessage avContentKeySpecifier identifierSelector

-- | Additional information necessary to obtain the key, can be empty if none needed.
--
-- ObjC selector: @- options@
options :: IsAVContentKeySpecifier avContentKeySpecifier => avContentKeySpecifier -> IO (Id NSDictionary)
options avContentKeySpecifier =
  sendMessage avContentKeySpecifier optionsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contentKeySpecifierForKeySystem:identifier:options:@
contentKeySpecifierForKeySystem_identifier_optionsSelector :: Selector '[Id NSString, RawId, Id NSDictionary] (Id AVContentKeySpecifier)
contentKeySpecifierForKeySystem_identifier_optionsSelector = mkSelector "contentKeySpecifierForKeySystem:identifier:options:"

-- | @Selector@ for @initForKeySystem:identifier:options:@
initForKeySystem_identifier_optionsSelector :: Selector '[Id NSString, RawId, Id NSDictionary] (Id AVContentKeySpecifier)
initForKeySystem_identifier_optionsSelector = mkSelector "initForKeySystem:identifier:options:"

-- | @Selector@ for @keySystem@
keySystemSelector :: Selector '[] (Id NSString)
keySystemSelector = mkSelector "keySystem"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] RawId
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @options@
optionsSelector :: Selector '[] (Id NSDictionary)
optionsSelector = mkSelector "options"

