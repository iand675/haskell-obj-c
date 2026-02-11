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
  , initForKeySystem_identifier_optionsSelector
  , keySystemSelector
  , identifierSelector
  , optionsSelector


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
    withObjCPtr keySystem $ \raw_keySystem ->
      withObjCPtr options $ \raw_options ->
        sendClassMsg cls' (mkSelector "contentKeySpecifierForKeySystem:identifier:options:") (retPtr retVoid) [argPtr (castPtr raw_keySystem :: Ptr ()), argPtr (castPtr (unRawId contentKeyIdentifier) :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

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
initForKeySystem_identifier_options avContentKeySpecifier  keySystem contentKeyIdentifier options =
withObjCPtr keySystem $ \raw_keySystem ->
  withObjCPtr options $ \raw_options ->
      sendMsg avContentKeySpecifier (mkSelector "initForKeySystem:identifier:options:") (retPtr retVoid) [argPtr (castPtr raw_keySystem :: Ptr ()), argPtr (castPtr (unRawId contentKeyIdentifier) :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

-- | A valid key system for content keys.
--
-- ObjC selector: @- keySystem@
keySystem :: IsAVContentKeySpecifier avContentKeySpecifier => avContentKeySpecifier -> IO (Id NSString)
keySystem avContentKeySpecifier  =
  sendMsg avContentKeySpecifier (mkSelector "keySystem") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Container and protocol-specific key identifier.
--
-- ObjC selector: @- identifier@
identifier :: IsAVContentKeySpecifier avContentKeySpecifier => avContentKeySpecifier -> IO RawId
identifier avContentKeySpecifier  =
  fmap (RawId . castPtr) $ sendMsg avContentKeySpecifier (mkSelector "identifier") (retPtr retVoid) []

-- | Additional information necessary to obtain the key, can be empty if none needed.
--
-- ObjC selector: @- options@
options :: IsAVContentKeySpecifier avContentKeySpecifier => avContentKeySpecifier -> IO (Id NSDictionary)
options avContentKeySpecifier  =
  sendMsg avContentKeySpecifier (mkSelector "options") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contentKeySpecifierForKeySystem:identifier:options:@
contentKeySpecifierForKeySystem_identifier_optionsSelector :: Selector
contentKeySpecifierForKeySystem_identifier_optionsSelector = mkSelector "contentKeySpecifierForKeySystem:identifier:options:"

-- | @Selector@ for @initForKeySystem:identifier:options:@
initForKeySystem_identifier_optionsSelector :: Selector
initForKeySystem_identifier_optionsSelector = mkSelector "initForKeySystem:identifier:options:"

-- | @Selector@ for @keySystem@
keySystemSelector :: Selector
keySystemSelector = mkSelector "keySystem"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @options@
optionsSelector :: Selector
optionsSelector = mkSelector "options"

