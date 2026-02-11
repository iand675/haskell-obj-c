{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVMediaDataStorage
--
-- Media sample data storage file.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVMediaDataStorage@.
module ObjC.AVFoundation.AVMediaDataStorage
  ( AVMediaDataStorage
  , IsAVMediaDataStorage(..)
  , init_
  , new
  , initWithURL_options
  , url
  , initSelector
  , newSelector
  , initWithURL_optionsSelector
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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVMediaDataStorage avMediaDataStorage => avMediaDataStorage -> IO (Id AVMediaDataStorage)
init_ avMediaDataStorage  =
  sendMsg avMediaDataStorage (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVMediaDataStorage)
new  =
  do
    cls' <- getRequiredClass "AVMediaDataStorage"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | initWithURL:options:
--
-- Creates an AVMediaDataStorage object associated with a file URL.
--
-- @URL@ — An NSURL object that specifies a file where sample data that is added to a movie or track should be written.
--
-- @options@ — An NSDictionary object that contains keys for specifying options for the initialization of the AVMediaDataStorage object. Currently no keys are defined.
--
-- Returns: An AVMediaDataStorage object
--
-- ObjC selector: @- initWithURL:options:@
initWithURL_options :: (IsAVMediaDataStorage avMediaDataStorage, IsNSURL url, IsNSDictionary options) => avMediaDataStorage -> url -> options -> IO (Id AVMediaDataStorage)
initWithURL_options avMediaDataStorage  url options =
withObjCPtr url $ \raw_url ->
  withObjCPtr options $ \raw_options ->
      sendMsg avMediaDataStorage (mkSelector "initWithURL:options:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

-- | URL
--
-- The URL from which the receiver was initialized; may be nil.
--
-- ObjC selector: @- URL@
url :: IsAVMediaDataStorage avMediaDataStorage => avMediaDataStorage -> IO (Id NSURL)
url avMediaDataStorage  =
  sendMsg avMediaDataStorage (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithURL:options:@
initWithURL_optionsSelector :: Selector
initWithURL_optionsSelector = mkSelector "initWithURL:options:"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

