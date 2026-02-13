{-# LANGUAGE DataKinds #-}
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
  , initWithURL_optionsSelector
  , newSelector
  , urlSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVMediaDataStorage avMediaDataStorage => avMediaDataStorage -> IO (Id AVMediaDataStorage)
init_ avMediaDataStorage =
  sendOwnedMessage avMediaDataStorage initSelector

-- | @+ new@
new :: IO (Id AVMediaDataStorage)
new  =
  do
    cls' <- getRequiredClass "AVMediaDataStorage"
    sendOwnedClassMessage cls' newSelector

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
initWithURL_options avMediaDataStorage url options =
  sendOwnedMessage avMediaDataStorage initWithURL_optionsSelector (toNSURL url) (toNSDictionary options)

-- | URL
--
-- The URL from which the receiver was initialized; may be nil.
--
-- ObjC selector: @- URL@
url :: IsAVMediaDataStorage avMediaDataStorage => avMediaDataStorage -> IO (Id NSURL)
url avMediaDataStorage =
  sendMessage avMediaDataStorage urlSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVMediaDataStorage)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVMediaDataStorage)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithURL:options:@
initWithURL_optionsSelector :: Selector '[Id NSURL, Id NSDictionary] (Id AVMediaDataStorage)
initWithURL_optionsSelector = mkSelector "initWithURL:options:"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

