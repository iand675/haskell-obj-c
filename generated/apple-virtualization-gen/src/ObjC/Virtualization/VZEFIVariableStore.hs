{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | EFI variable store
--
-- The EFI variable store contains NVRAM variables exposed by the EFI ROM.
--
-- VZEFIBootLoader
--
-- Generated bindings for @VZEFIVariableStore@.
module ObjC.Virtualization.VZEFIVariableStore
  ( VZEFIVariableStore
  , IsVZEFIVariableStore(..)
  , new
  , init_
  , initWithURL
  , initCreatingVariableStoreAtURL_options_error
  , url
  , initCreatingVariableStoreAtURL_options_errorSelector
  , initSelector
  , initWithURLSelector
  , newSelector
  , urlSelector

  -- * Enum types
  , VZEFIVariableStoreInitializationOptions(VZEFIVariableStoreInitializationOptions)
  , pattern VZEFIVariableStoreInitializationOptionAllowOverwrite

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Virtualization.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id VZEFIVariableStore)
new  =
  do
    cls' <- getRequiredClass "VZEFIVariableStore"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZEFIVariableStore vzefiVariableStore => vzefiVariableStore -> IO (Id VZEFIVariableStore)
init_ vzefiVariableStore =
  sendOwnedMessage vzefiVariableStore initSelector

-- | Initialize the variable store from the URL of an existing file.
--
-- @URL@ — The URL of the variable store on the local file system.
--
-- To create a new variable store, use -[VZEFIVariableStore initCreatingVariableStoreAtURL:options:error].
--
-- ObjC selector: @- initWithURL:@
initWithURL :: (IsVZEFIVariableStore vzefiVariableStore, IsNSURL url) => vzefiVariableStore -> url -> IO (Id VZEFIVariableStore)
initWithURL vzefiVariableStore url =
  sendOwnedMessage vzefiVariableStore initWithURLSelector (toNSURL url)

-- | Write an initialized VZEFIVariableStore to a URL on a file system.
--
-- @URL@ — The URL to write the variable store to on the local file system.
--
-- @options@ — Initialization options.
--
-- @error@ — If not nil, used to report errors if creation fails.
--
-- Returns: A newly initialized VZEFIVariableStore on success. If an error was encountered returns @nil,@ and @error@ contains the error.
--
-- ObjC selector: @- initCreatingVariableStoreAtURL:options:error:@
initCreatingVariableStoreAtURL_options_error :: (IsVZEFIVariableStore vzefiVariableStore, IsNSURL url, IsNSError error_) => vzefiVariableStore -> url -> VZEFIVariableStoreInitializationOptions -> error_ -> IO (Id VZEFIVariableStore)
initCreatingVariableStoreAtURL_options_error vzefiVariableStore url options error_ =
  sendOwnedMessage vzefiVariableStore initCreatingVariableStoreAtURL_options_errorSelector (toNSURL url) options (toNSError error_)

-- | The URL of the variable store on the local file system.
--
-- ObjC selector: @- URL@
url :: IsVZEFIVariableStore vzefiVariableStore => vzefiVariableStore -> IO (Id NSURL)
url vzefiVariableStore =
  sendMessage vzefiVariableStore urlSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZEFIVariableStore)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZEFIVariableStore)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithURL:@
initWithURLSelector :: Selector '[Id NSURL] (Id VZEFIVariableStore)
initWithURLSelector = mkSelector "initWithURL:"

-- | @Selector@ for @initCreatingVariableStoreAtURL:options:error:@
initCreatingVariableStoreAtURL_options_errorSelector :: Selector '[Id NSURL, VZEFIVariableStoreInitializationOptions, Id NSError] (Id VZEFIVariableStore)
initCreatingVariableStoreAtURL_options_errorSelector = mkSelector "initCreatingVariableStoreAtURL:options:error:"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

