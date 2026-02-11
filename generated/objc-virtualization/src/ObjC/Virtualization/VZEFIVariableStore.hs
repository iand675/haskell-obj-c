{-# LANGUAGE PatternSynonyms #-}
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
  , newSelector
  , initSelector
  , initWithURLSelector
  , initCreatingVariableStoreAtURL_options_errorSelector
  , urlSelector

  -- * Enum types
  , VZEFIVariableStoreInitializationOptions(VZEFIVariableStoreInitializationOptions)
  , pattern VZEFIVariableStoreInitializationOptionAllowOverwrite

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

import ObjC.Virtualization.Internal.Classes
import ObjC.Virtualization.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id VZEFIVariableStore)
new  =
  do
    cls' <- getRequiredClass "VZEFIVariableStore"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZEFIVariableStore vzefiVariableStore => vzefiVariableStore -> IO (Id VZEFIVariableStore)
init_ vzefiVariableStore  =
  sendMsg vzefiVariableStore (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Initialize the variable store from the URL of an existing file.
--
-- @URL@ — The URL of the variable store on the local file system.
--
-- To create a new variable store, use -[VZEFIVariableStore initCreatingVariableStoreAtURL:options:error].
--
-- ObjC selector: @- initWithURL:@
initWithURL :: (IsVZEFIVariableStore vzefiVariableStore, IsNSURL url) => vzefiVariableStore -> url -> IO (Id VZEFIVariableStore)
initWithURL vzefiVariableStore  url =
withObjCPtr url $ \raw_url ->
    sendMsg vzefiVariableStore (mkSelector "initWithURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= ownedObject . castPtr

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
initCreatingVariableStoreAtURL_options_error vzefiVariableStore  url options error_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg vzefiVariableStore (mkSelector "initCreatingVariableStoreAtURL:options:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argCULong (coerce options), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | The URL of the variable store on the local file system.
--
-- ObjC selector: @- URL@
url :: IsVZEFIVariableStore vzefiVariableStore => vzefiVariableStore -> IO (Id NSURL)
url vzefiVariableStore  =
  sendMsg vzefiVariableStore (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithURL:@
initWithURLSelector :: Selector
initWithURLSelector = mkSelector "initWithURL:"

-- | @Selector@ for @initCreatingVariableStoreAtURL:options:error:@
initCreatingVariableStoreAtURL_options_errorSelector :: Selector
initCreatingVariableStoreAtURL_options_errorSelector = mkSelector "initCreatingVariableStoreAtURL:options:error:"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

