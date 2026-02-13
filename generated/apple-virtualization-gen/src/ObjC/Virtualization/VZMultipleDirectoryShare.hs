{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Directory share for multiple directories.
--
-- This directory share exposes multiple directories from the host file system to the guest.
--
-- See: VZDirectorySharingDeviceConfiguration
--
-- See: VZSharedDirectory
--
-- Generated bindings for @VZMultipleDirectoryShare@.
module ObjC.Virtualization.VZMultipleDirectoryShare
  ( VZMultipleDirectoryShare
  , IsVZMultipleDirectoryShare(..)
  , init_
  , initWithDirectories
  , validateName_error
  , canonicalizedNameFromName
  , directories
  , canonicalizedNameFromNameSelector
  , directoriesSelector
  , initSelector
  , initWithDirectoriesSelector
  , validateName_errorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize the directory share with an empty set of directories.
--
-- ObjC selector: @- init@
init_ :: IsVZMultipleDirectoryShare vzMultipleDirectoryShare => vzMultipleDirectoryShare -> IO (Id VZMultipleDirectoryShare)
init_ vzMultipleDirectoryShare =
  sendOwnedMessage vzMultipleDirectoryShare initSelector

-- | Initialize the directory share with a set of directories on the host.
--
-- @directories@ — Directories on the host to expose to the guest by name.
--
-- The dictionary string keys will be the name for the directory. The keys must be valid names or an exception will be raised.
--
-- See: +[VZMultipleDirectoryShare validateName:error:]
--
-- ObjC selector: @- initWithDirectories:@
initWithDirectories :: (IsVZMultipleDirectoryShare vzMultipleDirectoryShare, IsNSDictionary directories) => vzMultipleDirectoryShare -> directories -> IO (Id VZMultipleDirectoryShare)
initWithDirectories vzMultipleDirectoryShare directories =
  sendOwnedMessage vzMultipleDirectoryShare initWithDirectoriesSelector (toNSDictionary directories)

-- | Check if a name is a valid directory name.
--
-- @name@ — The name to validate.
--
-- @error@ — If not nil, assigned with an error describing why the name is not valid.
--
-- The name must not be empty, have characters unsafe for file systems, be longer than NAME_MAX, or other restrictions.
--
-- See: +[VZMultipleDirectoryShare canonicalizedNameFromName:]
--
-- ObjC selector: @+ validateName:error:@
validateName_error :: (IsNSString name, IsNSError error_) => name -> error_ -> IO Bool
validateName_error name error_ =
  do
    cls' <- getRequiredClass "VZMultipleDirectoryShare"
    sendClassMessage cls' validateName_errorSelector (toNSString name) (toNSError error_)

-- | Canonicalize a string to be a valid directory name.
--
-- @name@ — The name to canonicalize.
--
-- This returns nil when it cannot produce a valid name. When not nil, the result is a valid directory name.
--
-- See: +[VZMultipleDirectoryShare validateName:error:]
--
-- ObjC selector: @+ canonicalizedNameFromName:@
canonicalizedNameFromName :: IsNSString name => name -> IO (Id NSString)
canonicalizedNameFromName name =
  do
    cls' <- getRequiredClass "VZMultipleDirectoryShare"
    sendClassMessage cls' canonicalizedNameFromNameSelector (toNSString name)

-- | The directories on the host to expose to the guest.
--
-- The dictionary string keys will be the name for the directory. The keys must be valid names or an exception will be raised.
--
-- See: +[VZMultipleDirectoryShare validateName:error:]
--
-- ObjC selector: @- directories@
directories :: IsVZMultipleDirectoryShare vzMultipleDirectoryShare => vzMultipleDirectoryShare -> IO (Id NSDictionary)
directories vzMultipleDirectoryShare =
  sendMessage vzMultipleDirectoryShare directoriesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZMultipleDirectoryShare)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithDirectories:@
initWithDirectoriesSelector :: Selector '[Id NSDictionary] (Id VZMultipleDirectoryShare)
initWithDirectoriesSelector = mkSelector "initWithDirectories:"

-- | @Selector@ for @validateName:error:@
validateName_errorSelector :: Selector '[Id NSString, Id NSError] Bool
validateName_errorSelector = mkSelector "validateName:error:"

-- | @Selector@ for @canonicalizedNameFromName:@
canonicalizedNameFromNameSelector :: Selector '[Id NSString] (Id NSString)
canonicalizedNameFromNameSelector = mkSelector "canonicalizedNameFromName:"

-- | @Selector@ for @directories@
directoriesSelector :: Selector '[] (Id NSDictionary)
directoriesSelector = mkSelector "directories"

