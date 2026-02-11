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
  , initSelector
  , initWithDirectoriesSelector
  , validateName_errorSelector
  , canonicalizedNameFromNameSelector
  , directoriesSelector


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
import ObjC.Foundation.Internal.Classes

-- | Initialize the directory share with an empty set of directories.
--
-- ObjC selector: @- init@
init_ :: IsVZMultipleDirectoryShare vzMultipleDirectoryShare => vzMultipleDirectoryShare -> IO (Id VZMultipleDirectoryShare)
init_ vzMultipleDirectoryShare  =
  sendMsg vzMultipleDirectoryShare (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
initWithDirectories vzMultipleDirectoryShare  directories =
withObjCPtr directories $ \raw_directories ->
    sendMsg vzMultipleDirectoryShare (mkSelector "initWithDirectories:") (retPtr retVoid) [argPtr (castPtr raw_directories :: Ptr ())] >>= ownedObject . castPtr

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
    withObjCPtr name $ \raw_name ->
      withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "validateName:error:") retCULong [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

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
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "canonicalizedNameFromName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | The directories on the host to expose to the guest.
--
-- The dictionary string keys will be the name for the directory. The keys must be valid names or an exception will be raised.
--
-- See: +[VZMultipleDirectoryShare validateName:error:]
--
-- ObjC selector: @- directories@
directories :: IsVZMultipleDirectoryShare vzMultipleDirectoryShare => vzMultipleDirectoryShare -> IO (Id NSDictionary)
directories vzMultipleDirectoryShare  =
  sendMsg vzMultipleDirectoryShare (mkSelector "directories") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithDirectories:@
initWithDirectoriesSelector :: Selector
initWithDirectoriesSelector = mkSelector "initWithDirectories:"

-- | @Selector@ for @validateName:error:@
validateName_errorSelector :: Selector
validateName_errorSelector = mkSelector "validateName:error:"

-- | @Selector@ for @canonicalizedNameFromName:@
canonicalizedNameFromNameSelector :: Selector
canonicalizedNameFromNameSelector = mkSelector "canonicalizedNameFromName:"

-- | @Selector@ for @directories@
directoriesSelector :: Selector
directoriesSelector = mkSelector "directories"

