{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Specify the locations at which known folders should be synced in the replicated tree.
--
-- Desktop and Documents candidate items need to have the same parent folder.
--
-- Generated bindings for @NSFileProviderKnownFolderLocations@.
module ObjC.FileProvider.NSFileProviderKnownFolderLocations
  ( NSFileProviderKnownFolderLocations
  , IsNSFileProviderKnownFolderLocations(..)
  , init_
  , shouldCreateBinaryCompatibilitySymlink
  , setShouldCreateBinaryCompatibilitySymlink
  , desktopLocation
  , setDesktopLocation
  , documentsLocation
  , setDocumentsLocation
  , initSelector
  , shouldCreateBinaryCompatibilitySymlinkSelector
  , setShouldCreateBinaryCompatibilitySymlinkSelector
  , desktopLocationSelector
  , setDesktopLocationSelector
  , documentsLocationSelector
  , setDocumentsLocationSelector


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

import ObjC.FileProvider.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSFileProviderKnownFolderLocations nsFileProviderKnownFolderLocations => nsFileProviderKnownFolderLocations -> IO (Id NSFileProviderKnownFolderLocations)
init_ nsFileProviderKnownFolderLocations  =
  sendMsg nsFileProviderKnownFolderLocations (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Specify whether the system should create a binary compatibility symlink folders.
--
-- If YES, the system creates a symlink from the logical location of the folder in the domain sync root to the known folder location. This symlink allows any app that would have hardcoded the previous location of the folder to still work after enabling the feature.
--
-- Default value is YES.
--
-- ObjC selector: @- shouldCreateBinaryCompatibilitySymlink@
shouldCreateBinaryCompatibilitySymlink :: IsNSFileProviderKnownFolderLocations nsFileProviderKnownFolderLocations => nsFileProviderKnownFolderLocations -> IO Bool
shouldCreateBinaryCompatibilitySymlink nsFileProviderKnownFolderLocations  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFileProviderKnownFolderLocations (mkSelector "shouldCreateBinaryCompatibilitySymlink") retCULong []

-- | Specify whether the system should create a binary compatibility symlink folders.
--
-- If YES, the system creates a symlink from the logical location of the folder in the domain sync root to the known folder location. This symlink allows any app that would have hardcoded the previous location of the folder to still work after enabling the feature.
--
-- Default value is YES.
--
-- ObjC selector: @- setShouldCreateBinaryCompatibilitySymlink:@
setShouldCreateBinaryCompatibilitySymlink :: IsNSFileProviderKnownFolderLocations nsFileProviderKnownFolderLocations => nsFileProviderKnownFolderLocations -> Bool -> IO ()
setShouldCreateBinaryCompatibilitySymlink nsFileProviderKnownFolderLocations  value =
  sendMsg nsFileProviderKnownFolderLocations (mkSelector "setShouldCreateBinaryCompatibilitySymlink:") retVoid [argCULong (if value then 1 else 0)]

-- | Candidate item for ~/Desktop
--
-- For user experience reasons, it is strongly recommended to name the target folder "Desktop".
--
-- ObjC selector: @- desktopLocation@
desktopLocation :: IsNSFileProviderKnownFolderLocations nsFileProviderKnownFolderLocations => nsFileProviderKnownFolderLocations -> IO (Id NSFileProviderKnownFolderLocation)
desktopLocation nsFileProviderKnownFolderLocations  =
  sendMsg nsFileProviderKnownFolderLocations (mkSelector "desktopLocation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Candidate item for ~/Desktop
--
-- For user experience reasons, it is strongly recommended to name the target folder "Desktop".
--
-- ObjC selector: @- setDesktopLocation:@
setDesktopLocation :: (IsNSFileProviderKnownFolderLocations nsFileProviderKnownFolderLocations, IsNSFileProviderKnownFolderLocation value) => nsFileProviderKnownFolderLocations -> value -> IO ()
setDesktopLocation nsFileProviderKnownFolderLocations  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsFileProviderKnownFolderLocations (mkSelector "setDesktopLocation:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Candidate item for ~/Documents
--
-- For user experience reasons, it is strongly recommended to name the target folder "Documents".
--
-- ObjC selector: @- documentsLocation@
documentsLocation :: IsNSFileProviderKnownFolderLocations nsFileProviderKnownFolderLocations => nsFileProviderKnownFolderLocations -> IO (Id NSFileProviderKnownFolderLocation)
documentsLocation nsFileProviderKnownFolderLocations  =
  sendMsg nsFileProviderKnownFolderLocations (mkSelector "documentsLocation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Candidate item for ~/Documents
--
-- For user experience reasons, it is strongly recommended to name the target folder "Documents".
--
-- ObjC selector: @- setDocumentsLocation:@
setDocumentsLocation :: (IsNSFileProviderKnownFolderLocations nsFileProviderKnownFolderLocations, IsNSFileProviderKnownFolderLocation value) => nsFileProviderKnownFolderLocations -> value -> IO ()
setDocumentsLocation nsFileProviderKnownFolderLocations  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsFileProviderKnownFolderLocations (mkSelector "setDocumentsLocation:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @shouldCreateBinaryCompatibilitySymlink@
shouldCreateBinaryCompatibilitySymlinkSelector :: Selector
shouldCreateBinaryCompatibilitySymlinkSelector = mkSelector "shouldCreateBinaryCompatibilitySymlink"

-- | @Selector@ for @setShouldCreateBinaryCompatibilitySymlink:@
setShouldCreateBinaryCompatibilitySymlinkSelector :: Selector
setShouldCreateBinaryCompatibilitySymlinkSelector = mkSelector "setShouldCreateBinaryCompatibilitySymlink:"

-- | @Selector@ for @desktopLocation@
desktopLocationSelector :: Selector
desktopLocationSelector = mkSelector "desktopLocation"

-- | @Selector@ for @setDesktopLocation:@
setDesktopLocationSelector :: Selector
setDesktopLocationSelector = mkSelector "setDesktopLocation:"

-- | @Selector@ for @documentsLocation@
documentsLocationSelector :: Selector
documentsLocationSelector = mkSelector "documentsLocation"

-- | @Selector@ for @setDocumentsLocation:@
setDocumentsLocationSelector :: Selector
setDocumentsLocationSelector = mkSelector "setDocumentsLocation:"

