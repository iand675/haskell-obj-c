{-# LANGUAGE DataKinds #-}
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
  , desktopLocationSelector
  , documentsLocationSelector
  , initSelector
  , setDesktopLocationSelector
  , setDocumentsLocationSelector
  , setShouldCreateBinaryCompatibilitySymlinkSelector
  , shouldCreateBinaryCompatibilitySymlinkSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.FileProvider.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSFileProviderKnownFolderLocations nsFileProviderKnownFolderLocations => nsFileProviderKnownFolderLocations -> IO (Id NSFileProviderKnownFolderLocations)
init_ nsFileProviderKnownFolderLocations =
  sendOwnedMessage nsFileProviderKnownFolderLocations initSelector

-- | Specify whether the system should create a binary compatibility symlink folders.
--
-- If YES, the system creates a symlink from the logical location of the folder in the domain sync root to the known folder location. This symlink allows any app that would have hardcoded the previous location of the folder to still work after enabling the feature.
--
-- Default value is YES.
--
-- ObjC selector: @- shouldCreateBinaryCompatibilitySymlink@
shouldCreateBinaryCompatibilitySymlink :: IsNSFileProviderKnownFolderLocations nsFileProviderKnownFolderLocations => nsFileProviderKnownFolderLocations -> IO Bool
shouldCreateBinaryCompatibilitySymlink nsFileProviderKnownFolderLocations =
  sendMessage nsFileProviderKnownFolderLocations shouldCreateBinaryCompatibilitySymlinkSelector

-- | Specify whether the system should create a binary compatibility symlink folders.
--
-- If YES, the system creates a symlink from the logical location of the folder in the domain sync root to the known folder location. This symlink allows any app that would have hardcoded the previous location of the folder to still work after enabling the feature.
--
-- Default value is YES.
--
-- ObjC selector: @- setShouldCreateBinaryCompatibilitySymlink:@
setShouldCreateBinaryCompatibilitySymlink :: IsNSFileProviderKnownFolderLocations nsFileProviderKnownFolderLocations => nsFileProviderKnownFolderLocations -> Bool -> IO ()
setShouldCreateBinaryCompatibilitySymlink nsFileProviderKnownFolderLocations value =
  sendMessage nsFileProviderKnownFolderLocations setShouldCreateBinaryCompatibilitySymlinkSelector value

-- | Candidate item for ~/Desktop
--
-- For user experience reasons, it is strongly recommended to name the target folder "Desktop".
--
-- ObjC selector: @- desktopLocation@
desktopLocation :: IsNSFileProviderKnownFolderLocations nsFileProviderKnownFolderLocations => nsFileProviderKnownFolderLocations -> IO (Id NSFileProviderKnownFolderLocation)
desktopLocation nsFileProviderKnownFolderLocations =
  sendMessage nsFileProviderKnownFolderLocations desktopLocationSelector

-- | Candidate item for ~/Desktop
--
-- For user experience reasons, it is strongly recommended to name the target folder "Desktop".
--
-- ObjC selector: @- setDesktopLocation:@
setDesktopLocation :: (IsNSFileProviderKnownFolderLocations nsFileProviderKnownFolderLocations, IsNSFileProviderKnownFolderLocation value) => nsFileProviderKnownFolderLocations -> value -> IO ()
setDesktopLocation nsFileProviderKnownFolderLocations value =
  sendMessage nsFileProviderKnownFolderLocations setDesktopLocationSelector (toNSFileProviderKnownFolderLocation value)

-- | Candidate item for ~/Documents
--
-- For user experience reasons, it is strongly recommended to name the target folder "Documents".
--
-- ObjC selector: @- documentsLocation@
documentsLocation :: IsNSFileProviderKnownFolderLocations nsFileProviderKnownFolderLocations => nsFileProviderKnownFolderLocations -> IO (Id NSFileProviderKnownFolderLocation)
documentsLocation nsFileProviderKnownFolderLocations =
  sendMessage nsFileProviderKnownFolderLocations documentsLocationSelector

-- | Candidate item for ~/Documents
--
-- For user experience reasons, it is strongly recommended to name the target folder "Documents".
--
-- ObjC selector: @- setDocumentsLocation:@
setDocumentsLocation :: (IsNSFileProviderKnownFolderLocations nsFileProviderKnownFolderLocations, IsNSFileProviderKnownFolderLocation value) => nsFileProviderKnownFolderLocations -> value -> IO ()
setDocumentsLocation nsFileProviderKnownFolderLocations value =
  sendMessage nsFileProviderKnownFolderLocations setDocumentsLocationSelector (toNSFileProviderKnownFolderLocation value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSFileProviderKnownFolderLocations)
initSelector = mkSelector "init"

-- | @Selector@ for @shouldCreateBinaryCompatibilitySymlink@
shouldCreateBinaryCompatibilitySymlinkSelector :: Selector '[] Bool
shouldCreateBinaryCompatibilitySymlinkSelector = mkSelector "shouldCreateBinaryCompatibilitySymlink"

-- | @Selector@ for @setShouldCreateBinaryCompatibilitySymlink:@
setShouldCreateBinaryCompatibilitySymlinkSelector :: Selector '[Bool] ()
setShouldCreateBinaryCompatibilitySymlinkSelector = mkSelector "setShouldCreateBinaryCompatibilitySymlink:"

-- | @Selector@ for @desktopLocation@
desktopLocationSelector :: Selector '[] (Id NSFileProviderKnownFolderLocation)
desktopLocationSelector = mkSelector "desktopLocation"

-- | @Selector@ for @setDesktopLocation:@
setDesktopLocationSelector :: Selector '[Id NSFileProviderKnownFolderLocation] ()
setDesktopLocationSelector = mkSelector "setDesktopLocation:"

-- | @Selector@ for @documentsLocation@
documentsLocationSelector :: Selector '[] (Id NSFileProviderKnownFolderLocation)
documentsLocationSelector = mkSelector "documentsLocation"

-- | @Selector@ for @setDocumentsLocation:@
setDocumentsLocationSelector :: Selector '[Id NSFileProviderKnownFolderLocation] ()
setDocumentsLocationSelector = mkSelector "setDocumentsLocation:"

