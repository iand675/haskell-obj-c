{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A directory structure for files and folders.
--
-- A file system, depending on its type, provides one or more volumes to clients. The ``FSUnaryFileSystem`` by definition provides only one volume, while an ``FSFileSystem`` supports multiple volumes.
--
-- You implement a volume for your file system type by subclassing this class, and also conforming to the ``FSVolume/Operations`` and ``FSVolume/PathConfOperations`` protocols. This protocol defines the minimum set of operations supported by a volume, such as mounting, activating, creating and removing items, and more.
--
-- Your volume can provide additional functionality by conforming to other volume operations protocols. These protocols add support for operations like open and close, read and write, extended attribute (Xattr) manipulation, and more.
--
-- Generated bindings for @FSVolume@.
module ObjC.FSKit.FSVolume
  ( FSVolume
  , IsFSVolume(..)
  , init_
  , initWithVolumeID_volumeName
  , volumeID
  , name
  , setName
  , initSelector
  , initWithVolumeID_volumeNameSelector
  , nameSelector
  , setNameSelector
  , volumeIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.FSKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsFSVolume fsVolume => fsVolume -> IO (Id FSVolume)
init_ fsVolume =
  sendOwnedMessage fsVolume initSelector

-- | Creates a volume with the given identifier and name. - Parameters:   - volumeID: An ``FSVolumeIdentifier`` to uniquely identify the volume. For a network file system that supports multiple authenticated users, disambiguate the users by using qualifying data in the identifier.   - volumeName: A name for the volume.
--
-- ObjC selector: @- initWithVolumeID:volumeName:@
initWithVolumeID_volumeName :: (IsFSVolume fsVolume, IsFSVolumeIdentifier volumeID, IsFSFileName volumeName) => fsVolume -> volumeID -> volumeName -> IO (Id FSVolume)
initWithVolumeID_volumeName fsVolume volumeID volumeName =
  sendOwnedMessage fsVolume initWithVolumeID_volumeNameSelector (toFSVolumeIdentifier volumeID) (toFSFileName volumeName)

-- | An identifier that uniquely identifies the volume.
--
-- ObjC selector: @- volumeID@
volumeID :: IsFSVolume fsVolume => fsVolume -> IO (Id FSVolumeIdentifier)
volumeID fsVolume =
  sendMessage fsVolume volumeIDSelector

-- | The name of the volume.
--
-- ObjC selector: @- name@
name :: IsFSVolume fsVolume => fsVolume -> IO (Id FSFileName)
name fsVolume =
  sendMessage fsVolume nameSelector

-- | The name of the volume.
--
-- ObjC selector: @- setName:@
setName :: (IsFSVolume fsVolume, IsFSFileName value) => fsVolume -> value -> IO ()
setName fsVolume value =
  sendMessage fsVolume setNameSelector (toFSFileName value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id FSVolume)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithVolumeID:volumeName:@
initWithVolumeID_volumeNameSelector :: Selector '[Id FSVolumeIdentifier, Id FSFileName] (Id FSVolume)
initWithVolumeID_volumeNameSelector = mkSelector "initWithVolumeID:volumeName:"

-- | @Selector@ for @volumeID@
volumeIDSelector :: Selector '[] (Id FSVolumeIdentifier)
volumeIDSelector = mkSelector "volumeID"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id FSFileName)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id FSFileName] ()
setNameSelector = mkSelector "setName:"

