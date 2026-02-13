{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | DRFolder
--
-- Represents a folder to be created on the burned disc.
--
-- DRFolders can be either a “real” folder pointing to an existing folder 				(residing on a hard drive for example) or can be a “virtual” folder which exists				only on the resulting burned disc. 				A DRFolder pointing to an existing folder cannot have it's 				contents changed - only those files/folders which are children of the actual folder on disk will 				be included on the resulting disc. Virtual folders are entirely created 				programatically and any virtual folder structure can exist and be burned 				to disc. It is possible to convert a real folder to a virtual folder 				using the
--
-- //apple_ref/occ/intm/DRFolder/makeVirtual makeVirtual
--
-- method.
--
-- Generated bindings for @DRFolder@.
module ObjC.DiscRecording.DRFolder
  ( DRFolder
  , IsDRFolder(..)
  , folderWithPath
  , initWithPath
  , virtualFolderWithName
  , initWithName
  , makeVirtual
  , addChild
  , removeChild
  , count
  , children
  , addChildSelector
  , childrenSelector
  , countSelector
  , folderWithPathSelector
  , initWithNameSelector
  , initWithPathSelector
  , makeVirtualSelector
  , removeChildSelector
  , virtualFolderWithNameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.DiscRecording.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | folderWithPath:
--
-- Creates a real folder object
--
-- Creates and initializes a DRFolder object that will use the folder contents of					the folder located at path as a source.
--
-- @path@ — The path to an existing file.
--
-- Returns: An autoreleased DRFolder object.
--
-- ObjC selector: @+ folderWithPath:@
folderWithPath :: IsNSString path => path -> IO (Id DRFolder)
folderWithPath path =
  do
    cls' <- getRequiredClass "DRFolder"
    sendClassMessage cls' folderWithPathSelector (toNSString path)

-- | initWithPath:
--
-- Initializes a real file object
--
-- Initializes a DRFolder object that will use the folder contents of					the folder located at path as a source.
--
-- @path@ — The path to an existing folder.
--
-- Returns: A DRFolder object.
--
-- ObjC selector: @- initWithPath:@
initWithPath :: (IsDRFolder drFolder, IsNSString path) => drFolder -> path -> IO RawId
initWithPath drFolder path =
  sendOwnedMessage drFolder initWithPathSelector (toNSString path)

-- | virtualFolderWithName:
--
-- Creates a virtual folder object
--
-- Creates and initializes a DRFolder object that will be populated with 					specified
--
-- //apple_ref/occ/cl/DRFile DRFile
--
-- and DRFolder objects at runtime.
--
-- @name@ — The name of the folder on the output disc.
--
-- Returns: An autoreleased DRFolder object.
--
-- ObjC selector: @+ virtualFolderWithName:@
virtualFolderWithName :: IsNSString name => name -> IO (Id DRFolder)
virtualFolderWithName name =
  do
    cls' <- getRequiredClass "DRFolder"
    sendClassMessage cls' virtualFolderWithNameSelector (toNSString name)

-- | initWithName:
--
-- Initializes a virtual file object
--
-- Initializes a DRFolder object that will be populated with 					specified
--
-- //apple_ref/occ/cl/DRFile DRFile
--
-- and DRFolder objects at runtime.
--
-- @name@ — The name of the folder on the output disc.
--
-- Returns: A DRFolder object.
--
-- ObjC selector: @- initWithName:@
initWithName :: (IsDRFolder drFolder, IsNSString name) => drFolder -> name -> IO RawId
initWithName drFolder name =
  sendOwnedMessage drFolder initWithNameSelector (toNSString name)

-- | makeVirtual
--
-- Changes the real DRFolder object into a virtual DRFolder object.
--
-- The virtual folder created in this way is a snapshot of the on-disk					folder at the moment of the call.  The newly created virtual folder					will contain real folder and file objects corresponding to the					on-disk children of the original on-disk folder.
--
-- If the on-disk folder is modified (eg, if the folder attributes change, 					or if children are added to or removed from the on-disk tree):					during this call, the virtual folder may or may not reflect the changes.					If modified after this call, the virtual folder will not reflect					the changes.
--
-- ObjC selector: @- makeVirtual@
makeVirtual :: IsDRFolder drFolder => drFolder -> IO ()
makeVirtual drFolder =
  sendMessage drFolder makeVirtualSelector

-- | addChild:
--
-- Adds an object reference (either a file or folder) as a child of					a virtual folder object.
--
-- This method only applies to virtual folders.  Real folders					are considered leaf nodes and cannot have children.
--
-- @child@ — The child to add to the folder
--
-- ObjC selector: @- addChild:@
addChild :: (IsDRFolder drFolder, IsDRFSObject child) => drFolder -> child -> IO ()
addChild drFolder child =
  sendMessage drFolder addChildSelector (toDRFSObject child)

-- | removeChild:
--
-- Removes an object reference (either a file or folder) as a child of					a virtual folder object.
--
-- This method only applies to virtual folders.  Real folders					are considered leaf nodes and cannot have children.
--
-- @child@ — The child to remove from the folder
--
-- ObjC selector: @- removeChild:@
removeChild :: (IsDRFolder drFolder, IsDRFSObject child) => drFolder -> child -> IO ()
removeChild drFolder child =
  sendMessage drFolder removeChildSelector (toDRFSObject child)

-- | count
--
-- Returns the number of children of a virtual folder.
--
-- This method returns a					shallow count of only those children that are immediately contained 					within the virtual folder.
--
-- This method only applies to virtual folders.  Real folders					are considered leaf nodes and should not be messaged with this call.
--
-- Returns: A count of the number of children.
--
-- ObjC selector: @- count@
count :: IsDRFolder drFolder => drFolder -> IO CULong
count drFolder =
  sendMessage drFolder countSelector

-- | children
--
-- Returns an array containing the children of a virtual folder.
--
-- The order of children in the array is arbitrary -- since the various filesystems being					generated may have different sorting requirements, there is no one true					way to sort the children.  The ordering will change only when children					are added or removed.  You should sort the children according to the needs					of your display, and in a consistent manner.
--
-- This function only applies to virtual folders.  Real folders					are considered leaf nodes and should not be passed into this call.
--
-- Returns: An NSArray of
--
-- //apple_ref/occ/cl/DRFile DRFile
--
-- and DRFolder objects.
--
-- ObjC selector: @- children@
children :: IsDRFolder drFolder => drFolder -> IO (Id NSArray)
children drFolder =
  sendMessage drFolder childrenSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @folderWithPath:@
folderWithPathSelector :: Selector '[Id NSString] (Id DRFolder)
folderWithPathSelector = mkSelector "folderWithPath:"

-- | @Selector@ for @initWithPath:@
initWithPathSelector :: Selector '[Id NSString] RawId
initWithPathSelector = mkSelector "initWithPath:"

-- | @Selector@ for @virtualFolderWithName:@
virtualFolderWithNameSelector :: Selector '[Id NSString] (Id DRFolder)
virtualFolderWithNameSelector = mkSelector "virtualFolderWithName:"

-- | @Selector@ for @initWithName:@
initWithNameSelector :: Selector '[Id NSString] RawId
initWithNameSelector = mkSelector "initWithName:"

-- | @Selector@ for @makeVirtual@
makeVirtualSelector :: Selector '[] ()
makeVirtualSelector = mkSelector "makeVirtual"

-- | @Selector@ for @addChild:@
addChildSelector :: Selector '[Id DRFSObject] ()
addChildSelector = mkSelector "addChild:"

-- | @Selector@ for @removeChild:@
removeChildSelector :: Selector '[Id DRFSObject] ()
removeChildSelector = mkSelector "removeChild:"

-- | @Selector@ for @count@
countSelector :: Selector '[] CULong
countSelector = mkSelector "count"

-- | @Selector@ for @children@
childrenSelector :: Selector '[] (Id NSArray)
childrenSelector = mkSelector "children"

