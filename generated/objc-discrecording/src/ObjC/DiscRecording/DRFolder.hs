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
  , folderWithPathSelector
  , initWithPathSelector
  , virtualFolderWithNameSelector
  , initWithNameSelector
  , makeVirtualSelector
  , addChildSelector
  , removeChildSelector
  , countSelector
  , childrenSelector


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
    withObjCPtr path $ \raw_path ->
      sendClassMsg cls' (mkSelector "folderWithPath:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= retainedObject . castPtr

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
initWithPath drFolder  path =
withObjCPtr path $ \raw_path ->
    fmap (RawId . castPtr) $ sendMsg drFolder (mkSelector "initWithPath:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())]

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
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "virtualFolderWithName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

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
initWithName drFolder  name =
withObjCPtr name $ \raw_name ->
    fmap (RawId . castPtr) $ sendMsg drFolder (mkSelector "initWithName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())]

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
makeVirtual drFolder  =
  sendMsg drFolder (mkSelector "makeVirtual") retVoid []

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
addChild drFolder  child =
withObjCPtr child $ \raw_child ->
    sendMsg drFolder (mkSelector "addChild:") retVoid [argPtr (castPtr raw_child :: Ptr ())]

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
removeChild drFolder  child =
withObjCPtr child $ \raw_child ->
    sendMsg drFolder (mkSelector "removeChild:") retVoid [argPtr (castPtr raw_child :: Ptr ())]

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
count drFolder  =
  sendMsg drFolder (mkSelector "count") retCULong []

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
children drFolder  =
  sendMsg drFolder (mkSelector "children") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @folderWithPath:@
folderWithPathSelector :: Selector
folderWithPathSelector = mkSelector "folderWithPath:"

-- | @Selector@ for @initWithPath:@
initWithPathSelector :: Selector
initWithPathSelector = mkSelector "initWithPath:"

-- | @Selector@ for @virtualFolderWithName:@
virtualFolderWithNameSelector :: Selector
virtualFolderWithNameSelector = mkSelector "virtualFolderWithName:"

-- | @Selector@ for @initWithName:@
initWithNameSelector :: Selector
initWithNameSelector = mkSelector "initWithName:"

-- | @Selector@ for @makeVirtual@
makeVirtualSelector :: Selector
makeVirtualSelector = mkSelector "makeVirtual"

-- | @Selector@ for @addChild:@
addChildSelector :: Selector
addChildSelector = mkSelector "addChild:"

-- | @Selector@ for @removeChild:@
removeChildSelector :: Selector
removeChildSelector = mkSelector "removeChild:"

-- | @Selector@ for @count@
countSelector :: Selector
countSelector = mkSelector "count"

-- | @Selector@ for @children@
childrenSelector :: Selector
childrenSelector = mkSelector "children"

