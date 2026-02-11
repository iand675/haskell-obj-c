{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | DRFile
--
-- Represents a file to be created on the disc.
--
-- A file can be either a pointer to an exiting file (residing on a hard drive for example)				or can be created at burn time from data passed into the file object as requested. DRFiles can only exist inside of virtual
--
-- //apple_ref/occ/cl/DRFolder DRFolder
--
-- objects.
--
-- Generated bindings for @DRFile@.
module ObjC.DiscRecording.DRFile
  ( DRFile
  , IsDRFile(..)
  , fileWithPath
  , initWithPath
  , hardLinkPointingTo_inFilesystem
  , symLinkPointingTo_inFilesystem
  , finderAliasPointingTo_inFilesystem
  , initWithLinkType_pointingTo_inFilesystem
  , virtualFileWithName_data
  , virtualFileWithName_dataProducer
  , initWithName_data
  , initWithName_dataProducer
  , fileWithPathSelector
  , initWithPathSelector
  , hardLinkPointingTo_inFilesystemSelector
  , symLinkPointingTo_inFilesystemSelector
  , finderAliasPointingTo_inFilesystemSelector
  , initWithLinkType_pointingTo_inFilesystemSelector
  , virtualFileWithName_dataSelector
  , virtualFileWithName_dataProducerSelector
  , initWithName_dataSelector
  , initWithName_dataProducerSelector


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

-- | fileWithPath:
--
-- Creates a real file object
--
-- This type of DRFile reads in data from an 					existing file located at path and burns that data to disc.
--
-- @path@ — The path to an existing file.
--
-- Returns: An autoreleased DRFile object.
--
-- ObjC selector: @+ fileWithPath:@
fileWithPath :: IsNSString path => path -> IO (Id DRFile)
fileWithPath path =
  do
    cls' <- getRequiredClass "DRFile"
    withObjCPtr path $ \raw_path ->
      sendClassMsg cls' (mkSelector "fileWithPath:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= retainedObject . castPtr

-- | initWithPath:
--
-- Initializes a real file object
--
-- This type of DRFile reads in data from an 					existing file located at path and burns that data to disc.
--
-- @path@ — The path to an existing file.
--
-- Returns: An DRFile object.
--
-- ObjC selector: @- initWithPath:@
initWithPath :: (IsDRFile drFile, IsNSString path) => drFile -> path -> IO RawId
initWithPath drFile  path =
withObjCPtr path $ \raw_path ->
    fmap (RawId . castPtr) $ sendMsg drFile (mkSelector "initWithPath:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())]

-- | hardLinkPointingTo:inFilesystem:
--
-- Creates a hard link to another file on the output disc.
--
-- @original@ — The file to point he hard link to
--
-- @filesystem@ — The filesystem this link will exist on.
--
-- Returns: An autoreleased DRFile object.
--
-- ObjC selector: @+ hardLinkPointingTo:inFilesystem:@
hardLinkPointingTo_inFilesystem :: (IsDRFile original, IsNSString filesystem) => original -> filesystem -> IO (Id DRFile)
hardLinkPointingTo_inFilesystem original filesystem =
  do
    cls' <- getRequiredClass "DRFile"
    withObjCPtr original $ \raw_original ->
      withObjCPtr filesystem $ \raw_filesystem ->
        sendClassMsg cls' (mkSelector "hardLinkPointingTo:inFilesystem:") (retPtr retVoid) [argPtr (castPtr raw_original :: Ptr ()), argPtr (castPtr raw_filesystem :: Ptr ())] >>= retainedObject . castPtr

-- | symLinkPointingTo:inFilesystem:
--
-- Creates a symbolic link to another file on the output disc.
--
-- @original@ — The file to point he hard link to
--
-- @filesystem@ — The filesystem this link will exist on.
--
-- Returns: An autoreleased DRFile object.
--
-- ObjC selector: @+ symLinkPointingTo:inFilesystem:@
symLinkPointingTo_inFilesystem :: (IsDRFSObject original, IsNSString filesystem) => original -> filesystem -> IO (Id DRFile)
symLinkPointingTo_inFilesystem original filesystem =
  do
    cls' <- getRequiredClass "DRFile"
    withObjCPtr original $ \raw_original ->
      withObjCPtr filesystem $ \raw_filesystem ->
        sendClassMsg cls' (mkSelector "symLinkPointingTo:inFilesystem:") (retPtr retVoid) [argPtr (castPtr raw_original :: Ptr ()), argPtr (castPtr raw_filesystem :: Ptr ())] >>= retainedObject . castPtr

-- | finderAliasPointingTo:inFilesystem:
--
-- Creates a Finder alias to another file on the output disc.
--
-- @original@ — The file to point he hard link to
--
-- @filesystem@ — The filesystem this link will exist on.
--
-- Returns: An autoreleased DRFile object.
--
-- ObjC selector: @+ finderAliasPointingTo:inFilesystem:@
finderAliasPointingTo_inFilesystem :: (IsDRFSObject original, IsNSString filesystem) => original -> filesystem -> IO (Id DRFile)
finderAliasPointingTo_inFilesystem original filesystem =
  do
    cls' <- getRequiredClass "DRFile"
    withObjCPtr original $ \raw_original ->
      withObjCPtr filesystem $ \raw_filesystem ->
        sendClassMsg cls' (mkSelector "finderAliasPointingTo:inFilesystem:") (retPtr retVoid) [argPtr (castPtr raw_original :: Ptr ()), argPtr (castPtr raw_filesystem :: Ptr ())] >>= retainedObject . castPtr

-- | initWithLinkType:pointingTo:inFilesystem:
--
-- Initializes a file object to point to another file on the output disc.
--
-- @linkType@ — The type of link that will be created.
--
-- @original@ — The file to point he hard link to
--
-- @filesystem@ — The filesystem this link will exist on.
--
-- Returns: A DRFile object.
--
-- ObjC selector: @- initWithLinkType:pointingTo:inFilesystem:@
initWithLinkType_pointingTo_inFilesystem :: (IsDRFile drFile, IsNSString linkType, IsDRFSObject original, IsNSString filesystem) => drFile -> linkType -> original -> filesystem -> IO RawId
initWithLinkType_pointingTo_inFilesystem drFile  linkType original filesystem =
withObjCPtr linkType $ \raw_linkType ->
  withObjCPtr original $ \raw_original ->
    withObjCPtr filesystem $ \raw_filesystem ->
        fmap (RawId . castPtr) $ sendMsg drFile (mkSelector "initWithLinkType:pointingTo:inFilesystem:") (retPtr retVoid) [argPtr (castPtr raw_linkType :: Ptr ()), argPtr (castPtr raw_original :: Ptr ()), argPtr (castPtr raw_filesystem :: Ptr ())]

-- | virtualFileWithName:data:
--
-- Creates a virtual file object
--
-- This type of DRFile burns the data passed in to disc, creating a					file with the passed in name.
--
-- @name@ — The name of the file on disc.
--
-- @data@ — The data that will become the contents of the file on the disc.
--
-- Returns: An autoreleased DRFile object.
--
-- ObjC selector: @+ virtualFileWithName:data:@
virtualFileWithName_data :: (IsNSString name, IsNSData data_) => name -> data_ -> IO (Id DRFile)
virtualFileWithName_data name data_ =
  do
    cls' <- getRequiredClass "DRFile"
    withObjCPtr name $ \raw_name ->
      withObjCPtr data_ $ \raw_data_ ->
        sendClassMsg cls' (mkSelector "virtualFileWithName:data:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_data_ :: Ptr ())] >>= retainedObject . castPtr

-- | virtualFileWithName:dataProducer:
--
-- Creates a virtual file object
--
-- This type of DRFile burns the data produced to the output disc, creating a					file with the passed in name.
--
-- @name@ — The name of the file on disc.
--
-- @data@ — The data that will become the contents of the file on the disc.
--
-- Returns: An autoreleased DRFile object.
--
-- ObjC selector: @+ virtualFileWithName:dataProducer:@
virtualFileWithName_dataProducer :: IsNSString name => name -> RawId -> IO (Id DRFile)
virtualFileWithName_dataProducer name producer =
  do
    cls' <- getRequiredClass "DRFile"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "virtualFileWithName:dataProducer:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr (unRawId producer) :: Ptr ())] >>= retainedObject . castPtr

-- | initWithName:data:
--
-- Initializes a virtual file object
--
-- This type of DRFile burns the data passed in to the output disc, creating a					file with the passed in name.
--
-- @name@ — The name of the file on output disc.
--
-- @data@ — The data that will become the contents of the file on the output disc.
--
-- Returns: A DRFile object.
--
-- ObjC selector: @- initWithName:data:@
initWithName_data :: (IsDRFile drFile, IsNSString name, IsNSData data_) => drFile -> name -> data_ -> IO RawId
initWithName_data drFile  name data_ =
withObjCPtr name $ \raw_name ->
  withObjCPtr data_ $ \raw_data_ ->
      fmap (RawId . castPtr) $ sendMsg drFile (mkSelector "initWithName:data:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_data_ :: Ptr ())]

-- | initWithName:dataProducer:
--
-- Initializes a virtual file object
--
-- This type of DRFile burns the data produced to the output disc, creating a					file with the passed in name.
--
-- @name@ — The name of the file on output disc.
--
-- @producer@ — The object supplying the file data to the burn.
--
-- Returns: A DRFile object.
--
-- ObjC selector: @- initWithName:dataProducer:@
initWithName_dataProducer :: (IsDRFile drFile, IsNSString name) => drFile -> name -> RawId -> IO RawId
initWithName_dataProducer drFile  name producer =
withObjCPtr name $ \raw_name ->
    fmap (RawId . castPtr) $ sendMsg drFile (mkSelector "initWithName:dataProducer:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr (unRawId producer) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fileWithPath:@
fileWithPathSelector :: Selector
fileWithPathSelector = mkSelector "fileWithPath:"

-- | @Selector@ for @initWithPath:@
initWithPathSelector :: Selector
initWithPathSelector = mkSelector "initWithPath:"

-- | @Selector@ for @hardLinkPointingTo:inFilesystem:@
hardLinkPointingTo_inFilesystemSelector :: Selector
hardLinkPointingTo_inFilesystemSelector = mkSelector "hardLinkPointingTo:inFilesystem:"

-- | @Selector@ for @symLinkPointingTo:inFilesystem:@
symLinkPointingTo_inFilesystemSelector :: Selector
symLinkPointingTo_inFilesystemSelector = mkSelector "symLinkPointingTo:inFilesystem:"

-- | @Selector@ for @finderAliasPointingTo:inFilesystem:@
finderAliasPointingTo_inFilesystemSelector :: Selector
finderAliasPointingTo_inFilesystemSelector = mkSelector "finderAliasPointingTo:inFilesystem:"

-- | @Selector@ for @initWithLinkType:pointingTo:inFilesystem:@
initWithLinkType_pointingTo_inFilesystemSelector :: Selector
initWithLinkType_pointingTo_inFilesystemSelector = mkSelector "initWithLinkType:pointingTo:inFilesystem:"

-- | @Selector@ for @virtualFileWithName:data:@
virtualFileWithName_dataSelector :: Selector
virtualFileWithName_dataSelector = mkSelector "virtualFileWithName:data:"

-- | @Selector@ for @virtualFileWithName:dataProducer:@
virtualFileWithName_dataProducerSelector :: Selector
virtualFileWithName_dataProducerSelector = mkSelector "virtualFileWithName:dataProducer:"

-- | @Selector@ for @initWithName:data:@
initWithName_dataSelector :: Selector
initWithName_dataSelector = mkSelector "initWithName:data:"

-- | @Selector@ for @initWithName:dataProducer:@
initWithName_dataProducerSelector :: Selector
initWithName_dataProducerSelector = mkSelector "initWithName:dataProducer:"

