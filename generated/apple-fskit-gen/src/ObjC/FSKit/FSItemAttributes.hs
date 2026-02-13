{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Attributes of an item, such as size, creation and modification times, and user and group identifiers.
--
-- Generated bindings for @FSItemAttributes@.
module ObjC.FSKit.FSItemAttributes
  ( FSItemAttributes
  , IsFSItemAttributes(..)
  , invalidateAllProperties
  , isValid
  , uid
  , setUid
  , gid
  , setGid
  , mode
  , setMode
  , type_
  , setType
  , linkCount
  , setLinkCount
  , flags
  , setFlags
  , size
  , setSize
  , allocSize
  , setAllocSize
  , fileID
  , setFileID
  , parentID
  , setParentID
  , supportsLimitedXAttrs
  , setSupportsLimitedXAttrs
  , inhibitKernelOffloadedIO
  , setInhibitKernelOffloadedIO
  , allocSizeSelector
  , fileIDSelector
  , flagsSelector
  , gidSelector
  , inhibitKernelOffloadedIOSelector
  , invalidateAllPropertiesSelector
  , isValidSelector
  , linkCountSelector
  , modeSelector
  , parentIDSelector
  , setAllocSizeSelector
  , setFileIDSelector
  , setFlagsSelector
  , setGidSelector
  , setInhibitKernelOffloadedIOSelector
  , setLinkCountSelector
  , setModeSelector
  , setParentIDSelector
  , setSizeSelector
  , setSupportsLimitedXAttrsSelector
  , setTypeSelector
  , setUidSelector
  , sizeSelector
  , supportsLimitedXAttrsSelector
  , typeSelector
  , uidSelector

  -- * Enum types
  , FSItemAttribute(FSItemAttribute)
  , pattern FSItemAttributeType
  , pattern FSItemAttributeMode
  , pattern FSItemAttributeLinkCount
  , pattern FSItemAttributeUID
  , pattern FSItemAttributeGID
  , pattern FSItemAttributeFlags
  , pattern FSItemAttributeSize
  , pattern FSItemAttributeAllocSize
  , pattern FSItemAttributeFileID
  , pattern FSItemAttributeParentID
  , pattern FSItemAttributeAccessTime
  , pattern FSItemAttributeModifyTime
  , pattern FSItemAttributeChangeTime
  , pattern FSItemAttributeBirthTime
  , pattern FSItemAttributeBackupTime
  , pattern FSItemAttributeAddedTime
  , pattern FSItemAttributeSupportsLimitedXAttrs
  , pattern FSItemAttributeInhibitKernelOffloadedIO
  , FSItemID(FSItemID)
  , pattern FSItemIDInvalid
  , pattern FSItemIDParentOfRoot
  , pattern FSItemIDRootDirectory
  , FSItemType(FSItemType)
  , pattern FSItemTypeUnknown
  , pattern FSItemTypeFile
  , pattern FSItemTypeDirectory
  , pattern FSItemTypeSymlink
  , pattern FSItemTypeFIFO
  , pattern FSItemTypeCharDevice
  , pattern FSItemTypeBlockDevice
  , pattern FSItemTypeSocket

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.FSKit.Internal.Classes
import ObjC.FSKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Marks all attributes inactive.
--
-- ObjC selector: @- invalidateAllProperties@
invalidateAllProperties :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> IO ()
invalidateAllProperties fsItemAttributes =
  sendMessage fsItemAttributes invalidateAllPropertiesSelector

-- | Returns a Boolean value that indicates whether the attribute is valid.
--
-- If the value returned by this method is @YES@ (Objective-C) or @true@ (Swift), a caller can safely use the given attribute.
--
-- ObjC selector: @- isValid:@
isValid :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> FSItemAttribute -> IO Bool
isValid fsItemAttributes attribute =
  sendMessage fsItemAttributes isValidSelector attribute

-- | The user identifier.
--
-- ObjC selector: @- uid@
uid :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> IO CUInt
uid fsItemAttributes =
  sendMessage fsItemAttributes uidSelector

-- | The user identifier.
--
-- ObjC selector: @- setUid:@
setUid :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> CUInt -> IO ()
setUid fsItemAttributes value =
  sendMessage fsItemAttributes setUidSelector value

-- | The group identifier.
--
-- ObjC selector: @- gid@
gid :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> IO CUInt
gid fsItemAttributes =
  sendMessage fsItemAttributes gidSelector

-- | The group identifier.
--
-- ObjC selector: @- setGid:@
setGid :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> CUInt -> IO ()
setGid fsItemAttributes value =
  sendMessage fsItemAttributes setGidSelector value

-- | The mode of the item.
--
-- The mode is often used for @setuid@, @setgid@, and @sticky@ bits.
--
-- ObjC selector: @- mode@
mode :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> IO CUInt
mode fsItemAttributes =
  sendMessage fsItemAttributes modeSelector

-- | The mode of the item.
--
-- The mode is often used for @setuid@, @setgid@, and @sticky@ bits.
--
-- ObjC selector: @- setMode:@
setMode :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> CUInt -> IO ()
setMode fsItemAttributes value =
  sendMessage fsItemAttributes setModeSelector value

-- | The item type, such as a regular file, directory, or symbolic link.
--
-- ObjC selector: @- type@
type_ :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> IO FSItemType
type_ fsItemAttributes =
  sendMessage fsItemAttributes typeSelector

-- | The item type, such as a regular file, directory, or symbolic link.
--
-- ObjC selector: @- setType:@
setType :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> FSItemType -> IO ()
setType fsItemAttributes value =
  sendMessage fsItemAttributes setTypeSelector value

-- | The number of hard links to the item.
--
-- ObjC selector: @- linkCount@
linkCount :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> IO CUInt
linkCount fsItemAttributes =
  sendMessage fsItemAttributes linkCountSelector

-- | The number of hard links to the item.
--
-- ObjC selector: @- setLinkCount:@
setLinkCount :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> CUInt -> IO ()
setLinkCount fsItemAttributes value =
  sendMessage fsItemAttributes setLinkCountSelector value

-- | The item's behavior flags.
--
-- See @st_flags@ in @stat.h@ for flag definitions.
--
-- ObjC selector: @- flags@
flags :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> IO CUInt
flags fsItemAttributes =
  sendMessage fsItemAttributes flagsSelector

-- | The item's behavior flags.
--
-- See @st_flags@ in @stat.h@ for flag definitions.
--
-- ObjC selector: @- setFlags:@
setFlags :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> CUInt -> IO ()
setFlags fsItemAttributes value =
  sendMessage fsItemAttributes setFlagsSelector value

-- | The item's size.
--
-- ObjC selector: @- size@
size :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> IO CULong
size fsItemAttributes =
  sendMessage fsItemAttributes sizeSelector

-- | The item's size.
--
-- ObjC selector: @- setSize:@
setSize :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> CULong -> IO ()
setSize fsItemAttributes value =
  sendMessage fsItemAttributes setSizeSelector value

-- | The item's allocated size.
--
-- ObjC selector: @- allocSize@
allocSize :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> IO CULong
allocSize fsItemAttributes =
  sendOwnedMessage fsItemAttributes allocSizeSelector

-- | The item's allocated size.
--
-- ObjC selector: @- setAllocSize:@
setAllocSize :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> CULong -> IO ()
setAllocSize fsItemAttributes value =
  sendMessage fsItemAttributes setAllocSizeSelector value

-- | The item's file identifier.
--
-- ObjC selector: @- fileID@
fileID :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> IO FSItemID
fileID fsItemAttributes =
  sendMessage fsItemAttributes fileIDSelector

-- | The item's file identifier.
--
-- ObjC selector: @- setFileID:@
setFileID :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> FSItemID -> IO ()
setFileID fsItemAttributes value =
  sendMessage fsItemAttributes setFileIDSelector value

-- | The identifier of the item's parent.
--
-- ObjC selector: @- parentID@
parentID :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> IO FSItemID
parentID fsItemAttributes =
  sendMessage fsItemAttributes parentIDSelector

-- | The identifier of the item's parent.
--
-- ObjC selector: @- setParentID:@
setParentID :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> FSItemID -> IO ()
setParentID fsItemAttributes value =
  sendMessage fsItemAttributes setParentIDSelector value

-- | A Boolean value that indicates whether the item supports a limited set of extended attributes.
--
-- ObjC selector: @- supportsLimitedXAttrs@
supportsLimitedXAttrs :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> IO Bool
supportsLimitedXAttrs fsItemAttributes =
  sendMessage fsItemAttributes supportsLimitedXAttrsSelector

-- | A Boolean value that indicates whether the item supports a limited set of extended attributes.
--
-- ObjC selector: @- setSupportsLimitedXAttrs:@
setSupportsLimitedXAttrs :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> Bool -> IO ()
setSupportsLimitedXAttrs fsItemAttributes value =
  sendMessage fsItemAttributes setSupportsLimitedXAttrsSelector value

-- | A Boolean value that indicates whether the file system overrides the per-volume settings for kernel offloaded I/O for a specific file.
--
-- This property has no meaning if the volume doesn't conform to ``FSVolumeKernelOffloadedIOOperations``.
--
-- ObjC selector: @- inhibitKernelOffloadedIO@
inhibitKernelOffloadedIO :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> IO Bool
inhibitKernelOffloadedIO fsItemAttributes =
  sendMessage fsItemAttributes inhibitKernelOffloadedIOSelector

-- | A Boolean value that indicates whether the file system overrides the per-volume settings for kernel offloaded I/O for a specific file.
--
-- This property has no meaning if the volume doesn't conform to ``FSVolumeKernelOffloadedIOOperations``.
--
-- ObjC selector: @- setInhibitKernelOffloadedIO:@
setInhibitKernelOffloadedIO :: IsFSItemAttributes fsItemAttributes => fsItemAttributes -> Bool -> IO ()
setInhibitKernelOffloadedIO fsItemAttributes value =
  sendMessage fsItemAttributes setInhibitKernelOffloadedIOSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @invalidateAllProperties@
invalidateAllPropertiesSelector :: Selector '[] ()
invalidateAllPropertiesSelector = mkSelector "invalidateAllProperties"

-- | @Selector@ for @isValid:@
isValidSelector :: Selector '[FSItemAttribute] Bool
isValidSelector = mkSelector "isValid:"

-- | @Selector@ for @uid@
uidSelector :: Selector '[] CUInt
uidSelector = mkSelector "uid"

-- | @Selector@ for @setUid:@
setUidSelector :: Selector '[CUInt] ()
setUidSelector = mkSelector "setUid:"

-- | @Selector@ for @gid@
gidSelector :: Selector '[] CUInt
gidSelector = mkSelector "gid"

-- | @Selector@ for @setGid:@
setGidSelector :: Selector '[CUInt] ()
setGidSelector = mkSelector "setGid:"

-- | @Selector@ for @mode@
modeSelector :: Selector '[] CUInt
modeSelector = mkSelector "mode"

-- | @Selector@ for @setMode:@
setModeSelector :: Selector '[CUInt] ()
setModeSelector = mkSelector "setMode:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] FSItemType
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector '[FSItemType] ()
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @linkCount@
linkCountSelector :: Selector '[] CUInt
linkCountSelector = mkSelector "linkCount"

-- | @Selector@ for @setLinkCount:@
setLinkCountSelector :: Selector '[CUInt] ()
setLinkCountSelector = mkSelector "setLinkCount:"

-- | @Selector@ for @flags@
flagsSelector :: Selector '[] CUInt
flagsSelector = mkSelector "flags"

-- | @Selector@ for @setFlags:@
setFlagsSelector :: Selector '[CUInt] ()
setFlagsSelector = mkSelector "setFlags:"

-- | @Selector@ for @size@
sizeSelector :: Selector '[] CULong
sizeSelector = mkSelector "size"

-- | @Selector@ for @setSize:@
setSizeSelector :: Selector '[CULong] ()
setSizeSelector = mkSelector "setSize:"

-- | @Selector@ for @allocSize@
allocSizeSelector :: Selector '[] CULong
allocSizeSelector = mkSelector "allocSize"

-- | @Selector@ for @setAllocSize:@
setAllocSizeSelector :: Selector '[CULong] ()
setAllocSizeSelector = mkSelector "setAllocSize:"

-- | @Selector@ for @fileID@
fileIDSelector :: Selector '[] FSItemID
fileIDSelector = mkSelector "fileID"

-- | @Selector@ for @setFileID:@
setFileIDSelector :: Selector '[FSItemID] ()
setFileIDSelector = mkSelector "setFileID:"

-- | @Selector@ for @parentID@
parentIDSelector :: Selector '[] FSItemID
parentIDSelector = mkSelector "parentID"

-- | @Selector@ for @setParentID:@
setParentIDSelector :: Selector '[FSItemID] ()
setParentIDSelector = mkSelector "setParentID:"

-- | @Selector@ for @supportsLimitedXAttrs@
supportsLimitedXAttrsSelector :: Selector '[] Bool
supportsLimitedXAttrsSelector = mkSelector "supportsLimitedXAttrs"

-- | @Selector@ for @setSupportsLimitedXAttrs:@
setSupportsLimitedXAttrsSelector :: Selector '[Bool] ()
setSupportsLimitedXAttrsSelector = mkSelector "setSupportsLimitedXAttrs:"

-- | @Selector@ for @inhibitKernelOffloadedIO@
inhibitKernelOffloadedIOSelector :: Selector '[] Bool
inhibitKernelOffloadedIOSelector = mkSelector "inhibitKernelOffloadedIO"

-- | @Selector@ for @setInhibitKernelOffloadedIO:@
setInhibitKernelOffloadedIOSelector :: Selector '[Bool] ()
setInhibitKernelOffloadedIOSelector = mkSelector "setInhibitKernelOffloadedIO:"

