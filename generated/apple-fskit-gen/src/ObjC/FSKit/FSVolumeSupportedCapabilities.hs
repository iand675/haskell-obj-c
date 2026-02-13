{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A type that represents capabillities supported by a volume, such as hard and symbolic links, journaling, and large file sizes.
--
-- Generated bindings for @FSVolumeSupportedCapabilities@.
module ObjC.FSKit.FSVolumeSupportedCapabilities
  ( FSVolumeSupportedCapabilities
  , IsFSVolumeSupportedCapabilities(..)
  , supportsPersistentObjectIDs
  , setSupportsPersistentObjectIDs
  , supportsSymbolicLinks
  , setSupportsSymbolicLinks
  , supportsHardLinks
  , setSupportsHardLinks
  , supportsJournal
  , setSupportsJournal
  , supportsActiveJournal
  , setSupportsActiveJournal
  , doesNotSupportRootTimes
  , setDoesNotSupportRootTimes
  , supportsSparseFiles
  , setSupportsSparseFiles
  , supportsZeroRuns
  , setSupportsZeroRuns
  , supportsFastStatFS
  , setSupportsFastStatFS
  , supports2TBFiles
  , setSupports2TBFiles
  , supportsOpenDenyModes
  , setSupportsOpenDenyModes
  , supportsHiddenFiles
  , setSupportsHiddenFiles
  , doesNotSupportVolumeSizes
  , setDoesNotSupportVolumeSizes
  , supports64BitObjectIDs
  , setSupports64BitObjectIDs
  , supportsDocumentID
  , setSupportsDocumentID
  , doesNotSupportImmutableFiles
  , setDoesNotSupportImmutableFiles
  , doesNotSupportSettingFilePermissions
  , setDoesNotSupportSettingFilePermissions
  , supportsSharedSpace
  , setSupportsSharedSpace
  , supportsVolumeGroups
  , setSupportsVolumeGroups
  , caseFormat
  , setCaseFormat
  , caseFormatSelector
  , doesNotSupportImmutableFilesSelector
  , doesNotSupportRootTimesSelector
  , doesNotSupportSettingFilePermissionsSelector
  , doesNotSupportVolumeSizesSelector
  , setCaseFormatSelector
  , setDoesNotSupportImmutableFilesSelector
  , setDoesNotSupportRootTimesSelector
  , setDoesNotSupportSettingFilePermissionsSelector
  , setDoesNotSupportVolumeSizesSelector
  , setSupports2TBFilesSelector
  , setSupports64BitObjectIDsSelector
  , setSupportsActiveJournalSelector
  , setSupportsDocumentIDSelector
  , setSupportsFastStatFSSelector
  , setSupportsHardLinksSelector
  , setSupportsHiddenFilesSelector
  , setSupportsJournalSelector
  , setSupportsOpenDenyModesSelector
  , setSupportsPersistentObjectIDsSelector
  , setSupportsSharedSpaceSelector
  , setSupportsSparseFilesSelector
  , setSupportsSymbolicLinksSelector
  , setSupportsVolumeGroupsSelector
  , setSupportsZeroRunsSelector
  , supports2TBFilesSelector
  , supports64BitObjectIDsSelector
  , supportsActiveJournalSelector
  , supportsDocumentIDSelector
  , supportsFastStatFSSelector
  , supportsHardLinksSelector
  , supportsHiddenFilesSelector
  , supportsJournalSelector
  , supportsOpenDenyModesSelector
  , supportsPersistentObjectIDsSelector
  , supportsSharedSpaceSelector
  , supportsSparseFilesSelector
  , supportsSymbolicLinksSelector
  , supportsVolumeGroupsSelector
  , supportsZeroRunsSelector

  -- * Enum types
  , FSVolumeCaseFormat(FSVolumeCaseFormat)
  , pattern FSVolumeCaseFormatSensitive
  , pattern FSVolumeCaseFormatInsensitive
  , pattern FSVolumeCaseFormatInsensitiveCasePreserving

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

-- | A Boolean property that indicates whether the volume supports persistent object identifiers and can look up file system objects by their IDs.
--
-- ObjC selector: @- supportsPersistentObjectIDs@
supportsPersistentObjectIDs :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> IO Bool
supportsPersistentObjectIDs fsVolumeSupportedCapabilities =
  sendMessage fsVolumeSupportedCapabilities supportsPersistentObjectIDsSelector

-- | A Boolean property that indicates whether the volume supports persistent object identifiers and can look up file system objects by their IDs.
--
-- ObjC selector: @- setSupportsPersistentObjectIDs:@
setSupportsPersistentObjectIDs :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> Bool -> IO ()
setSupportsPersistentObjectIDs fsVolumeSupportedCapabilities value =
  sendMessage fsVolumeSupportedCapabilities setSupportsPersistentObjectIDsSelector value

-- | A Boolean property that indicates whether the volume supports symbolic links.
--
-- ObjC selector: @- supportsSymbolicLinks@
supportsSymbolicLinks :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> IO Bool
supportsSymbolicLinks fsVolumeSupportedCapabilities =
  sendMessage fsVolumeSupportedCapabilities supportsSymbolicLinksSelector

-- | A Boolean property that indicates whether the volume supports symbolic links.
--
-- ObjC selector: @- setSupportsSymbolicLinks:@
setSupportsSymbolicLinks :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> Bool -> IO ()
setSupportsSymbolicLinks fsVolumeSupportedCapabilities value =
  sendMessage fsVolumeSupportedCapabilities setSupportsSymbolicLinksSelector value

-- | A Boolean property that indicates whether the volume supports hard links.
--
-- ObjC selector: @- supportsHardLinks@
supportsHardLinks :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> IO Bool
supportsHardLinks fsVolumeSupportedCapabilities =
  sendMessage fsVolumeSupportedCapabilities supportsHardLinksSelector

-- | A Boolean property that indicates whether the volume supports hard links.
--
-- ObjC selector: @- setSupportsHardLinks:@
setSupportsHardLinks :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> Bool -> IO ()
setSupportsHardLinks fsVolumeSupportedCapabilities value =
  sendMessage fsVolumeSupportedCapabilities setSupportsHardLinksSelector value

-- | A Boolean property that indicates whether the volume supports a journal used to speed recovery in case of unplanned restart, such as a power outage or crash.
--
-- This property doesn't necessarily mean the volume is actively using a journal.
--
-- ObjC selector: @- supportsJournal@
supportsJournal :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> IO Bool
supportsJournal fsVolumeSupportedCapabilities =
  sendMessage fsVolumeSupportedCapabilities supportsJournalSelector

-- | A Boolean property that indicates whether the volume supports a journal used to speed recovery in case of unplanned restart, such as a power outage or crash.
--
-- This property doesn't necessarily mean the volume is actively using a journal.
--
-- ObjC selector: @- setSupportsJournal:@
setSupportsJournal :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> Bool -> IO ()
setSupportsJournal fsVolumeSupportedCapabilities value =
  sendMessage fsVolumeSupportedCapabilities setSupportsJournalSelector value

-- | A Boolean property that indicates whether the volume currently uses a journal for speeding recovery after an unplanned shutdown.
--
-- ObjC selector: @- supportsActiveJournal@
supportsActiveJournal :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> IO Bool
supportsActiveJournal fsVolumeSupportedCapabilities =
  sendMessage fsVolumeSupportedCapabilities supportsActiveJournalSelector

-- | A Boolean property that indicates whether the volume currently uses a journal for speeding recovery after an unplanned shutdown.
--
-- ObjC selector: @- setSupportsActiveJournal:@
setSupportsActiveJournal :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> Bool -> IO ()
setSupportsActiveJournal fsVolumeSupportedCapabilities value =
  sendMessage fsVolumeSupportedCapabilities setSupportsActiveJournalSelector value

-- | A Boolan property that indicates the volume doesn't store reliable times for the root directory.
--
-- If this value is @true@ (Swift) or @YES@ (Objective-C), the volume doesn't store reliable times for the root directory.
--
-- ObjC selector: @- doesNotSupportRootTimes@
doesNotSupportRootTimes :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> IO Bool
doesNotSupportRootTimes fsVolumeSupportedCapabilities =
  sendMessage fsVolumeSupportedCapabilities doesNotSupportRootTimesSelector

-- | A Boolan property that indicates the volume doesn't store reliable times for the root directory.
--
-- If this value is @true@ (Swift) or @YES@ (Objective-C), the volume doesn't store reliable times for the root directory.
--
-- ObjC selector: @- setDoesNotSupportRootTimes:@
setDoesNotSupportRootTimes :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> Bool -> IO ()
setDoesNotSupportRootTimes fsVolumeSupportedCapabilities value =
  sendMessage fsVolumeSupportedCapabilities setDoesNotSupportRootTimesSelector value

-- | A Boolean property that indicates whether the volume supports sparse files.
--
-- A sparse file is a file that can have "holes" that the file system has never written to, and as a result don't consume space on disk.
--
-- ObjC selector: @- supportsSparseFiles@
supportsSparseFiles :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> IO Bool
supportsSparseFiles fsVolumeSupportedCapabilities =
  sendMessage fsVolumeSupportedCapabilities supportsSparseFilesSelector

-- | A Boolean property that indicates whether the volume supports sparse files.
--
-- A sparse file is a file that can have "holes" that the file system has never written to, and as a result don't consume space on disk.
--
-- ObjC selector: @- setSupportsSparseFiles:@
setSupportsSparseFiles :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> Bool -> IO ()
setSupportsSparseFiles fsVolumeSupportedCapabilities value =
  sendMessage fsVolumeSupportedCapabilities setSupportsSparseFilesSelector value

-- | A Boolean property that indicates whether the volume supports zero runs
--
-- If this value is true, the volume keeps track of allocated but unwritten runs of a file so that it can substitute zeroes without actually writing zeroes to the media.
--
-- ObjC selector: @- supportsZeroRuns@
supportsZeroRuns :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> IO Bool
supportsZeroRuns fsVolumeSupportedCapabilities =
  sendMessage fsVolumeSupportedCapabilities supportsZeroRunsSelector

-- | A Boolean property that indicates whether the volume supports zero runs
--
-- If this value is true, the volume keeps track of allocated but unwritten runs of a file so that it can substitute zeroes without actually writing zeroes to the media.
--
-- ObjC selector: @- setSupportsZeroRuns:@
setSupportsZeroRuns :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> Bool -> IO ()
setSupportsZeroRuns fsVolumeSupportedCapabilities value =
  sendMessage fsVolumeSupportedCapabilities setSupportsZeroRunsSelector value

-- | A Boolean property that indicates whether the volume supports fast results when fetching file system statistics.
--
-- A true value means this volume hints to upper layers to indicate that @statfs(2)@ is fast enough that its results need not be cached by the caller.
--
-- ObjC selector: @- supportsFastStatFS@
supportsFastStatFS :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> IO Bool
supportsFastStatFS fsVolumeSupportedCapabilities =
  sendMessage fsVolumeSupportedCapabilities supportsFastStatFSSelector

-- | A Boolean property that indicates whether the volume supports fast results when fetching file system statistics.
--
-- A true value means this volume hints to upper layers to indicate that @statfs(2)@ is fast enough that its results need not be cached by the caller.
--
-- ObjC selector: @- setSupportsFastStatFS:@
setSupportsFastStatFS :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> Bool -> IO ()
setSupportsFastStatFS fsVolumeSupportedCapabilities value =
  sendMessage fsVolumeSupportedCapabilities setSupportsFastStatFSSelector value

-- | A Boolean property that indicates whether the volume supports file sizes larger than 4GB, and potentially up to 2TB.
--
-- ObjC selector: @- supports2TBFiles@
supports2TBFiles :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> IO Bool
supports2TBFiles fsVolumeSupportedCapabilities =
  sendMessage fsVolumeSupportedCapabilities supports2TBFilesSelector

-- | A Boolean property that indicates whether the volume supports file sizes larger than 4GB, and potentially up to 2TB.
--
-- ObjC selector: @- setSupports2TBFiles:@
setSupports2TBFiles :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> Bool -> IO ()
setSupports2TBFiles fsVolumeSupportedCapabilities value =
  sendMessage fsVolumeSupportedCapabilities setSupports2TBFilesSelector value

-- | A Boolean property that indicates whether the volume supports open deny modes.
--
-- These are modes such as "open for read write, deny write".
--
-- ObjC selector: @- supportsOpenDenyModes@
supportsOpenDenyModes :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> IO Bool
supportsOpenDenyModes fsVolumeSupportedCapabilities =
  sendMessage fsVolumeSupportedCapabilities supportsOpenDenyModesSelector

-- | A Boolean property that indicates whether the volume supports open deny modes.
--
-- These are modes such as "open for read write, deny write".
--
-- ObjC selector: @- setSupportsOpenDenyModes:@
setSupportsOpenDenyModes :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> Bool -> IO ()
setSupportsOpenDenyModes fsVolumeSupportedCapabilities value =
  sendMessage fsVolumeSupportedCapabilities setSupportsOpenDenyModesSelector value

-- | A Boolean property that indicates whether the volume supports hidden files.
--
-- A @true@ value means the volume supports the @UF_HIDDEN@ file flag.
--
-- ObjC selector: @- supportsHiddenFiles@
supportsHiddenFiles :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> IO Bool
supportsHiddenFiles fsVolumeSupportedCapabilities =
  sendMessage fsVolumeSupportedCapabilities supportsHiddenFilesSelector

-- | A Boolean property that indicates whether the volume supports hidden files.
--
-- A @true@ value means the volume supports the @UF_HIDDEN@ file flag.
--
-- ObjC selector: @- setSupportsHiddenFiles:@
setSupportsHiddenFiles :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> Bool -> IO ()
setSupportsHiddenFiles fsVolumeSupportedCapabilities value =
  sendMessage fsVolumeSupportedCapabilities setSupportsHiddenFilesSelector value

-- | A Boolean property that indicates the volume doesn't support certain volume size reports.
--
-- A true value means the volume doesn't support determining values for total data blocks, available blocks, or free blocks, as in @f_blocks@, @f_bavail@, and @f_bfree@ in the struct @statFS@ returned by @statfs(2)@.
--
-- ObjC selector: @- doesNotSupportVolumeSizes@
doesNotSupportVolumeSizes :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> IO Bool
doesNotSupportVolumeSizes fsVolumeSupportedCapabilities =
  sendMessage fsVolumeSupportedCapabilities doesNotSupportVolumeSizesSelector

-- | A Boolean property that indicates the volume doesn't support certain volume size reports.
--
-- A true value means the volume doesn't support determining values for total data blocks, available blocks, or free blocks, as in @f_blocks@, @f_bavail@, and @f_bfree@ in the struct @statFS@ returned by @statfs(2)@.
--
-- ObjC selector: @- setDoesNotSupportVolumeSizes:@
setDoesNotSupportVolumeSizes :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> Bool -> IO ()
setDoesNotSupportVolumeSizes fsVolumeSupportedCapabilities value =
  sendMessage fsVolumeSupportedCapabilities setDoesNotSupportVolumeSizesSelector value

-- | A Boolean property that indicates whether the volume supports 64-bit object IDs.
--
-- ObjC selector: @- supports64BitObjectIDs@
supports64BitObjectIDs :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> IO Bool
supports64BitObjectIDs fsVolumeSupportedCapabilities =
  sendMessage fsVolumeSupportedCapabilities supports64BitObjectIDsSelector

-- | A Boolean property that indicates whether the volume supports 64-bit object IDs.
--
-- ObjC selector: @- setSupports64BitObjectIDs:@
setSupports64BitObjectIDs :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> Bool -> IO ()
setSupports64BitObjectIDs fsVolumeSupportedCapabilities value =
  sendMessage fsVolumeSupportedCapabilities setSupports64BitObjectIDsSelector value

-- | A Boolean property that indicates whether the volume supports document IDs for document revisions.
--
-- A document ID is an identifier that persists across object ID changes.
--
-- ObjC selector: @- supportsDocumentID@
supportsDocumentID :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> IO Bool
supportsDocumentID fsVolumeSupportedCapabilities =
  sendMessage fsVolumeSupportedCapabilities supportsDocumentIDSelector

-- | A Boolean property that indicates whether the volume supports document IDs for document revisions.
--
-- A document ID is an identifier that persists across object ID changes.
--
-- ObjC selector: @- setSupportsDocumentID:@
setSupportsDocumentID :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> Bool -> IO ()
setSupportsDocumentID fsVolumeSupportedCapabilities value =
  sendMessage fsVolumeSupportedCapabilities setSupportsDocumentIDSelector value

-- | A Boolean property that indicates the volume doesn't support immutable files.
--
-- A @true@ value means this volume doesn't support setting the @UF_IMMUTABLE@ flag.
--
-- ObjC selector: @- doesNotSupportImmutableFiles@
doesNotSupportImmutableFiles :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> IO Bool
doesNotSupportImmutableFiles fsVolumeSupportedCapabilities =
  sendMessage fsVolumeSupportedCapabilities doesNotSupportImmutableFilesSelector

-- | A Boolean property that indicates the volume doesn't support immutable files.
--
-- A @true@ value means this volume doesn't support setting the @UF_IMMUTABLE@ flag.
--
-- ObjC selector: @- setDoesNotSupportImmutableFiles:@
setDoesNotSupportImmutableFiles :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> Bool -> IO ()
setDoesNotSupportImmutableFiles fsVolumeSupportedCapabilities value =
  sendMessage fsVolumeSupportedCapabilities setDoesNotSupportImmutableFilesSelector value

-- | A Boolean property that indicates the volume doesn't set file permissions.
--
-- If this value is @true@ (Swift) or @YES@ (Objective-C), the volume doesn't support setting file permissions.
--
-- ObjC selector: @- doesNotSupportSettingFilePermissions@
doesNotSupportSettingFilePermissions :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> IO Bool
doesNotSupportSettingFilePermissions fsVolumeSupportedCapabilities =
  sendMessage fsVolumeSupportedCapabilities doesNotSupportSettingFilePermissionsSelector

-- | A Boolean property that indicates the volume doesn't set file permissions.
--
-- If this value is @true@ (Swift) or @YES@ (Objective-C), the volume doesn't support setting file permissions.
--
-- ObjC selector: @- setDoesNotSupportSettingFilePermissions:@
setDoesNotSupportSettingFilePermissions :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> Bool -> IO ()
setDoesNotSupportSettingFilePermissions fsVolumeSupportedCapabilities value =
  sendMessage fsVolumeSupportedCapabilities setDoesNotSupportSettingFilePermissionsSelector value

-- | A Boolean property that indicates whether the volume supports multiple logical file systems that share space in a single "partition."
--
-- ObjC selector: @- supportsSharedSpace@
supportsSharedSpace :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> IO Bool
supportsSharedSpace fsVolumeSupportedCapabilities =
  sendMessage fsVolumeSupportedCapabilities supportsSharedSpaceSelector

-- | A Boolean property that indicates whether the volume supports multiple logical file systems that share space in a single "partition."
--
-- ObjC selector: @- setSupportsSharedSpace:@
setSupportsSharedSpace :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> Bool -> IO ()
setSupportsSharedSpace fsVolumeSupportedCapabilities value =
  sendMessage fsVolumeSupportedCapabilities setSupportsSharedSpaceSelector value

-- | A Boolean property that indicates whether the volume supports volume groups.
--
-- Volume groups involve multiple logical file systems that the system can mount and unmount together, and for which the system can present common file system identifier information.
--
-- ObjC selector: @- supportsVolumeGroups@
supportsVolumeGroups :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> IO Bool
supportsVolumeGroups fsVolumeSupportedCapabilities =
  sendMessage fsVolumeSupportedCapabilities supportsVolumeGroupsSelector

-- | A Boolean property that indicates whether the volume supports volume groups.
--
-- Volume groups involve multiple logical file systems that the system can mount and unmount together, and for which the system can present common file system identifier information.
--
-- ObjC selector: @- setSupportsVolumeGroups:@
setSupportsVolumeGroups :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> Bool -> IO ()
setSupportsVolumeGroups fsVolumeSupportedCapabilities value =
  sendMessage fsVolumeSupportedCapabilities setSupportsVolumeGroupsSelector value

-- | A value that indicates the volume's support for case sensitivity.
--
-- ObjC selector: @- caseFormat@
caseFormat :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> IO FSVolumeCaseFormat
caseFormat fsVolumeSupportedCapabilities =
  sendMessage fsVolumeSupportedCapabilities caseFormatSelector

-- | A value that indicates the volume's support for case sensitivity.
--
-- ObjC selector: @- setCaseFormat:@
setCaseFormat :: IsFSVolumeSupportedCapabilities fsVolumeSupportedCapabilities => fsVolumeSupportedCapabilities -> FSVolumeCaseFormat -> IO ()
setCaseFormat fsVolumeSupportedCapabilities value =
  sendMessage fsVolumeSupportedCapabilities setCaseFormatSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @supportsPersistentObjectIDs@
supportsPersistentObjectIDsSelector :: Selector '[] Bool
supportsPersistentObjectIDsSelector = mkSelector "supportsPersistentObjectIDs"

-- | @Selector@ for @setSupportsPersistentObjectIDs:@
setSupportsPersistentObjectIDsSelector :: Selector '[Bool] ()
setSupportsPersistentObjectIDsSelector = mkSelector "setSupportsPersistentObjectIDs:"

-- | @Selector@ for @supportsSymbolicLinks@
supportsSymbolicLinksSelector :: Selector '[] Bool
supportsSymbolicLinksSelector = mkSelector "supportsSymbolicLinks"

-- | @Selector@ for @setSupportsSymbolicLinks:@
setSupportsSymbolicLinksSelector :: Selector '[Bool] ()
setSupportsSymbolicLinksSelector = mkSelector "setSupportsSymbolicLinks:"

-- | @Selector@ for @supportsHardLinks@
supportsHardLinksSelector :: Selector '[] Bool
supportsHardLinksSelector = mkSelector "supportsHardLinks"

-- | @Selector@ for @setSupportsHardLinks:@
setSupportsHardLinksSelector :: Selector '[Bool] ()
setSupportsHardLinksSelector = mkSelector "setSupportsHardLinks:"

-- | @Selector@ for @supportsJournal@
supportsJournalSelector :: Selector '[] Bool
supportsJournalSelector = mkSelector "supportsJournal"

-- | @Selector@ for @setSupportsJournal:@
setSupportsJournalSelector :: Selector '[Bool] ()
setSupportsJournalSelector = mkSelector "setSupportsJournal:"

-- | @Selector@ for @supportsActiveJournal@
supportsActiveJournalSelector :: Selector '[] Bool
supportsActiveJournalSelector = mkSelector "supportsActiveJournal"

-- | @Selector@ for @setSupportsActiveJournal:@
setSupportsActiveJournalSelector :: Selector '[Bool] ()
setSupportsActiveJournalSelector = mkSelector "setSupportsActiveJournal:"

-- | @Selector@ for @doesNotSupportRootTimes@
doesNotSupportRootTimesSelector :: Selector '[] Bool
doesNotSupportRootTimesSelector = mkSelector "doesNotSupportRootTimes"

-- | @Selector@ for @setDoesNotSupportRootTimes:@
setDoesNotSupportRootTimesSelector :: Selector '[Bool] ()
setDoesNotSupportRootTimesSelector = mkSelector "setDoesNotSupportRootTimes:"

-- | @Selector@ for @supportsSparseFiles@
supportsSparseFilesSelector :: Selector '[] Bool
supportsSparseFilesSelector = mkSelector "supportsSparseFiles"

-- | @Selector@ for @setSupportsSparseFiles:@
setSupportsSparseFilesSelector :: Selector '[Bool] ()
setSupportsSparseFilesSelector = mkSelector "setSupportsSparseFiles:"

-- | @Selector@ for @supportsZeroRuns@
supportsZeroRunsSelector :: Selector '[] Bool
supportsZeroRunsSelector = mkSelector "supportsZeroRuns"

-- | @Selector@ for @setSupportsZeroRuns:@
setSupportsZeroRunsSelector :: Selector '[Bool] ()
setSupportsZeroRunsSelector = mkSelector "setSupportsZeroRuns:"

-- | @Selector@ for @supportsFastStatFS@
supportsFastStatFSSelector :: Selector '[] Bool
supportsFastStatFSSelector = mkSelector "supportsFastStatFS"

-- | @Selector@ for @setSupportsFastStatFS:@
setSupportsFastStatFSSelector :: Selector '[Bool] ()
setSupportsFastStatFSSelector = mkSelector "setSupportsFastStatFS:"

-- | @Selector@ for @supports2TBFiles@
supports2TBFilesSelector :: Selector '[] Bool
supports2TBFilesSelector = mkSelector "supports2TBFiles"

-- | @Selector@ for @setSupports2TBFiles:@
setSupports2TBFilesSelector :: Selector '[Bool] ()
setSupports2TBFilesSelector = mkSelector "setSupports2TBFiles:"

-- | @Selector@ for @supportsOpenDenyModes@
supportsOpenDenyModesSelector :: Selector '[] Bool
supportsOpenDenyModesSelector = mkSelector "supportsOpenDenyModes"

-- | @Selector@ for @setSupportsOpenDenyModes:@
setSupportsOpenDenyModesSelector :: Selector '[Bool] ()
setSupportsOpenDenyModesSelector = mkSelector "setSupportsOpenDenyModes:"

-- | @Selector@ for @supportsHiddenFiles@
supportsHiddenFilesSelector :: Selector '[] Bool
supportsHiddenFilesSelector = mkSelector "supportsHiddenFiles"

-- | @Selector@ for @setSupportsHiddenFiles:@
setSupportsHiddenFilesSelector :: Selector '[Bool] ()
setSupportsHiddenFilesSelector = mkSelector "setSupportsHiddenFiles:"

-- | @Selector@ for @doesNotSupportVolumeSizes@
doesNotSupportVolumeSizesSelector :: Selector '[] Bool
doesNotSupportVolumeSizesSelector = mkSelector "doesNotSupportVolumeSizes"

-- | @Selector@ for @setDoesNotSupportVolumeSizes:@
setDoesNotSupportVolumeSizesSelector :: Selector '[Bool] ()
setDoesNotSupportVolumeSizesSelector = mkSelector "setDoesNotSupportVolumeSizes:"

-- | @Selector@ for @supports64BitObjectIDs@
supports64BitObjectIDsSelector :: Selector '[] Bool
supports64BitObjectIDsSelector = mkSelector "supports64BitObjectIDs"

-- | @Selector@ for @setSupports64BitObjectIDs:@
setSupports64BitObjectIDsSelector :: Selector '[Bool] ()
setSupports64BitObjectIDsSelector = mkSelector "setSupports64BitObjectIDs:"

-- | @Selector@ for @supportsDocumentID@
supportsDocumentIDSelector :: Selector '[] Bool
supportsDocumentIDSelector = mkSelector "supportsDocumentID"

-- | @Selector@ for @setSupportsDocumentID:@
setSupportsDocumentIDSelector :: Selector '[Bool] ()
setSupportsDocumentIDSelector = mkSelector "setSupportsDocumentID:"

-- | @Selector@ for @doesNotSupportImmutableFiles@
doesNotSupportImmutableFilesSelector :: Selector '[] Bool
doesNotSupportImmutableFilesSelector = mkSelector "doesNotSupportImmutableFiles"

-- | @Selector@ for @setDoesNotSupportImmutableFiles:@
setDoesNotSupportImmutableFilesSelector :: Selector '[Bool] ()
setDoesNotSupportImmutableFilesSelector = mkSelector "setDoesNotSupportImmutableFiles:"

-- | @Selector@ for @doesNotSupportSettingFilePermissions@
doesNotSupportSettingFilePermissionsSelector :: Selector '[] Bool
doesNotSupportSettingFilePermissionsSelector = mkSelector "doesNotSupportSettingFilePermissions"

-- | @Selector@ for @setDoesNotSupportSettingFilePermissions:@
setDoesNotSupportSettingFilePermissionsSelector :: Selector '[Bool] ()
setDoesNotSupportSettingFilePermissionsSelector = mkSelector "setDoesNotSupportSettingFilePermissions:"

-- | @Selector@ for @supportsSharedSpace@
supportsSharedSpaceSelector :: Selector '[] Bool
supportsSharedSpaceSelector = mkSelector "supportsSharedSpace"

-- | @Selector@ for @setSupportsSharedSpace:@
setSupportsSharedSpaceSelector :: Selector '[Bool] ()
setSupportsSharedSpaceSelector = mkSelector "setSupportsSharedSpace:"

-- | @Selector@ for @supportsVolumeGroups@
supportsVolumeGroupsSelector :: Selector '[] Bool
supportsVolumeGroupsSelector = mkSelector "supportsVolumeGroups"

-- | @Selector@ for @setSupportsVolumeGroups:@
setSupportsVolumeGroupsSelector :: Selector '[Bool] ()
setSupportsVolumeGroupsSelector = mkSelector "setSupportsVolumeGroups:"

-- | @Selector@ for @caseFormat@
caseFormatSelector :: Selector '[] FSVolumeCaseFormat
caseFormatSelector = mkSelector "caseFormat"

-- | @Selector@ for @setCaseFormat:@
setCaseFormatSelector :: Selector '[FSVolumeCaseFormat] ()
setCaseFormatSelector = mkSelector "setCaseFormat:"

