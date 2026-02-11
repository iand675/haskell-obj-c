{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.DiscRecording.Internal.Classes (
    module ObjC.DiscRecording.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- DRBurn ----------

-- | Phantom type for @DRBurn@.
data DRBurn

instance IsObjCObject (Id DRBurn) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DRBurn"

class IsNSObject a => IsDRBurn a where
  toDRBurn :: a -> Id DRBurn

instance IsDRBurn (Id DRBurn) where
  toDRBurn = unsafeCastId

instance IsNSObject (Id DRBurn) where
  toNSObject = unsafeCastId

-- ---------- DRCDTextBlock ----------

-- | DRCDTextBlock
--
-- Defines a CD-Text block, which holds the CD-Text strings				for the entire disc in one language.
-- 
-- Phantom type for @DRCDTextBlock@.
data DRCDTextBlock

instance IsObjCObject (Id DRCDTextBlock) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DRCDTextBlock"

class IsNSObject a => IsDRCDTextBlock a where
  toDRCDTextBlock :: a -> Id DRCDTextBlock

instance IsDRCDTextBlock (Id DRCDTextBlock) where
  toDRCDTextBlock = unsafeCastId

instance IsNSObject (Id DRCDTextBlock) where
  toNSObject = unsafeCastId

-- ---------- DRDevice ----------

-- | Phantom type for @DRDevice@.
data DRDevice

instance IsObjCObject (Id DRDevice) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DRDevice"

class IsNSObject a => IsDRDevice a where
  toDRDevice :: a -> Id DRDevice

instance IsDRDevice (Id DRDevice) where
  toDRDevice = unsafeCastId

instance IsNSObject (Id DRDevice) where
  toNSObject = unsafeCastId

-- ---------- DRErase ----------

-- | Phantom type for @DRErase@.
data DRErase

instance IsObjCObject (Id DRErase) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DRErase"

class IsNSObject a => IsDRErase a where
  toDRErase :: a -> Id DRErase

instance IsDRErase (Id DRErase) where
  toDRErase = unsafeCastId

instance IsNSObject (Id DRErase) where
  toNSObject = unsafeCastId

-- ---------- DRFSObject ----------

-- | DRFSObject
--
-- Abstract base class for the content creation framework
--
-- About Content Creation				Content creation provides an interface for dynamic filesystem creation, allowing				complex filesystem hierarchies to be created and burned on-the-fly without having				to generate a complete on-disk representation.
--
-- DRFSObject is the root object for the objects contained in the Objective C 				content creation hierarchy. Through DRFSObject, file and folder objects				inherit a basic interface to getting and setting filesystem properties,				names and masks. The DRFSObject class is an abstract class, there are no				methods available to create a DRFSObject directly, you create
--
-- //apple_ref/occ/cl/DRFile DRFile
--
-- and
--
-- //apple_ref/occ/cl/DRFolder DRFolder
--
-- objects instead.
--
-- Real and Virtual Objects				The interface is designed around folder and file objects which are laid out				in a one-parent-many-children hierarchy - this should be a familiar concept for				anyone who's ever used a modern filesystem.  There are two kinds of objects in				this API; "real" objects and "virtual" objects, and the distinction is important.
--
-- A real file or folder object corresponds directly to a file or folder					on disk.  The data for a real file object comes directly from the on-disk file.					The hierarchy underneath a real folder object corresponds 1:1 to the					hierarchy underneath the folder in the on-disk filesystem.
--
-- A virtual file or folder object does not have any actual representation					on disk.  The data for a virtual file object is specified through the API or					in a callback function.  The hierarchy underneath a virtual folder object 					is specified through the API.
--
-- Creating a Virtual Hierarchy				In the hierarchy specified through this API, only virtual folders may be assigned				children.  Real files, virtual files, and real folders are all considered leaf				nodes and may not have children.  (Real folders may of course contain files and				folders, but the files and folders are specified by the on-disk representation and				may not be changed through the API unless the real folder is made virtual first.)
--
-- A hierarchy may be as simple as a single real folder, or it can be as complicated				as needed - for example, a virtual folder with a deep hierarchy of children which are				a complex mix of real files, virtual files, real folders, and virtual folders.
--
-- Converting From Real To Virtual				A real folder can be dynamically converted to a virtual folder, in which case				its first level of children is read and converted into a virtual hierarchy.  The children				thus created will all be real.  For example: A real folder named root is converted				into a virtual folder.  The on-disk folder contains a file named file1 and				a folder named folder2.  After conversion, the result is a virtual folder named				root with two children: the real file file1 and the real folder folder2.
--
-- Base Names and Specific Names				Because the content creation API is able to generate multiple filesystems which				require multiple varied naming conventions, a sensible system for naming is required.				Thus each file has a base name which corresponds to its default name in any filesystem. 				Whenever possible, the base name will be used in the generated filesystem without				modification.
--
-- The initial base name for a real object is the name of the corresponding object				on disk.  The initial base name for a virtual object is specified when the object				is created.  The base names for both real and virtual objects may be modified using the
--
-- //apple_ref/occ/instm/DRFSObject/setBaseName: setBaseName:
--
-- method.
--
-- Inside a particular filesytem, if the base name cannot be used as-is (if, for example, it contains illegal				characters, exceeds the length requirements, or otherwise doesn't meet the required format)				then an acceptable name that meets the filesystem's criteria will be generated				automatically from the base name.  The name which is acceptable to a given filesystem				is that file's specific name for that filesystem.
--
-- A specific name may be obtained and modified through this API, or may be left empty to				be automatically generated from the base name.  When a specific name is set through the API,				it will be modified to ensure that the name is legal according to the particular filesystem.
--
-- Even when a specific name is set or generated through the API, it may not be the actual name				used on the disc.  If an object's specific name conflicts with the specific name of another				of the object's siblings in that filesystem, one or both specific names will be mangled				to obtain a unique name before burning, usually by adding a numeric mangle code such as _001				to each name.
--
-- There are two APIs available for getting the specific name from an object:
--
-- //apple_ref/occ/instm/DRFSObject/specificNameForFilesystem: specificNameForFilesystem:
--
-- returns the unmodified specific name, which would be used if there were				no conflicts.
--
-- //apple_ref/occ/instm/DRFSObject/mangledNameForFilesystem: mangledNameForFilesystem:
--
-- returns a modified specific name, mangled if necessary,				which is guaranteed to be unique amongst its siblings in the filesystem.
--
-- The filesystem keys are detailed in Filesystem data accessors.  Most of the keys are 				straightforward; however, ISO-9660				is a special case, since there are two possible naming conventions for ISO-9660, level 1				(8.3, limited charset) and level 2 (30 chars, marginally expanded charset).  You can't				specify DRISO9660 when obtaining a name; instead, you must explicitly specify whether				you want the level 1 or level 2 name with DRISO9660LevelOne or DRISO9660LevelTwo.
--
-- If the object's				name does not conflict with any of its siblings,
--
-- //apple_ref/occ/instm/DRFSObject/mangledNameForFilesystem: mangledNameForFilesystem:
--
-- will return the same				value as
--
-- //apple_ref/occ/instm/DRFSObject/specificNameForFilesystem: specificNameForFilesystem:
--
-- .  The converse is not necessarily true -- one object may get				the actual specific name, and other files with name collisions will be mangled.
--
-- //apple_ref/occ/instm/DRFSObject/mangledNameForFilesystem: mangledNameForFilesystem:
--
-- will check each of the object's siblings in the hierarchy and mangle to				resolve any filename conflicts, so it can be a much more expensive call than
--
-- //apple_ref/occ/instm/DRFSObject/specificNameForFilesystem: specificNameForFilesystem:
--
-- ,				taking at worst O(N^2) time where N is the number of siblings.  However, actual performance				tends to be much better, closer to O(N), particularly when there are only a few collisions.
--
-- //apple_ref/occ/instm/DRFSObject/mangledNameForFilesystem: mangledNameForFilesystem:
--
-- has the advantage of allowing you to see (and to show the user) the exact				names which would be generated on the disc if the burn were started immediately.
--
-- Both
--
-- //apple_ref/occ/instm/DRFSObject/specificNameForFilesystem: specificNameForFilesystem:
--
-- and
--
-- //apple_ref/occ/instm/DRFSObject/mangledNameForFilesystem: mangledNameForFilesystem:
--
-- will cache information when possible, so				that names are only generated and mangled when necessary.  Adding or removing children				from a folder, or changing the base or specific name on an object, may cause				the cached names of the object's children or siblings to be recomputed.
--
-- Properties and Other Meta-Data				Properties are generally accessed similarly to names.  Each object has overall				properties which apply to every filesystem, and it may also have different properties				in each filesystem.  For example, a file which has no relevance for a MacOS user				may be marked invisible in the HFS+ tree, but be visible in the Joliet tree.
--
-- The properties, like names, are also differentiated by filesystem. There is one				properties dictionary for
--
-- DRAllFilesystems DRAllFilesystems
--
-- , and one properties dictionary for each				individual filesystem -
--
-- DRISO9660 DRISO9660
--
-- ,
--
-- DRJoliet DRJoliet
--
-- ,
--
-- DRHFSPlus DRHFSPlus
--
-- , etc.
--
-- The properties for
--
-- DRAllFilesystems DRAllFilesystems
--
-- are treated as the base value, and then the				properties in the specific filesystem dictionary are treated as overrides.
--
-- When obtaining properties with
--
-- //apple_ref/occ/instm/DRFSObject/propertyForKey:inFilesystem:mergeWithOtherFilesystems: propertyForKey:inFilesystem:mergeWithOtherFilesystems:
--
-- or
--
-- //apple_ref/occ/instm/DRFSObject/propertiesForFilesystem:mergeWithOtherFilesystems: propertiesForFilesystem:mergeWithOtherFilesystems:
--
-- , you can specify whether you want to				automatically coalesce the properties between the specified filesystem dictionary and				the "all filesystems" dictionary.  This is useful if you want to obtain the effective				value of the property, because it will return the value from the "all filesystems"				dictionary if the specific filesystem does not assign an override.
--
-- Filesystem Masks				It's possible to suppress generation of particular items in a folder tree.  For example,				you may want a MacOS application file or bundle to only appear in the HFS+ tree, and				want an .EXE file to only appear in the Joliet tree.
--
-- Filesystem-specific suppression is handled through the
--
-- //apple_ref/c/tag/DRFilesystemInclusionMask%32constants filesystem mask
--
-- .  The filesystem				mask is a bitfield which contains a 1 if the object will appear in the corresponding filesystem,				and 0 otherwise.  This can be used to generate arbitrarily complex trees, where in the most				complex case each filesystem may theoretically have its own unique and disjoint tree.				(Such discs are discouraged, however, since they may be confusing to the user.)
--
-- An object can be considered to have two mask values.  The first one is the explicit mask				which has been set by the client, and may be zero if no mask has been set.  The other is the				effective mask, which is the actual mask which will be used.
--
-- If the explicit mask is non-zero, then the object's effective mask is equal to the				bitwise AND of the object's explicit mask and its parent's effective mask.
--
-- If the explicit mask is zero, the object will use the same mask as its parent.  (In				other words, the effective mask is equal to the parent's effective mask.)
--
-- If the root of the hierarchy does not have an explicit mask set, the effective mask of				the root and all its descendants will be zero.
--
-- The explicit mask may be cleared by changing it to zero.  By doing this, the				object's explicit mask becomes zero and its effective mask will be inherited				from its parent.
--
-- If an object's effective mask is zero, it will not be included in the burn.  The major				exception to this rule is when the root folder's explicit/effective mask is zero - when				this happens, DiscRecording will assign a default mask, typically one which will result in				the most cross-platform disc possible.
--
-- If the effective mask of the root is zero at the time of the burn, DiscRecording will				automatically pick a default mask, typically one which will result in the most				cross-platform disc possible.
--
-- Some combinations of filesystem mask have special requirements; for example, Joliet is				based on ISO-9660, and requires that ISO-9660 be enabled on at least the root object.				(You can still have something appear in Joliet but not ISO-9660, however.)  Some				combinations in the future may be mutually exclusive.
--
-- You do not have to set an explicit mask for anything but the root if you want all				filesystems to have the same data.  Since DiscRecording will automatically assign				a mask if none is provided, you do not even have to set an explicit mask for the root.
--
-- Symbolic Link Translation				During the burn, when a symbolic link is encountered in the on-disk filesystem corresponding				to a real file or folder, the semantics of the link will be preserved as closely as possible.				If the link contains an absolute path, it will be copied unmodified.  If the link contains a				relative path, it will be modified to contain an appropriate path.  An important detail to				recognize is that since naming requirements vary between filesystems, the appropriate				path may be different for each filesystem.
--
-- For example, a relative link to				"my long, long directory/this: is an unusual$ filename.with_extension" 				will be modified to contain something like the following.  Note that each component of				the path has been modified to conform to the rules of the target filesystem.
--
-- ISO-9660 level 1: "MYLONGLO/THISISAN.WIT"				ISO-9660 level 2: "MY LONG LONG DIRECTORY/THIS: IS AN UNU.WITH_EXTENSION"				Joliet:           "my long, long directory/this: is an unusual filename.with_extension"				HFS+:             "my long, long directory/this: is an unusual$ filename.with_extension"
--
-- The burn engine will make an effort to appropriately translate each component of the path.				However, it's still possible that the symlink might break in complex cases.				(For example, in the case of a relative-path symlink which traverses through an absolute-path				symlink, or when there are filename conflicts along a symlink's path which the burn				engine has to resolve by mangling.)
--
-- The burn engine's symlink preservation is usually good enough for most situations in which				symlinks are used.  And, when the source filesystem is the same as the target filesystem,				symlinks will be preserved perfectly.  (For example, the HFS+ filesystem generated from				an HFS+ source should never have symlink problems.)
--
-- However, the odds of symlink failure go up when there are complex arrangements of symlinks,				or when there are filename collisions which the burn engine resolves by mangling.
--
-- This is expected behavior.  At present, the only way to create a perfect symlink which				is guaranteed to have a correct path on all filesystems is to create a virtual symlink				using
--
-- //apple_ref/occ/clm/DRFSObject/symLinkPointingTo:inFilesystem: symLinkPointingTo:inFilesystem:
--
-- .
-- 
-- Phantom type for @DRFSObject@.
data DRFSObject

instance IsObjCObject (Id DRFSObject) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DRFSObject"

class IsNSObject a => IsDRFSObject a where
  toDRFSObject :: a -> Id DRFSObject

instance IsDRFSObject (Id DRFSObject) where
  toDRFSObject = unsafeCastId

instance IsNSObject (Id DRFSObject) where
  toNSObject = unsafeCastId

-- ---------- DRNotificationCenter ----------

-- | DRNotificationCenter
--
-- A DRNotificationCenter object (or simply, notification center) is				essentially a notification dispatch table. It notifies all observers of				notifications meeting specific criteria. This information is encapsulated in				NSNotification objects, also known as notifications. Client objects register				themselves with the notification center as observers of specific notifications				posted by DiscRecording. When an event occurs, DiscRecording posts an appropriate				notification to the notification center. The notification center dispatches a				message to each registered observer, passing the notification as the sole				argument.
--
-- There are two main differences between a DRNotificationCenter and the				NSNotificationCenter from Foundation. First is that only Disc Recording				posts notifications received through this mechanism. You use this to 				obtain device plug/unplug events, burn status, etc. Second, there can be				multple notification centers active at once. Each run loop of your application				will have it's own notification center and notifications from that notification				center will be posted to the runloop it was created on.
-- 
-- Phantom type for @DRNotificationCenter@.
data DRNotificationCenter

instance IsObjCObject (Id DRNotificationCenter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DRNotificationCenter"

class IsNSObject a => IsDRNotificationCenter a where
  toDRNotificationCenter :: a -> Id DRNotificationCenter

instance IsDRNotificationCenter (Id DRNotificationCenter) where
  toDRNotificationCenter = unsafeCastId

instance IsNSObject (Id DRNotificationCenter) where
  toNSObject = unsafeCastId

-- ---------- DRTrack ----------

-- | DRTrack
--
-- The DRTrack class represents a track on the burned disc.
--
-- About tracks
--
-- A DRTrack provides data to the for the burn and contains a description of the 	track on disc (length, block type, data format, etc). 	Data is provided for the burn in a real-time thread. It is up to the track to 	provide this data in a timely manner, otherwise a burn underrun can occur and	ruin a disc.
--
-- Data Production
--
-- DRTracks do not typically store or cache the data to be written to disk, instead the 	data is streamed to the disc from some data producer as it's needed. This is 	accomplished through an object associated with the track when the track is created 	called the track producer. A track producer is a class you create that implements 	the
--
-- DRTrackDataProduction DRTrackDataProduction
--
-- informal protocol. This protocol defines all of 	the methods that a track object will call during a burn to obtain data.
--
-- Track Properties
--
-- A DRTrack object contains several properties which define the track for the burn.	These properties are stored in an NSDictionary and are accessed through the
--
-- //apple_ref/occ/instm/DRTrack/properties properties
--
-- and
--
-- //apple_ref/occ/instm/DRTrack/setProperties: setProperties:
--
-- methods.
--
-- There are several properties that are required to be present and if they are not, will 	cause the burn to fail. These are:
--
-- DRTrackLengthKey DRTrackLengthKey
--
-- Length of the track
--
-- DRBlockSizeKey DRBlockSizeKey
--
-- Size in bytes of each track block
--
-- DRBlockTypeKey DRBlockTypeKey
--
-- Type of each track block
--
-- DRDataFormKey DRDataFormKey
--
-- Data form of each block in the track
--
-- DRSessionFormatKey DRSessionFormatKey
--
-- Session format of the track
--
-- DRTrackModeKey DRTrackModeKey
--
-- Track mode of the track
--
-- The possible values of these properties are defined in the Mt. Fuji (IFF-8090i) 	specification for CD/DVD devices. It's up to you to understand the possible values	and meanings of each.
--
-- All other keys contained in the properties dictionary are optional and can be omitted.
-- 
-- Phantom type for @DRTrack@.
data DRTrack

instance IsObjCObject (Id DRTrack) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DRTrack"

class IsNSObject a => IsDRTrack a where
  toDRTrack :: a -> Id DRTrack

instance IsDRTrack (Id DRTrack) where
  toDRTrack = unsafeCastId

instance IsNSObject (Id DRTrack) where
  toNSObject = unsafeCastId

-- ---------- DRFile ----------

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
-- Phantom type for @DRFile@.
data DRFile

instance IsObjCObject (Id DRFile) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DRFile"

class IsDRFSObject a => IsDRFile a where
  toDRFile :: a -> Id DRFile

instance IsDRFile (Id DRFile) where
  toDRFile = unsafeCastId

instance IsDRFSObject (Id DRFile) where
  toDRFSObject = unsafeCastId

instance IsNSObject (Id DRFile) where
  toNSObject = unsafeCastId

-- ---------- DRFolder ----------

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
-- Phantom type for @DRFolder@.
data DRFolder

instance IsObjCObject (Id DRFolder) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DRFolder"

class IsDRFSObject a => IsDRFolder a where
  toDRFolder :: a -> Id DRFolder

instance IsDRFolder (Id DRFolder) where
  toDRFolder = unsafeCastId

instance IsDRFSObject (Id DRFolder) where
  toDRFSObject = unsafeCastId

instance IsNSObject (Id DRFolder) where
  toNSObject = unsafeCastId

-- ---------- DRMSFFormatter ----------

-- | DRMSFFormatter
--
-- NSFormatter subclass
--
-- Instances of DRMSFFormatter format the textual representation of cells that contain 				MSF objects and convert textual representations of msf values into MSF objects.				DRMSFFormatters are typically instantiated in IB using the DiscRecording Interface builder palette.
-- 
-- Phantom type for @DRMSFFormatter@.
data DRMSFFormatter

instance IsObjCObject (Id DRMSFFormatter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DRMSFFormatter"

class IsNSFormatter a => IsDRMSFFormatter a where
  toDRMSFFormatter :: a -> Id DRMSFFormatter

instance IsDRMSFFormatter (Id DRMSFFormatter) where
  toDRMSFFormatter = unsafeCastId

instance IsNSFormatter (Id DRMSFFormatter) where
  toNSFormatter = unsafeCastId

instance IsNSObject (Id DRMSFFormatter) where
  toNSObject = unsafeCastId

-- ---------- DRMSF ----------

-- | Phantom type for @DRMSF@.
data DRMSF

instance IsObjCObject (Id DRMSF) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "DRMSF"

class IsNSNumber a => IsDRMSF a where
  toDRMSF :: a -> Id DRMSF

instance IsDRMSF (Id DRMSF) where
  toDRMSF = unsafeCastId

instance IsNSNumber (Id DRMSF) where
  toNSNumber = unsafeCastId

instance IsNSObject (Id DRMSF) where
  toNSObject = unsafeCastId

instance IsNSValue (Id DRMSF) where
  toNSValue = unsafeCastId
