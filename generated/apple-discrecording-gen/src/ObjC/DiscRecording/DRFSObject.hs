{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

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
-- Generated bindings for @DRFSObject@.
module ObjC.DiscRecording.DRFSObject
  ( DRFSObject
  , IsDRFSObject(..)
  , isVirtual
  , sourcePath
  , parent
  , baseName
  , setBaseName
  , specificNameForFilesystem
  , specificNames
  , setSpecificName_forFilesystem
  , setSpecificNames
  , mangledNameForFilesystem
  , mangledNames
  , propertyForKey_inFilesystem_mergeWithOtherFilesystems
  , propertiesForFilesystem_mergeWithOtherFilesystems
  , setProperty_forKey_inFilesystem
  , setProperties_inFilesystem
  , explicitFilesystemMask
  , setExplicitFilesystemMask
  , effectiveFilesystemMask
  , baseNameSelector
  , effectiveFilesystemMaskSelector
  , explicitFilesystemMaskSelector
  , isVirtualSelector
  , mangledNameForFilesystemSelector
  , mangledNamesSelector
  , parentSelector
  , propertiesForFilesystem_mergeWithOtherFilesystemsSelector
  , propertyForKey_inFilesystem_mergeWithOtherFilesystemsSelector
  , setBaseNameSelector
  , setExplicitFilesystemMaskSelector
  , setProperties_inFilesystemSelector
  , setProperty_forKey_inFilesystemSelector
  , setSpecificName_forFilesystemSelector
  , setSpecificNamesSelector
  , sourcePathSelector
  , specificNameForFilesystemSelector
  , specificNamesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.DiscRecording.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | isVirtual
--
-- Indicates whether the receiver is real or virtual.
--
-- Returns: YES if the receiver is virtual and NO if real.
--
-- ObjC selector: @- isVirtual@
isVirtual :: IsDRFSObject drfsObject => drfsObject -> IO Bool
isVirtual drfsObject =
  sendMessage drfsObject isVirtualSelector

-- | sourcePath
--
-- Returns the path to a real object.
--
-- This method only applies to DRFSObjects pointing to real objects.
--
-- Returns: A path
--
-- ObjC selector: @- sourcePath@
sourcePath :: IsDRFSObject drfsObject => drfsObject -> IO (Id NSString)
sourcePath drfsObject =
  sendMessage drfsObject sourcePathSelector

-- | parent
--
-- Returns the parent folder (if any) of the receiver in the content hierarchy.
--
-- Returns: A
--
-- //apple_ref/occ/cl/DRFolder DRFolder
--
-- object.
--
-- ObjC selector: @- parent@
parent :: IsDRFSObject drfsObject => drfsObject -> IO (Id DRFolder)
parent drfsObject =
  sendMessage drfsObject parentSelector

-- | baseName
--
-- Returns the base name for the receiver.
--
-- The base name is the name from which any necessary filesystem-specific				names are automatically generated.
--
-- Because the content creation API is able to generate multiple filesystems				which require multiple varied naming conventions, a sensible system for				naming is required.  Thus each file has a base name which corresponds				to its default name in any filesystem.
--
-- Whenever possible, the base name will be used in the generated filesystem				without modification.  If the name cannot be used as-is (if, for example, it				contains illegal characters, exceeds the length requirements, doesn't				meet the required format, or a name collision is detected) then an acceptable				name that meets the filesystem's criteria will be generated automatically				from the base name.
--
-- The default base name for a real file or folder is the actual on-disk				name of the item.
--
-- Returns: The base name of the object.
--
-- ObjC selector: @- baseName@
baseName :: IsDRFSObject drfsObject => drfsObject -> IO (Id NSString)
baseName drfsObject =
  sendMessage drfsObject baseNameSelector

-- | setBaseName:
--
-- Sets the base name for the receiver.
--
-- The base name is the name from which any necessary filesystem-specific				names are automatically generated.
--
-- Because the content creation API is able to generate multiple filesystems				which require multiple varied naming conventions, a sensible system for				naming is required.  Thus each file has a base name which corresponds				to its default name in any filesystem.
--
-- Whenever possible, the base name will be used in the generated filesystem				without modification.  If the name cannot be used as-is (if, for example, it				contains illegal characters, exceeds the length requirements, doesn't				meet the required format, or a name collision is detected) then an acceptable				name that meets the filesystem's criteria will be generated automatically				from the base name.
--
-- The default base name for a real file or folder is the actual on-disk				name of the item.
--
-- @baseName@ — The new base name of the object.
--
-- ObjC selector: @- setBaseName:@
setBaseName :: (IsDRFSObject drfsObject, IsNSString baseName) => drfsObject -> baseName -> IO ()
setBaseName drfsObject baseName =
  sendMessage drfsObject setBaseNameSelector (toNSString baseName)

-- | specificNameForFilesystem:
--
-- Returns a single filesystem-specific name for the receiver.
--
-- @filesystem@ — The filesystem to return the name from.
--
-- Returns: An NSString containing the name of the file.
--
-- ObjC selector: @- specificNameForFilesystem:@
specificNameForFilesystem :: (IsDRFSObject drfsObject, IsNSString filesystem) => drfsObject -> filesystem -> IO (Id NSString)
specificNameForFilesystem drfsObject filesystem =
  sendMessage drfsObject specificNameForFilesystemSelector (toNSString filesystem)

-- | specificNames
--
-- Returns all the filesystem-specific names for the receiver.
--
-- @filesystem@ — The filesystem to return the name from.
--
-- Returns: An NSDictionary containing the name of the file on all the filesystems.
--
-- ObjC selector: @- specificNames@
specificNames :: IsDRFSObject drfsObject => drfsObject -> IO (Id NSDictionary)
specificNames drfsObject =
  sendMessage drfsObject specificNamesSelector

-- | setSpecificName:forFilesystem:
--
-- Sets the name used for the receiver in a particular filesystem.
--
-- Every effort will be made to use the name passed in.  However, if				a name is illegal, it will be modified to fit the rules for the 				filesystem's names.  Because of this, you should always call
--
-- //apple_ref/occ/instm/DRFSObject/specificNameForFilesystem: specificNameForFilesystem:
--
-- after to ensure				that you are always displaying the most current names to the user.
--
-- @name@ — The name to set.
--
-- @filesystem@ — The filesystem to set the name for.
--
-- ObjC selector: @- setSpecificName:forFilesystem:@
setSpecificName_forFilesystem :: (IsDRFSObject drfsObject, IsNSString name, IsNSString filesystem) => drfsObject -> name -> filesystem -> IO ()
setSpecificName_forFilesystem drfsObject name filesystem =
  sendMessage drfsObject setSpecificName_forFilesystemSelector (toNSString name) (toNSString filesystem)

-- | setSpecificNames:
--
-- Sets the names used for the receiver in the different filesystems all at once.
--
-- Takes an NSDictionary of filesystem keys with corresponding name strings as				their values for each specific filesystem name that should be set.
--
-- Every effort will be made to use the names passed in.  However, if				a name is illegal, it will be modified to fit the rules for that				filesystem's names.  Because of this, you should always call
--
-- //apple_ref/occ/instm/DRFSObject/specificNames specificNames
--
-- after
--
-- //apple_ref/occ/instm/DRFSObject/setSpecificNames: setSpecificNames:
--
-- to ensure				that you are always displaying the most current names to the user.
--
-- @specificNames@ — The names to set.
--
-- ObjC selector: @- setSpecificNames:@
setSpecificNames :: (IsDRFSObject drfsObject, IsNSDictionary specificNames) => drfsObject -> specificNames -> IO ()
setSpecificNames drfsObject specificNames =
  sendMessage drfsObject setSpecificNamesSelector (toNSDictionary specificNames)

-- | mangledNameForFilesystem:
--
-- Returns a single filesystem-specific name for the receiver, mangled for uniqueness.
--
-- The string will be mangled for uniqueness amongst its siblings; if the burn				were to happen immediately after this call, this is the name which would be used 				on the resulting disc.
--
-- @filesystem@ — The filesystem to set the name for.
--
-- Returns: The name of the file mangled for filesystem constraints.
--
-- ObjC selector: @- mangledNameForFilesystem:@
mangledNameForFilesystem :: (IsDRFSObject drfsObject, IsNSString filesystem) => drfsObject -> filesystem -> IO (Id NSString)
mangledNameForFilesystem drfsObject filesystem =
  sendMessage drfsObject mangledNameForFilesystemSelector (toNSString filesystem)

-- | mangledNames
--
-- Returns a dictionary containing all of the filesystem-specific names				for the receiver, each one mangled for uniqueness.
--
-- The dictionary will return only the names which are indicated by the				receiver's effective mask.  If the receiver's effective mask is zero, an				empty dictionary is returned.
--
-- Returns: An NSDictionary containing the filesystem-specific mangled file names.
--
-- ObjC selector: @- mangledNames@
mangledNames :: IsDRFSObject drfsObject => drfsObject -> IO (Id NSDictionary)
mangledNames drfsObject =
  sendMessage drfsObject mangledNamesSelector

-- | propertyForKey:inFilesystem:mergeWithOtherFilesystems:
--
-- Returns a file/folder property specified by key for the specified filesystem.
--
-- Normally you would call this method with merge set to YES since you are interested in the 				property that will be used when writing the object to disc. But if you have a need to determine				what property is set just for a specific filesystem, then pass in NO for merge. In this case 				only the specific filesystem is checked. So if
--
-- DRHFSPlus DRHFSPlus
--
-- is passed in for filesystem and				merge is NO then the property returned is the value set for the HFS+ filesytem only. If				that property has not been directly set for HFS+ yet, then the returned value will be nil.
--
-- @key@ — The property to return.
--
-- @filesystem@ — The filesystem to look in.
--
-- @merge@ — If YES, also look for the property in the umbrella
--
-- DRAllFilesystems DRAllFilesystems
--
-- .
--
-- Returns: The value associated with the property.
--
-- ObjC selector: @- propertyForKey:inFilesystem:mergeWithOtherFilesystems:@
propertyForKey_inFilesystem_mergeWithOtherFilesystems :: (IsDRFSObject drfsObject, IsNSString key, IsNSString filesystem) => drfsObject -> key -> filesystem -> Bool -> IO RawId
propertyForKey_inFilesystem_mergeWithOtherFilesystems drfsObject key filesystem merge =
  sendMessage drfsObject propertyForKey_inFilesystem_mergeWithOtherFilesystemsSelector (toNSString key) (toNSString filesystem) merge

-- | propertiesForFilesystem:mergeWithOtherFilesystems:
--
-- Returns all the filesystem properties set for the specified filesystem.
--
-- Normally you would call this method with merge set to YES since you are interested in the  				set of properties that will be used when writing the object to disc. But if you have a need to determine 				what properties are set just for a specific filesystem, then pass in NO for merge. In this case  				only the specific filesystem is checked. So if filesystem is set to
--
-- DRHFSPlus DRHFSPlus
--
-- and 				merge is NO then the properties dictionary contains the values set for the HFS+ filesytem only. If 				no properties have been directly set for HFS+ yet, then this properties dictionary will be empty.
--
-- @filesystem@ — The filesystem to look in.
--
-- @merge@ — If YES, also look for properties in the umbrella
--
-- DRAllFilesystems DRAllFilesystems
--
-- .
--
-- Returns: A dictionary of property values.
--
-- ObjC selector: @- propertiesForFilesystem:mergeWithOtherFilesystems:@
propertiesForFilesystem_mergeWithOtherFilesystems :: (IsDRFSObject drfsObject, IsNSString filesystem) => drfsObject -> filesystem -> Bool -> IO (Id NSDictionary)
propertiesForFilesystem_mergeWithOtherFilesystems drfsObject filesystem merge =
  sendMessage drfsObject propertiesForFilesystem_mergeWithOtherFilesystemsSelector (toNSString filesystem) merge

-- | setProperty:forKey:inFilesystem:
--
-- Sets the value of the receiver's property specified by key for the specific				filesystem.
--
-- The property is set only in the filesystem dictionary specified by filesystem.
--
-- DRAllFilesystems DRAllFilesystems
--
-- may be specified as the filesystem in which case				the umbrella property affecting all filesystems at once will be set. Setting				a property for
--
-- DRAllFilesystems DRAllFilesystems
--
-- does not preclude setting a filesystem specific 				property.
--
-- @property@ — The value of the property.
--
-- @key@ — The property key.
--
-- @filesystem@ — The filesystem to set the property in.
--
-- ObjC selector: @- setProperty:forKey:inFilesystem:@
setProperty_forKey_inFilesystem :: (IsDRFSObject drfsObject, IsNSString key, IsNSString filesystem) => drfsObject -> RawId -> key -> filesystem -> IO ()
setProperty_forKey_inFilesystem drfsObject property key filesystem =
  sendMessage drfsObject setProperty_forKey_inFilesystemSelector property (toNSString key) (toNSString filesystem)

-- | setProperties:inFilesystem:
--
-- Sets the value of all the receiver's properties specified by the keys in properties				for the specific filesystem.
--
-- The properties are set only in the filesystem dictionary specified by filesystem.
--
-- DRAllFilesystems DRAllFilesystems
--
-- may be specified as the filesystem 				in which case he umbrella property affecting all filesystems at once will be set. 				Setting properties for
--
-- DRAllFilesystems DRAllFilesystems
--
-- does not preclude setting a filesystem specific 				property.
--
-- @properties@ — The value of the property.
--
-- @filesystem@ — The filesystem to set the property in.
--
-- ObjC selector: @- setProperties:inFilesystem:@
setProperties_inFilesystem :: (IsDRFSObject drfsObject, IsNSDictionary properties, IsNSString filesystem) => drfsObject -> properties -> filesystem -> IO ()
setProperties_inFilesystem drfsObject properties filesystem =
  sendMessage drfsObject setProperties_inFilesystemSelector (toNSDictionary properties) (toNSString filesystem)

-- | explicitFilesystemMask
--
-- Returns the explicit filesystem mask set for the reciever.
--
-- The explicit mask is one that has been explicitly set by a client 				through the
--
-- //apple_ref/occ/instm/DRFSObject/setExplicitFilesystemMask: setExplicitFilesystemMask:
--
-- method.
--
-- Returns: A filesystem mask
--
-- ObjC selector: @- explicitFilesystemMask@
explicitFilesystemMask :: IsDRFSObject drfsObject => drfsObject -> IO CUInt
explicitFilesystemMask drfsObject =
  sendMessage drfsObject explicitFilesystemMaskSelector

-- | setExplicitFilesystemMask:
--
-- Sets the filesystems the receiver will be included on.
--
-- The effective mask for an item cannot be more inclusive than the 				effective mask of it's parent. If the mask set for a child is more inclusive than its parent's mask,				those filesystems not allowed by the parent will be stripped from the resulting effective mask of the				child.
--
-- @mask@ — A filesystem mask
--
-- ObjC selector: @- setExplicitFilesystemMask:@
setExplicitFilesystemMask :: IsDRFSObject drfsObject => drfsObject -> CUInt -> IO ()
setExplicitFilesystemMask drfsObject mask =
  sendMessage drfsObject setExplicitFilesystemMaskSelector mask

-- | effectiveFilesystemMask
--
-- Returns the effective filesystem mask set for the reciever.
--
-- The parent filesystem mask is taken into account for the receiver.
--
-- Returns: A filesystem mask
--
-- ObjC selector: @- effectiveFilesystemMask@
effectiveFilesystemMask :: IsDRFSObject drfsObject => drfsObject -> IO CUInt
effectiveFilesystemMask drfsObject =
  sendMessage drfsObject effectiveFilesystemMaskSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isVirtual@
isVirtualSelector :: Selector '[] Bool
isVirtualSelector = mkSelector "isVirtual"

-- | @Selector@ for @sourcePath@
sourcePathSelector :: Selector '[] (Id NSString)
sourcePathSelector = mkSelector "sourcePath"

-- | @Selector@ for @parent@
parentSelector :: Selector '[] (Id DRFolder)
parentSelector = mkSelector "parent"

-- | @Selector@ for @baseName@
baseNameSelector :: Selector '[] (Id NSString)
baseNameSelector = mkSelector "baseName"

-- | @Selector@ for @setBaseName:@
setBaseNameSelector :: Selector '[Id NSString] ()
setBaseNameSelector = mkSelector "setBaseName:"

-- | @Selector@ for @specificNameForFilesystem:@
specificNameForFilesystemSelector :: Selector '[Id NSString] (Id NSString)
specificNameForFilesystemSelector = mkSelector "specificNameForFilesystem:"

-- | @Selector@ for @specificNames@
specificNamesSelector :: Selector '[] (Id NSDictionary)
specificNamesSelector = mkSelector "specificNames"

-- | @Selector@ for @setSpecificName:forFilesystem:@
setSpecificName_forFilesystemSelector :: Selector '[Id NSString, Id NSString] ()
setSpecificName_forFilesystemSelector = mkSelector "setSpecificName:forFilesystem:"

-- | @Selector@ for @setSpecificNames:@
setSpecificNamesSelector :: Selector '[Id NSDictionary] ()
setSpecificNamesSelector = mkSelector "setSpecificNames:"

-- | @Selector@ for @mangledNameForFilesystem:@
mangledNameForFilesystemSelector :: Selector '[Id NSString] (Id NSString)
mangledNameForFilesystemSelector = mkSelector "mangledNameForFilesystem:"

-- | @Selector@ for @mangledNames@
mangledNamesSelector :: Selector '[] (Id NSDictionary)
mangledNamesSelector = mkSelector "mangledNames"

-- | @Selector@ for @propertyForKey:inFilesystem:mergeWithOtherFilesystems:@
propertyForKey_inFilesystem_mergeWithOtherFilesystemsSelector :: Selector '[Id NSString, Id NSString, Bool] RawId
propertyForKey_inFilesystem_mergeWithOtherFilesystemsSelector = mkSelector "propertyForKey:inFilesystem:mergeWithOtherFilesystems:"

-- | @Selector@ for @propertiesForFilesystem:mergeWithOtherFilesystems:@
propertiesForFilesystem_mergeWithOtherFilesystemsSelector :: Selector '[Id NSString, Bool] (Id NSDictionary)
propertiesForFilesystem_mergeWithOtherFilesystemsSelector = mkSelector "propertiesForFilesystem:mergeWithOtherFilesystems:"

-- | @Selector@ for @setProperty:forKey:inFilesystem:@
setProperty_forKey_inFilesystemSelector :: Selector '[RawId, Id NSString, Id NSString] ()
setProperty_forKey_inFilesystemSelector = mkSelector "setProperty:forKey:inFilesystem:"

-- | @Selector@ for @setProperties:inFilesystem:@
setProperties_inFilesystemSelector :: Selector '[Id NSDictionary, Id NSString] ()
setProperties_inFilesystemSelector = mkSelector "setProperties:inFilesystem:"

-- | @Selector@ for @explicitFilesystemMask@
explicitFilesystemMaskSelector :: Selector '[] CUInt
explicitFilesystemMaskSelector = mkSelector "explicitFilesystemMask"

-- | @Selector@ for @setExplicitFilesystemMask:@
setExplicitFilesystemMaskSelector :: Selector '[CUInt] ()
setExplicitFilesystemMaskSelector = mkSelector "setExplicitFilesystemMask:"

-- | @Selector@ for @effectiveFilesystemMask@
effectiveFilesystemMaskSelector :: Selector '[] CUInt
effectiveFilesystemMaskSelector = mkSelector "effectiveFilesystemMask"

