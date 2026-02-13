{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object used to provide items during a directory enumeration.
--
-- You use this type in your implementation of ``FSVolume/Operations/enumerateDirectory(_:startingAt:verifier:attributes:packer:replyHandler:)``.
--
-- Packing allows your implementation to provide information FSKit needs, including each item's name, type, and identifier (such as an inode number). Some directory enumerations require other attributes, as indicated by the ``FSItemGetAttributesRequest`` sent to the enumerate method.
--
-- Generated bindings for @FSDirectoryEntryPacker@.
module ObjC.FSKit.FSDirectoryEntryPacker
  ( FSDirectoryEntryPacker
  , IsFSDirectoryEntryPacker(..)
  , init_
  , packEntryWithName_itemType_itemID_nextCookie_attributes
  , initSelector
  , packEntryWithName_itemType_itemID_nextCookie_attributesSelector

  -- * Enum types
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

-- | @- init@
init_ :: IsFSDirectoryEntryPacker fsDirectoryEntryPacker => fsDirectoryEntryPacker -> IO (Id FSDirectoryEntryPacker)
init_ fsDirectoryEntryPacker =
  sendOwnedMessage fsDirectoryEntryPacker initSelector

-- | Provides a directory entry during enumeration.
--
-- You call this method in your implementation of ``FSVolume/Operations/enumerateDirectory(_:startingAt:verifier:attributes:packer:replyHandler:)``, for each directory entry you want to provide to the enumeration.
--
-- - Parameters:   - name: The item's name.   - itemType: The type of the item.   - itemID: The item's identifier. Typically this is an inode number, or one of the constants defined by ``FSItem/Identifier`` like ``FSItem/Identifier/rootDirectory``.   - nextCookie: A value to indicate the next entry in the directory to enumerate. FSKit passes this value as the @cookie@ parameter on the next call to ``FSVolume/Operations/enumerateDirectory(_:startingAt:verifier:attributes:packer:replyHandler:)``. Use whatever value is appropriate for your implementation; the value is opaque to FSKit.   - attributes: The item's attributes. Pass @nil@ if the enumeration call didn't request attributes. - Returns: @true@ (Swift) or @YES@ (Objective-C) if packing was successful and enumeration can continue with the next directory entry. If the value is @false@ (Swift) or @NO@ (Objective-C), stop enumerating. This result can happen when the entry is too big for the remaining space in the buffer.
--
-- ObjC selector: @- packEntryWithName:itemType:itemID:nextCookie:attributes:@
packEntryWithName_itemType_itemID_nextCookie_attributes :: (IsFSDirectoryEntryPacker fsDirectoryEntryPacker, IsFSFileName name, IsFSItemAttributes attributes) => fsDirectoryEntryPacker -> name -> FSItemType -> FSItemID -> CULong -> attributes -> IO Bool
packEntryWithName_itemType_itemID_nextCookie_attributes fsDirectoryEntryPacker name itemType itemID nextCookie attributes =
  sendMessage fsDirectoryEntryPacker packEntryWithName_itemType_itemID_nextCookie_attributesSelector (toFSFileName name) itemType itemID nextCookie (toFSItemAttributes attributes)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id FSDirectoryEntryPacker)
initSelector = mkSelector "init"

-- | @Selector@ for @packEntryWithName:itemType:itemID:nextCookie:attributes:@
packEntryWithName_itemType_itemID_nextCookie_attributesSelector :: Selector '[Id FSFileName, FSItemType, FSItemID, CULong, Id FSItemAttributes] Bool
packEntryWithName_itemType_itemID_nextCookie_attributesSelector = mkSelector "packEntryWithName:itemType:itemID:nextCookie:attributes:"

