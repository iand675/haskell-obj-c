{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The ITLibMediaEntity class serves as the abstract superclass for ITLibMediaItem and ITLibPlaylist instances.				As the superclass, ITLibMediaEntity defines methods used by those subclasses.
--
-- Generated bindings for @ITLibMediaEntity@.
module ObjC.ITunesLibrary.ITLibMediaEntity
  ( ITLibMediaEntity
  , IsITLibMediaEntity(..)
  , valueForProperty
  , enumerateValuesForProperties_usingBlock
  , enumerateValuesExceptForProperties_usingBlock
  , persistentID
  , enumerateValuesExceptForProperties_usingBlockSelector
  , enumerateValuesForProperties_usingBlockSelector
  , persistentIDSelector
  , valueForPropertySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ITunesLibrary.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Gets the value for a specified media property key.
--
-- The media property keys you can use with this property are listed in this document 			   and in Media Item Property Keys and Playlist Property Keys.
--
-- @property@ — The media property key that you want the corresponding value of.
--
-- Returns: The value for the media property key.
--
-- ObjC selector: @- valueForProperty:@
valueForProperty :: (IsITLibMediaEntity itLibMediaEntity, IsNSString property) => itLibMediaEntity -> property -> IO RawId
valueForProperty itLibMediaEntity property =
  sendMessage itLibMediaEntity valueForPropertySelector (toNSString property)

-- | Executes a provided block with the fetched values for the given item properties.
--
-- Use this method to get property values in a batch fashion. 				In some cases, enumerating over a set of property keys can be more efficient 				than fetching each individual property with valueForProperty:.				The media property keys you can use with this property are listed in this document 				and in Media Item Property Keys and Playlist Property Keys.
--
-- @properties@ — A set of keys for the properties that will be enumerated, or nil to enumerate all properties.
--
-- @block@ — A block object that executes for each property in the properties set.
--
-- ObjC selector: @- enumerateValuesForProperties:usingBlock:@
enumerateValuesForProperties_usingBlock :: (IsITLibMediaEntity itLibMediaEntity, IsNSSet properties) => itLibMediaEntity -> properties -> Ptr () -> IO ()
enumerateValuesForProperties_usingBlock itLibMediaEntity properties block =
  sendMessage itLibMediaEntity enumerateValuesForProperties_usingBlockSelector (toNSSet properties) block

-- | Executes a provided block with the fetched values for all properties in the entity except for the provided set.
--
-- Use this method to get property values in a batch fashion. 				In some cases, enumerating over a set of property keys can be more efficient 				than fetching each individual property with valueForProperty:.				The media property keys you can use with this property are listed in this document 				and in Media Item Property Keys and Playlist Property Keys.
--
-- @properties@ — A set of property keys that should NOT be enumerated, or nil to enumerate all properties.
--
-- @block@ — A block object that executes for each property except for the ones in the properties set.
--
-- ObjC selector: @- enumerateValuesExceptForProperties:usingBlock:@
enumerateValuesExceptForProperties_usingBlock :: (IsITLibMediaEntity itLibMediaEntity, IsNSSet properties) => itLibMediaEntity -> properties -> Ptr () -> IO ()
enumerateValuesExceptForProperties_usingBlock itLibMediaEntity properties block =
  sendMessage itLibMediaEntity enumerateValuesExceptForProperties_usingBlockSelector (toNSSet properties) block

-- | The unique identifier of this media entity.
--
-- ObjC selector: @- persistentID@
persistentID :: IsITLibMediaEntity itLibMediaEntity => itLibMediaEntity -> IO (Id NSNumber)
persistentID itLibMediaEntity =
  sendMessage itLibMediaEntity persistentIDSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @valueForProperty:@
valueForPropertySelector :: Selector '[Id NSString] RawId
valueForPropertySelector = mkSelector "valueForProperty:"

-- | @Selector@ for @enumerateValuesForProperties:usingBlock:@
enumerateValuesForProperties_usingBlockSelector :: Selector '[Id NSSet, Ptr ()] ()
enumerateValuesForProperties_usingBlockSelector = mkSelector "enumerateValuesForProperties:usingBlock:"

-- | @Selector@ for @enumerateValuesExceptForProperties:usingBlock:@
enumerateValuesExceptForProperties_usingBlockSelector :: Selector '[Id NSSet, Ptr ()] ()
enumerateValuesExceptForProperties_usingBlockSelector = mkSelector "enumerateValuesExceptForProperties:usingBlock:"

-- | @Selector@ for @persistentID@
persistentIDSelector :: Selector '[] (Id NSNumber)
persistentIDSelector = mkSelector "persistentID"

