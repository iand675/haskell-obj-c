{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A tile set contains all of the tile definitions that are available for use in a tile map. In addition, it also contains tile groups, which define collections of related tile definitions and the rules that govern their placement.
--
-- Generated bindings for @SKTileSet@.
module ObjC.SpriteKit.SKTileSet
  ( SKTileSet
  , IsSKTileSet(..)
  , tileSetWithTileGroups
  , tileSetWithTileGroups_tileSetType
  , initWithTileGroups
  , initWithTileGroups_tileSetType
  , tileSetNamed
  , tileSetFromURL
  , tileGroups
  , setTileGroups
  , name
  , setName
  , type_
  , setType
  , defaultTileGroup
  , setDefaultTileGroup
  , defaultTileGroupSelector
  , initWithTileGroupsSelector
  , initWithTileGroups_tileSetTypeSelector
  , nameSelector
  , setDefaultTileGroupSelector
  , setNameSelector
  , setTileGroupsSelector
  , setTypeSelector
  , tileGroupsSelector
  , tileSetFromURLSelector
  , tileSetNamedSelector
  , tileSetWithTileGroupsSelector
  , tileSetWithTileGroups_tileSetTypeSelector
  , typeSelector

  -- * Enum types
  , SKTileSetType(SKTileSetType)
  , pattern SKTileSetTypeGrid
  , pattern SKTileSetTypeIsometric
  , pattern SKTileSetTypeHexagonalFlat
  , pattern SKTileSetTypeHexagonalPointy

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SpriteKit.Internal.Classes
import ObjC.SpriteKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Create a tile set with the specified tile groups.
--
-- @tileGroups@ — the tile groups that will be available for use with this set
--
-- ObjC selector: @+ tileSetWithTileGroups:@
tileSetWithTileGroups :: IsNSArray tileGroups => tileGroups -> IO (Id SKTileSet)
tileSetWithTileGroups tileGroups =
  do
    cls' <- getRequiredClass "SKTileSet"
    sendClassMessage cls' tileSetWithTileGroupsSelector (toNSArray tileGroups)

-- | Create a tile set with the specified tile groups and tile set type.
--
-- @tileGroups@ — the tile groups that will be available for use with this set
--
-- @tileSetType@ — the type of tile set this will be
--
-- ObjC selector: @+ tileSetWithTileGroups:tileSetType:@
tileSetWithTileGroups_tileSetType :: IsNSArray tileGroups => tileGroups -> SKTileSetType -> IO (Id SKTileSet)
tileSetWithTileGroups_tileSetType tileGroups tileSetType =
  do
    cls' <- getRequiredClass "SKTileSet"
    sendClassMessage cls' tileSetWithTileGroups_tileSetTypeSelector (toNSArray tileGroups) tileSetType

-- | Initilize a tile set with the specified tile groups.
--
-- @tileGroups@ — the tile groups that will be available for use with this set
--
-- ObjC selector: @- initWithTileGroups:@
initWithTileGroups :: (IsSKTileSet skTileSet, IsNSArray tileGroups) => skTileSet -> tileGroups -> IO (Id SKTileSet)
initWithTileGroups skTileSet tileGroups =
  sendOwnedMessage skTileSet initWithTileGroupsSelector (toNSArray tileGroups)

-- | Initilize a tile set with the specified tile groups and tile set type.
--
-- @tileGroups@ — the tile groups that will be available for use with this set
--
-- @tileSetType@ — the type of tile set this will be
--
-- ObjC selector: @- initWithTileGroups:tileSetType:@
initWithTileGroups_tileSetType :: (IsSKTileSet skTileSet, IsNSArray tileGroups) => skTileSet -> tileGroups -> SKTileSetType -> IO (Id SKTileSet)
initWithTileGroups_tileSetType skTileSet tileGroups tileSetType =
  sendOwnedMessage skTileSet initWithTileGroups_tileSetTypeSelector (toNSArray tileGroups) tileSetType

-- | Gets the tile set with the specified name from the SpriteKit resource bundle. Returns nil if a tile set with a matching name cannot be found.
--
-- @name@ — the name of the tile set to search for
--
-- ObjC selector: @+ tileSetNamed:@
tileSetNamed :: IsNSString name => name -> IO (Id SKTileSet)
tileSetNamed name =
  do
    cls' <- getRequiredClass "SKTileSet"
    sendClassMessage cls' tileSetNamedSelector (toNSString name)

-- | Creates a tile set from the specified tile set file. Returns nil if the URL doesn't point to a valid tile set file.
--
-- @url@ — the URL of the tile set file
--
-- ObjC selector: @+ tileSetFromURL:@
tileSetFromURL :: IsNSURL url => url -> IO (Id SKTileSet)
tileSetFromURL url =
  do
    cls' <- getRequiredClass "SKTileSet"
    sendClassMessage cls' tileSetFromURLSelector (toNSURL url)

-- | The tile groups that this set provides for use.
--
-- ObjC selector: @- tileGroups@
tileGroups :: IsSKTileSet skTileSet => skTileSet -> IO (Id NSArray)
tileGroups skTileSet =
  sendMessage skTileSet tileGroupsSelector

-- | The tile groups that this set provides for use.
--
-- ObjC selector: @- setTileGroups:@
setTileGroups :: (IsSKTileSet skTileSet, IsNSArray value) => skTileSet -> value -> IO ()
setTileGroups skTileSet value =
  sendMessage skTileSet setTileGroupsSelector (toNSArray value)

-- | Client-assignable name for the tile set. Defaults to nil.
--
-- ObjC selector: @- name@
name :: IsSKTileSet skTileSet => skTileSet -> IO (Id NSString)
name skTileSet =
  sendMessage skTileSet nameSelector

-- | Client-assignable name for the tile set. Defaults to nil.
--
-- ObjC selector: @- setName:@
setName :: (IsSKTileSet skTileSet, IsNSString value) => skTileSet -> value -> IO ()
setName skTileSet value =
  sendMessage skTileSet setNameSelector (toNSString value)

-- | The tile set type specifies how the tiles in the set will be arranged when placed in a tile map. Defaults to SKTileSetTypeGrid.
--
-- ObjC selector: @- type@
type_ :: IsSKTileSet skTileSet => skTileSet -> IO SKTileSetType
type_ skTileSet =
  sendMessage skTileSet typeSelector

-- | The tile set type specifies how the tiles in the set will be arranged when placed in a tile map. Defaults to SKTileSetTypeGrid.
--
-- ObjC selector: @- setType:@
setType :: IsSKTileSet skTileSet => skTileSet -> SKTileSetType -> IO ()
setType skTileSet value =
  sendMessage skTileSet setTypeSelector value

-- | @- defaultTileGroup@
defaultTileGroup :: IsSKTileSet skTileSet => skTileSet -> IO (Id SKTileGroup)
defaultTileGroup skTileSet =
  sendMessage skTileSet defaultTileGroupSelector

-- | @- setDefaultTileGroup:@
setDefaultTileGroup :: (IsSKTileSet skTileSet, IsSKTileGroup value) => skTileSet -> value -> IO ()
setDefaultTileGroup skTileSet value =
  sendMessage skTileSet setDefaultTileGroupSelector (toSKTileGroup value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @tileSetWithTileGroups:@
tileSetWithTileGroupsSelector :: Selector '[Id NSArray] (Id SKTileSet)
tileSetWithTileGroupsSelector = mkSelector "tileSetWithTileGroups:"

-- | @Selector@ for @tileSetWithTileGroups:tileSetType:@
tileSetWithTileGroups_tileSetTypeSelector :: Selector '[Id NSArray, SKTileSetType] (Id SKTileSet)
tileSetWithTileGroups_tileSetTypeSelector = mkSelector "tileSetWithTileGroups:tileSetType:"

-- | @Selector@ for @initWithTileGroups:@
initWithTileGroupsSelector :: Selector '[Id NSArray] (Id SKTileSet)
initWithTileGroupsSelector = mkSelector "initWithTileGroups:"

-- | @Selector@ for @initWithTileGroups:tileSetType:@
initWithTileGroups_tileSetTypeSelector :: Selector '[Id NSArray, SKTileSetType] (Id SKTileSet)
initWithTileGroups_tileSetTypeSelector = mkSelector "initWithTileGroups:tileSetType:"

-- | @Selector@ for @tileSetNamed:@
tileSetNamedSelector :: Selector '[Id NSString] (Id SKTileSet)
tileSetNamedSelector = mkSelector "tileSetNamed:"

-- | @Selector@ for @tileSetFromURL:@
tileSetFromURLSelector :: Selector '[Id NSURL] (Id SKTileSet)
tileSetFromURLSelector = mkSelector "tileSetFromURL:"

-- | @Selector@ for @tileGroups@
tileGroupsSelector :: Selector '[] (Id NSArray)
tileGroupsSelector = mkSelector "tileGroups"

-- | @Selector@ for @setTileGroups:@
setTileGroupsSelector :: Selector '[Id NSArray] ()
setTileGroupsSelector = mkSelector "setTileGroups:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] SKTileSetType
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector '[SKTileSetType] ()
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @defaultTileGroup@
defaultTileGroupSelector :: Selector '[] (Id SKTileGroup)
defaultTileGroupSelector = mkSelector "defaultTileGroup"

-- | @Selector@ for @setDefaultTileGroup:@
setDefaultTileGroupSelector :: Selector '[Id SKTileGroup] ()
setDefaultTileGroupSelector = mkSelector "setDefaultTileGroup:"

