{-# LANGUAGE PatternSynonyms #-}
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
  , tileSetWithTileGroupsSelector
  , tileSetWithTileGroups_tileSetTypeSelector
  , initWithTileGroupsSelector
  , initWithTileGroups_tileSetTypeSelector
  , tileSetNamedSelector
  , tileSetFromURLSelector
  , tileGroupsSelector
  , setTileGroupsSelector
  , nameSelector
  , setNameSelector
  , typeSelector
  , setTypeSelector
  , defaultTileGroupSelector
  , setDefaultTileGroupSelector

  -- * Enum types
  , SKTileSetType(SKTileSetType)
  , pattern SKTileSetTypeGrid
  , pattern SKTileSetTypeIsometric
  , pattern SKTileSetTypeHexagonalFlat
  , pattern SKTileSetTypeHexagonalPointy

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
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
    withObjCPtr tileGroups $ \raw_tileGroups ->
      sendClassMsg cls' (mkSelector "tileSetWithTileGroups:") (retPtr retVoid) [argPtr (castPtr raw_tileGroups :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr tileGroups $ \raw_tileGroups ->
      sendClassMsg cls' (mkSelector "tileSetWithTileGroups:tileSetType:") (retPtr retVoid) [argPtr (castPtr raw_tileGroups :: Ptr ()), argCULong (coerce tileSetType)] >>= retainedObject . castPtr

-- | Initilize a tile set with the specified tile groups.
--
-- @tileGroups@ — the tile groups that will be available for use with this set
--
-- ObjC selector: @- initWithTileGroups:@
initWithTileGroups :: (IsSKTileSet skTileSet, IsNSArray tileGroups) => skTileSet -> tileGroups -> IO (Id SKTileSet)
initWithTileGroups skTileSet  tileGroups =
withObjCPtr tileGroups $ \raw_tileGroups ->
    sendMsg skTileSet (mkSelector "initWithTileGroups:") (retPtr retVoid) [argPtr (castPtr raw_tileGroups :: Ptr ())] >>= ownedObject . castPtr

-- | Initilize a tile set with the specified tile groups and tile set type.
--
-- @tileGroups@ — the tile groups that will be available for use with this set
--
-- @tileSetType@ — the type of tile set this will be
--
-- ObjC selector: @- initWithTileGroups:tileSetType:@
initWithTileGroups_tileSetType :: (IsSKTileSet skTileSet, IsNSArray tileGroups) => skTileSet -> tileGroups -> SKTileSetType -> IO (Id SKTileSet)
initWithTileGroups_tileSetType skTileSet  tileGroups tileSetType =
withObjCPtr tileGroups $ \raw_tileGroups ->
    sendMsg skTileSet (mkSelector "initWithTileGroups:tileSetType:") (retPtr retVoid) [argPtr (castPtr raw_tileGroups :: Ptr ()), argCULong (coerce tileSetType)] >>= ownedObject . castPtr

-- | Gets the tile set with the specified name from the SpriteKit resource bundle. Returns nil if a tile set with a matching name cannot be found.
--
-- @name@ — the name of the tile set to search for
--
-- ObjC selector: @+ tileSetNamed:@
tileSetNamed :: IsNSString name => name -> IO (Id SKTileSet)
tileSetNamed name =
  do
    cls' <- getRequiredClass "SKTileSet"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "tileSetNamed:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | Creates a tile set from the specified tile set file. Returns nil if the URL doesn't point to a valid tile set file.
--
-- @url@ — the URL of the tile set file
--
-- ObjC selector: @+ tileSetFromURL:@
tileSetFromURL :: IsNSURL url => url -> IO (Id SKTileSet)
tileSetFromURL url =
  do
    cls' <- getRequiredClass "SKTileSet"
    withObjCPtr url $ \raw_url ->
      sendClassMsg cls' (mkSelector "tileSetFromURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | The tile groups that this set provides for use.
--
-- ObjC selector: @- tileGroups@
tileGroups :: IsSKTileSet skTileSet => skTileSet -> IO (Id NSArray)
tileGroups skTileSet  =
  sendMsg skTileSet (mkSelector "tileGroups") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The tile groups that this set provides for use.
--
-- ObjC selector: @- setTileGroups:@
setTileGroups :: (IsSKTileSet skTileSet, IsNSArray value) => skTileSet -> value -> IO ()
setTileGroups skTileSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg skTileSet (mkSelector "setTileGroups:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Client-assignable name for the tile set. Defaults to nil.
--
-- ObjC selector: @- name@
name :: IsSKTileSet skTileSet => skTileSet -> IO (Id NSString)
name skTileSet  =
  sendMsg skTileSet (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Client-assignable name for the tile set. Defaults to nil.
--
-- ObjC selector: @- setName:@
setName :: (IsSKTileSet skTileSet, IsNSString value) => skTileSet -> value -> IO ()
setName skTileSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg skTileSet (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The tile set type specifies how the tiles in the set will be arranged when placed in a tile map. Defaults to SKTileSetTypeGrid.
--
-- ObjC selector: @- type@
type_ :: IsSKTileSet skTileSet => skTileSet -> IO SKTileSetType
type_ skTileSet  =
  fmap (coerce :: CULong -> SKTileSetType) $ sendMsg skTileSet (mkSelector "type") retCULong []

-- | The tile set type specifies how the tiles in the set will be arranged when placed in a tile map. Defaults to SKTileSetTypeGrid.
--
-- ObjC selector: @- setType:@
setType :: IsSKTileSet skTileSet => skTileSet -> SKTileSetType -> IO ()
setType skTileSet  value =
  sendMsg skTileSet (mkSelector "setType:") retVoid [argCULong (coerce value)]

-- | @- defaultTileGroup@
defaultTileGroup :: IsSKTileSet skTileSet => skTileSet -> IO (Id SKTileGroup)
defaultTileGroup skTileSet  =
  sendMsg skTileSet (mkSelector "defaultTileGroup") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDefaultTileGroup:@
setDefaultTileGroup :: (IsSKTileSet skTileSet, IsSKTileGroup value) => skTileSet -> value -> IO ()
setDefaultTileGroup skTileSet  value =
withObjCPtr value $ \raw_value ->
    sendMsg skTileSet (mkSelector "setDefaultTileGroup:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @tileSetWithTileGroups:@
tileSetWithTileGroupsSelector :: Selector
tileSetWithTileGroupsSelector = mkSelector "tileSetWithTileGroups:"

-- | @Selector@ for @tileSetWithTileGroups:tileSetType:@
tileSetWithTileGroups_tileSetTypeSelector :: Selector
tileSetWithTileGroups_tileSetTypeSelector = mkSelector "tileSetWithTileGroups:tileSetType:"

-- | @Selector@ for @initWithTileGroups:@
initWithTileGroupsSelector :: Selector
initWithTileGroupsSelector = mkSelector "initWithTileGroups:"

-- | @Selector@ for @initWithTileGroups:tileSetType:@
initWithTileGroups_tileSetTypeSelector :: Selector
initWithTileGroups_tileSetTypeSelector = mkSelector "initWithTileGroups:tileSetType:"

-- | @Selector@ for @tileSetNamed:@
tileSetNamedSelector :: Selector
tileSetNamedSelector = mkSelector "tileSetNamed:"

-- | @Selector@ for @tileSetFromURL:@
tileSetFromURLSelector :: Selector
tileSetFromURLSelector = mkSelector "tileSetFromURL:"

-- | @Selector@ for @tileGroups@
tileGroupsSelector :: Selector
tileGroupsSelector = mkSelector "tileGroups"

-- | @Selector@ for @setTileGroups:@
setTileGroupsSelector :: Selector
setTileGroupsSelector = mkSelector "setTileGroups:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @defaultTileGroup@
defaultTileGroupSelector :: Selector
defaultTileGroupSelector = mkSelector "defaultTileGroup"

-- | @Selector@ for @setDefaultTileGroup:@
setDefaultTileGroupSelector :: Selector
setDefaultTileGroupSelector = mkSelector "setDefaultTileGroup:"

