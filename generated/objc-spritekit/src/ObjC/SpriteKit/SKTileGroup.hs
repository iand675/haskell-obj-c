{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A tile group encapsulates a collection of related tile definitions that are designed to be pieced together within a tile map. How those tiles are pieced together is governed by the set of rules. When a tile group is placed in a tile map, the map evaluates the rules to determine which tiles should be placed to achieve the desired outcome.
--
-- Generated bindings for @SKTileGroup@.
module ObjC.SpriteKit.SKTileGroup
  ( SKTileGroup
  , IsSKTileGroup(..)
  , tileGroupWithTileDefinition
  , tileGroupWithRules
  , emptyTileGroup
  , initWithTileDefinition
  , initWithRules
  , rules
  , setRules
  , name
  , setName
  , tileGroupWithTileDefinitionSelector
  , tileGroupWithRulesSelector
  , emptyTileGroupSelector
  , initWithTileDefinitionSelector
  , initWithRulesSelector
  , rulesSelector
  , setRulesSelector
  , nameSelector
  , setNameSelector


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

import ObjC.SpriteKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a simple tile group for a single tile definition. This creates and initializes the SKTileGroupRule necessary to place the provided tile definition in a tile map.
--
-- @tileDefinition@ — the tile definition we wish to place in a tile map
--
-- ObjC selector: @+ tileGroupWithTileDefinition:@
tileGroupWithTileDefinition :: IsSKTileDefinition tileDefinition => tileDefinition -> IO (Id SKTileGroup)
tileGroupWithTileDefinition tileDefinition =
  do
    cls' <- getRequiredClass "SKTileGroup"
    withObjCPtr tileDefinition $ \raw_tileDefinition ->
      sendClassMsg cls' (mkSelector "tileGroupWithTileDefinition:") (retPtr retVoid) [argPtr (castPtr raw_tileDefinition :: Ptr ())] >>= retainedObject . castPtr

-- | Create a tile group with the specified rules.
--
-- @rules@ — the rules the group will use to determine tile placement
--
-- ObjC selector: @+ tileGroupWithRules:@
tileGroupWithRules :: IsNSArray rules => rules -> IO (Id SKTileGroup)
tileGroupWithRules rules =
  do
    cls' <- getRequiredClass "SKTileGroup"
    withObjCPtr rules $ \raw_rules ->
      sendClassMsg cls' (mkSelector "tileGroupWithRules:") (retPtr retVoid) [argPtr (castPtr raw_rules :: Ptr ())] >>= retainedObject . castPtr

-- | Create an empty tile group. Placing this in a tile map will erase the existing tile at that location.
--
-- ObjC selector: @+ emptyTileGroup@
emptyTileGroup :: IO (Id SKTileGroup)
emptyTileGroup  =
  do
    cls' <- getRequiredClass "SKTileGroup"
    sendClassMsg cls' (mkSelector "emptyTileGroup") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Initilize a simple tile group for a single tile definition. This creates and initializes the SKTileGroupRule necessary to place the provided tile definition in a tile map.
--
-- @tileDefinition@ — tile definition we wish to place in a tile map
--
-- ObjC selector: @- initWithTileDefinition:@
initWithTileDefinition :: (IsSKTileGroup skTileGroup, IsSKTileDefinition tileDefinition) => skTileGroup -> tileDefinition -> IO (Id SKTileGroup)
initWithTileDefinition skTileGroup  tileDefinition =
withObjCPtr tileDefinition $ \raw_tileDefinition ->
    sendMsg skTileGroup (mkSelector "initWithTileDefinition:") (retPtr retVoid) [argPtr (castPtr raw_tileDefinition :: Ptr ())] >>= ownedObject . castPtr

-- | Initilize a tile group with the specified rules.
--
-- @rules@ — the rules the group will use to determine tile placement
--
-- ObjC selector: @- initWithRules:@
initWithRules :: (IsSKTileGroup skTileGroup, IsNSArray rules) => skTileGroup -> rules -> IO (Id SKTileGroup)
initWithRules skTileGroup  rules =
withObjCPtr rules $ \raw_rules ->
    sendMsg skTileGroup (mkSelector "initWithRules:") (retPtr retVoid) [argPtr (castPtr raw_rules :: Ptr ())] >>= ownedObject . castPtr

-- | The rules that govern which tiles are placed when this group is used, and where in the map they'll be placed.
--
-- ObjC selector: @- rules@
rules :: IsSKTileGroup skTileGroup => skTileGroup -> IO (Id NSArray)
rules skTileGroup  =
  sendMsg skTileGroup (mkSelector "rules") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The rules that govern which tiles are placed when this group is used, and where in the map they'll be placed.
--
-- ObjC selector: @- setRules:@
setRules :: (IsSKTileGroup skTileGroup, IsNSArray value) => skTileGroup -> value -> IO ()
setRules skTileGroup  value =
withObjCPtr value $ \raw_value ->
    sendMsg skTileGroup (mkSelector "setRules:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Client-assignable name for the tile group. Defaults to nil.
--
-- ObjC selector: @- name@
name :: IsSKTileGroup skTileGroup => skTileGroup -> IO (Id NSString)
name skTileGroup  =
  sendMsg skTileGroup (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Client-assignable name for the tile group. Defaults to nil.
--
-- ObjC selector: @- setName:@
setName :: (IsSKTileGroup skTileGroup, IsNSString value) => skTileGroup -> value -> IO ()
setName skTileGroup  value =
withObjCPtr value $ \raw_value ->
    sendMsg skTileGroup (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @tileGroupWithTileDefinition:@
tileGroupWithTileDefinitionSelector :: Selector
tileGroupWithTileDefinitionSelector = mkSelector "tileGroupWithTileDefinition:"

-- | @Selector@ for @tileGroupWithRules:@
tileGroupWithRulesSelector :: Selector
tileGroupWithRulesSelector = mkSelector "tileGroupWithRules:"

-- | @Selector@ for @emptyTileGroup@
emptyTileGroupSelector :: Selector
emptyTileGroupSelector = mkSelector "emptyTileGroup"

-- | @Selector@ for @initWithTileDefinition:@
initWithTileDefinitionSelector :: Selector
initWithTileDefinitionSelector = mkSelector "initWithTileDefinition:"

-- | @Selector@ for @initWithRules:@
initWithRulesSelector :: Selector
initWithRulesSelector = mkSelector "initWithRules:"

-- | @Selector@ for @rules@
rulesSelector :: Selector
rulesSelector = mkSelector "rules"

-- | @Selector@ for @setRules:@
setRulesSelector :: Selector
setRulesSelector = mkSelector "setRules:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

