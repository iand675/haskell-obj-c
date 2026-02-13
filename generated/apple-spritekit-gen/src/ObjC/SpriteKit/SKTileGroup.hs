{-# LANGUAGE DataKinds #-}
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
  , emptyTileGroupSelector
  , initWithRulesSelector
  , initWithTileDefinitionSelector
  , nameSelector
  , rulesSelector
  , setNameSelector
  , setRulesSelector
  , tileGroupWithRulesSelector
  , tileGroupWithTileDefinitionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' tileGroupWithTileDefinitionSelector (toSKTileDefinition tileDefinition)

-- | Create a tile group with the specified rules.
--
-- @rules@ — the rules the group will use to determine tile placement
--
-- ObjC selector: @+ tileGroupWithRules:@
tileGroupWithRules :: IsNSArray rules => rules -> IO (Id SKTileGroup)
tileGroupWithRules rules =
  do
    cls' <- getRequiredClass "SKTileGroup"
    sendClassMessage cls' tileGroupWithRulesSelector (toNSArray rules)

-- | Create an empty tile group. Placing this in a tile map will erase the existing tile at that location.
--
-- ObjC selector: @+ emptyTileGroup@
emptyTileGroup :: IO (Id SKTileGroup)
emptyTileGroup  =
  do
    cls' <- getRequiredClass "SKTileGroup"
    sendClassMessage cls' emptyTileGroupSelector

-- | Initilize a simple tile group for a single tile definition. This creates and initializes the SKTileGroupRule necessary to place the provided tile definition in a tile map.
--
-- @tileDefinition@ — tile definition we wish to place in a tile map
--
-- ObjC selector: @- initWithTileDefinition:@
initWithTileDefinition :: (IsSKTileGroup skTileGroup, IsSKTileDefinition tileDefinition) => skTileGroup -> tileDefinition -> IO (Id SKTileGroup)
initWithTileDefinition skTileGroup tileDefinition =
  sendOwnedMessage skTileGroup initWithTileDefinitionSelector (toSKTileDefinition tileDefinition)

-- | Initilize a tile group with the specified rules.
--
-- @rules@ — the rules the group will use to determine tile placement
--
-- ObjC selector: @- initWithRules:@
initWithRules :: (IsSKTileGroup skTileGroup, IsNSArray rules) => skTileGroup -> rules -> IO (Id SKTileGroup)
initWithRules skTileGroup rules =
  sendOwnedMessage skTileGroup initWithRulesSelector (toNSArray rules)

-- | The rules that govern which tiles are placed when this group is used, and where in the map they'll be placed.
--
-- ObjC selector: @- rules@
rules :: IsSKTileGroup skTileGroup => skTileGroup -> IO (Id NSArray)
rules skTileGroup =
  sendMessage skTileGroup rulesSelector

-- | The rules that govern which tiles are placed when this group is used, and where in the map they'll be placed.
--
-- ObjC selector: @- setRules:@
setRules :: (IsSKTileGroup skTileGroup, IsNSArray value) => skTileGroup -> value -> IO ()
setRules skTileGroup value =
  sendMessage skTileGroup setRulesSelector (toNSArray value)

-- | Client-assignable name for the tile group. Defaults to nil.
--
-- ObjC selector: @- name@
name :: IsSKTileGroup skTileGroup => skTileGroup -> IO (Id NSString)
name skTileGroup =
  sendMessage skTileGroup nameSelector

-- | Client-assignable name for the tile group. Defaults to nil.
--
-- ObjC selector: @- setName:@
setName :: (IsSKTileGroup skTileGroup, IsNSString value) => skTileGroup -> value -> IO ()
setName skTileGroup value =
  sendMessage skTileGroup setNameSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @tileGroupWithTileDefinition:@
tileGroupWithTileDefinitionSelector :: Selector '[Id SKTileDefinition] (Id SKTileGroup)
tileGroupWithTileDefinitionSelector = mkSelector "tileGroupWithTileDefinition:"

-- | @Selector@ for @tileGroupWithRules:@
tileGroupWithRulesSelector :: Selector '[Id NSArray] (Id SKTileGroup)
tileGroupWithRulesSelector = mkSelector "tileGroupWithRules:"

-- | @Selector@ for @emptyTileGroup@
emptyTileGroupSelector :: Selector '[] (Id SKTileGroup)
emptyTileGroupSelector = mkSelector "emptyTileGroup"

-- | @Selector@ for @initWithTileDefinition:@
initWithTileDefinitionSelector :: Selector '[Id SKTileDefinition] (Id SKTileGroup)
initWithTileDefinitionSelector = mkSelector "initWithTileDefinition:"

-- | @Selector@ for @initWithRules:@
initWithRulesSelector :: Selector '[Id NSArray] (Id SKTileGroup)
initWithRulesSelector = mkSelector "initWithRules:"

-- | @Selector@ for @rules@
rulesSelector :: Selector '[] (Id NSArray)
rulesSelector = mkSelector "rules"

-- | @Selector@ for @setRules:@
setRulesSelector :: Selector '[Id NSArray] ()
setRulesSelector = mkSelector "setRules:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

