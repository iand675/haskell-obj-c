{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A tile group rule defines how a certain type of tile should be placed on the map. These tiles are like puzzle pieces, and the rules define how they should be pieced together. This is accomplished by defining which neighboring spaces need to be filled with tiles that belong to the same group, and which tiles are required to be empty. The required pattern of neighboring tiles is defined using the SKTileAdjacencyMask.
--
-- Generated bindings for @SKTileGroupRule@.
module ObjC.SpriteKit.SKTileGroupRule
  ( SKTileGroupRule
  , IsSKTileGroupRule(..)
  , tileGroupRuleWithAdjacency_tileDefinitions
  , initWithAdjacency_tileDefinitions
  , adjacency
  , setAdjacency
  , tileDefinitions
  , setTileDefinitions
  , name
  , setName
  , adjacencySelector
  , initWithAdjacency_tileDefinitionsSelector
  , nameSelector
  , setAdjacencySelector
  , setNameSelector
  , setTileDefinitionsSelector
  , tileDefinitionsSelector
  , tileGroupRuleWithAdjacency_tileDefinitionsSelector

  -- * Enum types
  , SKTileAdjacencyMask(SKTileAdjacencyMask)
  , pattern SKTileAdjacencyUp
  , pattern SKTileAdjacencyUpperRight
  , pattern SKTileAdjacencyRight
  , pattern SKTileAdjacencyLowerRight
  , pattern SKTileAdjacencyDown
  , pattern SKTileAdjacencyLowerLeft
  , pattern SKTileAdjacencyLeft
  , pattern SKTileAdjacencyUpperLeft
  , pattern SKTileAdjacencyAll
  , pattern SKTileHexFlatAdjacencyUp
  , pattern SKTileHexFlatAdjacencyUpperRight
  , pattern SKTileHexFlatAdjacencyLowerRight
  , pattern SKTileHexFlatAdjacencyDown
  , pattern SKTileHexFlatAdjacencyLowerLeft
  , pattern SKTileHexFlatAdjacencyUpperLeft
  , pattern SKTileHexFlatAdjacencyAll
  , pattern SKTileHexPointyAdjacencyUpperLeft
  , pattern SKTileHexPointyAdjacencyUpperRight
  , pattern SKTileHexPointyAdjacencyRight
  , pattern SKTileHexPointyAdjacencyLowerRight
  , pattern SKTileHexPointyAdjacencyLowerLeft
  , pattern SKTileHexPointyAdjacencyLeft
  , pattern SKTileHexPointyAdjacencyAdd
  , pattern SKTileAdjacencyUpEdge
  , pattern SKTileAdjacencyUpperRightEdge
  , pattern SKTileAdjacencyRightEdge
  , pattern SKTileAdjacencyLowerRightEdge
  , pattern SKTileAdjacencyDownEdge
  , pattern SKTileAdjacencyLowerLeftEdge
  , pattern SKTileAdjacencyLeftEdge
  , pattern SKTileAdjacencyUpperLeftEdge
  , pattern SKTileAdjacencyUpperRightCorner
  , pattern SKTileAdjacencyLowerRightCorner
  , pattern SKTileAdjacencyLowerLeftCorner
  , pattern SKTileAdjacencyUpperLeftCorner

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

-- | Create a tile group rule with the specified adjacency and tile definitions.
--
-- @adjacency@ — the adjacency requirements for this rule; use the mask that covers the adjacent spaces that must be filled with tiles belonging to the same group; tiles not masked out must be empty
--
-- @tileDefinitions@ — the tile definitions used for this rule
--
-- ObjC selector: @+ tileGroupRuleWithAdjacency:tileDefinitions:@
tileGroupRuleWithAdjacency_tileDefinitions :: IsNSArray tileDefinitions => SKTileAdjacencyMask -> tileDefinitions -> IO (Id SKTileGroupRule)
tileGroupRuleWithAdjacency_tileDefinitions adjacency tileDefinitions =
  do
    cls' <- getRequiredClass "SKTileGroupRule"
    sendClassMessage cls' tileGroupRuleWithAdjacency_tileDefinitionsSelector adjacency (toNSArray tileDefinitions)

-- | Initilize a tile group rule with the specified adjacency and tile definitions.
--
-- @adjacency@ — the adjacency requirements for this rule; use the mask that covers the adjacent spaces that must be filled with tiles belonging to the same group; tiles not masked out must be empty
--
-- @tileDefinitions@ — the tile definitions used for this rule
--
-- ObjC selector: @- initWithAdjacency:tileDefinitions:@
initWithAdjacency_tileDefinitions :: (IsSKTileGroupRule skTileGroupRule, IsNSArray tileDefinitions) => skTileGroupRule -> SKTileAdjacencyMask -> tileDefinitions -> IO (Id SKTileGroupRule)
initWithAdjacency_tileDefinitions skTileGroupRule adjacency tileDefinitions =
  sendOwnedMessage skTileGroupRule initWithAdjacency_tileDefinitionsSelector adjacency (toNSArray tileDefinitions)

-- | The adjacency mask used by this rule. Set this to the mask that covers the adjacent spaces that must be filled with tiles belonging to the same group for this rule met.
--
-- ObjC selector: @- adjacency@
adjacency :: IsSKTileGroupRule skTileGroupRule => skTileGroupRule -> IO SKTileAdjacencyMask
adjacency skTileGroupRule =
  sendMessage skTileGroupRule adjacencySelector

-- | The adjacency mask used by this rule. Set this to the mask that covers the adjacent spaces that must be filled with tiles belonging to the same group for this rule met.
--
-- ObjC selector: @- setAdjacency:@
setAdjacency :: IsSKTileGroupRule skTileGroupRule => skTileGroupRule -> SKTileAdjacencyMask -> IO ()
setAdjacency skTileGroupRule value =
  sendMessage skTileGroupRule setAdjacencySelector value

-- | The tile definitions used by this rule. If the rule is evaluated and its conditions are met, one of the tile definitions within this array will be randomly selected for placement within the tile map. Each tile definitions' placement weight is taken into consideration to determine how likely each is to be selected; tile definitions with higher placement weights will be selected more frequently than those with lower placement weights.
--
-- ObjC selector: @- tileDefinitions@
tileDefinitions :: IsSKTileGroupRule skTileGroupRule => skTileGroupRule -> IO (Id NSArray)
tileDefinitions skTileGroupRule =
  sendMessage skTileGroupRule tileDefinitionsSelector

-- | The tile definitions used by this rule. If the rule is evaluated and its conditions are met, one of the tile definitions within this array will be randomly selected for placement within the tile map. Each tile definitions' placement weight is taken into consideration to determine how likely each is to be selected; tile definitions with higher placement weights will be selected more frequently than those with lower placement weights.
--
-- ObjC selector: @- setTileDefinitions:@
setTileDefinitions :: (IsSKTileGroupRule skTileGroupRule, IsNSArray value) => skTileGroupRule -> value -> IO ()
setTileDefinitions skTileGroupRule value =
  sendMessage skTileGroupRule setTileDefinitionsSelector (toNSArray value)

-- | Client-assignable name for the tile group rule. Defaults to nil.
--
-- ObjC selector: @- name@
name :: IsSKTileGroupRule skTileGroupRule => skTileGroupRule -> IO (Id NSString)
name skTileGroupRule =
  sendMessage skTileGroupRule nameSelector

-- | Client-assignable name for the tile group rule. Defaults to nil.
--
-- ObjC selector: @- setName:@
setName :: (IsSKTileGroupRule skTileGroupRule, IsNSString value) => skTileGroupRule -> value -> IO ()
setName skTileGroupRule value =
  sendMessage skTileGroupRule setNameSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @tileGroupRuleWithAdjacency:tileDefinitions:@
tileGroupRuleWithAdjacency_tileDefinitionsSelector :: Selector '[SKTileAdjacencyMask, Id NSArray] (Id SKTileGroupRule)
tileGroupRuleWithAdjacency_tileDefinitionsSelector = mkSelector "tileGroupRuleWithAdjacency:tileDefinitions:"

-- | @Selector@ for @initWithAdjacency:tileDefinitions:@
initWithAdjacency_tileDefinitionsSelector :: Selector '[SKTileAdjacencyMask, Id NSArray] (Id SKTileGroupRule)
initWithAdjacency_tileDefinitionsSelector = mkSelector "initWithAdjacency:tileDefinitions:"

-- | @Selector@ for @adjacency@
adjacencySelector :: Selector '[] SKTileAdjacencyMask
adjacencySelector = mkSelector "adjacency"

-- | @Selector@ for @setAdjacency:@
setAdjacencySelector :: Selector '[SKTileAdjacencyMask] ()
setAdjacencySelector = mkSelector "setAdjacency:"

-- | @Selector@ for @tileDefinitions@
tileDefinitionsSelector :: Selector '[] (Id NSArray)
tileDefinitionsSelector = mkSelector "tileDefinitions"

-- | @Selector@ for @setTileDefinitions:@
setTileDefinitionsSelector :: Selector '[Id NSArray] ()
setTileDefinitionsSelector = mkSelector "setTileDefinitions:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

