{-# LANGUAGE PatternSynonyms #-}
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
  , tileGroupRuleWithAdjacency_tileDefinitionsSelector
  , initWithAdjacency_tileDefinitionsSelector
  , adjacencySelector
  , setAdjacencySelector
  , tileDefinitionsSelector
  , setTileDefinitionsSelector
  , nameSelector
  , setNameSelector

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
    withObjCPtr tileDefinitions $ \raw_tileDefinitions ->
      sendClassMsg cls' (mkSelector "tileGroupRuleWithAdjacency:tileDefinitions:") (retPtr retVoid) [argCULong (coerce adjacency), argPtr (castPtr raw_tileDefinitions :: Ptr ())] >>= retainedObject . castPtr

-- | Initilize a tile group rule with the specified adjacency and tile definitions.
--
-- @adjacency@ — the adjacency requirements for this rule; use the mask that covers the adjacent spaces that must be filled with tiles belonging to the same group; tiles not masked out must be empty
--
-- @tileDefinitions@ — the tile definitions used for this rule
--
-- ObjC selector: @- initWithAdjacency:tileDefinitions:@
initWithAdjacency_tileDefinitions :: (IsSKTileGroupRule skTileGroupRule, IsNSArray tileDefinitions) => skTileGroupRule -> SKTileAdjacencyMask -> tileDefinitions -> IO (Id SKTileGroupRule)
initWithAdjacency_tileDefinitions skTileGroupRule  adjacency tileDefinitions =
withObjCPtr tileDefinitions $ \raw_tileDefinitions ->
    sendMsg skTileGroupRule (mkSelector "initWithAdjacency:tileDefinitions:") (retPtr retVoid) [argCULong (coerce adjacency), argPtr (castPtr raw_tileDefinitions :: Ptr ())] >>= ownedObject . castPtr

-- | The adjacency mask used by this rule. Set this to the mask that covers the adjacent spaces that must be filled with tiles belonging to the same group for this rule met.
--
-- ObjC selector: @- adjacency@
adjacency :: IsSKTileGroupRule skTileGroupRule => skTileGroupRule -> IO SKTileAdjacencyMask
adjacency skTileGroupRule  =
  fmap (coerce :: CULong -> SKTileAdjacencyMask) $ sendMsg skTileGroupRule (mkSelector "adjacency") retCULong []

-- | The adjacency mask used by this rule. Set this to the mask that covers the adjacent spaces that must be filled with tiles belonging to the same group for this rule met.
--
-- ObjC selector: @- setAdjacency:@
setAdjacency :: IsSKTileGroupRule skTileGroupRule => skTileGroupRule -> SKTileAdjacencyMask -> IO ()
setAdjacency skTileGroupRule  value =
  sendMsg skTileGroupRule (mkSelector "setAdjacency:") retVoid [argCULong (coerce value)]

-- | The tile definitions used by this rule. If the rule is evaluated and its conditions are met, one of the tile definitions within this array will be randomly selected for placement within the tile map. Each tile definitions' placement weight is taken into consideration to determine how likely each is to be selected; tile definitions with higher placement weights will be selected more frequently than those with lower placement weights.
--
-- ObjC selector: @- tileDefinitions@
tileDefinitions :: IsSKTileGroupRule skTileGroupRule => skTileGroupRule -> IO (Id NSArray)
tileDefinitions skTileGroupRule  =
  sendMsg skTileGroupRule (mkSelector "tileDefinitions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The tile definitions used by this rule. If the rule is evaluated and its conditions are met, one of the tile definitions within this array will be randomly selected for placement within the tile map. Each tile definitions' placement weight is taken into consideration to determine how likely each is to be selected; tile definitions with higher placement weights will be selected more frequently than those with lower placement weights.
--
-- ObjC selector: @- setTileDefinitions:@
setTileDefinitions :: (IsSKTileGroupRule skTileGroupRule, IsNSArray value) => skTileGroupRule -> value -> IO ()
setTileDefinitions skTileGroupRule  value =
withObjCPtr value $ \raw_value ->
    sendMsg skTileGroupRule (mkSelector "setTileDefinitions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Client-assignable name for the tile group rule. Defaults to nil.
--
-- ObjC selector: @- name@
name :: IsSKTileGroupRule skTileGroupRule => skTileGroupRule -> IO (Id NSString)
name skTileGroupRule  =
  sendMsg skTileGroupRule (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Client-assignable name for the tile group rule. Defaults to nil.
--
-- ObjC selector: @- setName:@
setName :: (IsSKTileGroupRule skTileGroupRule, IsNSString value) => skTileGroupRule -> value -> IO ()
setName skTileGroupRule  value =
withObjCPtr value $ \raw_value ->
    sendMsg skTileGroupRule (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @tileGroupRuleWithAdjacency:tileDefinitions:@
tileGroupRuleWithAdjacency_tileDefinitionsSelector :: Selector
tileGroupRuleWithAdjacency_tileDefinitionsSelector = mkSelector "tileGroupRuleWithAdjacency:tileDefinitions:"

-- | @Selector@ for @initWithAdjacency:tileDefinitions:@
initWithAdjacency_tileDefinitionsSelector :: Selector
initWithAdjacency_tileDefinitionsSelector = mkSelector "initWithAdjacency:tileDefinitions:"

-- | @Selector@ for @adjacency@
adjacencySelector :: Selector
adjacencySelector = mkSelector "adjacency"

-- | @Selector@ for @setAdjacency:@
setAdjacencySelector :: Selector
setAdjacencySelector = mkSelector "setAdjacency:"

-- | @Selector@ for @tileDefinitions@
tileDefinitionsSelector :: Selector
tileDefinitionsSelector = mkSelector "tileDefinitions"

-- | @Selector@ for @setTileDefinitions:@
setTileDefinitionsSelector :: Selector
setTileDefinitionsSelector = mkSelector "setTileDefinitions:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

