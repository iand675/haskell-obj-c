{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.GameplayKit.Internal.Classes (
    module ObjC.GameplayKit.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
    module ObjC.SceneKit.Internal.Classes,
    module ObjC.SpriteKit.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.SceneKit.Internal.Classes
import ObjC.SpriteKit.Internal.Classes

-- ---------- GKBehavior ----------

-- | A collection of GKGoals or GKBehaviors with weights that can be applied to a GKAgent The sub-goals or sub-behaviors are summed to produce a total force to be applied to an agent
-- 
-- Phantom type for @GKBehavior@.
data GKBehavior

instance IsObjCObject (Id GKBehavior) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKBehavior"

class IsNSObject a => IsGKBehavior a where
  toGKBehavior :: a -> Id GKBehavior

instance IsGKBehavior (Id GKBehavior) where
  toGKBehavior = unsafeCastId

instance IsNSObject (Id GKBehavior) where
  toNSObject = unsafeCastId

-- ---------- GKComponent ----------

-- | A component is the data and logic for one part of an object in an entity-component system. Entities have many components but components are associated with only a single entity.
--
-- Components across entities are best arranged in ComponentSystems, which are homogeneous collections of components that the game logic updates in a deterministic order.
--
-- See: GKComponentSystem
-- 
-- Phantom type for @GKComponent@.
data GKComponent

instance IsObjCObject (Id GKComponent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKComponent"

class IsNSObject a => IsGKComponent a where
  toGKComponent :: a -> Id GKComponent

instance IsGKComponent (Id GKComponent) where
  toGKComponent = unsafeCastId

instance IsNSObject (Id GKComponent) where
  toNSObject = unsafeCastId

-- ---------- GKComponentSystem ----------

-- | A component system is a homogeneous collection of components that are intended to be called at the same time. The system is homogeneous, meaning it only allows members of the same class into the system.
-- 
-- Phantom type for @GKComponentSystem@.
data GKComponentSystem

instance IsObjCObject (Id GKComponentSystem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKComponentSystem"

class IsNSObject a => IsGKComponentSystem a where
  toGKComponentSystem :: a -> Id GKComponentSystem

instance IsGKComponentSystem (Id GKComponentSystem) where
  toGKComponentSystem = unsafeCastId

instance IsNSObject (Id GKComponentSystem) where
  toNSObject = unsafeCastId

-- ---------- GKDecisionNode ----------

-- | Phantom type for @GKDecisionNode@.
data GKDecisionNode

instance IsObjCObject (Id GKDecisionNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKDecisionNode"

class IsNSObject a => IsGKDecisionNode a where
  toGKDecisionNode :: a -> Id GKDecisionNode

instance IsGKDecisionNode (Id GKDecisionNode) where
  toGKDecisionNode = unsafeCastId

instance IsNSObject (Id GKDecisionNode) where
  toNSObject = unsafeCastId

-- ---------- GKDecisionTree ----------

-- | Phantom type for @GKDecisionTree@.
data GKDecisionTree

instance IsObjCObject (Id GKDecisionTree) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKDecisionTree"

class IsNSObject a => IsGKDecisionTree a where
  toGKDecisionTree :: a -> Id GKDecisionTree

instance IsGKDecisionTree (Id GKDecisionTree) where
  toGKDecisionTree = unsafeCastId

instance IsNSObject (Id GKDecisionTree) where
  toNSObject = unsafeCastId

-- ---------- GKEntity ----------

-- | An entity is the general purpose object in an entity-component system. Entites have many components but components are associated with only a single entity.
--
-- Note: GKEntity supports NSCopying and NSSecureCoding, but your custom GKComponent's must also support NSCopying and NSSecureCoding
--
-- See: GKComponent
--
-- See: GKComponentSystem
-- 
-- Phantom type for @GKEntity@.
data GKEntity

instance IsObjCObject (Id GKEntity) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKEntity"

class IsNSObject a => IsGKEntity a where
  toGKEntity :: a -> Id GKEntity

instance IsGKEntity (Id GKEntity) where
  toGKEntity = unsafeCastId

instance IsNSObject (Id GKEntity) where
  toNSObject = unsafeCastId

-- ---------- GKGoal ----------

-- | Defines a spatial directive. The various goals cause force to be applied to agents to try to achieve said goal.
-- 
-- Phantom type for @GKGoal@.
data GKGoal

instance IsObjCObject (Id GKGoal) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKGoal"

class IsNSObject a => IsGKGoal a where
  toGKGoal :: a -> Id GKGoal

instance IsGKGoal (Id GKGoal) where
  toGKGoal = unsafeCastId

instance IsNSObject (Id GKGoal) where
  toNSObject = unsafeCastId

-- ---------- GKGraph ----------

-- | Representation of a directed graph of GKGraphNodes
-- 
-- Phantom type for @GKGraph@.
data GKGraph

instance IsObjCObject (Id GKGraph) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKGraph"

class IsNSObject a => IsGKGraph a where
  toGKGraph :: a -> Id GKGraph

instance IsGKGraph (Id GKGraph) where
  toGKGraph = unsafeCastId

instance IsNSObject (Id GKGraph) where
  toNSObject = unsafeCastId

-- ---------- GKGraphNode ----------

-- | A node in a directed graph. Edges are directed and can have variable costs.
-- 
-- Phantom type for @GKGraphNode@.
data GKGraphNode

instance IsObjCObject (Id GKGraphNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKGraphNode"

class IsNSObject a => IsGKGraphNode a where
  toGKGraphNode :: a -> Id GKGraphNode

instance IsGKGraphNode (Id GKGraphNode) where
  toGKGraphNode = unsafeCastId

instance IsNSObject (Id GKGraphNode) where
  toNSObject = unsafeCastId

-- ---------- GKMinmaxStrategist ----------

-- | The Minmax Strategist is a generic AI that selects a game model update for a given player that maximises  potential gain, while minimising potential loss. It does this by examining all of the updates available  to the player in question, extrapolating the potential moves opposing players may take, projecting out  maxLookAheadDepth number of turns. The selected update will result in the greatest potential gain, balanced  against the potential gain of other players.
-- 
-- Phantom type for @GKMinmaxStrategist@.
data GKMinmaxStrategist

instance IsObjCObject (Id GKMinmaxStrategist) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKMinmaxStrategist"

class IsNSObject a => IsGKMinmaxStrategist a where
  toGKMinmaxStrategist :: a -> Id GKMinmaxStrategist

instance IsGKMinmaxStrategist (Id GKMinmaxStrategist) where
  toGKMinmaxStrategist = unsafeCastId

instance IsNSObject (Id GKMinmaxStrategist) where
  toNSObject = unsafeCastId

-- ---------- GKMonteCarloStrategist ----------

-- | The Monte Carlo Strategist is a generic AI that selects a game model update for a given player that results in the highest likelihood for that player to eventually win the game. It does this by sampling the updates available to the player in question. In doing this it will select the update it knows to produce the best result so far, expanding on this selection, simulating the rest of the game from that expansion, and then propogating the results (win or loss) upwards. It will do this until the budget has been reached, then returning the choice it has deemed best suited for the player in question.
-- 
-- Phantom type for @GKMonteCarloStrategist@.
data GKMonteCarloStrategist

instance IsObjCObject (Id GKMonteCarloStrategist) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKMonteCarloStrategist"

class IsNSObject a => IsGKMonteCarloStrategist a where
  toGKMonteCarloStrategist :: a -> Id GKMonteCarloStrategist

instance IsGKMonteCarloStrategist (Id GKMonteCarloStrategist) where
  toGKMonteCarloStrategist = unsafeCastId

instance IsNSObject (Id GKMonteCarloStrategist) where
  toNSObject = unsafeCastId

-- ---------- GKNoise ----------

-- | GKNoise is the object used to manipulate and combine noise in continuous 3D space.  It takes a GKNoiseSource as input. To extract and use a portion of the noise within the 3D space use the GKNoiseMap class.
--
-- See: GKNoiseSource
--
-- See: GKNoiseMap
-- 
-- Phantom type for @GKNoise@.
data GKNoise

instance IsObjCObject (Id GKNoise) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKNoise"

class IsNSObject a => IsGKNoise a where
  toGKNoise :: a -> Id GKNoise

instance IsGKNoise (Id GKNoise) where
  toGKNoise = unsafeCastId

instance IsNSObject (Id GKNoise) where
  toNSObject = unsafeCastId

-- ---------- GKNoiseMap ----------

-- | GKNoiseMap represents an extracted portion of sampled points from continuous 3D noise.  Extracted values are useful for 2D and 3D games.  Noise values may be queried, set to explicit values or used as input for other uses, including textures and tile maps.
--
-- See: GKNoiseSource
--
-- See: GKNoise
--
-- See: SKTexture
--
-- See: SKTileMapNode
-- 
-- Phantom type for @GKNoiseMap@.
data GKNoiseMap

instance IsObjCObject (Id GKNoiseMap) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKNoiseMap"

class IsNSObject a => IsGKNoiseMap a where
  toGKNoiseMap :: a -> Id GKNoiseMap

instance IsGKNoiseMap (Id GKNoiseMap) where
  toGKNoiseMap = unsafeCastId

instance IsNSObject (Id GKNoiseMap) where
  toNSObject = unsafeCastId

-- ---------- GKNoiseSource ----------

-- | A GKNoiseSource instance is a description of procedural noise in 3D space.  Noise sources generate values between -1.0 and 1.0, inclusive, for any position in continuous 3D space. Subclasses represent specific types of noise, each with their own parameters that affect the nature of the noise. Noise sources are the starting point for generating and using procedural noise.  The 3D noise values may be manipulated and combined with the GKNoise class.  Portions of this 3D noise can be extracted and utilized via the GKNoiseMap class. Extracted portions of noise are useful in both 2D and 3D games.  Applications include creating realistic textures, height maps for 2D and 3D game world terrain, tile maps for 2D games, and intentionally imperfect game object and camera movements in 2D and 3D games. This class is not intended to be instantiated.
--
-- See: GKNoise
--
-- See: GKNoiseMap
-- 
-- Phantom type for @GKNoiseSource@.
data GKNoiseSource

instance IsObjCObject (Id GKNoiseSource) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKNoiseSource"

class IsNSObject a => IsGKNoiseSource a where
  toGKNoiseSource :: a -> Id GKNoiseSource

instance IsGKNoiseSource (Id GKNoiseSource) where
  toGKNoiseSource = unsafeCastId

instance IsNSObject (Id GKNoiseSource) where
  toNSObject = unsafeCastId

-- ---------- GKObstacle ----------

-- | Represents an impassible area in 2D space. Able to be avoided by GKAgent's steering functions GKGraph can generate navigation graphs from a list of obstacles
-- 
-- Phantom type for @GKObstacle@.
data GKObstacle

instance IsObjCObject (Id GKObstacle) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKObstacle"

class IsNSObject a => IsGKObstacle a where
  toGKObstacle :: a -> Id GKObstacle

instance IsGKObstacle (Id GKObstacle) where
  toGKObstacle = unsafeCastId

instance IsNSObject (Id GKObstacle) where
  toNSObject = unsafeCastId

-- ---------- GKOctree ----------

-- | A tree data structure where each level has 8 children that subdivide a given space into the eight octants. Stores arbitrary NSObject elements via points and boxes.
-- 
-- Phantom type for @GKOctree@.
data GKOctree

instance IsObjCObject (Id GKOctree) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKOctree"

class IsNSObject a => IsGKOctree a where
  toGKOctree :: a -> Id GKOctree

instance IsGKOctree (Id GKOctree) where
  toGKOctree = unsafeCastId

instance IsNSObject (Id GKOctree) where
  toNSObject = unsafeCastId

-- ---------- GKOctreeNode ----------

-- | The individual node(s) that make up a GKOctree. Used as a hint for faster removal via [GKOctree removeData:WithNode:]
-- 
-- Phantom type for @GKOctreeNode@.
data GKOctreeNode

instance IsObjCObject (Id GKOctreeNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKOctreeNode"

class IsNSObject a => IsGKOctreeNode a where
  toGKOctreeNode :: a -> Id GKOctreeNode

instance IsGKOctreeNode (Id GKOctreeNode) where
  toGKOctreeNode = unsafeCastId

instance IsNSObject (Id GKOctreeNode) where
  toNSObject = unsafeCastId

-- ---------- GKPath ----------

-- | Phantom type for @GKPath@.
data GKPath

instance IsObjCObject (Id GKPath) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKPath"

class IsNSObject a => IsGKPath a where
  toGKPath :: a -> Id GKPath

instance IsGKPath (Id GKPath) where
  toGKPath = unsafeCastId

instance IsNSObject (Id GKPath) where
  toNSObject = unsafeCastId

-- ---------- GKQuadtree ----------

-- | A tree data structure where each level has 4 children that subdivide a given space into the four quadrants. Stores arbitrary NSObject data via points and quads.
-- 
-- Phantom type for @GKQuadtree@.
data GKQuadtree

instance IsObjCObject (Id GKQuadtree) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKQuadtree"

class IsNSObject a => IsGKQuadtree a where
  toGKQuadtree :: a -> Id GKQuadtree

instance IsGKQuadtree (Id GKQuadtree) where
  toGKQuadtree = unsafeCastId

instance IsNSObject (Id GKQuadtree) where
  toNSObject = unsafeCastId

-- ---------- GKQuadtreeNode ----------

-- | The individual node(s) that make up a GKQuadtree. Used as a hint for faster removal via [GKQuadtree removeData:WithNode:]
-- 
-- Phantom type for @GKQuadtreeNode@.
data GKQuadtreeNode

instance IsObjCObject (Id GKQuadtreeNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKQuadtreeNode"

class IsNSObject a => IsGKQuadtreeNode a where
  toGKQuadtreeNode :: a -> Id GKQuadtreeNode

instance IsGKQuadtreeNode (Id GKQuadtreeNode) where
  toGKQuadtreeNode = unsafeCastId

instance IsNSObject (Id GKQuadtreeNode) where
  toNSObject = unsafeCastId

-- ---------- GKRTree ----------

-- | An R-tree is a data structure that partitions axis aligned bounding rectangles into groups spatially. When a group goes to large, it is split according to its split strategy into two new groups. Fast queries can be made on these partition bounding rectangles.
-- 
-- Phantom type for @GKRTree@.
data GKRTree

instance IsObjCObject (Id GKRTree) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKRTree"

class IsNSObject a => IsGKRTree a where
  toGKRTree :: a -> Id GKRTree

instance IsGKRTree (Id GKRTree) where
  toGKRTree = unsafeCastId

instance IsNSObject (Id GKRTree) where
  toNSObject = unsafeCastId

-- ---------- GKRandomDistribution ----------

-- | A random distribution is a random source itself with a specific mapping from the input source to the output values. The distribution is uniform, meaning there is no bias towards any of the possible outcomes.
-- 
-- Phantom type for @GKRandomDistribution@.
data GKRandomDistribution

instance IsObjCObject (Id GKRandomDistribution) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKRandomDistribution"

class IsNSObject a => IsGKRandomDistribution a where
  toGKRandomDistribution :: a -> Id GKRandomDistribution

instance IsGKRandomDistribution (Id GKRandomDistribution) where
  toGKRandomDistribution = unsafeCastId

instance IsNSObject (Id GKRandomDistribution) where
  toNSObject = unsafeCastId

-- ---------- GKRandomSource ----------

-- | A concrete random source that can generate random numbers. The implementation details are up to the system and if a particular algorithm is needed then use one of the provided subclasses.
--
-- For certain specialized applications a shared system source may be needed and for those instances there is a wrapped interface over arc4random_*, accessible via +[GKRandomSource sharedRandom].
--
-- See: GKARC4RandomSource
--
-- See: GKLinearCongruentialRandomSource
--
-- See: GKMersenneTwisterRandomSource
--
-- See: GKRandomSource.systemRandom
-- 
-- Phantom type for @GKRandomSource@.
data GKRandomSource

instance IsObjCObject (Id GKRandomSource) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKRandomSource"

class IsNSObject a => IsGKRandomSource a where
  toGKRandomSource :: a -> Id GKRandomSource

instance IsGKRandomSource (Id GKRandomSource) where
  toGKRandomSource = unsafeCastId

instance IsNSObject (Id GKRandomSource) where
  toNSObject = unsafeCastId

-- ---------- GKRule ----------

-- | The concrete class that the GKRuleSystem uses to evaluate the current state and facts with predicated rules. These are sharable between systems, so don't retain any state in the rules themselves. Use the system-provided state storage.
--
-- See: GKRuleSystem.state
-- 
-- Phantom type for @GKRule@.
data GKRule

instance IsObjCObject (Id GKRule) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKRule"

class IsNSObject a => IsGKRule a where
  toGKRule :: a -> Id GKRule

instance IsGKRule (Id GKRule) where
  toGKRule = unsafeCastId

instance IsNSObject (Id GKRule) where
  toNSObject = unsafeCastId

-- ---------- GKRuleSystem ----------

-- | A rule system consists of 3 things: - The current state, which upon creation is considered the inital state. - The current set of rules. - The current set of facts.
--
-- Each time a fact is added to the system, the set of rules are evaluated in order and their actions executed in the system if their predicates are true. Rules can be fuzzy, allowing predicates and facts to be asserted to a degree of confidence instead of just boolean on/off.
--
-- The facts can be any kind of objects as long as they correctly determine equality using isEqual: The simplest approach is to use strings or dictionaries as they provide the most flexibility in defining facts, but user defined classes work just as well and may describe the problem space better.
--
-- The fact set is at all times a fuzzy set, as defined by fact membership in the set being modulated by their grade of membership. The rules may use the grade of membership to predicate their actions and in such a manner create fuzzy logic. The fuzzy logic Zadeh operators are available on the system itself in order to query multiple facts for combined membership grade.
-- 
-- Phantom type for @GKRuleSystem@.
data GKRuleSystem

instance IsObjCObject (Id GKRuleSystem) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKRuleSystem"

class IsNSObject a => IsGKRuleSystem a where
  toGKRuleSystem :: a -> Id GKRuleSystem

instance IsGKRuleSystem (Id GKRuleSystem) where
  toGKRuleSystem = unsafeCastId

instance IsNSObject (Id GKRuleSystem) where
  toNSObject = unsafeCastId

-- ---------- GKScene ----------

-- | A scene stores and handles loading of data related to a particular scene.
-- 
-- Phantom type for @GKScene@.
data GKScene

instance IsObjCObject (Id GKScene) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKScene"

class IsNSObject a => IsGKScene a where
  toGKScene :: a -> Id GKScene

instance IsGKScene (Id GKScene) where
  toGKScene = unsafeCastId

instance IsNSObject (Id GKScene) where
  toNSObject = unsafeCastId

-- ---------- GKState ----------

-- | Represents a single state in a state machine. By default, states allow transitions freely to and from the states in the machine.
--
-- If a more restricted set of valid transitions are needed in the state machine, you may override isValidNextState: where applicable.
--
-- See: GKStateMachine
--
-- See: isValidNextState:
-- 
-- Phantom type for @GKState@.
data GKState

instance IsObjCObject (Id GKState) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKState"

class IsNSObject a => IsGKState a where
  toGKState :: a -> Id GKState

instance IsGKState (Id GKState) where
  toGKState = unsafeCastId

instance IsNSObject (Id GKState) where
  toNSObject = unsafeCastId

-- ---------- GKStateMachine ----------

-- | Models a finite state machine that has a single current state.
-- 
-- Phantom type for @GKStateMachine@.
data GKStateMachine

instance IsObjCObject (Id GKStateMachine) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKStateMachine"

class IsNSObject a => IsGKStateMachine a where
  toGKStateMachine :: a -> Id GKStateMachine

instance IsGKStateMachine (Id GKStateMachine) where
  toGKStateMachine = unsafeCastId

instance IsNSObject (Id GKStateMachine) where
  toNSObject = unsafeCastId

-- ---------- GKCompositeBehavior ----------

-- | Phantom type for @GKCompositeBehavior@.
data GKCompositeBehavior

instance IsObjCObject (Id GKCompositeBehavior) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKCompositeBehavior"

class IsGKBehavior a => IsGKCompositeBehavior a where
  toGKCompositeBehavior :: a -> Id GKCompositeBehavior

instance IsGKCompositeBehavior (Id GKCompositeBehavior) where
  toGKCompositeBehavior = unsafeCastId

instance IsGKBehavior (Id GKCompositeBehavior) where
  toGKBehavior = unsafeCastId

instance IsNSObject (Id GKCompositeBehavior) where
  toNSObject = unsafeCastId

-- ---------- GKAgent ----------

-- | An agent is a point mass whose local coordinate system is aligned to its velocity.  Agents have a variety of steering functions that can be used to simulate vehicles or entities with agency. The units of mass, velocity and radius are dimensionless but related. The visual representation of these values are specific to each game's own situation.
--
-- Values close to 1.0 should be canonical and are expected to yield pleasing results. When applied to visuals these values should be scaled and biased into their target coordinate system and a simple filter on top ensures any noise generated from the steering logic doesn't affect the visual represtentation.
-- 
-- Phantom type for @GKAgent@.
data GKAgent

instance IsObjCObject (Id GKAgent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKAgent"

class IsGKComponent a => IsGKAgent a where
  toGKAgent :: a -> Id GKAgent

instance IsGKAgent (Id GKAgent) where
  toGKAgent = unsafeCastId

instance IsGKComponent (Id GKAgent) where
  toGKComponent = unsafeCastId

instance IsNSObject (Id GKAgent) where
  toNSObject = unsafeCastId

-- ---------- GKSCNNodeComponent ----------

-- | A component that encapsulates a SceneKit node.
-- 
-- Phantom type for @GKSCNNodeComponent@.
data GKSCNNodeComponent

instance IsObjCObject (Id GKSCNNodeComponent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKSCNNodeComponent"

class IsGKComponent a => IsGKSCNNodeComponent a where
  toGKSCNNodeComponent :: a -> Id GKSCNNodeComponent

instance IsGKSCNNodeComponent (Id GKSCNNodeComponent) where
  toGKSCNNodeComponent = unsafeCastId

instance IsGKComponent (Id GKSCNNodeComponent) where
  toGKComponent = unsafeCastId

instance IsNSObject (Id GKSCNNodeComponent) where
  toNSObject = unsafeCastId

-- ---------- GKSKNodeComponent ----------

-- | A component that encapsulates a SpriteKit node.
-- 
-- Phantom type for @GKSKNodeComponent@.
data GKSKNodeComponent

instance IsObjCObject (Id GKSKNodeComponent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKSKNodeComponent"

class IsGKComponent a => IsGKSKNodeComponent a where
  toGKSKNodeComponent :: a -> Id GKSKNodeComponent

instance IsGKSKNodeComponent (Id GKSKNodeComponent) where
  toGKSKNodeComponent = unsafeCastId

instance IsGKComponent (Id GKSKNodeComponent) where
  toGKComponent = unsafeCastId

instance IsNSObject (Id GKSKNodeComponent) where
  toNSObject = unsafeCastId

-- ---------- GKGridGraph ----------

-- | Phantom type for @GKGridGraph@.
data GKGridGraph

instance IsObjCObject (Id GKGridGraph) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKGridGraph"

class IsGKGraph a => IsGKGridGraph a where
  toGKGridGraph :: a -> Id GKGridGraph

instance IsGKGridGraph (Id GKGridGraph) where
  toGKGridGraph = unsafeCastId

instance IsGKGraph (Id GKGridGraph) where
  toGKGraph = unsafeCastId

instance IsNSObject (Id GKGridGraph) where
  toNSObject = unsafeCastId

-- ---------- GKMeshGraph ----------

-- | A collection of GKGraphNodes that are governed by a mesh formed by the space between a set of GKPolygonObstacles
-- 
-- Phantom type for @GKMeshGraph@.
data GKMeshGraph

instance IsObjCObject (Id GKMeshGraph) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKMeshGraph"

class IsGKGraph a => IsGKMeshGraph a where
  toGKMeshGraph :: a -> Id GKMeshGraph

instance IsGKMeshGraph (Id GKMeshGraph) where
  toGKMeshGraph = unsafeCastId

instance IsGKGraph (Id GKMeshGraph) where
  toGKGraph = unsafeCastId

instance IsNSObject (Id GKMeshGraph) where
  toNSObject = unsafeCastId

-- ---------- GKObstacleGraph ----------

-- | A collection of GKGraphNodes that are governed by a set of extruded GKPolygonObstacles
-- 
-- Phantom type for @GKObstacleGraph@.
data GKObstacleGraph

instance IsObjCObject (Id GKObstacleGraph) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKObstacleGraph"

class IsGKGraph a => IsGKObstacleGraph a where
  toGKObstacleGraph :: a -> Id GKObstacleGraph

instance IsGKObstacleGraph (Id GKObstacleGraph) where
  toGKObstacleGraph = unsafeCastId

instance IsGKGraph (Id GKObstacleGraph) where
  toGKGraph = unsafeCastId

instance IsNSObject (Id GKObstacleGraph) where
  toNSObject = unsafeCastId

-- ---------- GKGraphNode2D ----------

-- | GKGraphNode coupled with a 2D position
-- 
-- Phantom type for @GKGraphNode2D@.
data GKGraphNode2D

instance IsObjCObject (Id GKGraphNode2D) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKGraphNode2D"

class IsGKGraphNode a => IsGKGraphNode2D a where
  toGKGraphNode2D :: a -> Id GKGraphNode2D

instance IsGKGraphNode2D (Id GKGraphNode2D) where
  toGKGraphNode2D = unsafeCastId

instance IsGKGraphNode (Id GKGraphNode2D) where
  toGKGraphNode = unsafeCastId

instance IsNSObject (Id GKGraphNode2D) where
  toNSObject = unsafeCastId

-- ---------- GKGraphNode3D ----------

-- | GKGraphNode coupled with a 3D position
-- 
-- Phantom type for @GKGraphNode3D@.
data GKGraphNode3D

instance IsObjCObject (Id GKGraphNode3D) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKGraphNode3D"

class IsGKGraphNode a => IsGKGraphNode3D a where
  toGKGraphNode3D :: a -> Id GKGraphNode3D

instance IsGKGraphNode3D (Id GKGraphNode3D) where
  toGKGraphNode3D = unsafeCastId

instance IsGKGraphNode (Id GKGraphNode3D) where
  toGKGraphNode = unsafeCastId

instance IsNSObject (Id GKGraphNode3D) where
  toNSObject = unsafeCastId

-- ---------- GKGridGraphNode ----------

-- | GKGraphNode coupled with a position on a 2D grid
-- 
-- Phantom type for @GKGridGraphNode@.
data GKGridGraphNode

instance IsObjCObject (Id GKGridGraphNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKGridGraphNode"

class IsGKGraphNode a => IsGKGridGraphNode a where
  toGKGridGraphNode :: a -> Id GKGridGraphNode

instance IsGKGridGraphNode (Id GKGridGraphNode) where
  toGKGridGraphNode = unsafeCastId

instance IsGKGraphNode (Id GKGridGraphNode) where
  toGKGraphNode = unsafeCastId

instance IsNSObject (Id GKGridGraphNode) where
  toNSObject = unsafeCastId

-- ---------- GKCheckerboardNoiseSource ----------

-- | Produces noise in a checkerboard pattern.
-- 
-- Phantom type for @GKCheckerboardNoiseSource@.
data GKCheckerboardNoiseSource

instance IsObjCObject (Id GKCheckerboardNoiseSource) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKCheckerboardNoiseSource"

class IsGKNoiseSource a => IsGKCheckerboardNoiseSource a where
  toGKCheckerboardNoiseSource :: a -> Id GKCheckerboardNoiseSource

instance IsGKCheckerboardNoiseSource (Id GKCheckerboardNoiseSource) where
  toGKCheckerboardNoiseSource = unsafeCastId

instance IsGKNoiseSource (Id GKCheckerboardNoiseSource) where
  toGKNoiseSource = unsafeCastId

instance IsNSObject (Id GKCheckerboardNoiseSource) where
  toNSObject = unsafeCastId

-- ---------- GKCoherentNoiseSource ----------

-- | Coherent noise is smoothly-changing, semi-random noise.  A given input always produces the same output. A small change in input produces a small change in output.  A large change in input produces a random change in output. This class is not intended to be instantiated.
-- 
-- Phantom type for @GKCoherentNoiseSource@.
data GKCoherentNoiseSource

instance IsObjCObject (Id GKCoherentNoiseSource) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKCoherentNoiseSource"

class IsGKNoiseSource a => IsGKCoherentNoiseSource a where
  toGKCoherentNoiseSource :: a -> Id GKCoherentNoiseSource

instance IsGKCoherentNoiseSource (Id GKCoherentNoiseSource) where
  toGKCoherentNoiseSource = unsafeCastId

instance IsGKNoiseSource (Id GKCoherentNoiseSource) where
  toGKNoiseSource = unsafeCastId

instance IsNSObject (Id GKCoherentNoiseSource) where
  toNSObject = unsafeCastId

-- ---------- GKConstantNoiseSource ----------

-- | Produces a single, constant value at all positions in the space.
-- 
-- Phantom type for @GKConstantNoiseSource@.
data GKConstantNoiseSource

instance IsObjCObject (Id GKConstantNoiseSource) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKConstantNoiseSource"

class IsGKNoiseSource a => IsGKConstantNoiseSource a where
  toGKConstantNoiseSource :: a -> Id GKConstantNoiseSource

instance IsGKConstantNoiseSource (Id GKConstantNoiseSource) where
  toGKConstantNoiseSource = unsafeCastId

instance IsGKNoiseSource (Id GKConstantNoiseSource) where
  toGKNoiseSource = unsafeCastId

instance IsNSObject (Id GKConstantNoiseSource) where
  toNSObject = unsafeCastId

-- ---------- GKCylindersNoiseSource ----------

-- | Produces 3D cylindrical noise with an infinite number of cylinders-within-cyliners of constantly-increasing radius.
-- 
-- Phantom type for @GKCylindersNoiseSource@.
data GKCylindersNoiseSource

instance IsObjCObject (Id GKCylindersNoiseSource) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKCylindersNoiseSource"

class IsGKNoiseSource a => IsGKCylindersNoiseSource a where
  toGKCylindersNoiseSource :: a -> Id GKCylindersNoiseSource

instance IsGKCylindersNoiseSource (Id GKCylindersNoiseSource) where
  toGKCylindersNoiseSource = unsafeCastId

instance IsGKNoiseSource (Id GKCylindersNoiseSource) where
  toGKNoiseSource = unsafeCastId

instance IsNSObject (Id GKCylindersNoiseSource) where
  toNSObject = unsafeCastId

-- ---------- GKSpheresNoiseSource ----------

-- | Produces 3D spherical noise with an infinite number of spheres-within-spheres of constantly-increasing radius.
-- 
-- Phantom type for @GKSpheresNoiseSource@.
data GKSpheresNoiseSource

instance IsObjCObject (Id GKSpheresNoiseSource) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKSpheresNoiseSource"

class IsGKNoiseSource a => IsGKSpheresNoiseSource a where
  toGKSpheresNoiseSource :: a -> Id GKSpheresNoiseSource

instance IsGKSpheresNoiseSource (Id GKSpheresNoiseSource) where
  toGKSpheresNoiseSource = unsafeCastId

instance IsGKNoiseSource (Id GKSpheresNoiseSource) where
  toGKNoiseSource = unsafeCastId

instance IsNSObject (Id GKSpheresNoiseSource) where
  toNSObject = unsafeCastId

-- ---------- GKVoronoiNoiseSource ----------

-- | Voronoi noise partitions the space into angular, polygonal "cells", which are reminiscent of stained glass or crystal-like structures.
-- 
-- Phantom type for @GKVoronoiNoiseSource@.
data GKVoronoiNoiseSource

instance IsObjCObject (Id GKVoronoiNoiseSource) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKVoronoiNoiseSource"

class IsGKNoiseSource a => IsGKVoronoiNoiseSource a where
  toGKVoronoiNoiseSource :: a -> Id GKVoronoiNoiseSource

instance IsGKVoronoiNoiseSource (Id GKVoronoiNoiseSource) where
  toGKVoronoiNoiseSource = unsafeCastId

instance IsGKNoiseSource (Id GKVoronoiNoiseSource) where
  toGKNoiseSource = unsafeCastId

instance IsNSObject (Id GKVoronoiNoiseSource) where
  toNSObject = unsafeCastId

-- ---------- GKCircleObstacle ----------

-- | An obstacle with an impassible radius
-- 
-- Phantom type for @GKCircleObstacle@.
data GKCircleObstacle

instance IsObjCObject (Id GKCircleObstacle) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKCircleObstacle"

class IsGKObstacle a => IsGKCircleObstacle a where
  toGKCircleObstacle :: a -> Id GKCircleObstacle

instance IsGKCircleObstacle (Id GKCircleObstacle) where
  toGKCircleObstacle = unsafeCastId

instance IsGKObstacle (Id GKCircleObstacle) where
  toGKObstacle = unsafeCastId

instance IsNSObject (Id GKCircleObstacle) where
  toNSObject = unsafeCastId

-- ---------- GKPolygonObstacle ----------

-- | An obstacle with an impassible closed polygon
-- 
-- Phantom type for @GKPolygonObstacle@.
data GKPolygonObstacle

instance IsObjCObject (Id GKPolygonObstacle) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKPolygonObstacle"

class IsGKObstacle a => IsGKPolygonObstacle a where
  toGKPolygonObstacle :: a -> Id GKPolygonObstacle

instance IsGKPolygonObstacle (Id GKPolygonObstacle) where
  toGKPolygonObstacle = unsafeCastId

instance IsGKObstacle (Id GKPolygonObstacle) where
  toGKObstacle = unsafeCastId

instance IsNSObject (Id GKPolygonObstacle) where
  toNSObject = unsafeCastId

-- ---------- GKSphereObstacle ----------

-- | An obstacle with an impassible radius in 3D space For use with GKAgent3D.  Using this with a GKAgent2D is no different than using GKCircleObstacle.
-- 
-- Phantom type for @GKSphereObstacle@.
data GKSphereObstacle

instance IsObjCObject (Id GKSphereObstacle) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKSphereObstacle"

class IsGKObstacle a => IsGKSphereObstacle a where
  toGKSphereObstacle :: a -> Id GKSphereObstacle

instance IsGKSphereObstacle (Id GKSphereObstacle) where
  toGKSphereObstacle = unsafeCastId

instance IsGKObstacle (Id GKSphereObstacle) where
  toGKObstacle = unsafeCastId

instance IsNSObject (Id GKSphereObstacle) where
  toNSObject = unsafeCastId

-- ---------- GKGaussianDistribution ----------

-- | A gaussian distribution is biased towards the mean value, the possible outcomes are spread out from the mean with decreasing probability. Values within 1 deviation of the mean make up 68.27% of the distribution, values within 2 deviations make up 95% and values within 3 deviations make up 99.7%.
--
-- Note that a gaussian distribution's unbounded behavior beyond 3 deviations is undesired, thus this distribution deviates nominally by modifying the bounds to 3 deviations. Thus values within 3 deviations actually make up 100% of the distribution.
-- 
-- Phantom type for @GKGaussianDistribution@.
data GKGaussianDistribution

instance IsObjCObject (Id GKGaussianDistribution) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKGaussianDistribution"

class IsGKRandomDistribution a => IsGKGaussianDistribution a where
  toGKGaussianDistribution :: a -> Id GKGaussianDistribution

instance IsGKGaussianDistribution (Id GKGaussianDistribution) where
  toGKGaussianDistribution = unsafeCastId

instance IsGKRandomDistribution (Id GKGaussianDistribution) where
  toGKRandomDistribution = unsafeCastId

instance IsNSObject (Id GKGaussianDistribution) where
  toNSObject = unsafeCastId

-- ---------- GKShuffledDistribution ----------

-- | A shuffled distribution tries to make sure individual samples are not clustered whilst retaining a uniform distribution of values over time. This is often referred to as fair or less random, as the predicatability of the outcomes in a series is vastly increased, yet the distribution of values is uniform.
--
-- Do not use with distributions ranging more than 256 between lowest and highest as the shuffling seqeunce is stored internally in memory.
-- 
-- Phantom type for @GKShuffledDistribution@.
data GKShuffledDistribution

instance IsObjCObject (Id GKShuffledDistribution) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKShuffledDistribution"

class IsGKRandomDistribution a => IsGKShuffledDistribution a where
  toGKShuffledDistribution :: a -> Id GKShuffledDistribution

instance IsGKShuffledDistribution (Id GKShuffledDistribution) where
  toGKShuffledDistribution = unsafeCastId

instance IsGKRandomDistribution (Id GKShuffledDistribution) where
  toGKRandomDistribution = unsafeCastId

instance IsNSObject (Id GKShuffledDistribution) where
  toNSObject = unsafeCastId

-- ---------- GKARC4RandomSource ----------

-- | A deterministic pseudo-random source that generates random numbers based on an arc4 algorithm. This is a deterministic random source suitable for creating reliable gameplay mechanics.
--
-- While deterministic, this is not a cryptographic random source, however it may be useful for obfuscation of gameplay data in manner similar to a stream cipher.
-- 
-- Phantom type for @GKARC4RandomSource@.
data GKARC4RandomSource

instance IsObjCObject (Id GKARC4RandomSource) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKARC4RandomSource"

class IsGKRandomSource a => IsGKARC4RandomSource a where
  toGKARC4RandomSource :: a -> Id GKARC4RandomSource

instance IsGKARC4RandomSource (Id GKARC4RandomSource) where
  toGKARC4RandomSource = unsafeCastId

instance IsGKRandomSource (Id GKARC4RandomSource) where
  toGKRandomSource = unsafeCastId

instance IsNSObject (Id GKARC4RandomSource) where
  toNSObject = unsafeCastId

-- ---------- GKLinearCongruentialRandomSource ----------

-- | A deterministic pseudo-random source that generates random numbers based on a linear congruential algorithm. This is a deterministic random source suitable for creating reliable gameplay mechanics. It is slightly faster than an Arc4 source, but less random. In particular the lower bits of the generated values are less random than the higher bits.
--
-- While deterministic, this is not a cryptographic random source. It is also not suitable for obfuscation of gameplay data.
-- 
-- Phantom type for @GKLinearCongruentialRandomSource@.
data GKLinearCongruentialRandomSource

instance IsObjCObject (Id GKLinearCongruentialRandomSource) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKLinearCongruentialRandomSource"

class IsGKRandomSource a => IsGKLinearCongruentialRandomSource a where
  toGKLinearCongruentialRandomSource :: a -> Id GKLinearCongruentialRandomSource

instance IsGKLinearCongruentialRandomSource (Id GKLinearCongruentialRandomSource) where
  toGKLinearCongruentialRandomSource = unsafeCastId

instance IsGKRandomSource (Id GKLinearCongruentialRandomSource) where
  toGKRandomSource = unsafeCastId

instance IsNSObject (Id GKLinearCongruentialRandomSource) where
  toNSObject = unsafeCastId

-- ---------- GKMersenneTwisterRandomSource ----------

-- | A deterministic pseudo-random source that generates random numbers based on a mersenne twister algorithm. This is a deterministic random source suitable for creating reliable gameplay mechanics. It is slightly slower than an Arc4 source, but more random, in that it has a longer period until repeating sequences.
--
-- While deterministic, this is not a cryptographic random source. It is however suitable for obfuscation of gameplay data.
-- 
-- Phantom type for @GKMersenneTwisterRandomSource@.
data GKMersenneTwisterRandomSource

instance IsObjCObject (Id GKMersenneTwisterRandomSource) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKMersenneTwisterRandomSource"

class IsGKRandomSource a => IsGKMersenneTwisterRandomSource a where
  toGKMersenneTwisterRandomSource :: a -> Id GKMersenneTwisterRandomSource

instance IsGKMersenneTwisterRandomSource (Id GKMersenneTwisterRandomSource) where
  toGKMersenneTwisterRandomSource = unsafeCastId

instance IsGKRandomSource (Id GKMersenneTwisterRandomSource) where
  toGKRandomSource = unsafeCastId

instance IsNSObject (Id GKMersenneTwisterRandomSource) where
  toNSObject = unsafeCastId

-- ---------- GKNSPredicateRule ----------

-- | A convenient subclass of GKRule that leverages existing NSPRedicate functionality for evaluating the predicate of the rule.
-- 
-- Phantom type for @GKNSPredicateRule@.
data GKNSPredicateRule

instance IsObjCObject (Id GKNSPredicateRule) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKNSPredicateRule"

class IsGKRule a => IsGKNSPredicateRule a where
  toGKNSPredicateRule :: a -> Id GKNSPredicateRule

instance IsGKNSPredicateRule (Id GKNSPredicateRule) where
  toGKNSPredicateRule = unsafeCastId

instance IsGKRule (Id GKNSPredicateRule) where
  toGKRule = unsafeCastId

instance IsNSObject (Id GKNSPredicateRule) where
  toNSObject = unsafeCastId

-- ---------- GKAgent2D ----------

-- | A 2D specalization of an agent that moves on a 2-axis logical coordinate system. This coordinate system does not need to match the visual coordinate system of the delegate. One simple case of that is isometric 2D content where the game model is on a flat 2D plane but the visuals are displayed on an angle where one of the logical axes are used for simulated depth as well as some translation in the display plane.
-- 
-- Phantom type for @GKAgent2D@.
data GKAgent2D

instance IsObjCObject (Id GKAgent2D) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKAgent2D"

class IsGKAgent a => IsGKAgent2D a where
  toGKAgent2D :: a -> Id GKAgent2D

instance IsGKAgent2D (Id GKAgent2D) where
  toGKAgent2D = unsafeCastId

instance IsGKAgent (Id GKAgent2D) where
  toGKAgent = unsafeCastId

instance IsGKComponent (Id GKAgent2D) where
  toGKComponent = unsafeCastId

instance IsNSObject (Id GKAgent2D) where
  toNSObject = unsafeCastId

-- ---------- GKAgent3D ----------

-- | A 3D specialization of an agent that moves on a 3-axis logical coordinate system.
-- 
-- Phantom type for @GKAgent3D@.
data GKAgent3D

instance IsObjCObject (Id GKAgent3D) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKAgent3D"

class IsGKAgent a => IsGKAgent3D a where
  toGKAgent3D :: a -> Id GKAgent3D

instance IsGKAgent3D (Id GKAgent3D) where
  toGKAgent3D = unsafeCastId

instance IsGKAgent (Id GKAgent3D) where
  toGKAgent = unsafeCastId

instance IsGKComponent (Id GKAgent3D) where
  toGKComponent = unsafeCastId

instance IsNSObject (Id GKAgent3D) where
  toNSObject = unsafeCastId

-- ---------- GKBillowNoiseSource ----------

-- | Billow noise is similar to Perlin noise, with more rounded shapes and clearly-defined transitions beween values.
-- 
-- Phantom type for @GKBillowNoiseSource@.
data GKBillowNoiseSource

instance IsObjCObject (Id GKBillowNoiseSource) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKBillowNoiseSource"

class IsGKCoherentNoiseSource a => IsGKBillowNoiseSource a where
  toGKBillowNoiseSource :: a -> Id GKBillowNoiseSource

instance IsGKBillowNoiseSource (Id GKBillowNoiseSource) where
  toGKBillowNoiseSource = unsafeCastId

instance IsGKCoherentNoiseSource (Id GKBillowNoiseSource) where
  toGKCoherentNoiseSource = unsafeCastId

instance IsGKNoiseSource (Id GKBillowNoiseSource) where
  toGKNoiseSource = unsafeCastId

instance IsNSObject (Id GKBillowNoiseSource) where
  toNSObject = unsafeCastId

-- ---------- GKPerlinNoiseSource ----------

-- | Perlin noise is useful for creating natural-looking textures and realistic-looking terrain.
-- 
-- Phantom type for @GKPerlinNoiseSource@.
data GKPerlinNoiseSource

instance IsObjCObject (Id GKPerlinNoiseSource) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKPerlinNoiseSource"

class IsGKCoherentNoiseSource a => IsGKPerlinNoiseSource a where
  toGKPerlinNoiseSource :: a -> Id GKPerlinNoiseSource

instance IsGKPerlinNoiseSource (Id GKPerlinNoiseSource) where
  toGKPerlinNoiseSource = unsafeCastId

instance IsGKCoherentNoiseSource (Id GKPerlinNoiseSource) where
  toGKCoherentNoiseSource = unsafeCastId

instance IsGKNoiseSource (Id GKPerlinNoiseSource) where
  toGKNoiseSource = unsafeCastId

instance IsNSObject (Id GKPerlinNoiseSource) where
  toNSObject = unsafeCastId

-- ---------- GKRidgedNoiseSource ----------

-- | Ridged noise is similar to Perlin noise, with sharply-defined, relatively thin peaks.
-- 
-- Phantom type for @GKRidgedNoiseSource@.
data GKRidgedNoiseSource

instance IsObjCObject (Id GKRidgedNoiseSource) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "GKRidgedNoiseSource"

class IsGKCoherentNoiseSource a => IsGKRidgedNoiseSource a where
  toGKRidgedNoiseSource :: a -> Id GKRidgedNoiseSource

instance IsGKRidgedNoiseSource (Id GKRidgedNoiseSource) where
  toGKRidgedNoiseSource = unsafeCastId

instance IsGKCoherentNoiseSource (Id GKRidgedNoiseSource) where
  toGKCoherentNoiseSource = unsafeCastId

instance IsGKNoiseSource (Id GKRidgedNoiseSource) where
  toGKNoiseSource = unsafeCastId

instance IsNSObject (Id GKRidgedNoiseSource) where
  toNSObject = unsafeCastId
