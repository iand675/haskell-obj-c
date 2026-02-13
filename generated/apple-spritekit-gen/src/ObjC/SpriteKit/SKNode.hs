{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKNode@.
module ObjC.SpriteKit.SKNode
  ( SKNode
  , IsSKNode(..)
  , init_
  , initWithCoder
  , node
  , nodeWithFileNamed
  , nodeWithFileNamed_securelyWithClasses_andError
  , valueForAttributeNamed
  , setValue_forAttributeNamed
  , setScale
  , addChild
  , insertChild_atIndex
  , removeChildrenInArray
  , removeAllChildren
  , removeFromParent
  , moveToParent
  , childNodeWithName
  , enumerateChildNodesWithName_usingBlock
  , objectForKeyedSubscript
  , inParentHierarchy
  , runAction
  , runAction_completion
  , runAction_withKey
  , hasActions
  , actionForKey
  , removeActionForKey
  , removeAllActions
  , intersectsNode
  , isEqualToNode
  , zPosition
  , setZPosition
  , zRotation
  , setZRotation
  , xScale
  , setXScale
  , yScale
  , setYScale
  , speed
  , setSpeed
  , alpha
  , setAlpha
  , paused
  , setPaused
  , hidden
  , setHidden
  , userInteractionEnabled
  , setUserInteractionEnabled
  , focusBehavior
  , setFocusBehavior
  , parent
  , children
  , name
  , setName
  , scene
  , physicsBody
  , setPhysicsBody
  , userData
  , setUserData
  , reachConstraints
  , setReachConstraints
  , constraints
  , setConstraints
  , accessibilityElement
  , setAccessibilityElement
  , accessibilityRole
  , setAccessibilityRole
  , accessibilityRoleDescription
  , setAccessibilityRoleDescription
  , accessibilitySubrole
  , setAccessibilitySubrole
  , accessibilityParent
  , setAccessibilityParent
  , accessibilityChildren
  , setAccessibilityChildren
  , accessibilityHelp
  , setAccessibilityHelp
  , accessibilityLabel
  , setAccessibilityLabel
  , accessibilityEnabled
  , setAccessibilityEnabled
  , accessibilityChildrenSelector
  , accessibilityElementSelector
  , accessibilityEnabledSelector
  , accessibilityHelpSelector
  , accessibilityLabelSelector
  , accessibilityParentSelector
  , accessibilityRoleDescriptionSelector
  , accessibilityRoleSelector
  , accessibilitySubroleSelector
  , actionForKeySelector
  , addChildSelector
  , alphaSelector
  , childNodeWithNameSelector
  , childrenSelector
  , constraintsSelector
  , enumerateChildNodesWithName_usingBlockSelector
  , focusBehaviorSelector
  , hasActionsSelector
  , hiddenSelector
  , inParentHierarchySelector
  , initSelector
  , initWithCoderSelector
  , insertChild_atIndexSelector
  , intersectsNodeSelector
  , isEqualToNodeSelector
  , moveToParentSelector
  , nameSelector
  , nodeSelector
  , nodeWithFileNamedSelector
  , nodeWithFileNamed_securelyWithClasses_andErrorSelector
  , objectForKeyedSubscriptSelector
  , parentSelector
  , pausedSelector
  , physicsBodySelector
  , reachConstraintsSelector
  , removeActionForKeySelector
  , removeAllActionsSelector
  , removeAllChildrenSelector
  , removeChildrenInArraySelector
  , removeFromParentSelector
  , runActionSelector
  , runAction_completionSelector
  , runAction_withKeySelector
  , sceneSelector
  , setAccessibilityChildrenSelector
  , setAccessibilityElementSelector
  , setAccessibilityEnabledSelector
  , setAccessibilityHelpSelector
  , setAccessibilityLabelSelector
  , setAccessibilityParentSelector
  , setAccessibilityRoleDescriptionSelector
  , setAccessibilityRoleSelector
  , setAccessibilitySubroleSelector
  , setAlphaSelector
  , setConstraintsSelector
  , setFocusBehaviorSelector
  , setHiddenSelector
  , setNameSelector
  , setPausedSelector
  , setPhysicsBodySelector
  , setReachConstraintsSelector
  , setScaleSelector
  , setSpeedSelector
  , setUserDataSelector
  , setUserInteractionEnabledSelector
  , setValue_forAttributeNamedSelector
  , setXScaleSelector
  , setYScaleSelector
  , setZPositionSelector
  , setZRotationSelector
  , speedSelector
  , userDataSelector
  , userInteractionEnabledSelector
  , valueForAttributeNamedSelector
  , xScaleSelector
  , yScaleSelector
  , zPositionSelector
  , zRotationSelector

  -- * Enum types
  , SKNodeFocusBehavior(SKNodeFocusBehavior)
  , pattern SKNodeFocusBehaviorNone
  , pattern SKNodeFocusBehaviorOccluding
  , pattern SKNodeFocusBehaviorFocusable

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SpriteKit.Internal.Classes
import ObjC.SpriteKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSKNode skNode => skNode -> IO (Id SKNode)
init_ skNode =
  sendOwnedMessage skNode initSelector

-- | Support coding and decoding via NSKeyedArchiver.
--
-- ObjC selector: @- initWithCoder:@
initWithCoder :: (IsSKNode skNode, IsNSCoder aDecoder) => skNode -> aDecoder -> IO (Id SKNode)
initWithCoder skNode aDecoder =
  sendOwnedMessage skNode initWithCoderSelector (toNSCoder aDecoder)

-- | @+ node@
node :: IO (Id SKNode)
node  =
  do
    cls' <- getRequiredClass "SKNode"
    sendClassMessage cls' nodeSelector

-- | @+ nodeWithFileNamed:@
nodeWithFileNamed :: IsNSString filename => filename -> IO (Id SKNode)
nodeWithFileNamed filename =
  do
    cls' <- getRequiredClass "SKNode"
    sendClassMessage cls' nodeWithFileNamedSelector (toNSString filename)

-- | @+ nodeWithFileNamed:securelyWithClasses:andError:@
nodeWithFileNamed_securelyWithClasses_andError :: (IsNSString filename, IsNSSet classes, IsNSError error_) => filename -> classes -> error_ -> IO (Id SKNode)
nodeWithFileNamed_securelyWithClasses_andError filename classes error_ =
  do
    cls' <- getRequiredClass "SKNode"
    sendClassMessage cls' nodeWithFileNamed_securelyWithClasses_andErrorSelector (toNSString filename) (toNSSet classes) (toNSError error_)

-- | @- valueForAttributeNamed:@
valueForAttributeNamed :: (IsSKNode skNode, IsNSString key) => skNode -> key -> IO (Id SKAttributeValue)
valueForAttributeNamed skNode key =
  sendMessage skNode valueForAttributeNamedSelector (toNSString key)

-- | @- setValue:forAttributeNamed:@
setValue_forAttributeNamed :: (IsSKNode skNode, IsSKAttributeValue value, IsNSString key) => skNode -> value -> key -> IO ()
setValue_forAttributeNamed skNode value key =
  sendMessage skNode setValue_forAttributeNamedSelector (toSKAttributeValue value) (toNSString key)

-- | Sets both the x & y scale
--
-- @scale@ — the uniform scale to set.
--
-- ObjC selector: @- setScale:@
setScale :: IsSKNode skNode => skNode -> CDouble -> IO ()
setScale skNode scale =
  sendMessage skNode setScaleSelector scale

-- | Adds a node as a child node of this node
--
-- The added node must not have a parent.
--
-- @node@ — the child node to add.
--
-- ObjC selector: @- addChild:@
addChild :: (IsSKNode skNode, IsSKNode node) => skNode -> node -> IO ()
addChild skNode node =
  sendMessage skNode addChildSelector (toSKNode node)

-- | @- insertChild:atIndex:@
insertChild_atIndex :: (IsSKNode skNode, IsSKNode node) => skNode -> node -> CLong -> IO ()
insertChild_atIndex skNode node index =
  sendMessage skNode insertChild_atIndexSelector (toSKNode node) index

-- | @- removeChildrenInArray:@
removeChildrenInArray :: (IsSKNode skNode, IsNSArray nodes) => skNode -> nodes -> IO ()
removeChildrenInArray skNode nodes =
  sendMessage skNode removeChildrenInArraySelector (toNSArray nodes)

-- | @- removeAllChildren@
removeAllChildren :: IsSKNode skNode => skNode -> IO ()
removeAllChildren skNode =
  sendMessage skNode removeAllChildrenSelector

-- | @- removeFromParent@
removeFromParent :: IsSKNode skNode => skNode -> IO ()
removeFromParent skNode =
  sendMessage skNode removeFromParentSelector

-- | @- moveToParent:@
moveToParent :: (IsSKNode skNode, IsSKNode parent) => skNode -> parent -> IO ()
moveToParent skNode parent =
  sendMessage skNode moveToParentSelector (toSKNode parent)

-- | @- childNodeWithName:@
childNodeWithName :: (IsSKNode skNode, IsNSString name) => skNode -> name -> IO (Id SKNode)
childNodeWithName skNode name =
  sendMessage skNode childNodeWithNameSelector (toNSString name)

-- | @- enumerateChildNodesWithName:usingBlock:@
enumerateChildNodesWithName_usingBlock :: (IsSKNode skNode, IsNSString name) => skNode -> name -> Ptr () -> IO ()
enumerateChildNodesWithName_usingBlock skNode name block =
  sendMessage skNode enumerateChildNodesWithName_usingBlockSelector (toNSString name) block

-- | Simplified shorthand for enumerateChildNodesWithName that returns an array of the matching nodes. This allows subscripting of the form:      NSArray *childrenMatchingName = node["name"]
--
-- or even complex like:      NSArray *siblingsBeginningWithA = node["../a*"]
--
-- @name@ — An Xpath style path that can include simple regular expressions for matching node names.
--
-- See: enumerateChildNodesWithName:usingBlock:
--
-- ObjC selector: @- objectForKeyedSubscript:@
objectForKeyedSubscript :: (IsSKNode skNode, IsNSString name) => skNode -> name -> IO (Id NSArray)
objectForKeyedSubscript skNode name =
  sendMessage skNode objectForKeyedSubscriptSelector (toNSString name)

-- | @- inParentHierarchy:@
inParentHierarchy :: (IsSKNode skNode, IsSKNode parent) => skNode -> parent -> IO Bool
inParentHierarchy skNode parent =
  sendMessage skNode inParentHierarchySelector (toSKNode parent)

-- | @- runAction:@
runAction :: (IsSKNode skNode, IsSKAction action) => skNode -> action -> IO ()
runAction skNode action =
  sendMessage skNode runActionSelector (toSKAction action)

-- | @- runAction:completion:@
runAction_completion :: (IsSKNode skNode, IsSKAction action) => skNode -> action -> Ptr () -> IO ()
runAction_completion skNode action block =
  sendMessage skNode runAction_completionSelector (toSKAction action) block

-- | @- runAction:withKey:@
runAction_withKey :: (IsSKNode skNode, IsSKAction action, IsNSString key) => skNode -> action -> key -> IO ()
runAction_withKey skNode action key =
  sendMessage skNode runAction_withKeySelector (toSKAction action) (toNSString key)

-- | @- hasActions@
hasActions :: IsSKNode skNode => skNode -> IO Bool
hasActions skNode =
  sendMessage skNode hasActionsSelector

-- | @- actionForKey:@
actionForKey :: (IsSKNode skNode, IsNSString key) => skNode -> key -> IO (Id SKAction)
actionForKey skNode key =
  sendMessage skNode actionForKeySelector (toNSString key)

-- | @- removeActionForKey:@
removeActionForKey :: (IsSKNode skNode, IsNSString key) => skNode -> key -> IO ()
removeActionForKey skNode key =
  sendMessage skNode removeActionForKeySelector (toNSString key)

-- | @- removeAllActions@
removeAllActions :: IsSKNode skNode => skNode -> IO ()
removeAllActions skNode =
  sendMessage skNode removeAllActionsSelector

-- | @- intersectsNode:@
intersectsNode :: (IsSKNode skNode, IsSKNode node) => skNode -> node -> IO Bool
intersectsNode skNode node =
  sendMessage skNode intersectsNodeSelector (toSKNode node)

-- | @- isEqualToNode:@
isEqualToNode :: (IsSKNode skNode, IsSKNode node) => skNode -> node -> IO Bool
isEqualToNode skNode node =
  sendMessage skNode isEqualToNodeSelector (toSKNode node)

-- | The z-order of the node (used for ordering). Negative z is "into" the screen, Positive z is "out" of the screen. A greater zPosition will sort in front of a lesser zPosition.
--
-- ObjC selector: @- zPosition@
zPosition :: IsSKNode skNode => skNode -> IO CDouble
zPosition skNode =
  sendMessage skNode zPositionSelector

-- | The z-order of the node (used for ordering). Negative z is "into" the screen, Positive z is "out" of the screen. A greater zPosition will sort in front of a lesser zPosition.
--
-- ObjC selector: @- setZPosition:@
setZPosition :: IsSKNode skNode => skNode -> CDouble -> IO ()
setZPosition skNode value =
  sendMessage skNode setZPositionSelector value

-- | The Euler rotation about the z axis (in radians)
--
-- ObjC selector: @- zRotation@
zRotation :: IsSKNode skNode => skNode -> IO CDouble
zRotation skNode =
  sendMessage skNode zRotationSelector

-- | The Euler rotation about the z axis (in radians)
--
-- ObjC selector: @- setZRotation:@
setZRotation :: IsSKNode skNode => skNode -> CDouble -> IO ()
setZRotation skNode value =
  sendMessage skNode setZRotationSelector value

-- | The scaling in the X axis
--
-- ObjC selector: @- xScale@
xScale :: IsSKNode skNode => skNode -> IO CDouble
xScale skNode =
  sendMessage skNode xScaleSelector

-- | The scaling in the X axis
--
-- ObjC selector: @- setXScale:@
setXScale :: IsSKNode skNode => skNode -> CDouble -> IO ()
setXScale skNode value =
  sendMessage skNode setXScaleSelector value

-- | The scaling in the Y axis
--
-- ObjC selector: @- yScale@
yScale :: IsSKNode skNode => skNode -> IO CDouble
yScale skNode =
  sendMessage skNode yScaleSelector

-- | The scaling in the Y axis
--
-- ObjC selector: @- setYScale:@
setYScale :: IsSKNode skNode => skNode -> CDouble -> IO ()
setYScale skNode value =
  sendMessage skNode setYScaleSelector value

-- | The speed multiplier applied to all actions run on this node. Inherited by its children.
--
-- ObjC selector: @- speed@
speed :: IsSKNode skNode => skNode -> IO CDouble
speed skNode =
  sendMessage skNode speedSelector

-- | The speed multiplier applied to all actions run on this node. Inherited by its children.
--
-- ObjC selector: @- setSpeed:@
setSpeed :: IsSKNode skNode => skNode -> CDouble -> IO ()
setSpeed skNode value =
  sendMessage skNode setSpeedSelector value

-- | Alpha of this node (multiplied by the output color to give the final result)
--
-- ObjC selector: @- alpha@
alpha :: IsSKNode skNode => skNode -> IO CDouble
alpha skNode =
  sendMessage skNode alphaSelector

-- | Alpha of this node (multiplied by the output color to give the final result)
--
-- ObjC selector: @- setAlpha:@
setAlpha :: IsSKNode skNode => skNode -> CDouble -> IO ()
setAlpha skNode value =
  sendMessage skNode setAlphaSelector value

-- | Controls whether or not the node's actions is updated or paused.
--
-- ObjC selector: @- paused@
paused :: IsSKNode skNode => skNode -> IO Bool
paused skNode =
  sendMessage skNode pausedSelector

-- | Controls whether or not the node's actions is updated or paused.
--
-- ObjC selector: @- setPaused:@
setPaused :: IsSKNode skNode => skNode -> Bool -> IO ()
setPaused skNode value =
  sendMessage skNode setPausedSelector value

-- | Controls whether or not the node and its children are rendered.
--
-- ObjC selector: @- hidden@
hidden :: IsSKNode skNode => skNode -> IO Bool
hidden skNode =
  sendMessage skNode hiddenSelector

-- | Controls whether or not the node and its children are rendered.
--
-- ObjC selector: @- setHidden:@
setHidden :: IsSKNode skNode => skNode -> Bool -> IO ()
setHidden skNode value =
  sendMessage skNode setHiddenSelector value

-- | Controls whether or not the node receives touch events
--
-- ObjC selector: @- userInteractionEnabled@
userInteractionEnabled :: IsSKNode skNode => skNode -> IO Bool
userInteractionEnabled skNode =
  sendMessage skNode userInteractionEnabledSelector

-- | Controls whether or not the node receives touch events
--
-- ObjC selector: @- setUserInteractionEnabled:@
setUserInteractionEnabled :: IsSKNode skNode => skNode -> Bool -> IO ()
setUserInteractionEnabled skNode value =
  sendMessage skNode setUserInteractionEnabledSelector value

-- | Determines how this node participates in the focus system.  The default is SKNodeFocusBehaviorNone.
--
-- ObjC selector: @- focusBehavior@
focusBehavior :: IsSKNode skNode => skNode -> IO SKNodeFocusBehavior
focusBehavior skNode =
  sendMessage skNode focusBehaviorSelector

-- | Determines how this node participates in the focus system.  The default is SKNodeFocusBehaviorNone.
--
-- ObjC selector: @- setFocusBehavior:@
setFocusBehavior :: IsSKNode skNode => skNode -> SKNodeFocusBehavior -> IO ()
setFocusBehavior skNode value =
  sendMessage skNode setFocusBehaviorSelector value

-- | The parent of the node.
--
-- If this is nil the node has not been added to another group and is thus the root node of its own graph.
--
-- ObjC selector: @- parent@
parent :: IsSKNode skNode => skNode -> IO (Id SKNode)
parent skNode =
  sendMessage skNode parentSelector

-- | The children of this node.
--
-- ObjC selector: @- children@
children :: IsSKNode skNode => skNode -> IO (Id NSArray)
children skNode =
  sendMessage skNode childrenSelector

-- | The client assignable name.
--
-- In general, this should be unique among peers in the scene graph.
--
-- ObjC selector: @- name@
name :: IsSKNode skNode => skNode -> IO (Id NSString)
name skNode =
  sendMessage skNode nameSelector

-- | The client assignable name.
--
-- In general, this should be unique among peers in the scene graph.
--
-- ObjC selector: @- setName:@
setName :: (IsSKNode skNode, IsNSString value) => skNode -> value -> IO ()
setName skNode value =
  sendMessage skNode setNameSelector (toNSString value)

-- | The scene that the node is currently in.
--
-- ObjC selector: @- scene@
scene :: IsSKNode skNode => skNode -> IO (Id SKScene)
scene skNode =
  sendMessage skNode sceneSelector

-- | Physics body attached to the node, with synchronized scale, rotation, and position
--
-- ObjC selector: @- physicsBody@
physicsBody :: IsSKNode skNode => skNode -> IO (Id SKPhysicsBody)
physicsBody skNode =
  sendMessage skNode physicsBodySelector

-- | Physics body attached to the node, with synchronized scale, rotation, and position
--
-- ObjC selector: @- setPhysicsBody:@
setPhysicsBody :: (IsSKNode skNode, IsSKPhysicsBody value) => skNode -> value -> IO ()
setPhysicsBody skNode value =
  sendMessage skNode setPhysicsBodySelector (toSKPhysicsBody value)

-- | An optional dictionary that can be used to store your own data in a node. Defaults to nil.
--
-- ObjC selector: @- userData@
userData :: IsSKNode skNode => skNode -> IO (Id NSMutableDictionary)
userData skNode =
  sendMessage skNode userDataSelector

-- | An optional dictionary that can be used to store your own data in a node. Defaults to nil.
--
-- ObjC selector: @- setUserData:@
setUserData :: (IsSKNode skNode, IsNSMutableDictionary value) => skNode -> value -> IO ()
setUserData skNode value =
  sendMessage skNode setUserDataSelector (toNSMutableDictionary value)

-- | Kinematic constraints, used in IK solving
--
-- ObjC selector: @- reachConstraints@
reachConstraints :: IsSKNode skNode => skNode -> IO (Id SKReachConstraints)
reachConstraints skNode =
  sendMessage skNode reachConstraintsSelector

-- | Kinematic constraints, used in IK solving
--
-- ObjC selector: @- setReachConstraints:@
setReachConstraints :: (IsSKNode skNode, IsSKReachConstraints value) => skNode -> value -> IO ()
setReachConstraints skNode value =
  sendMessage skNode setReachConstraintsSelector (toSKReachConstraints value)

-- | Optional array of SKConstraints Constraints are evaluated each frame after actions and physics. The node's transform will be changed to satisfy the constraint.
--
-- ObjC selector: @- constraints@
constraints :: IsSKNode skNode => skNode -> IO (Id NSArray)
constraints skNode =
  sendMessage skNode constraintsSelector

-- | Optional array of SKConstraints Constraints are evaluated each frame after actions and physics. The node's transform will be changed to satisfy the constraint.
--
-- ObjC selector: @- setConstraints:@
setConstraints :: (IsSKNode skNode, IsNSArray value) => skNode -> value -> IO ()
setConstraints skNode value =
  sendMessage skNode setConstraintsSelector (toNSArray value)

-- | @- accessibilityElement@
accessibilityElement :: IsSKNode skNode => skNode -> IO Bool
accessibilityElement skNode =
  sendMessage skNode accessibilityElementSelector

-- | @- setAccessibilityElement:@
setAccessibilityElement :: IsSKNode skNode => skNode -> Bool -> IO ()
setAccessibilityElement skNode value =
  sendMessage skNode setAccessibilityElementSelector value

-- | @- accessibilityRole@
accessibilityRole :: IsSKNode skNode => skNode -> IO (Id NSString)
accessibilityRole skNode =
  sendMessage skNode accessibilityRoleSelector

-- | @- setAccessibilityRole:@
setAccessibilityRole :: (IsSKNode skNode, IsNSString value) => skNode -> value -> IO ()
setAccessibilityRole skNode value =
  sendMessage skNode setAccessibilityRoleSelector (toNSString value)

-- | @- accessibilityRoleDescription@
accessibilityRoleDescription :: IsSKNode skNode => skNode -> IO (Id NSString)
accessibilityRoleDescription skNode =
  sendMessage skNode accessibilityRoleDescriptionSelector

-- | @- setAccessibilityRoleDescription:@
setAccessibilityRoleDescription :: (IsSKNode skNode, IsNSString value) => skNode -> value -> IO ()
setAccessibilityRoleDescription skNode value =
  sendMessage skNode setAccessibilityRoleDescriptionSelector (toNSString value)

-- | @- accessibilitySubrole@
accessibilitySubrole :: IsSKNode skNode => skNode -> IO (Id NSString)
accessibilitySubrole skNode =
  sendMessage skNode accessibilitySubroleSelector

-- | @- setAccessibilitySubrole:@
setAccessibilitySubrole :: (IsSKNode skNode, IsNSString value) => skNode -> value -> IO ()
setAccessibilitySubrole skNode value =
  sendMessage skNode setAccessibilitySubroleSelector (toNSString value)

-- | @- accessibilityParent@
accessibilityParent :: IsSKNode skNode => skNode -> IO RawId
accessibilityParent skNode =
  sendMessage skNode accessibilityParentSelector

-- | @- setAccessibilityParent:@
setAccessibilityParent :: IsSKNode skNode => skNode -> RawId -> IO ()
setAccessibilityParent skNode value =
  sendMessage skNode setAccessibilityParentSelector value

-- | @- accessibilityChildren@
accessibilityChildren :: IsSKNode skNode => skNode -> IO (Id NSArray)
accessibilityChildren skNode =
  sendMessage skNode accessibilityChildrenSelector

-- | @- setAccessibilityChildren:@
setAccessibilityChildren :: (IsSKNode skNode, IsNSArray value) => skNode -> value -> IO ()
setAccessibilityChildren skNode value =
  sendMessage skNode setAccessibilityChildrenSelector (toNSArray value)

-- | @- accessibilityHelp@
accessibilityHelp :: IsSKNode skNode => skNode -> IO (Id NSString)
accessibilityHelp skNode =
  sendMessage skNode accessibilityHelpSelector

-- | @- setAccessibilityHelp:@
setAccessibilityHelp :: (IsSKNode skNode, IsNSString value) => skNode -> value -> IO ()
setAccessibilityHelp skNode value =
  sendMessage skNode setAccessibilityHelpSelector (toNSString value)

-- | @- accessibilityLabel@
accessibilityLabel :: IsSKNode skNode => skNode -> IO (Id NSString)
accessibilityLabel skNode =
  sendMessage skNode accessibilityLabelSelector

-- | @- setAccessibilityLabel:@
setAccessibilityLabel :: (IsSKNode skNode, IsNSString value) => skNode -> value -> IO ()
setAccessibilityLabel skNode value =
  sendMessage skNode setAccessibilityLabelSelector (toNSString value)

-- | @- accessibilityEnabled@
accessibilityEnabled :: IsSKNode skNode => skNode -> IO Bool
accessibilityEnabled skNode =
  sendMessage skNode accessibilityEnabledSelector

-- | @- setAccessibilityEnabled:@
setAccessibilityEnabled :: IsSKNode skNode => skNode -> Bool -> IO ()
setAccessibilityEnabled skNode value =
  sendMessage skNode setAccessibilityEnabledSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SKNode)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id SKNode)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @node@
nodeSelector :: Selector '[] (Id SKNode)
nodeSelector = mkSelector "node"

-- | @Selector@ for @nodeWithFileNamed:@
nodeWithFileNamedSelector :: Selector '[Id NSString] (Id SKNode)
nodeWithFileNamedSelector = mkSelector "nodeWithFileNamed:"

-- | @Selector@ for @nodeWithFileNamed:securelyWithClasses:andError:@
nodeWithFileNamed_securelyWithClasses_andErrorSelector :: Selector '[Id NSString, Id NSSet, Id NSError] (Id SKNode)
nodeWithFileNamed_securelyWithClasses_andErrorSelector = mkSelector "nodeWithFileNamed:securelyWithClasses:andError:"

-- | @Selector@ for @valueForAttributeNamed:@
valueForAttributeNamedSelector :: Selector '[Id NSString] (Id SKAttributeValue)
valueForAttributeNamedSelector = mkSelector "valueForAttributeNamed:"

-- | @Selector@ for @setValue:forAttributeNamed:@
setValue_forAttributeNamedSelector :: Selector '[Id SKAttributeValue, Id NSString] ()
setValue_forAttributeNamedSelector = mkSelector "setValue:forAttributeNamed:"

-- | @Selector@ for @setScale:@
setScaleSelector :: Selector '[CDouble] ()
setScaleSelector = mkSelector "setScale:"

-- | @Selector@ for @addChild:@
addChildSelector :: Selector '[Id SKNode] ()
addChildSelector = mkSelector "addChild:"

-- | @Selector@ for @insertChild:atIndex:@
insertChild_atIndexSelector :: Selector '[Id SKNode, CLong] ()
insertChild_atIndexSelector = mkSelector "insertChild:atIndex:"

-- | @Selector@ for @removeChildrenInArray:@
removeChildrenInArraySelector :: Selector '[Id NSArray] ()
removeChildrenInArraySelector = mkSelector "removeChildrenInArray:"

-- | @Selector@ for @removeAllChildren@
removeAllChildrenSelector :: Selector '[] ()
removeAllChildrenSelector = mkSelector "removeAllChildren"

-- | @Selector@ for @removeFromParent@
removeFromParentSelector :: Selector '[] ()
removeFromParentSelector = mkSelector "removeFromParent"

-- | @Selector@ for @moveToParent:@
moveToParentSelector :: Selector '[Id SKNode] ()
moveToParentSelector = mkSelector "moveToParent:"

-- | @Selector@ for @childNodeWithName:@
childNodeWithNameSelector :: Selector '[Id NSString] (Id SKNode)
childNodeWithNameSelector = mkSelector "childNodeWithName:"

-- | @Selector@ for @enumerateChildNodesWithName:usingBlock:@
enumerateChildNodesWithName_usingBlockSelector :: Selector '[Id NSString, Ptr ()] ()
enumerateChildNodesWithName_usingBlockSelector = mkSelector "enumerateChildNodesWithName:usingBlock:"

-- | @Selector@ for @objectForKeyedSubscript:@
objectForKeyedSubscriptSelector :: Selector '[Id NSString] (Id NSArray)
objectForKeyedSubscriptSelector = mkSelector "objectForKeyedSubscript:"

-- | @Selector@ for @inParentHierarchy:@
inParentHierarchySelector :: Selector '[Id SKNode] Bool
inParentHierarchySelector = mkSelector "inParentHierarchy:"

-- | @Selector@ for @runAction:@
runActionSelector :: Selector '[Id SKAction] ()
runActionSelector = mkSelector "runAction:"

-- | @Selector@ for @runAction:completion:@
runAction_completionSelector :: Selector '[Id SKAction, Ptr ()] ()
runAction_completionSelector = mkSelector "runAction:completion:"

-- | @Selector@ for @runAction:withKey:@
runAction_withKeySelector :: Selector '[Id SKAction, Id NSString] ()
runAction_withKeySelector = mkSelector "runAction:withKey:"

-- | @Selector@ for @hasActions@
hasActionsSelector :: Selector '[] Bool
hasActionsSelector = mkSelector "hasActions"

-- | @Selector@ for @actionForKey:@
actionForKeySelector :: Selector '[Id NSString] (Id SKAction)
actionForKeySelector = mkSelector "actionForKey:"

-- | @Selector@ for @removeActionForKey:@
removeActionForKeySelector :: Selector '[Id NSString] ()
removeActionForKeySelector = mkSelector "removeActionForKey:"

-- | @Selector@ for @removeAllActions@
removeAllActionsSelector :: Selector '[] ()
removeAllActionsSelector = mkSelector "removeAllActions"

-- | @Selector@ for @intersectsNode:@
intersectsNodeSelector :: Selector '[Id SKNode] Bool
intersectsNodeSelector = mkSelector "intersectsNode:"

-- | @Selector@ for @isEqualToNode:@
isEqualToNodeSelector :: Selector '[Id SKNode] Bool
isEqualToNodeSelector = mkSelector "isEqualToNode:"

-- | @Selector@ for @zPosition@
zPositionSelector :: Selector '[] CDouble
zPositionSelector = mkSelector "zPosition"

-- | @Selector@ for @setZPosition:@
setZPositionSelector :: Selector '[CDouble] ()
setZPositionSelector = mkSelector "setZPosition:"

-- | @Selector@ for @zRotation@
zRotationSelector :: Selector '[] CDouble
zRotationSelector = mkSelector "zRotation"

-- | @Selector@ for @setZRotation:@
setZRotationSelector :: Selector '[CDouble] ()
setZRotationSelector = mkSelector "setZRotation:"

-- | @Selector@ for @xScale@
xScaleSelector :: Selector '[] CDouble
xScaleSelector = mkSelector "xScale"

-- | @Selector@ for @setXScale:@
setXScaleSelector :: Selector '[CDouble] ()
setXScaleSelector = mkSelector "setXScale:"

-- | @Selector@ for @yScale@
yScaleSelector :: Selector '[] CDouble
yScaleSelector = mkSelector "yScale"

-- | @Selector@ for @setYScale:@
setYScaleSelector :: Selector '[CDouble] ()
setYScaleSelector = mkSelector "setYScale:"

-- | @Selector@ for @speed@
speedSelector :: Selector '[] CDouble
speedSelector = mkSelector "speed"

-- | @Selector@ for @setSpeed:@
setSpeedSelector :: Selector '[CDouble] ()
setSpeedSelector = mkSelector "setSpeed:"

-- | @Selector@ for @alpha@
alphaSelector :: Selector '[] CDouble
alphaSelector = mkSelector "alpha"

-- | @Selector@ for @setAlpha:@
setAlphaSelector :: Selector '[CDouble] ()
setAlphaSelector = mkSelector "setAlpha:"

-- | @Selector@ for @paused@
pausedSelector :: Selector '[] Bool
pausedSelector = mkSelector "paused"

-- | @Selector@ for @setPaused:@
setPausedSelector :: Selector '[Bool] ()
setPausedSelector = mkSelector "setPaused:"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector '[] Bool
hiddenSelector = mkSelector "hidden"

-- | @Selector@ for @setHidden:@
setHiddenSelector :: Selector '[Bool] ()
setHiddenSelector = mkSelector "setHidden:"

-- | @Selector@ for @userInteractionEnabled@
userInteractionEnabledSelector :: Selector '[] Bool
userInteractionEnabledSelector = mkSelector "userInteractionEnabled"

-- | @Selector@ for @setUserInteractionEnabled:@
setUserInteractionEnabledSelector :: Selector '[Bool] ()
setUserInteractionEnabledSelector = mkSelector "setUserInteractionEnabled:"

-- | @Selector@ for @focusBehavior@
focusBehaviorSelector :: Selector '[] SKNodeFocusBehavior
focusBehaviorSelector = mkSelector "focusBehavior"

-- | @Selector@ for @setFocusBehavior:@
setFocusBehaviorSelector :: Selector '[SKNodeFocusBehavior] ()
setFocusBehaviorSelector = mkSelector "setFocusBehavior:"

-- | @Selector@ for @parent@
parentSelector :: Selector '[] (Id SKNode)
parentSelector = mkSelector "parent"

-- | @Selector@ for @children@
childrenSelector :: Selector '[] (Id NSArray)
childrenSelector = mkSelector "children"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @scene@
sceneSelector :: Selector '[] (Id SKScene)
sceneSelector = mkSelector "scene"

-- | @Selector@ for @physicsBody@
physicsBodySelector :: Selector '[] (Id SKPhysicsBody)
physicsBodySelector = mkSelector "physicsBody"

-- | @Selector@ for @setPhysicsBody:@
setPhysicsBodySelector :: Selector '[Id SKPhysicsBody] ()
setPhysicsBodySelector = mkSelector "setPhysicsBody:"

-- | @Selector@ for @userData@
userDataSelector :: Selector '[] (Id NSMutableDictionary)
userDataSelector = mkSelector "userData"

-- | @Selector@ for @setUserData:@
setUserDataSelector :: Selector '[Id NSMutableDictionary] ()
setUserDataSelector = mkSelector "setUserData:"

-- | @Selector@ for @reachConstraints@
reachConstraintsSelector :: Selector '[] (Id SKReachConstraints)
reachConstraintsSelector = mkSelector "reachConstraints"

-- | @Selector@ for @setReachConstraints:@
setReachConstraintsSelector :: Selector '[Id SKReachConstraints] ()
setReachConstraintsSelector = mkSelector "setReachConstraints:"

-- | @Selector@ for @constraints@
constraintsSelector :: Selector '[] (Id NSArray)
constraintsSelector = mkSelector "constraints"

-- | @Selector@ for @setConstraints:@
setConstraintsSelector :: Selector '[Id NSArray] ()
setConstraintsSelector = mkSelector "setConstraints:"

-- | @Selector@ for @accessibilityElement@
accessibilityElementSelector :: Selector '[] Bool
accessibilityElementSelector = mkSelector "accessibilityElement"

-- | @Selector@ for @setAccessibilityElement:@
setAccessibilityElementSelector :: Selector '[Bool] ()
setAccessibilityElementSelector = mkSelector "setAccessibilityElement:"

-- | @Selector@ for @accessibilityRole@
accessibilityRoleSelector :: Selector '[] (Id NSString)
accessibilityRoleSelector = mkSelector "accessibilityRole"

-- | @Selector@ for @setAccessibilityRole:@
setAccessibilityRoleSelector :: Selector '[Id NSString] ()
setAccessibilityRoleSelector = mkSelector "setAccessibilityRole:"

-- | @Selector@ for @accessibilityRoleDescription@
accessibilityRoleDescriptionSelector :: Selector '[] (Id NSString)
accessibilityRoleDescriptionSelector = mkSelector "accessibilityRoleDescription"

-- | @Selector@ for @setAccessibilityRoleDescription:@
setAccessibilityRoleDescriptionSelector :: Selector '[Id NSString] ()
setAccessibilityRoleDescriptionSelector = mkSelector "setAccessibilityRoleDescription:"

-- | @Selector@ for @accessibilitySubrole@
accessibilitySubroleSelector :: Selector '[] (Id NSString)
accessibilitySubroleSelector = mkSelector "accessibilitySubrole"

-- | @Selector@ for @setAccessibilitySubrole:@
setAccessibilitySubroleSelector :: Selector '[Id NSString] ()
setAccessibilitySubroleSelector = mkSelector "setAccessibilitySubrole:"

-- | @Selector@ for @accessibilityParent@
accessibilityParentSelector :: Selector '[] RawId
accessibilityParentSelector = mkSelector "accessibilityParent"

-- | @Selector@ for @setAccessibilityParent:@
setAccessibilityParentSelector :: Selector '[RawId] ()
setAccessibilityParentSelector = mkSelector "setAccessibilityParent:"

-- | @Selector@ for @accessibilityChildren@
accessibilityChildrenSelector :: Selector '[] (Id NSArray)
accessibilityChildrenSelector = mkSelector "accessibilityChildren"

-- | @Selector@ for @setAccessibilityChildren:@
setAccessibilityChildrenSelector :: Selector '[Id NSArray] ()
setAccessibilityChildrenSelector = mkSelector "setAccessibilityChildren:"

-- | @Selector@ for @accessibilityHelp@
accessibilityHelpSelector :: Selector '[] (Id NSString)
accessibilityHelpSelector = mkSelector "accessibilityHelp"

-- | @Selector@ for @setAccessibilityHelp:@
setAccessibilityHelpSelector :: Selector '[Id NSString] ()
setAccessibilityHelpSelector = mkSelector "setAccessibilityHelp:"

-- | @Selector@ for @accessibilityLabel@
accessibilityLabelSelector :: Selector '[] (Id NSString)
accessibilityLabelSelector = mkSelector "accessibilityLabel"

-- | @Selector@ for @setAccessibilityLabel:@
setAccessibilityLabelSelector :: Selector '[Id NSString] ()
setAccessibilityLabelSelector = mkSelector "setAccessibilityLabel:"

-- | @Selector@ for @accessibilityEnabled@
accessibilityEnabledSelector :: Selector '[] Bool
accessibilityEnabledSelector = mkSelector "accessibilityEnabled"

-- | @Selector@ for @setAccessibilityEnabled:@
setAccessibilityEnabledSelector :: Selector '[Bool] ()
setAccessibilityEnabledSelector = mkSelector "setAccessibilityEnabled:"

