{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithCoderSelector
  , nodeSelector
  , nodeWithFileNamedSelector
  , nodeWithFileNamed_securelyWithClasses_andErrorSelector
  , valueForAttributeNamedSelector
  , setValue_forAttributeNamedSelector
  , setScaleSelector
  , addChildSelector
  , insertChild_atIndexSelector
  , removeChildrenInArraySelector
  , removeAllChildrenSelector
  , removeFromParentSelector
  , moveToParentSelector
  , childNodeWithNameSelector
  , enumerateChildNodesWithName_usingBlockSelector
  , objectForKeyedSubscriptSelector
  , inParentHierarchySelector
  , runActionSelector
  , runAction_completionSelector
  , runAction_withKeySelector
  , hasActionsSelector
  , actionForKeySelector
  , removeActionForKeySelector
  , removeAllActionsSelector
  , intersectsNodeSelector
  , isEqualToNodeSelector
  , zPositionSelector
  , setZPositionSelector
  , zRotationSelector
  , setZRotationSelector
  , xScaleSelector
  , setXScaleSelector
  , yScaleSelector
  , setYScaleSelector
  , speedSelector
  , setSpeedSelector
  , alphaSelector
  , setAlphaSelector
  , pausedSelector
  , setPausedSelector
  , hiddenSelector
  , setHiddenSelector
  , userInteractionEnabledSelector
  , setUserInteractionEnabledSelector
  , focusBehaviorSelector
  , setFocusBehaviorSelector
  , parentSelector
  , childrenSelector
  , nameSelector
  , setNameSelector
  , sceneSelector
  , physicsBodySelector
  , setPhysicsBodySelector
  , userDataSelector
  , setUserDataSelector
  , reachConstraintsSelector
  , setReachConstraintsSelector
  , constraintsSelector
  , setConstraintsSelector
  , accessibilityElementSelector
  , setAccessibilityElementSelector
  , accessibilityRoleSelector
  , setAccessibilityRoleSelector
  , accessibilityRoleDescriptionSelector
  , setAccessibilityRoleDescriptionSelector
  , accessibilitySubroleSelector
  , setAccessibilitySubroleSelector
  , accessibilityParentSelector
  , setAccessibilityParentSelector
  , accessibilityChildrenSelector
  , setAccessibilityChildrenSelector
  , accessibilityHelpSelector
  , setAccessibilityHelpSelector
  , accessibilityLabelSelector
  , setAccessibilityLabelSelector
  , accessibilityEnabledSelector
  , setAccessibilityEnabledSelector

  -- * Enum types
  , SKNodeFocusBehavior(SKNodeFocusBehavior)
  , pattern SKNodeFocusBehaviorNone
  , pattern SKNodeFocusBehaviorOccluding
  , pattern SKNodeFocusBehaviorFocusable

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
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSKNode skNode => skNode -> IO (Id SKNode)
init_ skNode  =
  sendMsg skNode (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Support coding and decoding via NSKeyedArchiver.
--
-- ObjC selector: @- initWithCoder:@
initWithCoder :: (IsSKNode skNode, IsNSCoder aDecoder) => skNode -> aDecoder -> IO (Id SKNode)
initWithCoder skNode  aDecoder =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg skNode (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ())] >>= ownedObject . castPtr

-- | @+ node@
node :: IO (Id SKNode)
node  =
  do
    cls' <- getRequiredClass "SKNode"
    sendClassMsg cls' (mkSelector "node") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ nodeWithFileNamed:@
nodeWithFileNamed :: IsNSString filename => filename -> IO (Id SKNode)
nodeWithFileNamed filename =
  do
    cls' <- getRequiredClass "SKNode"
    withObjCPtr filename $ \raw_filename ->
      sendClassMsg cls' (mkSelector "nodeWithFileNamed:") (retPtr retVoid) [argPtr (castPtr raw_filename :: Ptr ())] >>= retainedObject . castPtr

-- | @+ nodeWithFileNamed:securelyWithClasses:andError:@
nodeWithFileNamed_securelyWithClasses_andError :: (IsNSString filename, IsNSSet classes, IsNSError error_) => filename -> classes -> error_ -> IO (Id SKNode)
nodeWithFileNamed_securelyWithClasses_andError filename classes error_ =
  do
    cls' <- getRequiredClass "SKNode"
    withObjCPtr filename $ \raw_filename ->
      withObjCPtr classes $ \raw_classes ->
        withObjCPtr error_ $ \raw_error_ ->
          sendClassMsg cls' (mkSelector "nodeWithFileNamed:securelyWithClasses:andError:") (retPtr retVoid) [argPtr (castPtr raw_filename :: Ptr ()), argPtr (castPtr raw_classes :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- valueForAttributeNamed:@
valueForAttributeNamed :: (IsSKNode skNode, IsNSString key) => skNode -> key -> IO (Id SKAttributeValue)
valueForAttributeNamed skNode  key =
withObjCPtr key $ \raw_key ->
    sendMsg skNode (mkSelector "valueForAttributeNamed:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())] >>= retainedObject . castPtr

-- | @- setValue:forAttributeNamed:@
setValue_forAttributeNamed :: (IsSKNode skNode, IsSKAttributeValue value, IsNSString key) => skNode -> value -> key -> IO ()
setValue_forAttributeNamed skNode  value key =
withObjCPtr value $ \raw_value ->
  withObjCPtr key $ \raw_key ->
      sendMsg skNode (mkSelector "setValue:forAttributeNamed:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | Sets both the x & y scale
--
-- @scale@ — the uniform scale to set.
--
-- ObjC selector: @- setScale:@
setScale :: IsSKNode skNode => skNode -> CDouble -> IO ()
setScale skNode  scale =
  sendMsg skNode (mkSelector "setScale:") retVoid [argCDouble (fromIntegral scale)]

-- | Adds a node as a child node of this node
--
-- The added node must not have a parent.
--
-- @node@ — the child node to add.
--
-- ObjC selector: @- addChild:@
addChild :: (IsSKNode skNode, IsSKNode node) => skNode -> node -> IO ()
addChild skNode  node =
withObjCPtr node $ \raw_node ->
    sendMsg skNode (mkSelector "addChild:") retVoid [argPtr (castPtr raw_node :: Ptr ())]

-- | @- insertChild:atIndex:@
insertChild_atIndex :: (IsSKNode skNode, IsSKNode node) => skNode -> node -> CLong -> IO ()
insertChild_atIndex skNode  node index =
withObjCPtr node $ \raw_node ->
    sendMsg skNode (mkSelector "insertChild:atIndex:") retVoid [argPtr (castPtr raw_node :: Ptr ()), argCLong (fromIntegral index)]

-- | @- removeChildrenInArray:@
removeChildrenInArray :: (IsSKNode skNode, IsNSArray nodes) => skNode -> nodes -> IO ()
removeChildrenInArray skNode  nodes =
withObjCPtr nodes $ \raw_nodes ->
    sendMsg skNode (mkSelector "removeChildrenInArray:") retVoid [argPtr (castPtr raw_nodes :: Ptr ())]

-- | @- removeAllChildren@
removeAllChildren :: IsSKNode skNode => skNode -> IO ()
removeAllChildren skNode  =
  sendMsg skNode (mkSelector "removeAllChildren") retVoid []

-- | @- removeFromParent@
removeFromParent :: IsSKNode skNode => skNode -> IO ()
removeFromParent skNode  =
  sendMsg skNode (mkSelector "removeFromParent") retVoid []

-- | @- moveToParent:@
moveToParent :: (IsSKNode skNode, IsSKNode parent) => skNode -> parent -> IO ()
moveToParent skNode  parent =
withObjCPtr parent $ \raw_parent ->
    sendMsg skNode (mkSelector "moveToParent:") retVoid [argPtr (castPtr raw_parent :: Ptr ())]

-- | @- childNodeWithName:@
childNodeWithName :: (IsSKNode skNode, IsNSString name) => skNode -> name -> IO (Id SKNode)
childNodeWithName skNode  name =
withObjCPtr name $ \raw_name ->
    sendMsg skNode (mkSelector "childNodeWithName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @- enumerateChildNodesWithName:usingBlock:@
enumerateChildNodesWithName_usingBlock :: (IsSKNode skNode, IsNSString name) => skNode -> name -> Ptr () -> IO ()
enumerateChildNodesWithName_usingBlock skNode  name block =
withObjCPtr name $ \raw_name ->
    sendMsg skNode (mkSelector "enumerateChildNodesWithName:usingBlock:") retVoid [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr block :: Ptr ())]

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
objectForKeyedSubscript skNode  name =
withObjCPtr name $ \raw_name ->
    sendMsg skNode (mkSelector "objectForKeyedSubscript:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @- inParentHierarchy:@
inParentHierarchy :: (IsSKNode skNode, IsSKNode parent) => skNode -> parent -> IO Bool
inParentHierarchy skNode  parent =
withObjCPtr parent $ \raw_parent ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg skNode (mkSelector "inParentHierarchy:") retCULong [argPtr (castPtr raw_parent :: Ptr ())]

-- | @- runAction:@
runAction :: (IsSKNode skNode, IsSKAction action) => skNode -> action -> IO ()
runAction skNode  action =
withObjCPtr action $ \raw_action ->
    sendMsg skNode (mkSelector "runAction:") retVoid [argPtr (castPtr raw_action :: Ptr ())]

-- | @- runAction:completion:@
runAction_completion :: (IsSKNode skNode, IsSKAction action) => skNode -> action -> Ptr () -> IO ()
runAction_completion skNode  action block =
withObjCPtr action $ \raw_action ->
    sendMsg skNode (mkSelector "runAction:completion:") retVoid [argPtr (castPtr raw_action :: Ptr ()), argPtr (castPtr block :: Ptr ())]

-- | @- runAction:withKey:@
runAction_withKey :: (IsSKNode skNode, IsSKAction action, IsNSString key) => skNode -> action -> key -> IO ()
runAction_withKey skNode  action key =
withObjCPtr action $ \raw_action ->
  withObjCPtr key $ \raw_key ->
      sendMsg skNode (mkSelector "runAction:withKey:") retVoid [argPtr (castPtr raw_action :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @- hasActions@
hasActions :: IsSKNode skNode => skNode -> IO Bool
hasActions skNode  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skNode (mkSelector "hasActions") retCULong []

-- | @- actionForKey:@
actionForKey :: (IsSKNode skNode, IsNSString key) => skNode -> key -> IO (Id SKAction)
actionForKey skNode  key =
withObjCPtr key $ \raw_key ->
    sendMsg skNode (mkSelector "actionForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())] >>= retainedObject . castPtr

-- | @- removeActionForKey:@
removeActionForKey :: (IsSKNode skNode, IsNSString key) => skNode -> key -> IO ()
removeActionForKey skNode  key =
withObjCPtr key $ \raw_key ->
    sendMsg skNode (mkSelector "removeActionForKey:") retVoid [argPtr (castPtr raw_key :: Ptr ())]

-- | @- removeAllActions@
removeAllActions :: IsSKNode skNode => skNode -> IO ()
removeAllActions skNode  =
  sendMsg skNode (mkSelector "removeAllActions") retVoid []

-- | @- intersectsNode:@
intersectsNode :: (IsSKNode skNode, IsSKNode node) => skNode -> node -> IO Bool
intersectsNode skNode  node =
withObjCPtr node $ \raw_node ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg skNode (mkSelector "intersectsNode:") retCULong [argPtr (castPtr raw_node :: Ptr ())]

-- | @- isEqualToNode:@
isEqualToNode :: (IsSKNode skNode, IsSKNode node) => skNode -> node -> IO Bool
isEqualToNode skNode  node =
withObjCPtr node $ \raw_node ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg skNode (mkSelector "isEqualToNode:") retCULong [argPtr (castPtr raw_node :: Ptr ())]

-- | The z-order of the node (used for ordering). Negative z is "into" the screen, Positive z is "out" of the screen. A greater zPosition will sort in front of a lesser zPosition.
--
-- ObjC selector: @- zPosition@
zPosition :: IsSKNode skNode => skNode -> IO CDouble
zPosition skNode  =
  sendMsg skNode (mkSelector "zPosition") retCDouble []

-- | The z-order of the node (used for ordering). Negative z is "into" the screen, Positive z is "out" of the screen. A greater zPosition will sort in front of a lesser zPosition.
--
-- ObjC selector: @- setZPosition:@
setZPosition :: IsSKNode skNode => skNode -> CDouble -> IO ()
setZPosition skNode  value =
  sendMsg skNode (mkSelector "setZPosition:") retVoid [argCDouble (fromIntegral value)]

-- | The Euler rotation about the z axis (in radians)
--
-- ObjC selector: @- zRotation@
zRotation :: IsSKNode skNode => skNode -> IO CDouble
zRotation skNode  =
  sendMsg skNode (mkSelector "zRotation") retCDouble []

-- | The Euler rotation about the z axis (in radians)
--
-- ObjC selector: @- setZRotation:@
setZRotation :: IsSKNode skNode => skNode -> CDouble -> IO ()
setZRotation skNode  value =
  sendMsg skNode (mkSelector "setZRotation:") retVoid [argCDouble (fromIntegral value)]

-- | The scaling in the X axis
--
-- ObjC selector: @- xScale@
xScale :: IsSKNode skNode => skNode -> IO CDouble
xScale skNode  =
  sendMsg skNode (mkSelector "xScale") retCDouble []

-- | The scaling in the X axis
--
-- ObjC selector: @- setXScale:@
setXScale :: IsSKNode skNode => skNode -> CDouble -> IO ()
setXScale skNode  value =
  sendMsg skNode (mkSelector "setXScale:") retVoid [argCDouble (fromIntegral value)]

-- | The scaling in the Y axis
--
-- ObjC selector: @- yScale@
yScale :: IsSKNode skNode => skNode -> IO CDouble
yScale skNode  =
  sendMsg skNode (mkSelector "yScale") retCDouble []

-- | The scaling in the Y axis
--
-- ObjC selector: @- setYScale:@
setYScale :: IsSKNode skNode => skNode -> CDouble -> IO ()
setYScale skNode  value =
  sendMsg skNode (mkSelector "setYScale:") retVoid [argCDouble (fromIntegral value)]

-- | The speed multiplier applied to all actions run on this node. Inherited by its children.
--
-- ObjC selector: @- speed@
speed :: IsSKNode skNode => skNode -> IO CDouble
speed skNode  =
  sendMsg skNode (mkSelector "speed") retCDouble []

-- | The speed multiplier applied to all actions run on this node. Inherited by its children.
--
-- ObjC selector: @- setSpeed:@
setSpeed :: IsSKNode skNode => skNode -> CDouble -> IO ()
setSpeed skNode  value =
  sendMsg skNode (mkSelector "setSpeed:") retVoid [argCDouble (fromIntegral value)]

-- | Alpha of this node (multiplied by the output color to give the final result)
--
-- ObjC selector: @- alpha@
alpha :: IsSKNode skNode => skNode -> IO CDouble
alpha skNode  =
  sendMsg skNode (mkSelector "alpha") retCDouble []

-- | Alpha of this node (multiplied by the output color to give the final result)
--
-- ObjC selector: @- setAlpha:@
setAlpha :: IsSKNode skNode => skNode -> CDouble -> IO ()
setAlpha skNode  value =
  sendMsg skNode (mkSelector "setAlpha:") retVoid [argCDouble (fromIntegral value)]

-- | Controls whether or not the node's actions is updated or paused.
--
-- ObjC selector: @- paused@
paused :: IsSKNode skNode => skNode -> IO Bool
paused skNode  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skNode (mkSelector "paused") retCULong []

-- | Controls whether or not the node's actions is updated or paused.
--
-- ObjC selector: @- setPaused:@
setPaused :: IsSKNode skNode => skNode -> Bool -> IO ()
setPaused skNode  value =
  sendMsg skNode (mkSelector "setPaused:") retVoid [argCULong (if value then 1 else 0)]

-- | Controls whether or not the node and its children are rendered.
--
-- ObjC selector: @- hidden@
hidden :: IsSKNode skNode => skNode -> IO Bool
hidden skNode  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skNode (mkSelector "hidden") retCULong []

-- | Controls whether or not the node and its children are rendered.
--
-- ObjC selector: @- setHidden:@
setHidden :: IsSKNode skNode => skNode -> Bool -> IO ()
setHidden skNode  value =
  sendMsg skNode (mkSelector "setHidden:") retVoid [argCULong (if value then 1 else 0)]

-- | Controls whether or not the node receives touch events
--
-- ObjC selector: @- userInteractionEnabled@
userInteractionEnabled :: IsSKNode skNode => skNode -> IO Bool
userInteractionEnabled skNode  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skNode (mkSelector "userInteractionEnabled") retCULong []

-- | Controls whether or not the node receives touch events
--
-- ObjC selector: @- setUserInteractionEnabled:@
setUserInteractionEnabled :: IsSKNode skNode => skNode -> Bool -> IO ()
setUserInteractionEnabled skNode  value =
  sendMsg skNode (mkSelector "setUserInteractionEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | Determines how this node participates in the focus system.  The default is SKNodeFocusBehaviorNone.
--
-- ObjC selector: @- focusBehavior@
focusBehavior :: IsSKNode skNode => skNode -> IO SKNodeFocusBehavior
focusBehavior skNode  =
  fmap (coerce :: CLong -> SKNodeFocusBehavior) $ sendMsg skNode (mkSelector "focusBehavior") retCLong []

-- | Determines how this node participates in the focus system.  The default is SKNodeFocusBehaviorNone.
--
-- ObjC selector: @- setFocusBehavior:@
setFocusBehavior :: IsSKNode skNode => skNode -> SKNodeFocusBehavior -> IO ()
setFocusBehavior skNode  value =
  sendMsg skNode (mkSelector "setFocusBehavior:") retVoid [argCLong (coerce value)]

-- | The parent of the node.
--
-- If this is nil the node has not been added to another group and is thus the root node of its own graph.
--
-- ObjC selector: @- parent@
parent :: IsSKNode skNode => skNode -> IO (Id SKNode)
parent skNode  =
  sendMsg skNode (mkSelector "parent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The children of this node.
--
-- ObjC selector: @- children@
children :: IsSKNode skNode => skNode -> IO (Id NSArray)
children skNode  =
  sendMsg skNode (mkSelector "children") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The client assignable name.
--
-- In general, this should be unique among peers in the scene graph.
--
-- ObjC selector: @- name@
name :: IsSKNode skNode => skNode -> IO (Id NSString)
name skNode  =
  sendMsg skNode (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The client assignable name.
--
-- In general, this should be unique among peers in the scene graph.
--
-- ObjC selector: @- setName:@
setName :: (IsSKNode skNode, IsNSString value) => skNode -> value -> IO ()
setName skNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skNode (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The scene that the node is currently in.
--
-- ObjC selector: @- scene@
scene :: IsSKNode skNode => skNode -> IO (Id SKScene)
scene skNode  =
  sendMsg skNode (mkSelector "scene") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Physics body attached to the node, with synchronized scale, rotation, and position
--
-- ObjC selector: @- physicsBody@
physicsBody :: IsSKNode skNode => skNode -> IO (Id SKPhysicsBody)
physicsBody skNode  =
  sendMsg skNode (mkSelector "physicsBody") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Physics body attached to the node, with synchronized scale, rotation, and position
--
-- ObjC selector: @- setPhysicsBody:@
setPhysicsBody :: (IsSKNode skNode, IsSKPhysicsBody value) => skNode -> value -> IO ()
setPhysicsBody skNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skNode (mkSelector "setPhysicsBody:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | An optional dictionary that can be used to store your own data in a node. Defaults to nil.
--
-- ObjC selector: @- userData@
userData :: IsSKNode skNode => skNode -> IO (Id NSMutableDictionary)
userData skNode  =
  sendMsg skNode (mkSelector "userData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An optional dictionary that can be used to store your own data in a node. Defaults to nil.
--
-- ObjC selector: @- setUserData:@
setUserData :: (IsSKNode skNode, IsNSMutableDictionary value) => skNode -> value -> IO ()
setUserData skNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skNode (mkSelector "setUserData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Kinematic constraints, used in IK solving
--
-- ObjC selector: @- reachConstraints@
reachConstraints :: IsSKNode skNode => skNode -> IO (Id SKReachConstraints)
reachConstraints skNode  =
  sendMsg skNode (mkSelector "reachConstraints") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Kinematic constraints, used in IK solving
--
-- ObjC selector: @- setReachConstraints:@
setReachConstraints :: (IsSKNode skNode, IsSKReachConstraints value) => skNode -> value -> IO ()
setReachConstraints skNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skNode (mkSelector "setReachConstraints:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Optional array of SKConstraints Constraints are evaluated each frame after actions and physics. The node's transform will be changed to satisfy the constraint.
--
-- ObjC selector: @- constraints@
constraints :: IsSKNode skNode => skNode -> IO (Id NSArray)
constraints skNode  =
  sendMsg skNode (mkSelector "constraints") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Optional array of SKConstraints Constraints are evaluated each frame after actions and physics. The node's transform will be changed to satisfy the constraint.
--
-- ObjC selector: @- setConstraints:@
setConstraints :: (IsSKNode skNode, IsNSArray value) => skNode -> value -> IO ()
setConstraints skNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skNode (mkSelector "setConstraints:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- accessibilityElement@
accessibilityElement :: IsSKNode skNode => skNode -> IO Bool
accessibilityElement skNode  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skNode (mkSelector "accessibilityElement") retCULong []

-- | @- setAccessibilityElement:@
setAccessibilityElement :: IsSKNode skNode => skNode -> Bool -> IO ()
setAccessibilityElement skNode  value =
  sendMsg skNode (mkSelector "setAccessibilityElement:") retVoid [argCULong (if value then 1 else 0)]

-- | @- accessibilityRole@
accessibilityRole :: IsSKNode skNode => skNode -> IO (Id NSString)
accessibilityRole skNode  =
  sendMsg skNode (mkSelector "accessibilityRole") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAccessibilityRole:@
setAccessibilityRole :: (IsSKNode skNode, IsNSString value) => skNode -> value -> IO ()
setAccessibilityRole skNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skNode (mkSelector "setAccessibilityRole:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- accessibilityRoleDescription@
accessibilityRoleDescription :: IsSKNode skNode => skNode -> IO (Id NSString)
accessibilityRoleDescription skNode  =
  sendMsg skNode (mkSelector "accessibilityRoleDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAccessibilityRoleDescription:@
setAccessibilityRoleDescription :: (IsSKNode skNode, IsNSString value) => skNode -> value -> IO ()
setAccessibilityRoleDescription skNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skNode (mkSelector "setAccessibilityRoleDescription:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- accessibilitySubrole@
accessibilitySubrole :: IsSKNode skNode => skNode -> IO (Id NSString)
accessibilitySubrole skNode  =
  sendMsg skNode (mkSelector "accessibilitySubrole") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAccessibilitySubrole:@
setAccessibilitySubrole :: (IsSKNode skNode, IsNSString value) => skNode -> value -> IO ()
setAccessibilitySubrole skNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skNode (mkSelector "setAccessibilitySubrole:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- accessibilityParent@
accessibilityParent :: IsSKNode skNode => skNode -> IO RawId
accessibilityParent skNode  =
  fmap (RawId . castPtr) $ sendMsg skNode (mkSelector "accessibilityParent") (retPtr retVoid) []

-- | @- setAccessibilityParent:@
setAccessibilityParent :: IsSKNode skNode => skNode -> RawId -> IO ()
setAccessibilityParent skNode  value =
  sendMsg skNode (mkSelector "setAccessibilityParent:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- accessibilityChildren@
accessibilityChildren :: IsSKNode skNode => skNode -> IO (Id NSArray)
accessibilityChildren skNode  =
  sendMsg skNode (mkSelector "accessibilityChildren") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAccessibilityChildren:@
setAccessibilityChildren :: (IsSKNode skNode, IsNSArray value) => skNode -> value -> IO ()
setAccessibilityChildren skNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skNode (mkSelector "setAccessibilityChildren:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- accessibilityHelp@
accessibilityHelp :: IsSKNode skNode => skNode -> IO (Id NSString)
accessibilityHelp skNode  =
  sendMsg skNode (mkSelector "accessibilityHelp") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAccessibilityHelp:@
setAccessibilityHelp :: (IsSKNode skNode, IsNSString value) => skNode -> value -> IO ()
setAccessibilityHelp skNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skNode (mkSelector "setAccessibilityHelp:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- accessibilityLabel@
accessibilityLabel :: IsSKNode skNode => skNode -> IO (Id NSString)
accessibilityLabel skNode  =
  sendMsg skNode (mkSelector "accessibilityLabel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAccessibilityLabel:@
setAccessibilityLabel :: (IsSKNode skNode, IsNSString value) => skNode -> value -> IO ()
setAccessibilityLabel skNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skNode (mkSelector "setAccessibilityLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- accessibilityEnabled@
accessibilityEnabled :: IsSKNode skNode => skNode -> IO Bool
accessibilityEnabled skNode  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skNode (mkSelector "accessibilityEnabled") retCULong []

-- | @- setAccessibilityEnabled:@
setAccessibilityEnabled :: IsSKNode skNode => skNode -> Bool -> IO ()
setAccessibilityEnabled skNode  value =
  sendMsg skNode (mkSelector "setAccessibilityEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @node@
nodeSelector :: Selector
nodeSelector = mkSelector "node"

-- | @Selector@ for @nodeWithFileNamed:@
nodeWithFileNamedSelector :: Selector
nodeWithFileNamedSelector = mkSelector "nodeWithFileNamed:"

-- | @Selector@ for @nodeWithFileNamed:securelyWithClasses:andError:@
nodeWithFileNamed_securelyWithClasses_andErrorSelector :: Selector
nodeWithFileNamed_securelyWithClasses_andErrorSelector = mkSelector "nodeWithFileNamed:securelyWithClasses:andError:"

-- | @Selector@ for @valueForAttributeNamed:@
valueForAttributeNamedSelector :: Selector
valueForAttributeNamedSelector = mkSelector "valueForAttributeNamed:"

-- | @Selector@ for @setValue:forAttributeNamed:@
setValue_forAttributeNamedSelector :: Selector
setValue_forAttributeNamedSelector = mkSelector "setValue:forAttributeNamed:"

-- | @Selector@ for @setScale:@
setScaleSelector :: Selector
setScaleSelector = mkSelector "setScale:"

-- | @Selector@ for @addChild:@
addChildSelector :: Selector
addChildSelector = mkSelector "addChild:"

-- | @Selector@ for @insertChild:atIndex:@
insertChild_atIndexSelector :: Selector
insertChild_atIndexSelector = mkSelector "insertChild:atIndex:"

-- | @Selector@ for @removeChildrenInArray:@
removeChildrenInArraySelector :: Selector
removeChildrenInArraySelector = mkSelector "removeChildrenInArray:"

-- | @Selector@ for @removeAllChildren@
removeAllChildrenSelector :: Selector
removeAllChildrenSelector = mkSelector "removeAllChildren"

-- | @Selector@ for @removeFromParent@
removeFromParentSelector :: Selector
removeFromParentSelector = mkSelector "removeFromParent"

-- | @Selector@ for @moveToParent:@
moveToParentSelector :: Selector
moveToParentSelector = mkSelector "moveToParent:"

-- | @Selector@ for @childNodeWithName:@
childNodeWithNameSelector :: Selector
childNodeWithNameSelector = mkSelector "childNodeWithName:"

-- | @Selector@ for @enumerateChildNodesWithName:usingBlock:@
enumerateChildNodesWithName_usingBlockSelector :: Selector
enumerateChildNodesWithName_usingBlockSelector = mkSelector "enumerateChildNodesWithName:usingBlock:"

-- | @Selector@ for @objectForKeyedSubscript:@
objectForKeyedSubscriptSelector :: Selector
objectForKeyedSubscriptSelector = mkSelector "objectForKeyedSubscript:"

-- | @Selector@ for @inParentHierarchy:@
inParentHierarchySelector :: Selector
inParentHierarchySelector = mkSelector "inParentHierarchy:"

-- | @Selector@ for @runAction:@
runActionSelector :: Selector
runActionSelector = mkSelector "runAction:"

-- | @Selector@ for @runAction:completion:@
runAction_completionSelector :: Selector
runAction_completionSelector = mkSelector "runAction:completion:"

-- | @Selector@ for @runAction:withKey:@
runAction_withKeySelector :: Selector
runAction_withKeySelector = mkSelector "runAction:withKey:"

-- | @Selector@ for @hasActions@
hasActionsSelector :: Selector
hasActionsSelector = mkSelector "hasActions"

-- | @Selector@ for @actionForKey:@
actionForKeySelector :: Selector
actionForKeySelector = mkSelector "actionForKey:"

-- | @Selector@ for @removeActionForKey:@
removeActionForKeySelector :: Selector
removeActionForKeySelector = mkSelector "removeActionForKey:"

-- | @Selector@ for @removeAllActions@
removeAllActionsSelector :: Selector
removeAllActionsSelector = mkSelector "removeAllActions"

-- | @Selector@ for @intersectsNode:@
intersectsNodeSelector :: Selector
intersectsNodeSelector = mkSelector "intersectsNode:"

-- | @Selector@ for @isEqualToNode:@
isEqualToNodeSelector :: Selector
isEqualToNodeSelector = mkSelector "isEqualToNode:"

-- | @Selector@ for @zPosition@
zPositionSelector :: Selector
zPositionSelector = mkSelector "zPosition"

-- | @Selector@ for @setZPosition:@
setZPositionSelector :: Selector
setZPositionSelector = mkSelector "setZPosition:"

-- | @Selector@ for @zRotation@
zRotationSelector :: Selector
zRotationSelector = mkSelector "zRotation"

-- | @Selector@ for @setZRotation:@
setZRotationSelector :: Selector
setZRotationSelector = mkSelector "setZRotation:"

-- | @Selector@ for @xScale@
xScaleSelector :: Selector
xScaleSelector = mkSelector "xScale"

-- | @Selector@ for @setXScale:@
setXScaleSelector :: Selector
setXScaleSelector = mkSelector "setXScale:"

-- | @Selector@ for @yScale@
yScaleSelector :: Selector
yScaleSelector = mkSelector "yScale"

-- | @Selector@ for @setYScale:@
setYScaleSelector :: Selector
setYScaleSelector = mkSelector "setYScale:"

-- | @Selector@ for @speed@
speedSelector :: Selector
speedSelector = mkSelector "speed"

-- | @Selector@ for @setSpeed:@
setSpeedSelector :: Selector
setSpeedSelector = mkSelector "setSpeed:"

-- | @Selector@ for @alpha@
alphaSelector :: Selector
alphaSelector = mkSelector "alpha"

-- | @Selector@ for @setAlpha:@
setAlphaSelector :: Selector
setAlphaSelector = mkSelector "setAlpha:"

-- | @Selector@ for @paused@
pausedSelector :: Selector
pausedSelector = mkSelector "paused"

-- | @Selector@ for @setPaused:@
setPausedSelector :: Selector
setPausedSelector = mkSelector "setPaused:"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector
hiddenSelector = mkSelector "hidden"

-- | @Selector@ for @setHidden:@
setHiddenSelector :: Selector
setHiddenSelector = mkSelector "setHidden:"

-- | @Selector@ for @userInteractionEnabled@
userInteractionEnabledSelector :: Selector
userInteractionEnabledSelector = mkSelector "userInteractionEnabled"

-- | @Selector@ for @setUserInteractionEnabled:@
setUserInteractionEnabledSelector :: Selector
setUserInteractionEnabledSelector = mkSelector "setUserInteractionEnabled:"

-- | @Selector@ for @focusBehavior@
focusBehaviorSelector :: Selector
focusBehaviorSelector = mkSelector "focusBehavior"

-- | @Selector@ for @setFocusBehavior:@
setFocusBehaviorSelector :: Selector
setFocusBehaviorSelector = mkSelector "setFocusBehavior:"

-- | @Selector@ for @parent@
parentSelector :: Selector
parentSelector = mkSelector "parent"

-- | @Selector@ for @children@
childrenSelector :: Selector
childrenSelector = mkSelector "children"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @scene@
sceneSelector :: Selector
sceneSelector = mkSelector "scene"

-- | @Selector@ for @physicsBody@
physicsBodySelector :: Selector
physicsBodySelector = mkSelector "physicsBody"

-- | @Selector@ for @setPhysicsBody:@
setPhysicsBodySelector :: Selector
setPhysicsBodySelector = mkSelector "setPhysicsBody:"

-- | @Selector@ for @userData@
userDataSelector :: Selector
userDataSelector = mkSelector "userData"

-- | @Selector@ for @setUserData:@
setUserDataSelector :: Selector
setUserDataSelector = mkSelector "setUserData:"

-- | @Selector@ for @reachConstraints@
reachConstraintsSelector :: Selector
reachConstraintsSelector = mkSelector "reachConstraints"

-- | @Selector@ for @setReachConstraints:@
setReachConstraintsSelector :: Selector
setReachConstraintsSelector = mkSelector "setReachConstraints:"

-- | @Selector@ for @constraints@
constraintsSelector :: Selector
constraintsSelector = mkSelector "constraints"

-- | @Selector@ for @setConstraints:@
setConstraintsSelector :: Selector
setConstraintsSelector = mkSelector "setConstraints:"

-- | @Selector@ for @accessibilityElement@
accessibilityElementSelector :: Selector
accessibilityElementSelector = mkSelector "accessibilityElement"

-- | @Selector@ for @setAccessibilityElement:@
setAccessibilityElementSelector :: Selector
setAccessibilityElementSelector = mkSelector "setAccessibilityElement:"

-- | @Selector@ for @accessibilityRole@
accessibilityRoleSelector :: Selector
accessibilityRoleSelector = mkSelector "accessibilityRole"

-- | @Selector@ for @setAccessibilityRole:@
setAccessibilityRoleSelector :: Selector
setAccessibilityRoleSelector = mkSelector "setAccessibilityRole:"

-- | @Selector@ for @accessibilityRoleDescription@
accessibilityRoleDescriptionSelector :: Selector
accessibilityRoleDescriptionSelector = mkSelector "accessibilityRoleDescription"

-- | @Selector@ for @setAccessibilityRoleDescription:@
setAccessibilityRoleDescriptionSelector :: Selector
setAccessibilityRoleDescriptionSelector = mkSelector "setAccessibilityRoleDescription:"

-- | @Selector@ for @accessibilitySubrole@
accessibilitySubroleSelector :: Selector
accessibilitySubroleSelector = mkSelector "accessibilitySubrole"

-- | @Selector@ for @setAccessibilitySubrole:@
setAccessibilitySubroleSelector :: Selector
setAccessibilitySubroleSelector = mkSelector "setAccessibilitySubrole:"

-- | @Selector@ for @accessibilityParent@
accessibilityParentSelector :: Selector
accessibilityParentSelector = mkSelector "accessibilityParent"

-- | @Selector@ for @setAccessibilityParent:@
setAccessibilityParentSelector :: Selector
setAccessibilityParentSelector = mkSelector "setAccessibilityParent:"

-- | @Selector@ for @accessibilityChildren@
accessibilityChildrenSelector :: Selector
accessibilityChildrenSelector = mkSelector "accessibilityChildren"

-- | @Selector@ for @setAccessibilityChildren:@
setAccessibilityChildrenSelector :: Selector
setAccessibilityChildrenSelector = mkSelector "setAccessibilityChildren:"

-- | @Selector@ for @accessibilityHelp@
accessibilityHelpSelector :: Selector
accessibilityHelpSelector = mkSelector "accessibilityHelp"

-- | @Selector@ for @setAccessibilityHelp:@
setAccessibilityHelpSelector :: Selector
setAccessibilityHelpSelector = mkSelector "setAccessibilityHelp:"

-- | @Selector@ for @accessibilityLabel@
accessibilityLabelSelector :: Selector
accessibilityLabelSelector = mkSelector "accessibilityLabel"

-- | @Selector@ for @setAccessibilityLabel:@
setAccessibilityLabelSelector :: Selector
setAccessibilityLabelSelector = mkSelector "setAccessibilityLabel:"

-- | @Selector@ for @accessibilityEnabled@
accessibilityEnabledSelector :: Selector
accessibilityEnabledSelector = mkSelector "accessibilityEnabled"

-- | @Selector@ for @setAccessibilityEnabled:@
setAccessibilityEnabledSelector :: Selector
setAccessibilityEnabledSelector = mkSelector "setAccessibilityEnabled:"

