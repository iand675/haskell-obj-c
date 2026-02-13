{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MDLObject
--
-- Base class for object in a ModelIO asset hierarchy
--
-- Includes transformation and bounds info, links to parent and             children in the hierachy
--
-- Generated bindings for @MDLObject@.
module ObjC.ModelIO.MDLObject
  ( MDLObject
  , IsMDLObject(..)
  , setComponent_forProtocol
  , componentConformingToProtocol
  , objectForKeyedSubscript
  , setObject_forKeyedSubscript
  , objectAtPath
  , enumerateChildObjectsOfClass_root_usingBlock_stopPointer
  , addChild
  , components
  , parent
  , setParent
  , instance_
  , setInstance
  , path
  , transform
  , setTransform
  , children
  , setChildren
  , hidden
  , setHidden
  , addChildSelector
  , childrenSelector
  , componentConformingToProtocolSelector
  , componentsSelector
  , enumerateChildObjectsOfClass_root_usingBlock_stopPointerSelector
  , hiddenSelector
  , instanceSelector
  , objectAtPathSelector
  , objectForKeyedSubscriptSelector
  , parentSelector
  , pathSelector
  , setChildrenSelector
  , setComponent_forProtocolSelector
  , setHiddenSelector
  , setInstanceSelector
  , setObject_forKeyedSubscriptSelector
  , setParentSelector
  , setTransformSelector
  , transformSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ModelIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | setComponent:forProtocol:
--
-- Extensible component support that allows user of ModelIO to customize            MDLObjects to fit their format and workflow.
--
-- ObjC selector: @- setComponent:forProtocol:@
setComponent_forProtocol :: IsMDLObject mdlObject => mdlObject -> RawId -> RawId -> IO ()
setComponent_forProtocol mdlObject component protocol =
  sendMessage mdlObject setComponent_forProtocolSelector component protocol

-- | componentConformingToProtocol:
--
-- Extensible component support that allows user of ModelIO to customize            MDLObjects to fit their format and workflow.
--
-- ObjC selector: @- componentConformingToProtocol:@
componentConformingToProtocol :: IsMDLObject mdlObject => mdlObject -> RawId -> IO RawId
componentConformingToProtocol mdlObject protocol =
  sendMessage mdlObject componentConformingToProtocolSelector protocol

-- | objectForKeyedSubscript:
--
-- Allows shorthand [key] syntax for componentConformingToProtocol:.
--
-- @key@ — The protocol that the component conforms to.
--
-- See: componentConformingToProtocol:
--
-- ObjC selector: @- objectForKeyedSubscript:@
objectForKeyedSubscript :: IsMDLObject mdlObject => mdlObject -> RawId -> IO RawId
objectForKeyedSubscript mdlObject key =
  sendMessage mdlObject objectForKeyedSubscriptSelector key

-- | setObject:forKeyedSubscript:
--
-- Allows shorthand [key] syntax for setComponent:forProtocol:.
--
-- @key@ — The protocol that the component conforms to.
--
-- See: setComponent:forProtocol:
--
-- ObjC selector: @- setObject:forKeyedSubscript:@
setObject_forKeyedSubscript :: IsMDLObject mdlObject => mdlObject -> RawId -> RawId -> IO ()
setObject_forKeyedSubscript mdlObject obj_ key =
  sendMessage mdlObject setObject_forKeyedSubscriptSelector obj_ key

-- | Return the object at the specified path, or nil if none exists there
--
-- ObjC selector: @- objectAtPath:@
objectAtPath :: (IsMDLObject mdlObject, IsNSString path) => mdlObject -> path -> IO (Id MDLObject)
objectAtPath mdlObject path =
  sendMessage mdlObject objectAtPathSelector (toNSString path)

-- | @- enumerateChildObjectsOfClass:root:usingBlock:stopPointer:@
enumerateChildObjectsOfClass_root_usingBlock_stopPointer :: (IsMDLObject mdlObject, IsMDLObject root) => mdlObject -> Class -> root -> Ptr () -> Ptr Bool -> IO ()
enumerateChildObjectsOfClass_root_usingBlock_stopPointer mdlObject objectClass root block stopPointer =
  sendMessage mdlObject enumerateChildObjectsOfClass_root_usingBlock_stopPointerSelector objectClass (toMDLObject root) block stopPointer

-- | addChild:
--
-- Short hand for adding a child to the current container component and            setting the parent to this object.
--
-- It will create a default container if none exists. If children are               explicitly disallowed for an object, then add a container component               that throws on addition.
--
-- See: MDLObjectContainer
--
-- ObjC selector: @- addChild:@
addChild :: (IsMDLObject mdlObject, IsMDLObject child) => mdlObject -> child -> IO ()
addChild mdlObject child =
  sendMessage mdlObject addChildSelector (toMDLObject child)

-- | components
--
-- Allows applications to introspect the components on the objects.
--
-- ObjC selector: @- components@
components :: IsMDLObject mdlObject => mdlObject -> IO (Id NSArray)
components mdlObject =
  sendMessage mdlObject componentsSelector

-- | parent
--
-- Parent object. Nil if no parent.
--
-- Set to nil when you remove this from an object container inside the              parent object.
--
-- ObjC selector: @- parent@
parent :: IsMDLObject mdlObject => mdlObject -> IO (Id MDLObject)
parent mdlObject =
  sendMessage mdlObject parentSelector

-- | parent
--
-- Parent object. Nil if no parent.
--
-- Set to nil when you remove this from an object container inside the              parent object.
--
-- ObjC selector: @- setParent:@
setParent :: (IsMDLObject mdlObject, IsMDLObject value) => mdlObject -> value -> IO ()
setParent mdlObject value =
  sendMessage mdlObject setParentSelector (toMDLObject value)

-- | instance
--
-- Instance object
--
-- nil, unless this object refers to original data to be instanced. The             original data object can be any MDLObject that does not have a parent.             If an MDLAsset has been created from a data file, any original objects             parsed from that file will be found in the originals property.             A typical use of a original and instance might be to have one original             chair MDLObject, and instance six chairs around a table. The             transform of each chair would be found on the parent MDLObject, but             the various items making up the chair would be found in the original             object.
--
-- ObjC selector: @- instance@
instance_ :: IsMDLObject mdlObject => mdlObject -> IO (Id MDLObject)
instance_ mdlObject =
  sendMessage mdlObject instanceSelector

-- | instance
--
-- Instance object
--
-- nil, unless this object refers to original data to be instanced. The             original data object can be any MDLObject that does not have a parent.             If an MDLAsset has been created from a data file, any original objects             parsed from that file will be found in the originals property.             A typical use of a original and instance might be to have one original             chair MDLObject, and instance six chairs around a table. The             transform of each chair would be found on the parent MDLObject, but             the various items making up the chair would be found in the original             object.
--
-- ObjC selector: @- setInstance:@
setInstance :: (IsMDLObject mdlObject, IsMDLObject value) => mdlObject -> value -> IO ()
setInstance mdlObject value =
  sendMessage mdlObject setInstanceSelector (toMDLObject value)

-- | path
--
-- a string representing a path to the object
--
-- a path is of the form /path/to/object where the path is formed by             concatenating the names of the objects up the parent chain.             Requesting a path will force any unnamed objects to became uniquely             named. Any characters outside of [A-Z][a-z][0-9][:-_.] will be             forced to underscore.
--
-- ObjC selector: @- path@
path :: IsMDLObject mdlObject => mdlObject -> IO (Id NSString)
path mdlObject =
  sendMessage mdlObject pathSelector

-- | transform
--
-- Short hand property for the MDLTransformComponent.
--
-- The default value is nil
--
-- See: MDLTransformComponent
--
-- ObjC selector: @- transform@
transform :: IsMDLObject mdlObject => mdlObject -> IO RawId
transform mdlObject =
  sendMessage mdlObject transformSelector

-- | transform
--
-- Short hand property for the MDLTransformComponent.
--
-- The default value is nil
--
-- See: MDLTransformComponent
--
-- ObjC selector: @- setTransform:@
setTransform :: IsMDLObject mdlObject => mdlObject -> RawId -> IO ()
setTransform mdlObject value =
  sendMessage mdlObject setTransformSelector value

-- | children
--
-- Short hand property for the MDLObjectContainerComponent.
--
-- The default value is an empty MDLObjectContainer
--
-- See: MDLObjectContainerComponent
--
-- ObjC selector: @- children@
children :: IsMDLObject mdlObject => mdlObject -> IO RawId
children mdlObject =
  sendMessage mdlObject childrenSelector

-- | children
--
-- Short hand property for the MDLObjectContainerComponent.
--
-- The default value is an empty MDLObjectContainer
--
-- See: MDLObjectContainerComponent
--
-- ObjC selector: @- setChildren:@
setChildren :: IsMDLObject mdlObject => mdlObject -> RawId -> IO ()
setChildren mdlObject value =
  sendMessage mdlObject setChildrenSelector value

-- | hidden
--
-- Visibility of the node
--
-- default is NO
--
-- ObjC selector: @- hidden@
hidden :: IsMDLObject mdlObject => mdlObject -> IO Bool
hidden mdlObject =
  sendMessage mdlObject hiddenSelector

-- | hidden
--
-- Visibility of the node
--
-- default is NO
--
-- ObjC selector: @- setHidden:@
setHidden :: IsMDLObject mdlObject => mdlObject -> Bool -> IO ()
setHidden mdlObject value =
  sendMessage mdlObject setHiddenSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setComponent:forProtocol:@
setComponent_forProtocolSelector :: Selector '[RawId, RawId] ()
setComponent_forProtocolSelector = mkSelector "setComponent:forProtocol:"

-- | @Selector@ for @componentConformingToProtocol:@
componentConformingToProtocolSelector :: Selector '[RawId] RawId
componentConformingToProtocolSelector = mkSelector "componentConformingToProtocol:"

-- | @Selector@ for @objectForKeyedSubscript:@
objectForKeyedSubscriptSelector :: Selector '[RawId] RawId
objectForKeyedSubscriptSelector = mkSelector "objectForKeyedSubscript:"

-- | @Selector@ for @setObject:forKeyedSubscript:@
setObject_forKeyedSubscriptSelector :: Selector '[RawId, RawId] ()
setObject_forKeyedSubscriptSelector = mkSelector "setObject:forKeyedSubscript:"

-- | @Selector@ for @objectAtPath:@
objectAtPathSelector :: Selector '[Id NSString] (Id MDLObject)
objectAtPathSelector = mkSelector "objectAtPath:"

-- | @Selector@ for @enumerateChildObjectsOfClass:root:usingBlock:stopPointer:@
enumerateChildObjectsOfClass_root_usingBlock_stopPointerSelector :: Selector '[Class, Id MDLObject, Ptr (), Ptr Bool] ()
enumerateChildObjectsOfClass_root_usingBlock_stopPointerSelector = mkSelector "enumerateChildObjectsOfClass:root:usingBlock:stopPointer:"

-- | @Selector@ for @addChild:@
addChildSelector :: Selector '[Id MDLObject] ()
addChildSelector = mkSelector "addChild:"

-- | @Selector@ for @components@
componentsSelector :: Selector '[] (Id NSArray)
componentsSelector = mkSelector "components"

-- | @Selector@ for @parent@
parentSelector :: Selector '[] (Id MDLObject)
parentSelector = mkSelector "parent"

-- | @Selector@ for @setParent:@
setParentSelector :: Selector '[Id MDLObject] ()
setParentSelector = mkSelector "setParent:"

-- | @Selector@ for @instance@
instanceSelector :: Selector '[] (Id MDLObject)
instanceSelector = mkSelector "instance"

-- | @Selector@ for @setInstance:@
setInstanceSelector :: Selector '[Id MDLObject] ()
setInstanceSelector = mkSelector "setInstance:"

-- | @Selector@ for @path@
pathSelector :: Selector '[] (Id NSString)
pathSelector = mkSelector "path"

-- | @Selector@ for @transform@
transformSelector :: Selector '[] RawId
transformSelector = mkSelector "transform"

-- | @Selector@ for @setTransform:@
setTransformSelector :: Selector '[RawId] ()
setTransformSelector = mkSelector "setTransform:"

-- | @Selector@ for @children@
childrenSelector :: Selector '[] RawId
childrenSelector = mkSelector "children"

-- | @Selector@ for @setChildren:@
setChildrenSelector :: Selector '[RawId] ()
setChildrenSelector = mkSelector "setChildren:"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector '[] Bool
hiddenSelector = mkSelector "hidden"

-- | @Selector@ for @setHidden:@
setHiddenSelector :: Selector '[Bool] ()
setHiddenSelector = mkSelector "setHidden:"

