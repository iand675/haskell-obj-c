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
  , setComponent_forProtocolSelector
  , componentConformingToProtocolSelector
  , objectForKeyedSubscriptSelector
  , setObject_forKeyedSubscriptSelector
  , objectAtPathSelector
  , enumerateChildObjectsOfClass_root_usingBlock_stopPointerSelector
  , addChildSelector
  , componentsSelector
  , parentSelector
  , setParentSelector
  , instanceSelector
  , setInstanceSelector
  , pathSelector
  , transformSelector
  , setTransformSelector
  , childrenSelector
  , setChildrenSelector
  , hiddenSelector
  , setHiddenSelector


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

import ObjC.ModelIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | setComponent:forProtocol:
--
-- Extensible component support that allows user of ModelIO to customize            MDLObjects to fit their format and workflow.
--
-- ObjC selector: @- setComponent:forProtocol:@
setComponent_forProtocol :: IsMDLObject mdlObject => mdlObject -> RawId -> RawId -> IO ()
setComponent_forProtocol mdlObject  component protocol =
    sendMsg mdlObject (mkSelector "setComponent:forProtocol:") retVoid [argPtr (castPtr (unRawId component) :: Ptr ()), argPtr (castPtr (unRawId protocol) :: Ptr ())]

-- | componentConformingToProtocol:
--
-- Extensible component support that allows user of ModelIO to customize            MDLObjects to fit their format and workflow.
--
-- ObjC selector: @- componentConformingToProtocol:@
componentConformingToProtocol :: IsMDLObject mdlObject => mdlObject -> RawId -> IO RawId
componentConformingToProtocol mdlObject  protocol =
    fmap (RawId . castPtr) $ sendMsg mdlObject (mkSelector "componentConformingToProtocol:") (retPtr retVoid) [argPtr (castPtr (unRawId protocol) :: Ptr ())]

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
objectForKeyedSubscript mdlObject  key =
    fmap (RawId . castPtr) $ sendMsg mdlObject (mkSelector "objectForKeyedSubscript:") (retPtr retVoid) [argPtr (castPtr (unRawId key) :: Ptr ())]

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
setObject_forKeyedSubscript mdlObject  obj_ key =
    sendMsg mdlObject (mkSelector "setObject:forKeyedSubscript:") retVoid [argPtr (castPtr (unRawId obj_) :: Ptr ()), argPtr (castPtr (unRawId key) :: Ptr ())]

-- | Return the object at the specified path, or nil if none exists there
--
-- ObjC selector: @- objectAtPath:@
objectAtPath :: (IsMDLObject mdlObject, IsNSString path) => mdlObject -> path -> IO (Id MDLObject)
objectAtPath mdlObject  path =
  withObjCPtr path $ \raw_path ->
      sendMsg mdlObject (mkSelector "objectAtPath:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= retainedObject . castPtr

-- | @- enumerateChildObjectsOfClass:root:usingBlock:stopPointer:@
enumerateChildObjectsOfClass_root_usingBlock_stopPointer :: (IsMDLObject mdlObject, IsMDLObject root) => mdlObject -> Class -> root -> Ptr () -> Ptr Bool -> IO ()
enumerateChildObjectsOfClass_root_usingBlock_stopPointer mdlObject  objectClass root block stopPointer =
  withObjCPtr root $ \raw_root ->
      sendMsg mdlObject (mkSelector "enumerateChildObjectsOfClass:root:usingBlock:stopPointer:") retVoid [argPtr (unClass objectClass), argPtr (castPtr raw_root :: Ptr ()), argPtr (castPtr block :: Ptr ()), argPtr stopPointer]

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
addChild mdlObject  child =
  withObjCPtr child $ \raw_child ->
      sendMsg mdlObject (mkSelector "addChild:") retVoid [argPtr (castPtr raw_child :: Ptr ())]

-- | components
--
-- Allows applications to introspect the components on the objects.
--
-- ObjC selector: @- components@
components :: IsMDLObject mdlObject => mdlObject -> IO (Id NSArray)
components mdlObject  =
    sendMsg mdlObject (mkSelector "components") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | parent
--
-- Parent object. Nil if no parent.
--
-- Set to nil when you remove this from an object container inside the              parent object.
--
-- ObjC selector: @- parent@
parent :: IsMDLObject mdlObject => mdlObject -> IO (Id MDLObject)
parent mdlObject  =
    sendMsg mdlObject (mkSelector "parent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | parent
--
-- Parent object. Nil if no parent.
--
-- Set to nil when you remove this from an object container inside the              parent object.
--
-- ObjC selector: @- setParent:@
setParent :: (IsMDLObject mdlObject, IsMDLObject value) => mdlObject -> value -> IO ()
setParent mdlObject  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mdlObject (mkSelector "setParent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | instance
--
-- Instance object
--
-- nil, unless this object refers to original data to be instanced. The             original data object can be any MDLObject that does not have a parent.             If an MDLAsset has been created from a data file, any original objects             parsed from that file will be found in the originals property.             A typical use of a original and instance might be to have one original             chair MDLObject, and instance six chairs around a table. The             transform of each chair would be found on the parent MDLObject, but             the various items making up the chair would be found in the original             object.
--
-- ObjC selector: @- instance@
instance_ :: IsMDLObject mdlObject => mdlObject -> IO (Id MDLObject)
instance_ mdlObject  =
    sendMsg mdlObject (mkSelector "instance") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | instance
--
-- Instance object
--
-- nil, unless this object refers to original data to be instanced. The             original data object can be any MDLObject that does not have a parent.             If an MDLAsset has been created from a data file, any original objects             parsed from that file will be found in the originals property.             A typical use of a original and instance might be to have one original             chair MDLObject, and instance six chairs around a table. The             transform of each chair would be found on the parent MDLObject, but             the various items making up the chair would be found in the original             object.
--
-- ObjC selector: @- setInstance:@
setInstance :: (IsMDLObject mdlObject, IsMDLObject value) => mdlObject -> value -> IO ()
setInstance mdlObject  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mdlObject (mkSelector "setInstance:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | path
--
-- a string representing a path to the object
--
-- a path is of the form /path/to/object where the path is formed by             concatenating the names of the objects up the parent chain.             Requesting a path will force any unnamed objects to became uniquely             named. Any characters outside of [A-Z][a-z][0-9][:-_.] will be             forced to underscore.
--
-- ObjC selector: @- path@
path :: IsMDLObject mdlObject => mdlObject -> IO (Id NSString)
path mdlObject  =
    sendMsg mdlObject (mkSelector "path") (retPtr retVoid) [] >>= retainedObject . castPtr

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
transform mdlObject  =
    fmap (RawId . castPtr) $ sendMsg mdlObject (mkSelector "transform") (retPtr retVoid) []

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
setTransform mdlObject  value =
    sendMsg mdlObject (mkSelector "setTransform:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

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
children mdlObject  =
    fmap (RawId . castPtr) $ sendMsg mdlObject (mkSelector "children") (retPtr retVoid) []

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
setChildren mdlObject  value =
    sendMsg mdlObject (mkSelector "setChildren:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | hidden
--
-- Visibility of the node
--
-- default is NO
--
-- ObjC selector: @- hidden@
hidden :: IsMDLObject mdlObject => mdlObject -> IO Bool
hidden mdlObject  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mdlObject (mkSelector "hidden") retCULong []

-- | hidden
--
-- Visibility of the node
--
-- default is NO
--
-- ObjC selector: @- setHidden:@
setHidden :: IsMDLObject mdlObject => mdlObject -> Bool -> IO ()
setHidden mdlObject  value =
    sendMsg mdlObject (mkSelector "setHidden:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setComponent:forProtocol:@
setComponent_forProtocolSelector :: Selector
setComponent_forProtocolSelector = mkSelector "setComponent:forProtocol:"

-- | @Selector@ for @componentConformingToProtocol:@
componentConformingToProtocolSelector :: Selector
componentConformingToProtocolSelector = mkSelector "componentConformingToProtocol:"

-- | @Selector@ for @objectForKeyedSubscript:@
objectForKeyedSubscriptSelector :: Selector
objectForKeyedSubscriptSelector = mkSelector "objectForKeyedSubscript:"

-- | @Selector@ for @setObject:forKeyedSubscript:@
setObject_forKeyedSubscriptSelector :: Selector
setObject_forKeyedSubscriptSelector = mkSelector "setObject:forKeyedSubscript:"

-- | @Selector@ for @objectAtPath:@
objectAtPathSelector :: Selector
objectAtPathSelector = mkSelector "objectAtPath:"

-- | @Selector@ for @enumerateChildObjectsOfClass:root:usingBlock:stopPointer:@
enumerateChildObjectsOfClass_root_usingBlock_stopPointerSelector :: Selector
enumerateChildObjectsOfClass_root_usingBlock_stopPointerSelector = mkSelector "enumerateChildObjectsOfClass:root:usingBlock:stopPointer:"

-- | @Selector@ for @addChild:@
addChildSelector :: Selector
addChildSelector = mkSelector "addChild:"

-- | @Selector@ for @components@
componentsSelector :: Selector
componentsSelector = mkSelector "components"

-- | @Selector@ for @parent@
parentSelector :: Selector
parentSelector = mkSelector "parent"

-- | @Selector@ for @setParent:@
setParentSelector :: Selector
setParentSelector = mkSelector "setParent:"

-- | @Selector@ for @instance@
instanceSelector :: Selector
instanceSelector = mkSelector "instance"

-- | @Selector@ for @setInstance:@
setInstanceSelector :: Selector
setInstanceSelector = mkSelector "setInstance:"

-- | @Selector@ for @path@
pathSelector :: Selector
pathSelector = mkSelector "path"

-- | @Selector@ for @transform@
transformSelector :: Selector
transformSelector = mkSelector "transform"

-- | @Selector@ for @setTransform:@
setTransformSelector :: Selector
setTransformSelector = mkSelector "setTransform:"

-- | @Selector@ for @children@
childrenSelector :: Selector
childrenSelector = mkSelector "children"

-- | @Selector@ for @setChildren:@
setChildrenSelector :: Selector
setChildrenSelector = mkSelector "setChildren:"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector
hiddenSelector = mkSelector "hidden"

-- | @Selector@ for @setHidden:@
setHiddenSelector :: Selector
setHiddenSelector = mkSelector "setHidden:"

