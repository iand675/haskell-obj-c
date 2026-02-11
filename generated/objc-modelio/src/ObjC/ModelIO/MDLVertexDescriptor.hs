{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MDLVertexDescriptor
--
-- Describes the layout of vertex buffers in MDLMesh objects
--
-- This object is a property of MDLMesh describing the current state of attributes and buffer layouts of the vertex buffers in the mesh. This must be  immutable otherwise even small changes could cause the buffers to be out of sync  with the layout described here.
--
-- Designed to be very similar to MTLVertexDescriptor to ease creation of one from  the other
--
-- Generated bindings for @MDLVertexDescriptor@.
module ObjC.ModelIO.MDLVertexDescriptor
  ( MDLVertexDescriptor
  , IsMDLVertexDescriptor(..)
  , initWithVertexDescriptor
  , attributeNamed
  , addOrReplaceAttribute
  , removeAttributeNamed
  , reset
  , setPackedStrides
  , setPackedOffsets
  , attributes
  , setAttributes
  , layouts
  , setLayouts
  , initWithVertexDescriptorSelector
  , attributeNamedSelector
  , addOrReplaceAttributeSelector
  , removeAttributeNamedSelector
  , resetSelector
  , setPackedStridesSelector
  , setPackedOffsetsSelector
  , attributesSelector
  , setAttributesSelector
  , layoutsSelector
  , setLayoutsSelector


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

-- | initVertexDescriptor:
--
-- Initializes the object with values from supplied vertexDescriptor
--
-- This performs a deep copy of all data in the supplied descriptor.
--
-- ObjC selector: @- initWithVertexDescriptor:@
initWithVertexDescriptor :: (IsMDLVertexDescriptor mdlVertexDescriptor, IsMDLVertexDescriptor vertexDescriptor) => mdlVertexDescriptor -> vertexDescriptor -> IO (Id MDLVertexDescriptor)
initWithVertexDescriptor mdlVertexDescriptor  vertexDescriptor =
withObjCPtr vertexDescriptor $ \raw_vertexDescriptor ->
    sendMsg mdlVertexDescriptor (mkSelector "initWithVertexDescriptor:") (retPtr retVoid) [argPtr (castPtr raw_vertexDescriptor :: Ptr ())] >>= ownedObject . castPtr

-- | attributeNamed:
--
-- Retrieves the attribute with the given name
--
-- Returns: The attribute with the supplied name or nil if attribute with the given          name does not exist in the descriptor object
--
-- ObjC selector: @- attributeNamed:@
attributeNamed :: (IsMDLVertexDescriptor mdlVertexDescriptor, IsNSString name) => mdlVertexDescriptor -> name -> IO (Id MDLVertexAttribute)
attributeNamed mdlVertexDescriptor  name =
withObjCPtr name $ \raw_name ->
    sendMsg mdlVertexDescriptor (mkSelector "attributeNamed:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | addOrReplaceAttribute:
--
-- Replace any attribute with the same name and time, or add it if it does not           already exist.
--
-- ObjC selector: @- addOrReplaceAttribute:@
addOrReplaceAttribute :: (IsMDLVertexDescriptor mdlVertexDescriptor, IsMDLVertexAttribute attribute) => mdlVertexDescriptor -> attribute -> IO ()
addOrReplaceAttribute mdlVertexDescriptor  attribute =
withObjCPtr attribute $ \raw_attribute ->
    sendMsg mdlVertexDescriptor (mkSelector "addOrReplaceAttribute:") retVoid [argPtr (castPtr raw_attribute :: Ptr ())]

-- | removeAttributeNamed:
--
-- Remove the named attribute if it exists
--
-- ObjC selector: @- removeAttributeNamed:@
removeAttributeNamed :: (IsMDLVertexDescriptor mdlVertexDescriptor, IsNSString name) => mdlVertexDescriptor -> name -> IO ()
removeAttributeNamed mdlVertexDescriptor  name =
withObjCPtr name $ \raw_name ->
    sendMsg mdlVertexDescriptor (mkSelector "removeAttributeNamed:") retVoid [argPtr (castPtr raw_name :: Ptr ())]

-- | reset
--
-- Tesets the descriptor to initial values
--
-- ObjC selector: @- reset@
reset :: IsMDLVertexDescriptor mdlVertexDescriptor => mdlVertexDescriptor -> IO ()
reset mdlVertexDescriptor  =
  sendMsg mdlVertexDescriptor (mkSelector "reset") retVoid []

-- | setPackedStrides
--
-- Sets the stride in each VertexBufferLout in the layouts array to the            minimum value encompassing all attributes in the vertex buffer
--
-- ObjC selector: @- setPackedStrides@
setPackedStrides :: IsMDLVertexDescriptor mdlVertexDescriptor => mdlVertexDescriptor -> IO ()
setPackedStrides mdlVertexDescriptor  =
  sendMsg mdlVertexDescriptor (mkSelector "setPackedStrides") retVoid []

-- | setPackedOffsets
--
-- Sets the stride in each VertexAttribute in the attributes array to             the minimum value to pack each attribute next to each other in its             vertexbuffer
--
-- ObjC selector: @- setPackedOffsets@
setPackedOffsets :: IsMDLVertexDescriptor mdlVertexDescriptor => mdlVertexDescriptor -> IO ()
setPackedOffsets mdlVertexDescriptor  =
  sendMsg mdlVertexDescriptor (mkSelector "setPackedOffsets") retVoid []

-- | attributes
--
-- An array of MDLVertexAttribute objects
--
-- ay describing the current attribute state of vertex buffers in an             MDLMesh mesh
--
-- ObjC selector: @- attributes@
attributes :: IsMDLVertexDescriptor mdlVertexDescriptor => mdlVertexDescriptor -> IO (Id NSMutableArray)
attributes mdlVertexDescriptor  =
  sendMsg mdlVertexDescriptor (mkSelector "attributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | attributes
--
-- An array of MDLVertexAttribute objects
--
-- ay describing the current attribute state of vertex buffers in an             MDLMesh mesh
--
-- ObjC selector: @- setAttributes:@
setAttributes :: (IsMDLVertexDescriptor mdlVertexDescriptor, IsNSMutableArray value) => mdlVertexDescriptor -> value -> IO ()
setAttributes mdlVertexDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mdlVertexDescriptor (mkSelector "setAttributes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | layouts
--
-- An array of MDLVertexBufferLayout
--
-- An array describing the current layout state of vertex buffers in an              MDLMesh mesh
--
-- ObjC selector: @- layouts@
layouts :: IsMDLVertexDescriptor mdlVertexDescriptor => mdlVertexDescriptor -> IO (Id NSMutableArray)
layouts mdlVertexDescriptor  =
  sendMsg mdlVertexDescriptor (mkSelector "layouts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | layouts
--
-- An array of MDLVertexBufferLayout
--
-- An array describing the current layout state of vertex buffers in an              MDLMesh mesh
--
-- ObjC selector: @- setLayouts:@
setLayouts :: (IsMDLVertexDescriptor mdlVertexDescriptor, IsNSMutableArray value) => mdlVertexDescriptor -> value -> IO ()
setLayouts mdlVertexDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mdlVertexDescriptor (mkSelector "setLayouts:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithVertexDescriptor:@
initWithVertexDescriptorSelector :: Selector
initWithVertexDescriptorSelector = mkSelector "initWithVertexDescriptor:"

-- | @Selector@ for @attributeNamed:@
attributeNamedSelector :: Selector
attributeNamedSelector = mkSelector "attributeNamed:"

-- | @Selector@ for @addOrReplaceAttribute:@
addOrReplaceAttributeSelector :: Selector
addOrReplaceAttributeSelector = mkSelector "addOrReplaceAttribute:"

-- | @Selector@ for @removeAttributeNamed:@
removeAttributeNamedSelector :: Selector
removeAttributeNamedSelector = mkSelector "removeAttributeNamed:"

-- | @Selector@ for @reset@
resetSelector :: Selector
resetSelector = mkSelector "reset"

-- | @Selector@ for @setPackedStrides@
setPackedStridesSelector :: Selector
setPackedStridesSelector = mkSelector "setPackedStrides"

-- | @Selector@ for @setPackedOffsets@
setPackedOffsetsSelector :: Selector
setPackedOffsetsSelector = mkSelector "setPackedOffsets"

-- | @Selector@ for @attributes@
attributesSelector :: Selector
attributesSelector = mkSelector "attributes"

-- | @Selector@ for @setAttributes:@
setAttributesSelector :: Selector
setAttributesSelector = mkSelector "setAttributes:"

-- | @Selector@ for @layouts@
layoutsSelector :: Selector
layoutsSelector = mkSelector "layouts"

-- | @Selector@ for @setLayouts:@
setLayoutsSelector :: Selector
setLayoutsSelector = mkSelector "setLayouts:"

