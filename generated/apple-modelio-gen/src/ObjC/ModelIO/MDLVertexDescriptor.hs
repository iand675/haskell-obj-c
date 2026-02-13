{-# LANGUAGE DataKinds #-}
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
  , addOrReplaceAttributeSelector
  , attributeNamedSelector
  , attributesSelector
  , initWithVertexDescriptorSelector
  , layoutsSelector
  , removeAttributeNamedSelector
  , resetSelector
  , setAttributesSelector
  , setLayoutsSelector
  , setPackedOffsetsSelector
  , setPackedStridesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithVertexDescriptor mdlVertexDescriptor vertexDescriptor =
  sendOwnedMessage mdlVertexDescriptor initWithVertexDescriptorSelector (toMDLVertexDescriptor vertexDescriptor)

-- | attributeNamed:
--
-- Retrieves the attribute with the given name
--
-- Returns: The attribute with the supplied name or nil if attribute with the given          name does not exist in the descriptor object
--
-- ObjC selector: @- attributeNamed:@
attributeNamed :: (IsMDLVertexDescriptor mdlVertexDescriptor, IsNSString name) => mdlVertexDescriptor -> name -> IO (Id MDLVertexAttribute)
attributeNamed mdlVertexDescriptor name =
  sendMessage mdlVertexDescriptor attributeNamedSelector (toNSString name)

-- | addOrReplaceAttribute:
--
-- Replace any attribute with the same name and time, or add it if it does not           already exist.
--
-- ObjC selector: @- addOrReplaceAttribute:@
addOrReplaceAttribute :: (IsMDLVertexDescriptor mdlVertexDescriptor, IsMDLVertexAttribute attribute) => mdlVertexDescriptor -> attribute -> IO ()
addOrReplaceAttribute mdlVertexDescriptor attribute =
  sendMessage mdlVertexDescriptor addOrReplaceAttributeSelector (toMDLVertexAttribute attribute)

-- | removeAttributeNamed:
--
-- Remove the named attribute if it exists
--
-- ObjC selector: @- removeAttributeNamed:@
removeAttributeNamed :: (IsMDLVertexDescriptor mdlVertexDescriptor, IsNSString name) => mdlVertexDescriptor -> name -> IO ()
removeAttributeNamed mdlVertexDescriptor name =
  sendMessage mdlVertexDescriptor removeAttributeNamedSelector (toNSString name)

-- | reset
--
-- Tesets the descriptor to initial values
--
-- ObjC selector: @- reset@
reset :: IsMDLVertexDescriptor mdlVertexDescriptor => mdlVertexDescriptor -> IO ()
reset mdlVertexDescriptor =
  sendMessage mdlVertexDescriptor resetSelector

-- | setPackedStrides
--
-- Sets the stride in each VertexBufferLout in the layouts array to the            minimum value encompassing all attributes in the vertex buffer
--
-- ObjC selector: @- setPackedStrides@
setPackedStrides :: IsMDLVertexDescriptor mdlVertexDescriptor => mdlVertexDescriptor -> IO ()
setPackedStrides mdlVertexDescriptor =
  sendMessage mdlVertexDescriptor setPackedStridesSelector

-- | setPackedOffsets
--
-- Sets the stride in each VertexAttribute in the attributes array to             the minimum value to pack each attribute next to each other in its             vertexbuffer
--
-- ObjC selector: @- setPackedOffsets@
setPackedOffsets :: IsMDLVertexDescriptor mdlVertexDescriptor => mdlVertexDescriptor -> IO ()
setPackedOffsets mdlVertexDescriptor =
  sendMessage mdlVertexDescriptor setPackedOffsetsSelector

-- | attributes
--
-- An array of MDLVertexAttribute objects
--
-- ay describing the current attribute state of vertex buffers in an             MDLMesh mesh
--
-- ObjC selector: @- attributes@
attributes :: IsMDLVertexDescriptor mdlVertexDescriptor => mdlVertexDescriptor -> IO (Id NSMutableArray)
attributes mdlVertexDescriptor =
  sendMessage mdlVertexDescriptor attributesSelector

-- | attributes
--
-- An array of MDLVertexAttribute objects
--
-- ay describing the current attribute state of vertex buffers in an             MDLMesh mesh
--
-- ObjC selector: @- setAttributes:@
setAttributes :: (IsMDLVertexDescriptor mdlVertexDescriptor, IsNSMutableArray value) => mdlVertexDescriptor -> value -> IO ()
setAttributes mdlVertexDescriptor value =
  sendMessage mdlVertexDescriptor setAttributesSelector (toNSMutableArray value)

-- | layouts
--
-- An array of MDLVertexBufferLayout
--
-- An array describing the current layout state of vertex buffers in an              MDLMesh mesh
--
-- ObjC selector: @- layouts@
layouts :: IsMDLVertexDescriptor mdlVertexDescriptor => mdlVertexDescriptor -> IO (Id NSMutableArray)
layouts mdlVertexDescriptor =
  sendMessage mdlVertexDescriptor layoutsSelector

-- | layouts
--
-- An array of MDLVertexBufferLayout
--
-- An array describing the current layout state of vertex buffers in an              MDLMesh mesh
--
-- ObjC selector: @- setLayouts:@
setLayouts :: (IsMDLVertexDescriptor mdlVertexDescriptor, IsNSMutableArray value) => mdlVertexDescriptor -> value -> IO ()
setLayouts mdlVertexDescriptor value =
  sendMessage mdlVertexDescriptor setLayoutsSelector (toNSMutableArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithVertexDescriptor:@
initWithVertexDescriptorSelector :: Selector '[Id MDLVertexDescriptor] (Id MDLVertexDescriptor)
initWithVertexDescriptorSelector = mkSelector "initWithVertexDescriptor:"

-- | @Selector@ for @attributeNamed:@
attributeNamedSelector :: Selector '[Id NSString] (Id MDLVertexAttribute)
attributeNamedSelector = mkSelector "attributeNamed:"

-- | @Selector@ for @addOrReplaceAttribute:@
addOrReplaceAttributeSelector :: Selector '[Id MDLVertexAttribute] ()
addOrReplaceAttributeSelector = mkSelector "addOrReplaceAttribute:"

-- | @Selector@ for @removeAttributeNamed:@
removeAttributeNamedSelector :: Selector '[Id NSString] ()
removeAttributeNamedSelector = mkSelector "removeAttributeNamed:"

-- | @Selector@ for @reset@
resetSelector :: Selector '[] ()
resetSelector = mkSelector "reset"

-- | @Selector@ for @setPackedStrides@
setPackedStridesSelector :: Selector '[] ()
setPackedStridesSelector = mkSelector "setPackedStrides"

-- | @Selector@ for @setPackedOffsets@
setPackedOffsetsSelector :: Selector '[] ()
setPackedOffsetsSelector = mkSelector "setPackedOffsets"

-- | @Selector@ for @attributes@
attributesSelector :: Selector '[] (Id NSMutableArray)
attributesSelector = mkSelector "attributes"

-- | @Selector@ for @setAttributes:@
setAttributesSelector :: Selector '[Id NSMutableArray] ()
setAttributesSelector = mkSelector "setAttributes:"

-- | @Selector@ for @layouts@
layoutsSelector :: Selector '[] (Id NSMutableArray)
layoutsSelector = mkSelector "layouts"

-- | @Selector@ for @setLayouts:@
setLayoutsSelector :: Selector '[Id NSMutableArray] ()
setLayoutsSelector = mkSelector "setLayouts:"

