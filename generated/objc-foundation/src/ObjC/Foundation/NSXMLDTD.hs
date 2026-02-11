{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NSXMLDTD
--
-- Defines the order, repetition, and allowable values for a document
--
-- Generated bindings for @NSXMLDTD@.
module ObjC.Foundation.NSXMLDTD
  ( NSXMLDTD
  , IsNSXMLDTD(..)
  , init_
  , initWithKind_options
  , initWithContentsOfURL_options_error
  , initWithData_options_error
  , insertChild_atIndex
  , insertChildren_atIndex
  , removeChildAtIndex
  , setChildren
  , addChild
  , replaceChildAtIndex_withNode
  , entityDeclarationForName
  , notationDeclarationForName
  , elementDeclarationForName
  , attributeDeclarationForName_elementName
  , predefinedEntityDeclarationForName
  , publicID
  , setPublicID
  , systemID
  , setSystemID
  , initSelector
  , initWithKind_optionsSelector
  , initWithContentsOfURL_options_errorSelector
  , initWithData_options_errorSelector
  , insertChild_atIndexSelector
  , insertChildren_atIndexSelector
  , removeChildAtIndexSelector
  , setChildrenSelector
  , addChildSelector
  , replaceChildAtIndex_withNodeSelector
  , entityDeclarationForNameSelector
  , notationDeclarationForNameSelector
  , elementDeclarationForNameSelector
  , attributeDeclarationForName_elementNameSelector
  , predefinedEntityDeclarationForNameSelector
  , publicIDSelector
  , setPublicIDSelector
  , systemIDSelector
  , setSystemIDSelector

  -- * Enum types
  , NSXMLNodeKind(NSXMLNodeKind)
  , pattern NSXMLInvalidKind
  , pattern NSXMLDocumentKind
  , pattern NSXMLElementKind
  , pattern NSXMLAttributeKind
  , pattern NSXMLNamespaceKind
  , pattern NSXMLProcessingInstructionKind
  , pattern NSXMLCommentKind
  , pattern NSXMLTextKind
  , pattern NSXMLDTDKind
  , pattern NSXMLEntityDeclarationKind
  , pattern NSXMLAttributeDeclarationKind
  , pattern NSXMLElementDeclarationKind
  , pattern NSXMLNotationDeclarationKind
  , NSXMLNodeOptions(NSXMLNodeOptions)
  , pattern NSXMLNodeOptionsNone
  , pattern NSXMLNodeIsCDATA
  , pattern NSXMLNodeExpandEmptyElement
  , pattern NSXMLNodeCompactEmptyElement
  , pattern NSXMLNodeUseSingleQuotes
  , pattern NSXMLNodeUseDoubleQuotes
  , pattern NSXMLNodeNeverEscapeContents
  , pattern NSXMLDocumentTidyHTML
  , pattern NSXMLDocumentTidyXML
  , pattern NSXMLDocumentValidate
  , pattern NSXMLNodeLoadExternalEntitiesAlways
  , pattern NSXMLNodeLoadExternalEntitiesSameOriginOnly
  , pattern NSXMLNodeLoadExternalEntitiesNever
  , pattern NSXMLDocumentXInclude
  , pattern NSXMLNodePrettyPrint
  , pattern NSXMLDocumentIncludeContentTypeDeclaration
  , pattern NSXMLNodePreserveNamespaceOrder
  , pattern NSXMLNodePreserveAttributeOrder
  , pattern NSXMLNodePreserveEntities
  , pattern NSXMLNodePreservePrefixes
  , pattern NSXMLNodePreserveCDATA
  , pattern NSXMLNodePreserveWhitespace
  , pattern NSXMLNodePreserveDTD
  , pattern NSXMLNodePreserveCharacterReferences
  , pattern NSXMLNodePromoteSignificantWhitespace
  , pattern NSXMLNodePreserveEmptyElements
  , pattern NSXMLNodePreserveQuotes
  , pattern NSXMLNodePreserveAll

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

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- init@
init_ :: IsNSXMLDTD nsxmldtd => nsxmldtd -> IO (Id NSXMLDTD)
init_ nsxmldtd  =
  sendMsg nsxmldtd (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithKind:options:@
initWithKind_options :: IsNSXMLDTD nsxmldtd => nsxmldtd -> NSXMLNodeKind -> NSXMLNodeOptions -> IO (Id NSXMLDTD)
initWithKind_options nsxmldtd  kind options =
  sendMsg nsxmldtd (mkSelector "initWithKind:options:") (retPtr retVoid) [argCULong (coerce kind), argCULong (coerce options)] >>= ownedObject . castPtr

-- | @- initWithContentsOfURL:options:error:@
initWithContentsOfURL_options_error :: (IsNSXMLDTD nsxmldtd, IsNSURL url, IsNSError error_) => nsxmldtd -> url -> NSXMLNodeOptions -> error_ -> IO (Id NSXMLDTD)
initWithContentsOfURL_options_error nsxmldtd  url mask error_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg nsxmldtd (mkSelector "initWithContentsOfURL:options:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argCULong (coerce mask), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithData:options:error:@
initWithData_options_error :: (IsNSXMLDTD nsxmldtd, IsNSData data_, IsNSError error_) => nsxmldtd -> data_ -> NSXMLNodeOptions -> error_ -> IO (Id NSXMLDTD)
initWithData_options_error nsxmldtd  data_ mask error_ =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg nsxmldtd (mkSelector "initWithData:options:error:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argCULong (coerce mask), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | insertChild:atIndex:
--
-- Inserts a child at a particular index.
--
-- ObjC selector: @- insertChild:atIndex:@
insertChild_atIndex :: (IsNSXMLDTD nsxmldtd, IsNSXMLNode child) => nsxmldtd -> child -> CULong -> IO ()
insertChild_atIndex nsxmldtd  child index =
withObjCPtr child $ \raw_child ->
    sendMsg nsxmldtd (mkSelector "insertChild:atIndex:") retVoid [argPtr (castPtr raw_child :: Ptr ()), argCULong (fromIntegral index)]

-- | insertChildren:atIndex:
--
-- Insert several children at a particular index.
--
-- ObjC selector: @- insertChildren:atIndex:@
insertChildren_atIndex :: (IsNSXMLDTD nsxmldtd, IsNSArray children) => nsxmldtd -> children -> CULong -> IO ()
insertChildren_atIndex nsxmldtd  children index =
withObjCPtr children $ \raw_children ->
    sendMsg nsxmldtd (mkSelector "insertChildren:atIndex:") retVoid [argPtr (castPtr raw_children :: Ptr ()), argCULong (fromIntegral index)]

-- | removeChildAtIndex:
--
-- Removes a child at a particular index.
--
-- ObjC selector: @- removeChildAtIndex:@
removeChildAtIndex :: IsNSXMLDTD nsxmldtd => nsxmldtd -> CULong -> IO ()
removeChildAtIndex nsxmldtd  index =
  sendMsg nsxmldtd (mkSelector "removeChildAtIndex:") retVoid [argCULong (fromIntegral index)]

-- | setChildren:
--
-- Removes all existing children and replaces them with the new children. Set children to nil to simply remove all children.
--
-- ObjC selector: @- setChildren:@
setChildren :: (IsNSXMLDTD nsxmldtd, IsNSArray children) => nsxmldtd -> children -> IO ()
setChildren nsxmldtd  children =
withObjCPtr children $ \raw_children ->
    sendMsg nsxmldtd (mkSelector "setChildren:") retVoid [argPtr (castPtr raw_children :: Ptr ())]

-- | addChild:
--
-- Adds a child to the end of the existing children.
--
-- ObjC selector: @- addChild:@
addChild :: (IsNSXMLDTD nsxmldtd, IsNSXMLNode child) => nsxmldtd -> child -> IO ()
addChild nsxmldtd  child =
withObjCPtr child $ \raw_child ->
    sendMsg nsxmldtd (mkSelector "addChild:") retVoid [argPtr (castPtr raw_child :: Ptr ())]

-- | replaceChildAtIndex:withNode:
--
-- Replaces a child at a particular index with another child.
--
-- ObjC selector: @- replaceChildAtIndex:withNode:@
replaceChildAtIndex_withNode :: (IsNSXMLDTD nsxmldtd, IsNSXMLNode node) => nsxmldtd -> CULong -> node -> IO ()
replaceChildAtIndex_withNode nsxmldtd  index node =
withObjCPtr node $ \raw_node ->
    sendMsg nsxmldtd (mkSelector "replaceChildAtIndex:withNode:") retVoid [argCULong (fromIntegral index), argPtr (castPtr raw_node :: Ptr ())]

-- | entityDeclarationForName:
--
-- Returns the entity declaration matching this name.
--
-- ObjC selector: @- entityDeclarationForName:@
entityDeclarationForName :: (IsNSXMLDTD nsxmldtd, IsNSString name) => nsxmldtd -> name -> IO (Id NSXMLDTDNode)
entityDeclarationForName nsxmldtd  name =
withObjCPtr name $ \raw_name ->
    sendMsg nsxmldtd (mkSelector "entityDeclarationForName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | notationDeclarationForName:
--
-- Returns the notation declaration matching this name.
--
-- ObjC selector: @- notationDeclarationForName:@
notationDeclarationForName :: (IsNSXMLDTD nsxmldtd, IsNSString name) => nsxmldtd -> name -> IO (Id NSXMLDTDNode)
notationDeclarationForName nsxmldtd  name =
withObjCPtr name $ \raw_name ->
    sendMsg nsxmldtd (mkSelector "notationDeclarationForName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | elementDeclarationForName:
--
-- Returns the element declaration matching this name.
--
-- ObjC selector: @- elementDeclarationForName:@
elementDeclarationForName :: (IsNSXMLDTD nsxmldtd, IsNSString name) => nsxmldtd -> name -> IO (Id NSXMLDTDNode)
elementDeclarationForName nsxmldtd  name =
withObjCPtr name $ \raw_name ->
    sendMsg nsxmldtd (mkSelector "elementDeclarationForName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | attributeDeclarationForName:
--
-- Returns the attribute declaration matching this name.
--
-- ObjC selector: @- attributeDeclarationForName:elementName:@
attributeDeclarationForName_elementName :: (IsNSXMLDTD nsxmldtd, IsNSString name, IsNSString elementName) => nsxmldtd -> name -> elementName -> IO (Id NSXMLDTDNode)
attributeDeclarationForName_elementName nsxmldtd  name elementName =
withObjCPtr name $ \raw_name ->
  withObjCPtr elementName $ \raw_elementName ->
      sendMsg nsxmldtd (mkSelector "attributeDeclarationForName:elementName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_elementName :: Ptr ())] >>= retainedObject . castPtr

-- | predefinedEntityDeclarationForName:
--
-- Returns the predefined entity declaration matching this name.
--
-- The five predefined entities are	&lt; - <&gt; - >&amp; - &&quot; - "&apos; - &
--
-- ObjC selector: @+ predefinedEntityDeclarationForName:@
predefinedEntityDeclarationForName :: IsNSString name => name -> IO (Id NSXMLDTDNode)
predefinedEntityDeclarationForName name =
  do
    cls' <- getRequiredClass "NSXMLDTD"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "predefinedEntityDeclarationForName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | Sets the public id. This identifier should be in the default catalog in /etc/xml/catalog or in a path specified by the environment variable XML_CATALOG_FILES. When the public id is set the system id must also be set.
--
-- ObjC selector: @- publicID@
publicID :: IsNSXMLDTD nsxmldtd => nsxmldtd -> IO (Id NSString)
publicID nsxmldtd  =
  sendMsg nsxmldtd (mkSelector "publicID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Sets the public id. This identifier should be in the default catalog in /etc/xml/catalog or in a path specified by the environment variable XML_CATALOG_FILES. When the public id is set the system id must also be set.
--
-- ObjC selector: @- setPublicID:@
setPublicID :: (IsNSXMLDTD nsxmldtd, IsNSString value) => nsxmldtd -> value -> IO ()
setPublicID nsxmldtd  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsxmldtd (mkSelector "setPublicID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Sets the system id. This should be a URL that points to a valid DTD.
--
-- ObjC selector: @- systemID@
systemID :: IsNSXMLDTD nsxmldtd => nsxmldtd -> IO (Id NSString)
systemID nsxmldtd  =
  sendMsg nsxmldtd (mkSelector "systemID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Sets the system id. This should be a URL that points to a valid DTD.
--
-- ObjC selector: @- setSystemID:@
setSystemID :: (IsNSXMLDTD nsxmldtd, IsNSString value) => nsxmldtd -> value -> IO ()
setSystemID nsxmldtd  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsxmldtd (mkSelector "setSystemID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithKind:options:@
initWithKind_optionsSelector :: Selector
initWithKind_optionsSelector = mkSelector "initWithKind:options:"

-- | @Selector@ for @initWithContentsOfURL:options:error:@
initWithContentsOfURL_options_errorSelector :: Selector
initWithContentsOfURL_options_errorSelector = mkSelector "initWithContentsOfURL:options:error:"

-- | @Selector@ for @initWithData:options:error:@
initWithData_options_errorSelector :: Selector
initWithData_options_errorSelector = mkSelector "initWithData:options:error:"

-- | @Selector@ for @insertChild:atIndex:@
insertChild_atIndexSelector :: Selector
insertChild_atIndexSelector = mkSelector "insertChild:atIndex:"

-- | @Selector@ for @insertChildren:atIndex:@
insertChildren_atIndexSelector :: Selector
insertChildren_atIndexSelector = mkSelector "insertChildren:atIndex:"

-- | @Selector@ for @removeChildAtIndex:@
removeChildAtIndexSelector :: Selector
removeChildAtIndexSelector = mkSelector "removeChildAtIndex:"

-- | @Selector@ for @setChildren:@
setChildrenSelector :: Selector
setChildrenSelector = mkSelector "setChildren:"

-- | @Selector@ for @addChild:@
addChildSelector :: Selector
addChildSelector = mkSelector "addChild:"

-- | @Selector@ for @replaceChildAtIndex:withNode:@
replaceChildAtIndex_withNodeSelector :: Selector
replaceChildAtIndex_withNodeSelector = mkSelector "replaceChildAtIndex:withNode:"

-- | @Selector@ for @entityDeclarationForName:@
entityDeclarationForNameSelector :: Selector
entityDeclarationForNameSelector = mkSelector "entityDeclarationForName:"

-- | @Selector@ for @notationDeclarationForName:@
notationDeclarationForNameSelector :: Selector
notationDeclarationForNameSelector = mkSelector "notationDeclarationForName:"

-- | @Selector@ for @elementDeclarationForName:@
elementDeclarationForNameSelector :: Selector
elementDeclarationForNameSelector = mkSelector "elementDeclarationForName:"

-- | @Selector@ for @attributeDeclarationForName:elementName:@
attributeDeclarationForName_elementNameSelector :: Selector
attributeDeclarationForName_elementNameSelector = mkSelector "attributeDeclarationForName:elementName:"

-- | @Selector@ for @predefinedEntityDeclarationForName:@
predefinedEntityDeclarationForNameSelector :: Selector
predefinedEntityDeclarationForNameSelector = mkSelector "predefinedEntityDeclarationForName:"

-- | @Selector@ for @publicID@
publicIDSelector :: Selector
publicIDSelector = mkSelector "publicID"

-- | @Selector@ for @setPublicID:@
setPublicIDSelector :: Selector
setPublicIDSelector = mkSelector "setPublicID:"

-- | @Selector@ for @systemID@
systemIDSelector :: Selector
systemIDSelector = mkSelector "systemID"

-- | @Selector@ for @setSystemID:@
setSystemIDSelector :: Selector
setSystemIDSelector = mkSelector "setSystemID:"

