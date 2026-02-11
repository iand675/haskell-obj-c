{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NSXMLElement
--
-- An XML element
--
-- Note: Trying to add a document, namespace, attribute, or node with a parent throws an exception. To add a node with a parent first detach or create a copy of it.
--
-- Generated bindings for @NSXMLElement@.
module ObjC.Foundation.NSXMLElement
  ( NSXMLElement
  , IsNSXMLElement(..)
  , initWithName
  , initWithName_URI
  , initWithName_stringValue
  , initWithXMLString_error
  , initWithKind_options
  , elementsForName
  , elementsForLocalName_URI
  , addAttribute
  , removeAttributeForName
  , setAttributesWithDictionary
  , attributeForName
  , attributeForLocalName_URI
  , addNamespace
  , removeNamespaceForPrefix
  , namespaceForPrefix
  , resolveNamespaceForName
  , resolvePrefixForNamespaceURI
  , insertChild_atIndex
  , insertChildren_atIndex
  , removeChildAtIndex
  , setChildren
  , addChild
  , replaceChildAtIndex_withNode
  , normalizeAdjacentTextNodesPreservingCDATA
  , setAttributesAsDictionary
  , attributes
  , setAttributes
  , namespaces
  , setNamespaces
  , initWithNameSelector
  , initWithName_URISelector
  , initWithName_stringValueSelector
  , initWithXMLString_errorSelector
  , initWithKind_optionsSelector
  , elementsForNameSelector
  , elementsForLocalName_URISelector
  , addAttributeSelector
  , removeAttributeForNameSelector
  , setAttributesWithDictionarySelector
  , attributeForNameSelector
  , attributeForLocalName_URISelector
  , addNamespaceSelector
  , removeNamespaceForPrefixSelector
  , namespaceForPrefixSelector
  , resolveNamespaceForNameSelector
  , resolvePrefixForNamespaceURISelector
  , insertChild_atIndexSelector
  , insertChildren_atIndexSelector
  , removeChildAtIndexSelector
  , setChildrenSelector
  , addChildSelector
  , replaceChildAtIndex_withNodeSelector
  , normalizeAdjacentTextNodesPreservingCDATASelector
  , setAttributesAsDictionarySelector
  , attributesSelector
  , setAttributesSelector
  , namespacesSelector
  , setNamespacesSelector

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

-- | initWithName:
--
-- Returns an element <name></name>.
--
-- ObjC selector: @- initWithName:@
initWithName :: (IsNSXMLElement nsxmlElement, IsNSString name) => nsxmlElement -> name -> IO (Id NSXMLElement)
initWithName nsxmlElement  name =
withObjCPtr name $ \raw_name ->
    sendMsg nsxmlElement (mkSelector "initWithName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= ownedObject . castPtr

-- | initWithName:URI:
--
-- Returns an element whose full QName is specified.
--
-- ObjC selector: @- initWithName:URI:@
initWithName_URI :: (IsNSXMLElement nsxmlElement, IsNSString name, IsNSString uri) => nsxmlElement -> name -> uri -> IO (Id NSXMLElement)
initWithName_URI nsxmlElement  name uri =
withObjCPtr name $ \raw_name ->
  withObjCPtr uri $ \raw_uri ->
      sendMsg nsxmlElement (mkSelector "initWithName:URI:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_uri :: Ptr ())] >>= ownedObject . castPtr

-- | initWithName:stringValue:
--
-- Returns an element with a single text node child <name>string</name>.
--
-- ObjC selector: @- initWithName:stringValue:@
initWithName_stringValue :: (IsNSXMLElement nsxmlElement, IsNSString name, IsNSString string) => nsxmlElement -> name -> string -> IO (Id NSXMLElement)
initWithName_stringValue nsxmlElement  name string =
withObjCPtr name $ \raw_name ->
  withObjCPtr string $ \raw_string ->
      sendMsg nsxmlElement (mkSelector "initWithName:stringValue:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_string :: Ptr ())] >>= ownedObject . castPtr

-- | initWithXMLString:error:
--
-- Returns an element created from a string. Parse errors are collected in error.
--
-- ObjC selector: @- initWithXMLString:error:@
initWithXMLString_error :: (IsNSXMLElement nsxmlElement, IsNSString string, IsNSError error_) => nsxmlElement -> string -> error_ -> IO (Id NSXMLElement)
initWithXMLString_error nsxmlElement  string error_ =
withObjCPtr string $ \raw_string ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg nsxmlElement (mkSelector "initWithXMLString:error:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithKind:options:@
initWithKind_options :: IsNSXMLElement nsxmlElement => nsxmlElement -> NSXMLNodeKind -> NSXMLNodeOptions -> IO (Id NSXMLElement)
initWithKind_options nsxmlElement  kind options =
  sendMsg nsxmlElement (mkSelector "initWithKind:options:") (retPtr retVoid) [argCULong (coerce kind), argCULong (coerce options)] >>= ownedObject . castPtr

-- | elementsForName:
--
-- Returns all of the child elements that match this name.
--
-- ObjC selector: @- elementsForName:@
elementsForName :: (IsNSXMLElement nsxmlElement, IsNSString name) => nsxmlElement -> name -> IO (Id NSArray)
elementsForName nsxmlElement  name =
withObjCPtr name $ \raw_name ->
    sendMsg nsxmlElement (mkSelector "elementsForName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | elementsForLocalName:URI
--
-- Returns all of the child elements that match this localname URI pair.
--
-- ObjC selector: @- elementsForLocalName:URI:@
elementsForLocalName_URI :: (IsNSXMLElement nsxmlElement, IsNSString localName, IsNSString uri) => nsxmlElement -> localName -> uri -> IO (Id NSArray)
elementsForLocalName_URI nsxmlElement  localName uri =
withObjCPtr localName $ \raw_localName ->
  withObjCPtr uri $ \raw_uri ->
      sendMsg nsxmlElement (mkSelector "elementsForLocalName:URI:") (retPtr retVoid) [argPtr (castPtr raw_localName :: Ptr ()), argPtr (castPtr raw_uri :: Ptr ())] >>= retainedObject . castPtr

-- | addAttribute:
--
-- Adds an attribute. Attributes with duplicate names are not added.
--
-- ObjC selector: @- addAttribute:@
addAttribute :: (IsNSXMLElement nsxmlElement, IsNSXMLNode attribute) => nsxmlElement -> attribute -> IO ()
addAttribute nsxmlElement  attribute =
withObjCPtr attribute $ \raw_attribute ->
    sendMsg nsxmlElement (mkSelector "addAttribute:") retVoid [argPtr (castPtr raw_attribute :: Ptr ())]

-- | removeAttributeForName:
--
-- Removes an attribute based on its name.
--
-- ObjC selector: @- removeAttributeForName:@
removeAttributeForName :: (IsNSXMLElement nsxmlElement, IsNSString name) => nsxmlElement -> name -> IO ()
removeAttributeForName nsxmlElement  name =
withObjCPtr name $ \raw_name ->
    sendMsg nsxmlElement (mkSelector "removeAttributeForName:") retVoid [argPtr (castPtr raw_name :: Ptr ())]

-- | setAttributesWithDictionary:
--
-- Set the attributes based on a name-value dictionary.
--
-- ObjC selector: @- setAttributesWithDictionary:@
setAttributesWithDictionary :: (IsNSXMLElement nsxmlElement, IsNSDictionary attributes) => nsxmlElement -> attributes -> IO ()
setAttributesWithDictionary nsxmlElement  attributes =
withObjCPtr attributes $ \raw_attributes ->
    sendMsg nsxmlElement (mkSelector "setAttributesWithDictionary:") retVoid [argPtr (castPtr raw_attributes :: Ptr ())]

-- | attributeForName:
--
-- Returns an attribute matching this name.
--
-- ObjC selector: @- attributeForName:@
attributeForName :: (IsNSXMLElement nsxmlElement, IsNSString name) => nsxmlElement -> name -> IO (Id NSXMLNode)
attributeForName nsxmlElement  name =
withObjCPtr name $ \raw_name ->
    sendMsg nsxmlElement (mkSelector "attributeForName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | attributeForLocalName:URI:
--
-- Returns an attribute matching this localname URI pair.
--
-- ObjC selector: @- attributeForLocalName:URI:@
attributeForLocalName_URI :: (IsNSXMLElement nsxmlElement, IsNSString localName, IsNSString uri) => nsxmlElement -> localName -> uri -> IO (Id NSXMLNode)
attributeForLocalName_URI nsxmlElement  localName uri =
withObjCPtr localName $ \raw_localName ->
  withObjCPtr uri $ \raw_uri ->
      sendMsg nsxmlElement (mkSelector "attributeForLocalName:URI:") (retPtr retVoid) [argPtr (castPtr raw_localName :: Ptr ()), argPtr (castPtr raw_uri :: Ptr ())] >>= retainedObject . castPtr

-- | addNamespace:URI:
--
-- Adds a namespace. Namespaces with duplicate names are not added.
--
-- ObjC selector: @- addNamespace:@
addNamespace :: (IsNSXMLElement nsxmlElement, IsNSXMLNode aNamespace) => nsxmlElement -> aNamespace -> IO ()
addNamespace nsxmlElement  aNamespace =
withObjCPtr aNamespace $ \raw_aNamespace ->
    sendMsg nsxmlElement (mkSelector "addNamespace:") retVoid [argPtr (castPtr raw_aNamespace :: Ptr ())]

-- | addNamespace:URI:
--
-- Removes a namespace with a particular name.
--
-- ObjC selector: @- removeNamespaceForPrefix:@
removeNamespaceForPrefix :: (IsNSXMLElement nsxmlElement, IsNSString name) => nsxmlElement -> name -> IO ()
removeNamespaceForPrefix nsxmlElement  name =
withObjCPtr name $ \raw_name ->
    sendMsg nsxmlElement (mkSelector "removeNamespaceForPrefix:") retVoid [argPtr (castPtr raw_name :: Ptr ())]

-- | namespaceForPrefix:
--
-- Returns the namespace matching this prefix.
--
-- ObjC selector: @- namespaceForPrefix:@
namespaceForPrefix :: (IsNSXMLElement nsxmlElement, IsNSString name) => nsxmlElement -> name -> IO (Id NSXMLNode)
namespaceForPrefix nsxmlElement  name =
withObjCPtr name $ \raw_name ->
    sendMsg nsxmlElement (mkSelector "namespaceForPrefix:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | resolveNamespaceForName:
--
-- Returns the namespace who matches the prefix of the name given. Looks in the entire namespace chain.
--
-- ObjC selector: @- resolveNamespaceForName:@
resolveNamespaceForName :: (IsNSXMLElement nsxmlElement, IsNSString name) => nsxmlElement -> name -> IO (Id NSXMLNode)
resolveNamespaceForName nsxmlElement  name =
withObjCPtr name $ \raw_name ->
    sendMsg nsxmlElement (mkSelector "resolveNamespaceForName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | resolvePrefixForNamespaceURI:
--
-- Returns the URI of this prefix. Looks in the entire namespace chain.
--
-- ObjC selector: @- resolvePrefixForNamespaceURI:@
resolvePrefixForNamespaceURI :: (IsNSXMLElement nsxmlElement, IsNSString namespaceURI) => nsxmlElement -> namespaceURI -> IO (Id NSString)
resolvePrefixForNamespaceURI nsxmlElement  namespaceURI =
withObjCPtr namespaceURI $ \raw_namespaceURI ->
    sendMsg nsxmlElement (mkSelector "resolvePrefixForNamespaceURI:") (retPtr retVoid) [argPtr (castPtr raw_namespaceURI :: Ptr ())] >>= retainedObject . castPtr

-- | insertChild:atIndex:
--
-- Inserts a child at a particular index.
--
-- ObjC selector: @- insertChild:atIndex:@
insertChild_atIndex :: (IsNSXMLElement nsxmlElement, IsNSXMLNode child) => nsxmlElement -> child -> CULong -> IO ()
insertChild_atIndex nsxmlElement  child index =
withObjCPtr child $ \raw_child ->
    sendMsg nsxmlElement (mkSelector "insertChild:atIndex:") retVoid [argPtr (castPtr raw_child :: Ptr ()), argCULong (fromIntegral index)]

-- | insertChildren:atIndex:
--
-- Insert several children at a particular index.
--
-- ObjC selector: @- insertChildren:atIndex:@
insertChildren_atIndex :: (IsNSXMLElement nsxmlElement, IsNSArray children) => nsxmlElement -> children -> CULong -> IO ()
insertChildren_atIndex nsxmlElement  children index =
withObjCPtr children $ \raw_children ->
    sendMsg nsxmlElement (mkSelector "insertChildren:atIndex:") retVoid [argPtr (castPtr raw_children :: Ptr ()), argCULong (fromIntegral index)]

-- | removeChildAtIndex:atIndex:
--
-- Removes a child at a particular index.
--
-- ObjC selector: @- removeChildAtIndex:@
removeChildAtIndex :: IsNSXMLElement nsxmlElement => nsxmlElement -> CULong -> IO ()
removeChildAtIndex nsxmlElement  index =
  sendMsg nsxmlElement (mkSelector "removeChildAtIndex:") retVoid [argCULong (fromIntegral index)]

-- | setChildren:
--
-- Removes all existing children and replaces them with the new children. Set children to nil to simply remove all children.
--
-- ObjC selector: @- setChildren:@
setChildren :: (IsNSXMLElement nsxmlElement, IsNSArray children) => nsxmlElement -> children -> IO ()
setChildren nsxmlElement  children =
withObjCPtr children $ \raw_children ->
    sendMsg nsxmlElement (mkSelector "setChildren:") retVoid [argPtr (castPtr raw_children :: Ptr ())]

-- | addChild:
--
-- Adds a child to the end of the existing children.
--
-- ObjC selector: @- addChild:@
addChild :: (IsNSXMLElement nsxmlElement, IsNSXMLNode child) => nsxmlElement -> child -> IO ()
addChild nsxmlElement  child =
withObjCPtr child $ \raw_child ->
    sendMsg nsxmlElement (mkSelector "addChild:") retVoid [argPtr (castPtr raw_child :: Ptr ())]

-- | replaceChildAtIndex:withNode:
--
-- Replaces a child at a particular index with another child.
--
-- ObjC selector: @- replaceChildAtIndex:withNode:@
replaceChildAtIndex_withNode :: (IsNSXMLElement nsxmlElement, IsNSXMLNode node) => nsxmlElement -> CULong -> node -> IO ()
replaceChildAtIndex_withNode nsxmlElement  index node =
withObjCPtr node $ \raw_node ->
    sendMsg nsxmlElement (mkSelector "replaceChildAtIndex:withNode:") retVoid [argCULong (fromIntegral index), argPtr (castPtr raw_node :: Ptr ())]

-- | normalizeAdjacentTextNodesPreservingCDATA:
--
-- Adjacent text nodes are coalesced. If the node's value is the empty string, it is removed. This should be called with a value of NO before using XQuery or XPath.
--
-- ObjC selector: @- normalizeAdjacentTextNodesPreservingCDATA:@
normalizeAdjacentTextNodesPreservingCDATA :: IsNSXMLElement nsxmlElement => nsxmlElement -> Bool -> IO ()
normalizeAdjacentTextNodesPreservingCDATA nsxmlElement  preserve =
  sendMsg nsxmlElement (mkSelector "normalizeAdjacentTextNodesPreservingCDATA:") retVoid [argCULong (if preserve then 1 else 0)]

-- | setAttributesAsDictionary:
--
-- Set the attributes base on a name-value dictionary.
--
-- This method is deprecated and does not function correctly. Use -setAttributesWithDictionary: instead.
--
-- ObjC selector: @- setAttributesAsDictionary:@
setAttributesAsDictionary :: (IsNSXMLElement nsxmlElement, IsNSDictionary attributes) => nsxmlElement -> attributes -> IO ()
setAttributesAsDictionary nsxmlElement  attributes =
withObjCPtr attributes $ \raw_attributes ->
    sendMsg nsxmlElement (mkSelector "setAttributesAsDictionary:") retVoid [argPtr (castPtr raw_attributes :: Ptr ())]

-- | Set the attributes. In the case of duplicate names, the first attribute with the name is used.
--
-- ObjC selector: @- attributes@
attributes :: IsNSXMLElement nsxmlElement => nsxmlElement -> IO (Id NSArray)
attributes nsxmlElement  =
  sendMsg nsxmlElement (mkSelector "attributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Set the attributes. In the case of duplicate names, the first attribute with the name is used.
--
-- ObjC selector: @- setAttributes:@
setAttributes :: (IsNSXMLElement nsxmlElement, IsNSArray value) => nsxmlElement -> value -> IO ()
setAttributes nsxmlElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsxmlElement (mkSelector "setAttributes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Set the namespaces. In the case of duplicate names, the first namespace with the name is used.
--
-- ObjC selector: @- namespaces@
namespaces :: IsNSXMLElement nsxmlElement => nsxmlElement -> IO (Id NSArray)
namespaces nsxmlElement  =
  sendMsg nsxmlElement (mkSelector "namespaces") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Set the namespaces. In the case of duplicate names, the first namespace with the name is used.
--
-- ObjC selector: @- setNamespaces:@
setNamespaces :: (IsNSXMLElement nsxmlElement, IsNSArray value) => nsxmlElement -> value -> IO ()
setNamespaces nsxmlElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsxmlElement (mkSelector "setNamespaces:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:@
initWithNameSelector :: Selector
initWithNameSelector = mkSelector "initWithName:"

-- | @Selector@ for @initWithName:URI:@
initWithName_URISelector :: Selector
initWithName_URISelector = mkSelector "initWithName:URI:"

-- | @Selector@ for @initWithName:stringValue:@
initWithName_stringValueSelector :: Selector
initWithName_stringValueSelector = mkSelector "initWithName:stringValue:"

-- | @Selector@ for @initWithXMLString:error:@
initWithXMLString_errorSelector :: Selector
initWithXMLString_errorSelector = mkSelector "initWithXMLString:error:"

-- | @Selector@ for @initWithKind:options:@
initWithKind_optionsSelector :: Selector
initWithKind_optionsSelector = mkSelector "initWithKind:options:"

-- | @Selector@ for @elementsForName:@
elementsForNameSelector :: Selector
elementsForNameSelector = mkSelector "elementsForName:"

-- | @Selector@ for @elementsForLocalName:URI:@
elementsForLocalName_URISelector :: Selector
elementsForLocalName_URISelector = mkSelector "elementsForLocalName:URI:"

-- | @Selector@ for @addAttribute:@
addAttributeSelector :: Selector
addAttributeSelector = mkSelector "addAttribute:"

-- | @Selector@ for @removeAttributeForName:@
removeAttributeForNameSelector :: Selector
removeAttributeForNameSelector = mkSelector "removeAttributeForName:"

-- | @Selector@ for @setAttributesWithDictionary:@
setAttributesWithDictionarySelector :: Selector
setAttributesWithDictionarySelector = mkSelector "setAttributesWithDictionary:"

-- | @Selector@ for @attributeForName:@
attributeForNameSelector :: Selector
attributeForNameSelector = mkSelector "attributeForName:"

-- | @Selector@ for @attributeForLocalName:URI:@
attributeForLocalName_URISelector :: Selector
attributeForLocalName_URISelector = mkSelector "attributeForLocalName:URI:"

-- | @Selector@ for @addNamespace:@
addNamespaceSelector :: Selector
addNamespaceSelector = mkSelector "addNamespace:"

-- | @Selector@ for @removeNamespaceForPrefix:@
removeNamespaceForPrefixSelector :: Selector
removeNamespaceForPrefixSelector = mkSelector "removeNamespaceForPrefix:"

-- | @Selector@ for @namespaceForPrefix:@
namespaceForPrefixSelector :: Selector
namespaceForPrefixSelector = mkSelector "namespaceForPrefix:"

-- | @Selector@ for @resolveNamespaceForName:@
resolveNamespaceForNameSelector :: Selector
resolveNamespaceForNameSelector = mkSelector "resolveNamespaceForName:"

-- | @Selector@ for @resolvePrefixForNamespaceURI:@
resolvePrefixForNamespaceURISelector :: Selector
resolvePrefixForNamespaceURISelector = mkSelector "resolvePrefixForNamespaceURI:"

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

-- | @Selector@ for @normalizeAdjacentTextNodesPreservingCDATA:@
normalizeAdjacentTextNodesPreservingCDATASelector :: Selector
normalizeAdjacentTextNodesPreservingCDATASelector = mkSelector "normalizeAdjacentTextNodesPreservingCDATA:"

-- | @Selector@ for @setAttributesAsDictionary:@
setAttributesAsDictionarySelector :: Selector
setAttributesAsDictionarySelector = mkSelector "setAttributesAsDictionary:"

-- | @Selector@ for @attributes@
attributesSelector :: Selector
attributesSelector = mkSelector "attributes"

-- | @Selector@ for @setAttributes:@
setAttributesSelector :: Selector
setAttributesSelector = mkSelector "setAttributes:"

-- | @Selector@ for @namespaces@
namespacesSelector :: Selector
namespacesSelector = mkSelector "namespaces"

-- | @Selector@ for @setNamespaces:@
setNamespacesSelector :: Selector
setNamespacesSelector = mkSelector "setNamespaces:"

