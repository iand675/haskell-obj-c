{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , addAttributeSelector
  , addChildSelector
  , addNamespaceSelector
  , attributeForLocalName_URISelector
  , attributeForNameSelector
  , attributesSelector
  , elementsForLocalName_URISelector
  , elementsForNameSelector
  , initWithKind_optionsSelector
  , initWithNameSelector
  , initWithName_URISelector
  , initWithName_stringValueSelector
  , initWithXMLString_errorSelector
  , insertChild_atIndexSelector
  , insertChildren_atIndexSelector
  , namespaceForPrefixSelector
  , namespacesSelector
  , normalizeAdjacentTextNodesPreservingCDATASelector
  , removeAttributeForNameSelector
  , removeChildAtIndexSelector
  , removeNamespaceForPrefixSelector
  , replaceChildAtIndex_withNodeSelector
  , resolveNamespaceForNameSelector
  , resolvePrefixForNamespaceURISelector
  , setAttributesAsDictionarySelector
  , setAttributesSelector
  , setAttributesWithDictionarySelector
  , setChildrenSelector
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithName nsxmlElement name =
  sendOwnedMessage nsxmlElement initWithNameSelector (toNSString name)

-- | initWithName:URI:
--
-- Returns an element whose full QName is specified.
--
-- ObjC selector: @- initWithName:URI:@
initWithName_URI :: (IsNSXMLElement nsxmlElement, IsNSString name, IsNSString uri) => nsxmlElement -> name -> uri -> IO (Id NSXMLElement)
initWithName_URI nsxmlElement name uri =
  sendOwnedMessage nsxmlElement initWithName_URISelector (toNSString name) (toNSString uri)

-- | initWithName:stringValue:
--
-- Returns an element with a single text node child <name>string</name>.
--
-- ObjC selector: @- initWithName:stringValue:@
initWithName_stringValue :: (IsNSXMLElement nsxmlElement, IsNSString name, IsNSString string) => nsxmlElement -> name -> string -> IO (Id NSXMLElement)
initWithName_stringValue nsxmlElement name string =
  sendOwnedMessage nsxmlElement initWithName_stringValueSelector (toNSString name) (toNSString string)

-- | initWithXMLString:error:
--
-- Returns an element created from a string. Parse errors are collected in error.
--
-- ObjC selector: @- initWithXMLString:error:@
initWithXMLString_error :: (IsNSXMLElement nsxmlElement, IsNSString string, IsNSError error_) => nsxmlElement -> string -> error_ -> IO (Id NSXMLElement)
initWithXMLString_error nsxmlElement string error_ =
  sendOwnedMessage nsxmlElement initWithXMLString_errorSelector (toNSString string) (toNSError error_)

-- | @- initWithKind:options:@
initWithKind_options :: IsNSXMLElement nsxmlElement => nsxmlElement -> NSXMLNodeKind -> NSXMLNodeOptions -> IO (Id NSXMLElement)
initWithKind_options nsxmlElement kind options =
  sendOwnedMessage nsxmlElement initWithKind_optionsSelector kind options

-- | elementsForName:
--
-- Returns all of the child elements that match this name.
--
-- ObjC selector: @- elementsForName:@
elementsForName :: (IsNSXMLElement nsxmlElement, IsNSString name) => nsxmlElement -> name -> IO (Id NSArray)
elementsForName nsxmlElement name =
  sendMessage nsxmlElement elementsForNameSelector (toNSString name)

-- | elementsForLocalName:URI
--
-- Returns all of the child elements that match this localname URI pair.
--
-- ObjC selector: @- elementsForLocalName:URI:@
elementsForLocalName_URI :: (IsNSXMLElement nsxmlElement, IsNSString localName, IsNSString uri) => nsxmlElement -> localName -> uri -> IO (Id NSArray)
elementsForLocalName_URI nsxmlElement localName uri =
  sendMessage nsxmlElement elementsForLocalName_URISelector (toNSString localName) (toNSString uri)

-- | addAttribute:
--
-- Adds an attribute. Attributes with duplicate names are not added.
--
-- ObjC selector: @- addAttribute:@
addAttribute :: (IsNSXMLElement nsxmlElement, IsNSXMLNode attribute) => nsxmlElement -> attribute -> IO ()
addAttribute nsxmlElement attribute =
  sendMessage nsxmlElement addAttributeSelector (toNSXMLNode attribute)

-- | removeAttributeForName:
--
-- Removes an attribute based on its name.
--
-- ObjC selector: @- removeAttributeForName:@
removeAttributeForName :: (IsNSXMLElement nsxmlElement, IsNSString name) => nsxmlElement -> name -> IO ()
removeAttributeForName nsxmlElement name =
  sendMessage nsxmlElement removeAttributeForNameSelector (toNSString name)

-- | setAttributesWithDictionary:
--
-- Set the attributes based on a name-value dictionary.
--
-- ObjC selector: @- setAttributesWithDictionary:@
setAttributesWithDictionary :: (IsNSXMLElement nsxmlElement, IsNSDictionary attributes) => nsxmlElement -> attributes -> IO ()
setAttributesWithDictionary nsxmlElement attributes =
  sendMessage nsxmlElement setAttributesWithDictionarySelector (toNSDictionary attributes)

-- | attributeForName:
--
-- Returns an attribute matching this name.
--
-- ObjC selector: @- attributeForName:@
attributeForName :: (IsNSXMLElement nsxmlElement, IsNSString name) => nsxmlElement -> name -> IO (Id NSXMLNode)
attributeForName nsxmlElement name =
  sendMessage nsxmlElement attributeForNameSelector (toNSString name)

-- | attributeForLocalName:URI:
--
-- Returns an attribute matching this localname URI pair.
--
-- ObjC selector: @- attributeForLocalName:URI:@
attributeForLocalName_URI :: (IsNSXMLElement nsxmlElement, IsNSString localName, IsNSString uri) => nsxmlElement -> localName -> uri -> IO (Id NSXMLNode)
attributeForLocalName_URI nsxmlElement localName uri =
  sendMessage nsxmlElement attributeForLocalName_URISelector (toNSString localName) (toNSString uri)

-- | addNamespace:URI:
--
-- Adds a namespace. Namespaces with duplicate names are not added.
--
-- ObjC selector: @- addNamespace:@
addNamespace :: (IsNSXMLElement nsxmlElement, IsNSXMLNode aNamespace) => nsxmlElement -> aNamespace -> IO ()
addNamespace nsxmlElement aNamespace =
  sendMessage nsxmlElement addNamespaceSelector (toNSXMLNode aNamespace)

-- | addNamespace:URI:
--
-- Removes a namespace with a particular name.
--
-- ObjC selector: @- removeNamespaceForPrefix:@
removeNamespaceForPrefix :: (IsNSXMLElement nsxmlElement, IsNSString name) => nsxmlElement -> name -> IO ()
removeNamespaceForPrefix nsxmlElement name =
  sendMessage nsxmlElement removeNamespaceForPrefixSelector (toNSString name)

-- | namespaceForPrefix:
--
-- Returns the namespace matching this prefix.
--
-- ObjC selector: @- namespaceForPrefix:@
namespaceForPrefix :: (IsNSXMLElement nsxmlElement, IsNSString name) => nsxmlElement -> name -> IO (Id NSXMLNode)
namespaceForPrefix nsxmlElement name =
  sendMessage nsxmlElement namespaceForPrefixSelector (toNSString name)

-- | resolveNamespaceForName:
--
-- Returns the namespace who matches the prefix of the name given. Looks in the entire namespace chain.
--
-- ObjC selector: @- resolveNamespaceForName:@
resolveNamespaceForName :: (IsNSXMLElement nsxmlElement, IsNSString name) => nsxmlElement -> name -> IO (Id NSXMLNode)
resolveNamespaceForName nsxmlElement name =
  sendMessage nsxmlElement resolveNamespaceForNameSelector (toNSString name)

-- | resolvePrefixForNamespaceURI:
--
-- Returns the URI of this prefix. Looks in the entire namespace chain.
--
-- ObjC selector: @- resolvePrefixForNamespaceURI:@
resolvePrefixForNamespaceURI :: (IsNSXMLElement nsxmlElement, IsNSString namespaceURI) => nsxmlElement -> namespaceURI -> IO (Id NSString)
resolvePrefixForNamespaceURI nsxmlElement namespaceURI =
  sendMessage nsxmlElement resolvePrefixForNamespaceURISelector (toNSString namespaceURI)

-- | insertChild:atIndex:
--
-- Inserts a child at a particular index.
--
-- ObjC selector: @- insertChild:atIndex:@
insertChild_atIndex :: (IsNSXMLElement nsxmlElement, IsNSXMLNode child) => nsxmlElement -> child -> CULong -> IO ()
insertChild_atIndex nsxmlElement child index =
  sendMessage nsxmlElement insertChild_atIndexSelector (toNSXMLNode child) index

-- | insertChildren:atIndex:
--
-- Insert several children at a particular index.
--
-- ObjC selector: @- insertChildren:atIndex:@
insertChildren_atIndex :: (IsNSXMLElement nsxmlElement, IsNSArray children) => nsxmlElement -> children -> CULong -> IO ()
insertChildren_atIndex nsxmlElement children index =
  sendMessage nsxmlElement insertChildren_atIndexSelector (toNSArray children) index

-- | removeChildAtIndex:atIndex:
--
-- Removes a child at a particular index.
--
-- ObjC selector: @- removeChildAtIndex:@
removeChildAtIndex :: IsNSXMLElement nsxmlElement => nsxmlElement -> CULong -> IO ()
removeChildAtIndex nsxmlElement index =
  sendMessage nsxmlElement removeChildAtIndexSelector index

-- | setChildren:
--
-- Removes all existing children and replaces them with the new children. Set children to nil to simply remove all children.
--
-- ObjC selector: @- setChildren:@
setChildren :: (IsNSXMLElement nsxmlElement, IsNSArray children) => nsxmlElement -> children -> IO ()
setChildren nsxmlElement children =
  sendMessage nsxmlElement setChildrenSelector (toNSArray children)

-- | addChild:
--
-- Adds a child to the end of the existing children.
--
-- ObjC selector: @- addChild:@
addChild :: (IsNSXMLElement nsxmlElement, IsNSXMLNode child) => nsxmlElement -> child -> IO ()
addChild nsxmlElement child =
  sendMessage nsxmlElement addChildSelector (toNSXMLNode child)

-- | replaceChildAtIndex:withNode:
--
-- Replaces a child at a particular index with another child.
--
-- ObjC selector: @- replaceChildAtIndex:withNode:@
replaceChildAtIndex_withNode :: (IsNSXMLElement nsxmlElement, IsNSXMLNode node) => nsxmlElement -> CULong -> node -> IO ()
replaceChildAtIndex_withNode nsxmlElement index node =
  sendMessage nsxmlElement replaceChildAtIndex_withNodeSelector index (toNSXMLNode node)

-- | normalizeAdjacentTextNodesPreservingCDATA:
--
-- Adjacent text nodes are coalesced. If the node's value is the empty string, it is removed. This should be called with a value of NO before using XQuery or XPath.
--
-- ObjC selector: @- normalizeAdjacentTextNodesPreservingCDATA:@
normalizeAdjacentTextNodesPreservingCDATA :: IsNSXMLElement nsxmlElement => nsxmlElement -> Bool -> IO ()
normalizeAdjacentTextNodesPreservingCDATA nsxmlElement preserve =
  sendMessage nsxmlElement normalizeAdjacentTextNodesPreservingCDATASelector preserve

-- | setAttributesAsDictionary:
--
-- Set the attributes base on a name-value dictionary.
--
-- This method is deprecated and does not function correctly. Use -setAttributesWithDictionary: instead.
--
-- ObjC selector: @- setAttributesAsDictionary:@
setAttributesAsDictionary :: (IsNSXMLElement nsxmlElement, IsNSDictionary attributes) => nsxmlElement -> attributes -> IO ()
setAttributesAsDictionary nsxmlElement attributes =
  sendMessage nsxmlElement setAttributesAsDictionarySelector (toNSDictionary attributes)

-- | Set the attributes. In the case of duplicate names, the first attribute with the name is used.
--
-- ObjC selector: @- attributes@
attributes :: IsNSXMLElement nsxmlElement => nsxmlElement -> IO (Id NSArray)
attributes nsxmlElement =
  sendMessage nsxmlElement attributesSelector

-- | Set the attributes. In the case of duplicate names, the first attribute with the name is used.
--
-- ObjC selector: @- setAttributes:@
setAttributes :: (IsNSXMLElement nsxmlElement, IsNSArray value) => nsxmlElement -> value -> IO ()
setAttributes nsxmlElement value =
  sendMessage nsxmlElement setAttributesSelector (toNSArray value)

-- | Set the namespaces. In the case of duplicate names, the first namespace with the name is used.
--
-- ObjC selector: @- namespaces@
namespaces :: IsNSXMLElement nsxmlElement => nsxmlElement -> IO (Id NSArray)
namespaces nsxmlElement =
  sendMessage nsxmlElement namespacesSelector

-- | Set the namespaces. In the case of duplicate names, the first namespace with the name is used.
--
-- ObjC selector: @- setNamespaces:@
setNamespaces :: (IsNSXMLElement nsxmlElement, IsNSArray value) => nsxmlElement -> value -> IO ()
setNamespaces nsxmlElement value =
  sendMessage nsxmlElement setNamespacesSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:@
initWithNameSelector :: Selector '[Id NSString] (Id NSXMLElement)
initWithNameSelector = mkSelector "initWithName:"

-- | @Selector@ for @initWithName:URI:@
initWithName_URISelector :: Selector '[Id NSString, Id NSString] (Id NSXMLElement)
initWithName_URISelector = mkSelector "initWithName:URI:"

-- | @Selector@ for @initWithName:stringValue:@
initWithName_stringValueSelector :: Selector '[Id NSString, Id NSString] (Id NSXMLElement)
initWithName_stringValueSelector = mkSelector "initWithName:stringValue:"

-- | @Selector@ for @initWithXMLString:error:@
initWithXMLString_errorSelector :: Selector '[Id NSString, Id NSError] (Id NSXMLElement)
initWithXMLString_errorSelector = mkSelector "initWithXMLString:error:"

-- | @Selector@ for @initWithKind:options:@
initWithKind_optionsSelector :: Selector '[NSXMLNodeKind, NSXMLNodeOptions] (Id NSXMLElement)
initWithKind_optionsSelector = mkSelector "initWithKind:options:"

-- | @Selector@ for @elementsForName:@
elementsForNameSelector :: Selector '[Id NSString] (Id NSArray)
elementsForNameSelector = mkSelector "elementsForName:"

-- | @Selector@ for @elementsForLocalName:URI:@
elementsForLocalName_URISelector :: Selector '[Id NSString, Id NSString] (Id NSArray)
elementsForLocalName_URISelector = mkSelector "elementsForLocalName:URI:"

-- | @Selector@ for @addAttribute:@
addAttributeSelector :: Selector '[Id NSXMLNode] ()
addAttributeSelector = mkSelector "addAttribute:"

-- | @Selector@ for @removeAttributeForName:@
removeAttributeForNameSelector :: Selector '[Id NSString] ()
removeAttributeForNameSelector = mkSelector "removeAttributeForName:"

-- | @Selector@ for @setAttributesWithDictionary:@
setAttributesWithDictionarySelector :: Selector '[Id NSDictionary] ()
setAttributesWithDictionarySelector = mkSelector "setAttributesWithDictionary:"

-- | @Selector@ for @attributeForName:@
attributeForNameSelector :: Selector '[Id NSString] (Id NSXMLNode)
attributeForNameSelector = mkSelector "attributeForName:"

-- | @Selector@ for @attributeForLocalName:URI:@
attributeForLocalName_URISelector :: Selector '[Id NSString, Id NSString] (Id NSXMLNode)
attributeForLocalName_URISelector = mkSelector "attributeForLocalName:URI:"

-- | @Selector@ for @addNamespace:@
addNamespaceSelector :: Selector '[Id NSXMLNode] ()
addNamespaceSelector = mkSelector "addNamespace:"

-- | @Selector@ for @removeNamespaceForPrefix:@
removeNamespaceForPrefixSelector :: Selector '[Id NSString] ()
removeNamespaceForPrefixSelector = mkSelector "removeNamespaceForPrefix:"

-- | @Selector@ for @namespaceForPrefix:@
namespaceForPrefixSelector :: Selector '[Id NSString] (Id NSXMLNode)
namespaceForPrefixSelector = mkSelector "namespaceForPrefix:"

-- | @Selector@ for @resolveNamespaceForName:@
resolveNamespaceForNameSelector :: Selector '[Id NSString] (Id NSXMLNode)
resolveNamespaceForNameSelector = mkSelector "resolveNamespaceForName:"

-- | @Selector@ for @resolvePrefixForNamespaceURI:@
resolvePrefixForNamespaceURISelector :: Selector '[Id NSString] (Id NSString)
resolvePrefixForNamespaceURISelector = mkSelector "resolvePrefixForNamespaceURI:"

-- | @Selector@ for @insertChild:atIndex:@
insertChild_atIndexSelector :: Selector '[Id NSXMLNode, CULong] ()
insertChild_atIndexSelector = mkSelector "insertChild:atIndex:"

-- | @Selector@ for @insertChildren:atIndex:@
insertChildren_atIndexSelector :: Selector '[Id NSArray, CULong] ()
insertChildren_atIndexSelector = mkSelector "insertChildren:atIndex:"

-- | @Selector@ for @removeChildAtIndex:@
removeChildAtIndexSelector :: Selector '[CULong] ()
removeChildAtIndexSelector = mkSelector "removeChildAtIndex:"

-- | @Selector@ for @setChildren:@
setChildrenSelector :: Selector '[Id NSArray] ()
setChildrenSelector = mkSelector "setChildren:"

-- | @Selector@ for @addChild:@
addChildSelector :: Selector '[Id NSXMLNode] ()
addChildSelector = mkSelector "addChild:"

-- | @Selector@ for @replaceChildAtIndex:withNode:@
replaceChildAtIndex_withNodeSelector :: Selector '[CULong, Id NSXMLNode] ()
replaceChildAtIndex_withNodeSelector = mkSelector "replaceChildAtIndex:withNode:"

-- | @Selector@ for @normalizeAdjacentTextNodesPreservingCDATA:@
normalizeAdjacentTextNodesPreservingCDATASelector :: Selector '[Bool] ()
normalizeAdjacentTextNodesPreservingCDATASelector = mkSelector "normalizeAdjacentTextNodesPreservingCDATA:"

-- | @Selector@ for @setAttributesAsDictionary:@
setAttributesAsDictionarySelector :: Selector '[Id NSDictionary] ()
setAttributesAsDictionarySelector = mkSelector "setAttributesAsDictionary:"

-- | @Selector@ for @attributes@
attributesSelector :: Selector '[] (Id NSArray)
attributesSelector = mkSelector "attributes"

-- | @Selector@ for @setAttributes:@
setAttributesSelector :: Selector '[Id NSArray] ()
setAttributesSelector = mkSelector "setAttributes:"

-- | @Selector@ for @namespaces@
namespacesSelector :: Selector '[] (Id NSArray)
namespacesSelector = mkSelector "namespaces"

-- | @Selector@ for @setNamespaces:@
setNamespacesSelector :: Selector '[Id NSArray] ()
setNamespacesSelector = mkSelector "setNamespaces:"

