{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NSXMLNode
--
-- The basic unit of an XML document.
--
-- Generated bindings for @NSXMLNode@.
module ObjC.Foundation.NSXMLNode
  ( NSXMLNode
  , IsNSXMLNode(..)
  , init_
  , initWithKind
  , initWithKind_options
  , document
  , documentWithRootElement
  , elementWithName
  , elementWithName_URI
  , elementWithName_stringValue
  , elementWithName_children_attributes
  , attributeWithName_stringValue
  , attributeWithName_URI_stringValue
  , namespaceWithName_stringValue
  , processingInstructionWithName_stringValue
  , commentWithStringValue
  , textWithStringValue
  , dtdNodeWithXMLString
  , setStringValue_resolvingEntities
  , childAtIndex
  , detach
  , localNameForName
  , prefixForName
  , predefinedNamespaceForPrefix
  , xmlStringWithOptions
  , canonicalXMLStringPreservingComments
  , nodesForXPath_error
  , objectsForXQuery_constants_error
  , objectsForXQuery_error
  , kind
  , name
  , setName
  , objectValue
  , setObjectValue
  , stringValue
  , setStringValue
  , index
  , level
  , rootDocument
  , parent
  , childCount
  , children
  , previousSibling
  , nextSibling
  , previousNode
  , nextNode
  , xPath
  , localName
  , prefix
  , uri
  , setURI
  , description
  , xmlString
  , attributeWithName_URI_stringValueSelector
  , attributeWithName_stringValueSelector
  , canonicalXMLStringPreservingCommentsSelector
  , childAtIndexSelector
  , childCountSelector
  , childrenSelector
  , commentWithStringValueSelector
  , descriptionSelector
  , detachSelector
  , documentSelector
  , documentWithRootElementSelector
  , dtdNodeWithXMLStringSelector
  , elementWithNameSelector
  , elementWithName_URISelector
  , elementWithName_children_attributesSelector
  , elementWithName_stringValueSelector
  , indexSelector
  , initSelector
  , initWithKindSelector
  , initWithKind_optionsSelector
  , kindSelector
  , levelSelector
  , localNameForNameSelector
  , localNameSelector
  , nameSelector
  , namespaceWithName_stringValueSelector
  , nextNodeSelector
  , nextSiblingSelector
  , nodesForXPath_errorSelector
  , objectValueSelector
  , objectsForXQuery_constants_errorSelector
  , objectsForXQuery_errorSelector
  , parentSelector
  , predefinedNamespaceForPrefixSelector
  , prefixForNameSelector
  , prefixSelector
  , previousNodeSelector
  , previousSiblingSelector
  , processingInstructionWithName_stringValueSelector
  , rootDocumentSelector
  , setNameSelector
  , setObjectValueSelector
  , setStringValueSelector
  , setStringValue_resolvingEntitiesSelector
  , setURISelector
  , stringValueSelector
  , textWithStringValueSelector
  , uriSelector
  , xPathSelector
  , xmlStringSelector
  , xmlStringWithOptionsSelector

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

-- | @- init@
init_ :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO (Id NSXMLNode)
init_ nsxmlNode =
  sendOwnedMessage nsxmlNode initSelector

-- | initWithKind:
--
-- Invokes
--
-- initWithKind:options:
--
-- with options set to NSXMLNodeOptionsNone
--
-- ObjC selector: @- initWithKind:@
initWithKind :: IsNSXMLNode nsxmlNode => nsxmlNode -> NSXMLNodeKind -> IO (Id NSXMLNode)
initWithKind nsxmlNode kind =
  sendOwnedMessage nsxmlNode initWithKindSelector kind

-- | initWithKind:options:
--
-- Inits a node with fidelity options as description NSXMLNodeOptions.h
--
-- ObjC selector: @- initWithKind:options:@
initWithKind_options :: IsNSXMLNode nsxmlNode => nsxmlNode -> NSXMLNodeKind -> NSXMLNodeOptions -> IO (Id NSXMLNode)
initWithKind_options nsxmlNode kind options =
  sendOwnedMessage nsxmlNode initWithKind_optionsSelector kind options

-- | document:
--
-- Returns an empty document.
--
-- ObjC selector: @+ document@
document :: IO RawId
document  =
  do
    cls' <- getRequiredClass "NSXMLNode"
    sendClassMessage cls' documentSelector

-- | documentWithRootElement:
--
-- Returns a document
--
-- @element@ — The document's root node.
--
-- ObjC selector: @+ documentWithRootElement:@
documentWithRootElement :: IsNSXMLElement element => element -> IO RawId
documentWithRootElement element =
  do
    cls' <- getRequiredClass "NSXMLNode"
    sendClassMessage cls' documentWithRootElementSelector (toNSXMLElement element)

-- | elementWithName:
--
-- Returns an element <name></name>.
--
-- ObjC selector: @+ elementWithName:@
elementWithName :: IsNSString name => name -> IO RawId
elementWithName name =
  do
    cls' <- getRequiredClass "NSXMLNode"
    sendClassMessage cls' elementWithNameSelector (toNSString name)

-- | elementWithName:URI:
--
-- Returns an element whose full QName is specified.
--
-- ObjC selector: @+ elementWithName:URI:@
elementWithName_URI :: (IsNSString name, IsNSString uri) => name -> uri -> IO RawId
elementWithName_URI name uri =
  do
    cls' <- getRequiredClass "NSXMLNode"
    sendClassMessage cls' elementWithName_URISelector (toNSString name) (toNSString uri)

-- | elementWithName:stringValue:
--
-- Returns an element with a single text node child <name>string</name>.
--
-- ObjC selector: @+ elementWithName:stringValue:@
elementWithName_stringValue :: (IsNSString name, IsNSString string) => name -> string -> IO RawId
elementWithName_stringValue name string =
  do
    cls' <- getRequiredClass "NSXMLNode"
    sendClassMessage cls' elementWithName_stringValueSelector (toNSString name) (toNSString string)

-- | elementWithName:children:attributes:
--
-- Returns an element children and attributes <name attr1="foo" attr2="bar"><-- child1 -->child2</name>.
--
-- ObjC selector: @+ elementWithName:children:attributes:@
elementWithName_children_attributes :: (IsNSString name, IsNSArray children, IsNSArray attributes) => name -> children -> attributes -> IO RawId
elementWithName_children_attributes name children attributes =
  do
    cls' <- getRequiredClass "NSXMLNode"
    sendClassMessage cls' elementWithName_children_attributesSelector (toNSString name) (toNSArray children) (toNSArray attributes)

-- | attributeWithName:stringValue:
--
-- Returns an attribute name="stringValue".
--
-- ObjC selector: @+ attributeWithName:stringValue:@
attributeWithName_stringValue :: (IsNSString name, IsNSString stringValue) => name -> stringValue -> IO RawId
attributeWithName_stringValue name stringValue =
  do
    cls' <- getRequiredClass "NSXMLNode"
    sendClassMessage cls' attributeWithName_stringValueSelector (toNSString name) (toNSString stringValue)

-- | attributeWithLocalName:URI:stringValue:
--
-- Returns an attribute whose full QName is specified.
--
-- ObjC selector: @+ attributeWithName:URI:stringValue:@
attributeWithName_URI_stringValue :: (IsNSString name, IsNSString uri, IsNSString stringValue) => name -> uri -> stringValue -> IO RawId
attributeWithName_URI_stringValue name uri stringValue =
  do
    cls' <- getRequiredClass "NSXMLNode"
    sendClassMessage cls' attributeWithName_URI_stringValueSelector (toNSString name) (toNSString uri) (toNSString stringValue)

-- | namespaceWithName:stringValue:
--
-- Returns a namespace xmlns:name="stringValue".
--
-- ObjC selector: @+ namespaceWithName:stringValue:@
namespaceWithName_stringValue :: (IsNSString name, IsNSString stringValue) => name -> stringValue -> IO RawId
namespaceWithName_stringValue name stringValue =
  do
    cls' <- getRequiredClass "NSXMLNode"
    sendClassMessage cls' namespaceWithName_stringValueSelector (toNSString name) (toNSString stringValue)

-- | processingInstructionWithName:stringValue:
--
-- Returns a processing instruction <?name stringValue>.
--
-- ObjC selector: @+ processingInstructionWithName:stringValue:@
processingInstructionWithName_stringValue :: (IsNSString name, IsNSString stringValue) => name -> stringValue -> IO RawId
processingInstructionWithName_stringValue name stringValue =
  do
    cls' <- getRequiredClass "NSXMLNode"
    sendClassMessage cls' processingInstructionWithName_stringValueSelector (toNSString name) (toNSString stringValue)

-- | commentWithStringValue:
--
-- Returns a comment <--stringValue-->.
--
-- ObjC selector: @+ commentWithStringValue:@
commentWithStringValue :: IsNSString stringValue => stringValue -> IO RawId
commentWithStringValue stringValue =
  do
    cls' <- getRequiredClass "NSXMLNode"
    sendClassMessage cls' commentWithStringValueSelector (toNSString stringValue)

-- | textWithStringValue:
--
-- Returns a text node.
--
-- ObjC selector: @+ textWithStringValue:@
textWithStringValue :: IsNSString stringValue => stringValue -> IO RawId
textWithStringValue stringValue =
  do
    cls' <- getRequiredClass "NSXMLNode"
    sendClassMessage cls' textWithStringValueSelector (toNSString stringValue)

-- | DTDNodeWithXMLString:
--
-- Returns an element, attribute, entity, or notation DTD node based on the full XML string.
--
-- ObjC selector: @+ DTDNodeWithXMLString:@
dtdNodeWithXMLString :: IsNSString string => string -> IO RawId
dtdNodeWithXMLString string =
  do
    cls' <- getRequiredClass "NSXMLNode"
    sendClassMessage cls' dtdNodeWithXMLStringSelector (toNSString string)

-- | setStringValue:resolvingEntities:
--
-- Sets the content as with
--
-- setStringValue:
--
-- , but when "resolve" is true, character references, predefined entities and user entities available in the document's dtd are resolved. Entities not available in the dtd remain in their entity form.
--
-- ObjC selector: @- setStringValue:resolvingEntities:@
setStringValue_resolvingEntities :: (IsNSXMLNode nsxmlNode, IsNSString string) => nsxmlNode -> string -> Bool -> IO ()
setStringValue_resolvingEntities nsxmlNode string resolve =
  sendMessage nsxmlNode setStringValue_resolvingEntitiesSelector (toNSString string) resolve

-- | childAtIndex:
--
-- Returns the child node at a particular index.
--
-- ObjC selector: @- childAtIndex:@
childAtIndex :: IsNSXMLNode nsxmlNode => nsxmlNode -> CULong -> IO (Id NSXMLNode)
childAtIndex nsxmlNode index =
  sendMessage nsxmlNode childAtIndexSelector index

-- | detach:
--
-- Detaches this node from its parent.
--
-- ObjC selector: @- detach@
detach :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO ()
detach nsxmlNode =
  sendMessage nsxmlNode detachSelector

-- | localNameForName:
--
-- Returns the local name bar in foo:bar.
--
-- ObjC selector: @+ localNameForName:@
localNameForName :: IsNSString name => name -> IO (Id NSString)
localNameForName name =
  do
    cls' <- getRequiredClass "NSXMLNode"
    sendClassMessage cls' localNameForNameSelector (toNSString name)

-- | localNameForName:
--
-- Returns the prefix foo in the name foo:bar.
--
-- ObjC selector: @+ prefixForName:@
prefixForName :: IsNSString name => name -> IO (Id NSString)
prefixForName name =
  do
    cls' <- getRequiredClass "NSXMLNode"
    sendClassMessage cls' prefixForNameSelector (toNSString name)

-- | predefinedNamespaceForPrefix:
--
-- Returns the namespace belonging to one of the predefined namespaces xml, xs, or xsi
--
-- ObjC selector: @+ predefinedNamespaceForPrefix:@
predefinedNamespaceForPrefix :: IsNSString name => name -> IO (Id NSXMLNode)
predefinedNamespaceForPrefix name =
  do
    cls' <- getRequiredClass "NSXMLNode"
    sendClassMessage cls' predefinedNamespaceForPrefixSelector (toNSString name)

-- | XMLStringWithOptions:
--
-- The representation of this node as it would appear in an XML document, with various output options available.
--
-- ObjC selector: @- XMLStringWithOptions:@
xmlStringWithOptions :: IsNSXMLNode nsxmlNode => nsxmlNode -> NSXMLNodeOptions -> IO (Id NSString)
xmlStringWithOptions nsxmlNode options =
  sendMessage nsxmlNode xmlStringWithOptionsSelector options

-- | canonicalXMLStringPreservingComments:
--
-- W3 canonical form (http://www.w3.org/TR/xml-c14n). The input option NSXMLNodePreserveWhitespace should be set for true canonical form.
--
-- ObjC selector: @- canonicalXMLStringPreservingComments:@
canonicalXMLStringPreservingComments :: IsNSXMLNode nsxmlNode => nsxmlNode -> Bool -> IO (Id NSString)
canonicalXMLStringPreservingComments nsxmlNode comments =
  sendMessage nsxmlNode canonicalXMLStringPreservingCommentsSelector comments

-- | nodesForXPath:error:
--
-- Returns the nodes resulting from applying an XPath to this node using the node as the context item ("."). normalizeAdjacentTextNodesPreservingCDATA:NO should be called if there are adjacent text nodes since they are not allowed under the XPath/XQuery Data Model.
--
-- An array whose elements are a kind of NSXMLNode.
--
-- ObjC selector: @- nodesForXPath:error:@
nodesForXPath_error :: (IsNSXMLNode nsxmlNode, IsNSString xpath, IsNSError error_) => nsxmlNode -> xpath -> error_ -> IO (Id NSArray)
nodesForXPath_error nsxmlNode xpath error_ =
  sendMessage nsxmlNode nodesForXPath_errorSelector (toNSString xpath) (toNSError error_)

-- | objectsForXQuery:constants:error:
--
-- Returns the objects resulting from applying an XQuery to this node using the node as the context item ("."). Constants are a name-value dictionary for constants declared "external" in the query. normalizeAdjacentTextNodesPreservingCDATA:NO should be called if there are adjacent text nodes since they are not allowed under the XPath/XQuery Data Model.
--
-- An array whose elements are kinds of NSArray, NSData, NSDate, NSNumber, NSString, NSURL, or NSXMLNode.
--
-- ObjC selector: @- objectsForXQuery:constants:error:@
objectsForXQuery_constants_error :: (IsNSXMLNode nsxmlNode, IsNSString xquery, IsNSDictionary constants, IsNSError error_) => nsxmlNode -> xquery -> constants -> error_ -> IO (Id NSArray)
objectsForXQuery_constants_error nsxmlNode xquery constants error_ =
  sendMessage nsxmlNode objectsForXQuery_constants_errorSelector (toNSString xquery) (toNSDictionary constants) (toNSError error_)

-- | @- objectsForXQuery:error:@
objectsForXQuery_error :: (IsNSXMLNode nsxmlNode, IsNSString xquery, IsNSError error_) => nsxmlNode -> xquery -> error_ -> IO (Id NSArray)
objectsForXQuery_error nsxmlNode xquery error_ =
  sendMessage nsxmlNode objectsForXQuery_errorSelector (toNSString xquery) (toNSError error_)

-- | Returns an element, attribute, entity, or notation DTD node based on the full XML string.
--
-- ObjC selector: @- kind@
kind :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO NSXMLNodeKind
kind nsxmlNode =
  sendMessage nsxmlNode kindSelector

-- | Sets the nodes name. Applicable for element, attribute, namespace, processing-instruction, document type declaration, element declaration, attribute declaration, entity declaration, and notation declaration.
--
-- ObjC selector: @- name@
name :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO (Id NSString)
name nsxmlNode =
  sendMessage nsxmlNode nameSelector

-- | Sets the nodes name. Applicable for element, attribute, namespace, processing-instruction, document type declaration, element declaration, attribute declaration, entity declaration, and notation declaration.
--
-- ObjC selector: @- setName:@
setName :: (IsNSXMLNode nsxmlNode, IsNSString value) => nsxmlNode -> value -> IO ()
setName nsxmlNode value =
  sendMessage nsxmlNode setNameSelector (toNSString value)

-- | Sets the content of the node. Setting the objectValue removes all existing children including processing instructions and comments. Setting the object value on an element creates a single text node child.
--
-- ObjC selector: @- objectValue@
objectValue :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO RawId
objectValue nsxmlNode =
  sendMessage nsxmlNode objectValueSelector

-- | Sets the content of the node. Setting the objectValue removes all existing children including processing instructions and comments. Setting the object value on an element creates a single text node child.
--
-- ObjC selector: @- setObjectValue:@
setObjectValue :: IsNSXMLNode nsxmlNode => nsxmlNode -> RawId -> IO ()
setObjectValue nsxmlNode value =
  sendMessage nsxmlNode setObjectValueSelector value

-- | Sets the content of the node. Setting the stringValue removes all existing children including processing instructions and comments. Setting the string value on an element creates a single text node child. The getter returns the string value of the node, which may be either its content or child text nodes, depending on the type of node. Elements are recursed and text nodes concatenated in document order with no intervening spaces.
--
-- ObjC selector: @- stringValue@
stringValue :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO (Id NSString)
stringValue nsxmlNode =
  sendMessage nsxmlNode stringValueSelector

-- | Sets the content of the node. Setting the stringValue removes all existing children including processing instructions and comments. Setting the string value on an element creates a single text node child. The getter returns the string value of the node, which may be either its content or child text nodes, depending on the type of node. Elements are recursed and text nodes concatenated in document order with no intervening spaces.
--
-- ObjC selector: @- setStringValue:@
setStringValue :: (IsNSXMLNode nsxmlNode, IsNSString value) => nsxmlNode -> value -> IO ()
setStringValue nsxmlNode value =
  sendMessage nsxmlNode setStringValueSelector (toNSString value)

-- | A node's index amongst its siblings.
--
-- ObjC selector: @- index@
index :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO CULong
index nsxmlNode =
  sendMessage nsxmlNode indexSelector

-- | The depth of the node within the tree. Documents and standalone nodes are level 0.
--
-- ObjC selector: @- level@
level :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO CULong
level nsxmlNode =
  sendMessage nsxmlNode levelSelector

-- | The encompassing document or nil.
--
-- ObjC selector: @- rootDocument@
rootDocument :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO (Id NSXMLDocument)
rootDocument nsxmlNode =
  sendMessage nsxmlNode rootDocumentSelector

-- | The parent of this node. Documents and standalone Nodes have a nil parent; there is not a 1-to-1 relationship between parent and children, eg a namespace cannot be a child but has a parent element.
--
-- ObjC selector: @- parent@
parent :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO (Id NSXMLNode)
parent nsxmlNode =
  sendMessage nsxmlNode parentSelector

-- | The amount of children, relevant for documents, elements, and document type declarations. Use this instead of [[self children] count].
--
-- ObjC selector: @- childCount@
childCount :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO CULong
childCount nsxmlNode =
  sendMessage nsxmlNode childCountSelector

-- | An immutable array of child nodes. Relevant for documents, elements, and document type declarations.
--
-- ObjC selector: @- children@
children :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO (Id NSArray)
children nsxmlNode =
  sendMessage nsxmlNode childrenSelector

-- | Returns the previous sibling, or nil if there isn't one.
--
-- ObjC selector: @- previousSibling@
previousSibling :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO (Id NSXMLNode)
previousSibling nsxmlNode =
  sendMessage nsxmlNode previousSiblingSelector

-- | Returns the next sibling, or nil if there isn't one.
--
-- ObjC selector: @- nextSibling@
nextSibling :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO (Id NSXMLNode)
nextSibling nsxmlNode =
  sendMessage nsxmlNode nextSiblingSelector

-- | Returns the previous node in document order. This can be used to walk the tree backwards.
--
-- ObjC selector: @- previousNode@
previousNode :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO (Id NSXMLNode)
previousNode nsxmlNode =
  sendMessage nsxmlNode previousNodeSelector

-- | Returns the next node in document order. This can be used to walk the tree forwards.
--
-- ObjC selector: @- nextNode@
nextNode :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO (Id NSXMLNode)
nextNode nsxmlNode =
  sendMessage nsxmlNode nextNodeSelector

-- | Returns the XPath to this node, for example foo/bar[2]/baz.
--
-- ObjC selector: @- XPath@
xPath :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO (Id NSString)
xPath nsxmlNode =
  sendMessage nsxmlNode xPathSelector

-- | Returns the local name bar if this attribute or element's name is foo:bar
--
-- ObjC selector: @- localName@
localName :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO (Id NSString)
localName nsxmlNode =
  sendMessage nsxmlNode localNameSelector

-- | Returns the prefix foo if this attribute or element's name if foo:bar
--
-- ObjC selector: @- prefix@
prefix :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO (Id NSString)
prefix nsxmlNode =
  sendMessage nsxmlNode prefixSelector

-- | Set the URI of this element, attribute, or document. For documents it is the URI of document origin. Getter returns the URI of this element, attribute, or document. For documents it is the URI of document origin and is automatically set when using initWithContentsOfURL.
--
-- ObjC selector: @- URI@
uri :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO (Id NSString)
uri nsxmlNode =
  sendMessage nsxmlNode uriSelector

-- | Set the URI of this element, attribute, or document. For documents it is the URI of document origin. Getter returns the URI of this element, attribute, or document. For documents it is the URI of document origin and is automatically set when using initWithContentsOfURL.
--
-- ObjC selector: @- setURI:@
setURI :: (IsNSXMLNode nsxmlNode, IsNSString value) => nsxmlNode -> value -> IO ()
setURI nsxmlNode value =
  sendMessage nsxmlNode setURISelector (toNSString value)

-- | Used for debugging. May give more information than XMLString.
--
-- ObjC selector: @- description@
description :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO (Id NSString)
description nsxmlNode =
  sendMessage nsxmlNode descriptionSelector

-- | The representation of this node as it would appear in an XML document.
--
-- ObjC selector: @- XMLString@
xmlString :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO (Id NSString)
xmlString nsxmlNode =
  sendMessage nsxmlNode xmlStringSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSXMLNode)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithKind:@
initWithKindSelector :: Selector '[NSXMLNodeKind] (Id NSXMLNode)
initWithKindSelector = mkSelector "initWithKind:"

-- | @Selector@ for @initWithKind:options:@
initWithKind_optionsSelector :: Selector '[NSXMLNodeKind, NSXMLNodeOptions] (Id NSXMLNode)
initWithKind_optionsSelector = mkSelector "initWithKind:options:"

-- | @Selector@ for @document@
documentSelector :: Selector '[] RawId
documentSelector = mkSelector "document"

-- | @Selector@ for @documentWithRootElement:@
documentWithRootElementSelector :: Selector '[Id NSXMLElement] RawId
documentWithRootElementSelector = mkSelector "documentWithRootElement:"

-- | @Selector@ for @elementWithName:@
elementWithNameSelector :: Selector '[Id NSString] RawId
elementWithNameSelector = mkSelector "elementWithName:"

-- | @Selector@ for @elementWithName:URI:@
elementWithName_URISelector :: Selector '[Id NSString, Id NSString] RawId
elementWithName_URISelector = mkSelector "elementWithName:URI:"

-- | @Selector@ for @elementWithName:stringValue:@
elementWithName_stringValueSelector :: Selector '[Id NSString, Id NSString] RawId
elementWithName_stringValueSelector = mkSelector "elementWithName:stringValue:"

-- | @Selector@ for @elementWithName:children:attributes:@
elementWithName_children_attributesSelector :: Selector '[Id NSString, Id NSArray, Id NSArray] RawId
elementWithName_children_attributesSelector = mkSelector "elementWithName:children:attributes:"

-- | @Selector@ for @attributeWithName:stringValue:@
attributeWithName_stringValueSelector :: Selector '[Id NSString, Id NSString] RawId
attributeWithName_stringValueSelector = mkSelector "attributeWithName:stringValue:"

-- | @Selector@ for @attributeWithName:URI:stringValue:@
attributeWithName_URI_stringValueSelector :: Selector '[Id NSString, Id NSString, Id NSString] RawId
attributeWithName_URI_stringValueSelector = mkSelector "attributeWithName:URI:stringValue:"

-- | @Selector@ for @namespaceWithName:stringValue:@
namespaceWithName_stringValueSelector :: Selector '[Id NSString, Id NSString] RawId
namespaceWithName_stringValueSelector = mkSelector "namespaceWithName:stringValue:"

-- | @Selector@ for @processingInstructionWithName:stringValue:@
processingInstructionWithName_stringValueSelector :: Selector '[Id NSString, Id NSString] RawId
processingInstructionWithName_stringValueSelector = mkSelector "processingInstructionWithName:stringValue:"

-- | @Selector@ for @commentWithStringValue:@
commentWithStringValueSelector :: Selector '[Id NSString] RawId
commentWithStringValueSelector = mkSelector "commentWithStringValue:"

-- | @Selector@ for @textWithStringValue:@
textWithStringValueSelector :: Selector '[Id NSString] RawId
textWithStringValueSelector = mkSelector "textWithStringValue:"

-- | @Selector@ for @DTDNodeWithXMLString:@
dtdNodeWithXMLStringSelector :: Selector '[Id NSString] RawId
dtdNodeWithXMLStringSelector = mkSelector "DTDNodeWithXMLString:"

-- | @Selector@ for @setStringValue:resolvingEntities:@
setStringValue_resolvingEntitiesSelector :: Selector '[Id NSString, Bool] ()
setStringValue_resolvingEntitiesSelector = mkSelector "setStringValue:resolvingEntities:"

-- | @Selector@ for @childAtIndex:@
childAtIndexSelector :: Selector '[CULong] (Id NSXMLNode)
childAtIndexSelector = mkSelector "childAtIndex:"

-- | @Selector@ for @detach@
detachSelector :: Selector '[] ()
detachSelector = mkSelector "detach"

-- | @Selector@ for @localNameForName:@
localNameForNameSelector :: Selector '[Id NSString] (Id NSString)
localNameForNameSelector = mkSelector "localNameForName:"

-- | @Selector@ for @prefixForName:@
prefixForNameSelector :: Selector '[Id NSString] (Id NSString)
prefixForNameSelector = mkSelector "prefixForName:"

-- | @Selector@ for @predefinedNamespaceForPrefix:@
predefinedNamespaceForPrefixSelector :: Selector '[Id NSString] (Id NSXMLNode)
predefinedNamespaceForPrefixSelector = mkSelector "predefinedNamespaceForPrefix:"

-- | @Selector@ for @XMLStringWithOptions:@
xmlStringWithOptionsSelector :: Selector '[NSXMLNodeOptions] (Id NSString)
xmlStringWithOptionsSelector = mkSelector "XMLStringWithOptions:"

-- | @Selector@ for @canonicalXMLStringPreservingComments:@
canonicalXMLStringPreservingCommentsSelector :: Selector '[Bool] (Id NSString)
canonicalXMLStringPreservingCommentsSelector = mkSelector "canonicalXMLStringPreservingComments:"

-- | @Selector@ for @nodesForXPath:error:@
nodesForXPath_errorSelector :: Selector '[Id NSString, Id NSError] (Id NSArray)
nodesForXPath_errorSelector = mkSelector "nodesForXPath:error:"

-- | @Selector@ for @objectsForXQuery:constants:error:@
objectsForXQuery_constants_errorSelector :: Selector '[Id NSString, Id NSDictionary, Id NSError] (Id NSArray)
objectsForXQuery_constants_errorSelector = mkSelector "objectsForXQuery:constants:error:"

-- | @Selector@ for @objectsForXQuery:error:@
objectsForXQuery_errorSelector :: Selector '[Id NSString, Id NSError] (Id NSArray)
objectsForXQuery_errorSelector = mkSelector "objectsForXQuery:error:"

-- | @Selector@ for @kind@
kindSelector :: Selector '[] NSXMLNodeKind
kindSelector = mkSelector "kind"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @objectValue@
objectValueSelector :: Selector '[] RawId
objectValueSelector = mkSelector "objectValue"

-- | @Selector@ for @setObjectValue:@
setObjectValueSelector :: Selector '[RawId] ()
setObjectValueSelector = mkSelector "setObjectValue:"

-- | @Selector@ for @stringValue@
stringValueSelector :: Selector '[] (Id NSString)
stringValueSelector = mkSelector "stringValue"

-- | @Selector@ for @setStringValue:@
setStringValueSelector :: Selector '[Id NSString] ()
setStringValueSelector = mkSelector "setStringValue:"

-- | @Selector@ for @index@
indexSelector :: Selector '[] CULong
indexSelector = mkSelector "index"

-- | @Selector@ for @level@
levelSelector :: Selector '[] CULong
levelSelector = mkSelector "level"

-- | @Selector@ for @rootDocument@
rootDocumentSelector :: Selector '[] (Id NSXMLDocument)
rootDocumentSelector = mkSelector "rootDocument"

-- | @Selector@ for @parent@
parentSelector :: Selector '[] (Id NSXMLNode)
parentSelector = mkSelector "parent"

-- | @Selector@ for @childCount@
childCountSelector :: Selector '[] CULong
childCountSelector = mkSelector "childCount"

-- | @Selector@ for @children@
childrenSelector :: Selector '[] (Id NSArray)
childrenSelector = mkSelector "children"

-- | @Selector@ for @previousSibling@
previousSiblingSelector :: Selector '[] (Id NSXMLNode)
previousSiblingSelector = mkSelector "previousSibling"

-- | @Selector@ for @nextSibling@
nextSiblingSelector :: Selector '[] (Id NSXMLNode)
nextSiblingSelector = mkSelector "nextSibling"

-- | @Selector@ for @previousNode@
previousNodeSelector :: Selector '[] (Id NSXMLNode)
previousNodeSelector = mkSelector "previousNode"

-- | @Selector@ for @nextNode@
nextNodeSelector :: Selector '[] (Id NSXMLNode)
nextNodeSelector = mkSelector "nextNode"

-- | @Selector@ for @XPath@
xPathSelector :: Selector '[] (Id NSString)
xPathSelector = mkSelector "XPath"

-- | @Selector@ for @localName@
localNameSelector :: Selector '[] (Id NSString)
localNameSelector = mkSelector "localName"

-- | @Selector@ for @prefix@
prefixSelector :: Selector '[] (Id NSString)
prefixSelector = mkSelector "prefix"

-- | @Selector@ for @URI@
uriSelector :: Selector '[] (Id NSString)
uriSelector = mkSelector "URI"

-- | @Selector@ for @setURI:@
setURISelector :: Selector '[Id NSString] ()
setURISelector = mkSelector "setURI:"

-- | @Selector@ for @description@
descriptionSelector :: Selector '[] (Id NSString)
descriptionSelector = mkSelector "description"

-- | @Selector@ for @XMLString@
xmlStringSelector :: Selector '[] (Id NSString)
xmlStringSelector = mkSelector "XMLString"

