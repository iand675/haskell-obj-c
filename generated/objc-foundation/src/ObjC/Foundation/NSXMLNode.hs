{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithKindSelector
  , initWithKind_optionsSelector
  , documentSelector
  , documentWithRootElementSelector
  , elementWithNameSelector
  , elementWithName_URISelector
  , elementWithName_stringValueSelector
  , elementWithName_children_attributesSelector
  , attributeWithName_stringValueSelector
  , attributeWithName_URI_stringValueSelector
  , namespaceWithName_stringValueSelector
  , processingInstructionWithName_stringValueSelector
  , commentWithStringValueSelector
  , textWithStringValueSelector
  , dtdNodeWithXMLStringSelector
  , setStringValue_resolvingEntitiesSelector
  , childAtIndexSelector
  , detachSelector
  , localNameForNameSelector
  , prefixForNameSelector
  , predefinedNamespaceForPrefixSelector
  , xmlStringWithOptionsSelector
  , canonicalXMLStringPreservingCommentsSelector
  , nodesForXPath_errorSelector
  , objectsForXQuery_constants_errorSelector
  , objectsForXQuery_errorSelector
  , kindSelector
  , nameSelector
  , setNameSelector
  , objectValueSelector
  , setObjectValueSelector
  , stringValueSelector
  , setStringValueSelector
  , indexSelector
  , levelSelector
  , rootDocumentSelector
  , parentSelector
  , childCountSelector
  , childrenSelector
  , previousSiblingSelector
  , nextSiblingSelector
  , previousNodeSelector
  , nextNodeSelector
  , xPathSelector
  , localNameSelector
  , prefixSelector
  , uriSelector
  , setURISelector
  , descriptionSelector
  , xmlStringSelector

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
init_ :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO (Id NSXMLNode)
init_ nsxmlNode  =
  sendMsg nsxmlNode (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
initWithKind nsxmlNode  kind =
  sendMsg nsxmlNode (mkSelector "initWithKind:") (retPtr retVoid) [argCULong (coerce kind)] >>= ownedObject . castPtr

-- | initWithKind:options:
--
-- Inits a node with fidelity options as description NSXMLNodeOptions.h
--
-- ObjC selector: @- initWithKind:options:@
initWithKind_options :: IsNSXMLNode nsxmlNode => nsxmlNode -> NSXMLNodeKind -> NSXMLNodeOptions -> IO (Id NSXMLNode)
initWithKind_options nsxmlNode  kind options =
  sendMsg nsxmlNode (mkSelector "initWithKind:options:") (retPtr retVoid) [argCULong (coerce kind), argCULong (coerce options)] >>= ownedObject . castPtr

-- | document:
--
-- Returns an empty document.
--
-- ObjC selector: @+ document@
document :: IO RawId
document  =
  do
    cls' <- getRequiredClass "NSXMLNode"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "document") (retPtr retVoid) []

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
    withObjCPtr element $ \raw_element ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "documentWithRootElement:") (retPtr retVoid) [argPtr (castPtr raw_element :: Ptr ())]

-- | elementWithName:
--
-- Returns an element <name></name>.
--
-- ObjC selector: @+ elementWithName:@
elementWithName :: IsNSString name => name -> IO RawId
elementWithName name =
  do
    cls' <- getRequiredClass "NSXMLNode"
    withObjCPtr name $ \raw_name ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "elementWithName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())]

-- | elementWithName:URI:
--
-- Returns an element whose full QName is specified.
--
-- ObjC selector: @+ elementWithName:URI:@
elementWithName_URI :: (IsNSString name, IsNSString uri) => name -> uri -> IO RawId
elementWithName_URI name uri =
  do
    cls' <- getRequiredClass "NSXMLNode"
    withObjCPtr name $ \raw_name ->
      withObjCPtr uri $ \raw_uri ->
        fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "elementWithName:URI:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_uri :: Ptr ())]

-- | elementWithName:stringValue:
--
-- Returns an element with a single text node child <name>string</name>.
--
-- ObjC selector: @+ elementWithName:stringValue:@
elementWithName_stringValue :: (IsNSString name, IsNSString string) => name -> string -> IO RawId
elementWithName_stringValue name string =
  do
    cls' <- getRequiredClass "NSXMLNode"
    withObjCPtr name $ \raw_name ->
      withObjCPtr string $ \raw_string ->
        fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "elementWithName:stringValue:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_string :: Ptr ())]

-- | elementWithName:children:attributes:
--
-- Returns an element children and attributes <name attr1="foo" attr2="bar"><-- child1 -->child2</name>.
--
-- ObjC selector: @+ elementWithName:children:attributes:@
elementWithName_children_attributes :: (IsNSString name, IsNSArray children, IsNSArray attributes) => name -> children -> attributes -> IO RawId
elementWithName_children_attributes name children attributes =
  do
    cls' <- getRequiredClass "NSXMLNode"
    withObjCPtr name $ \raw_name ->
      withObjCPtr children $ \raw_children ->
        withObjCPtr attributes $ \raw_attributes ->
          fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "elementWithName:children:attributes:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_children :: Ptr ()), argPtr (castPtr raw_attributes :: Ptr ())]

-- | attributeWithName:stringValue:
--
-- Returns an attribute name="stringValue".
--
-- ObjC selector: @+ attributeWithName:stringValue:@
attributeWithName_stringValue :: (IsNSString name, IsNSString stringValue) => name -> stringValue -> IO RawId
attributeWithName_stringValue name stringValue =
  do
    cls' <- getRequiredClass "NSXMLNode"
    withObjCPtr name $ \raw_name ->
      withObjCPtr stringValue $ \raw_stringValue ->
        fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "attributeWithName:stringValue:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_stringValue :: Ptr ())]

-- | attributeWithLocalName:URI:stringValue:
--
-- Returns an attribute whose full QName is specified.
--
-- ObjC selector: @+ attributeWithName:URI:stringValue:@
attributeWithName_URI_stringValue :: (IsNSString name, IsNSString uri, IsNSString stringValue) => name -> uri -> stringValue -> IO RawId
attributeWithName_URI_stringValue name uri stringValue =
  do
    cls' <- getRequiredClass "NSXMLNode"
    withObjCPtr name $ \raw_name ->
      withObjCPtr uri $ \raw_uri ->
        withObjCPtr stringValue $ \raw_stringValue ->
          fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "attributeWithName:URI:stringValue:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_uri :: Ptr ()), argPtr (castPtr raw_stringValue :: Ptr ())]

-- | namespaceWithName:stringValue:
--
-- Returns a namespace xmlns:name="stringValue".
--
-- ObjC selector: @+ namespaceWithName:stringValue:@
namespaceWithName_stringValue :: (IsNSString name, IsNSString stringValue) => name -> stringValue -> IO RawId
namespaceWithName_stringValue name stringValue =
  do
    cls' <- getRequiredClass "NSXMLNode"
    withObjCPtr name $ \raw_name ->
      withObjCPtr stringValue $ \raw_stringValue ->
        fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "namespaceWithName:stringValue:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_stringValue :: Ptr ())]

-- | processingInstructionWithName:stringValue:
--
-- Returns a processing instruction <?name stringValue>.
--
-- ObjC selector: @+ processingInstructionWithName:stringValue:@
processingInstructionWithName_stringValue :: (IsNSString name, IsNSString stringValue) => name -> stringValue -> IO RawId
processingInstructionWithName_stringValue name stringValue =
  do
    cls' <- getRequiredClass "NSXMLNode"
    withObjCPtr name $ \raw_name ->
      withObjCPtr stringValue $ \raw_stringValue ->
        fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "processingInstructionWithName:stringValue:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_stringValue :: Ptr ())]

-- | commentWithStringValue:
--
-- Returns a comment <--stringValue-->.
--
-- ObjC selector: @+ commentWithStringValue:@
commentWithStringValue :: IsNSString stringValue => stringValue -> IO RawId
commentWithStringValue stringValue =
  do
    cls' <- getRequiredClass "NSXMLNode"
    withObjCPtr stringValue $ \raw_stringValue ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "commentWithStringValue:") (retPtr retVoid) [argPtr (castPtr raw_stringValue :: Ptr ())]

-- | textWithStringValue:
--
-- Returns a text node.
--
-- ObjC selector: @+ textWithStringValue:@
textWithStringValue :: IsNSString stringValue => stringValue -> IO RawId
textWithStringValue stringValue =
  do
    cls' <- getRequiredClass "NSXMLNode"
    withObjCPtr stringValue $ \raw_stringValue ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "textWithStringValue:") (retPtr retVoid) [argPtr (castPtr raw_stringValue :: Ptr ())]

-- | DTDNodeWithXMLString:
--
-- Returns an element, attribute, entity, or notation DTD node based on the full XML string.
--
-- ObjC selector: @+ DTDNodeWithXMLString:@
dtdNodeWithXMLString :: IsNSString string => string -> IO RawId
dtdNodeWithXMLString string =
  do
    cls' <- getRequiredClass "NSXMLNode"
    withObjCPtr string $ \raw_string ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "DTDNodeWithXMLString:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())]

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
setStringValue_resolvingEntities nsxmlNode  string resolve =
withObjCPtr string $ \raw_string ->
    sendMsg nsxmlNode (mkSelector "setStringValue:resolvingEntities:") retVoid [argPtr (castPtr raw_string :: Ptr ()), argCULong (if resolve then 1 else 0)]

-- | childAtIndex:
--
-- Returns the child node at a particular index.
--
-- ObjC selector: @- childAtIndex:@
childAtIndex :: IsNSXMLNode nsxmlNode => nsxmlNode -> CULong -> IO (Id NSXMLNode)
childAtIndex nsxmlNode  index =
  sendMsg nsxmlNode (mkSelector "childAtIndex:") (retPtr retVoid) [argCULong (fromIntegral index)] >>= retainedObject . castPtr

-- | detach:
--
-- Detaches this node from its parent.
--
-- ObjC selector: @- detach@
detach :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO ()
detach nsxmlNode  =
  sendMsg nsxmlNode (mkSelector "detach") retVoid []

-- | localNameForName:
--
-- Returns the local name bar in foo:bar.
--
-- ObjC selector: @+ localNameForName:@
localNameForName :: IsNSString name => name -> IO (Id NSString)
localNameForName name =
  do
    cls' <- getRequiredClass "NSXMLNode"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "localNameForName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | localNameForName:
--
-- Returns the prefix foo in the name foo:bar.
--
-- ObjC selector: @+ prefixForName:@
prefixForName :: IsNSString name => name -> IO (Id NSString)
prefixForName name =
  do
    cls' <- getRequiredClass "NSXMLNode"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "prefixForName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | predefinedNamespaceForPrefix:
--
-- Returns the namespace belonging to one of the predefined namespaces xml, xs, or xsi
--
-- ObjC selector: @+ predefinedNamespaceForPrefix:@
predefinedNamespaceForPrefix :: IsNSString name => name -> IO (Id NSXMLNode)
predefinedNamespaceForPrefix name =
  do
    cls' <- getRequiredClass "NSXMLNode"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "predefinedNamespaceForPrefix:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | XMLStringWithOptions:
--
-- The representation of this node as it would appear in an XML document, with various output options available.
--
-- ObjC selector: @- XMLStringWithOptions:@
xmlStringWithOptions :: IsNSXMLNode nsxmlNode => nsxmlNode -> NSXMLNodeOptions -> IO (Id NSString)
xmlStringWithOptions nsxmlNode  options =
  sendMsg nsxmlNode (mkSelector "XMLStringWithOptions:") (retPtr retVoid) [argCULong (coerce options)] >>= retainedObject . castPtr

-- | canonicalXMLStringPreservingComments:
--
-- W3 canonical form (http://www.w3.org/TR/xml-c14n). The input option NSXMLNodePreserveWhitespace should be set for true canonical form.
--
-- ObjC selector: @- canonicalXMLStringPreservingComments:@
canonicalXMLStringPreservingComments :: IsNSXMLNode nsxmlNode => nsxmlNode -> Bool -> IO (Id NSString)
canonicalXMLStringPreservingComments nsxmlNode  comments =
  sendMsg nsxmlNode (mkSelector "canonicalXMLStringPreservingComments:") (retPtr retVoid) [argCULong (if comments then 1 else 0)] >>= retainedObject . castPtr

-- | nodesForXPath:error:
--
-- Returns the nodes resulting from applying an XPath to this node using the node as the context item ("."). normalizeAdjacentTextNodesPreservingCDATA:NO should be called if there are adjacent text nodes since they are not allowed under the XPath/XQuery Data Model.
--
-- An array whose elements are a kind of NSXMLNode.
--
-- ObjC selector: @- nodesForXPath:error:@
nodesForXPath_error :: (IsNSXMLNode nsxmlNode, IsNSString xpath, IsNSError error_) => nsxmlNode -> xpath -> error_ -> IO (Id NSArray)
nodesForXPath_error nsxmlNode  xpath error_ =
withObjCPtr xpath $ \raw_xpath ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg nsxmlNode (mkSelector "nodesForXPath:error:") (retPtr retVoid) [argPtr (castPtr raw_xpath :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | objectsForXQuery:constants:error:
--
-- Returns the objects resulting from applying an XQuery to this node using the node as the context item ("."). Constants are a name-value dictionary for constants declared "external" in the query. normalizeAdjacentTextNodesPreservingCDATA:NO should be called if there are adjacent text nodes since they are not allowed under the XPath/XQuery Data Model.
--
-- An array whose elements are kinds of NSArray, NSData, NSDate, NSNumber, NSString, NSURL, or NSXMLNode.
--
-- ObjC selector: @- objectsForXQuery:constants:error:@
objectsForXQuery_constants_error :: (IsNSXMLNode nsxmlNode, IsNSString xquery, IsNSDictionary constants, IsNSError error_) => nsxmlNode -> xquery -> constants -> error_ -> IO (Id NSArray)
objectsForXQuery_constants_error nsxmlNode  xquery constants error_ =
withObjCPtr xquery $ \raw_xquery ->
  withObjCPtr constants $ \raw_constants ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg nsxmlNode (mkSelector "objectsForXQuery:constants:error:") (retPtr retVoid) [argPtr (castPtr raw_xquery :: Ptr ()), argPtr (castPtr raw_constants :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- objectsForXQuery:error:@
objectsForXQuery_error :: (IsNSXMLNode nsxmlNode, IsNSString xquery, IsNSError error_) => nsxmlNode -> xquery -> error_ -> IO (Id NSArray)
objectsForXQuery_error nsxmlNode  xquery error_ =
withObjCPtr xquery $ \raw_xquery ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg nsxmlNode (mkSelector "objectsForXQuery:error:") (retPtr retVoid) [argPtr (castPtr raw_xquery :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Returns an element, attribute, entity, or notation DTD node based on the full XML string.
--
-- ObjC selector: @- kind@
kind :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO NSXMLNodeKind
kind nsxmlNode  =
  fmap (coerce :: CULong -> NSXMLNodeKind) $ sendMsg nsxmlNode (mkSelector "kind") retCULong []

-- | Sets the nodes name. Applicable for element, attribute, namespace, processing-instruction, document type declaration, element declaration, attribute declaration, entity declaration, and notation declaration.
--
-- ObjC selector: @- name@
name :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO (Id NSString)
name nsxmlNode  =
  sendMsg nsxmlNode (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Sets the nodes name. Applicable for element, attribute, namespace, processing-instruction, document type declaration, element declaration, attribute declaration, entity declaration, and notation declaration.
--
-- ObjC selector: @- setName:@
setName :: (IsNSXMLNode nsxmlNode, IsNSString value) => nsxmlNode -> value -> IO ()
setName nsxmlNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsxmlNode (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Sets the content of the node. Setting the objectValue removes all existing children including processing instructions and comments. Setting the object value on an element creates a single text node child.
--
-- ObjC selector: @- objectValue@
objectValue :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO RawId
objectValue nsxmlNode  =
  fmap (RawId . castPtr) $ sendMsg nsxmlNode (mkSelector "objectValue") (retPtr retVoid) []

-- | Sets the content of the node. Setting the objectValue removes all existing children including processing instructions and comments. Setting the object value on an element creates a single text node child.
--
-- ObjC selector: @- setObjectValue:@
setObjectValue :: IsNSXMLNode nsxmlNode => nsxmlNode -> RawId -> IO ()
setObjectValue nsxmlNode  value =
  sendMsg nsxmlNode (mkSelector "setObjectValue:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | Sets the content of the node. Setting the stringValue removes all existing children including processing instructions and comments. Setting the string value on an element creates a single text node child. The getter returns the string value of the node, which may be either its content or child text nodes, depending on the type of node. Elements are recursed and text nodes concatenated in document order with no intervening spaces.
--
-- ObjC selector: @- stringValue@
stringValue :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO (Id NSString)
stringValue nsxmlNode  =
  sendMsg nsxmlNode (mkSelector "stringValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Sets the content of the node. Setting the stringValue removes all existing children including processing instructions and comments. Setting the string value on an element creates a single text node child. The getter returns the string value of the node, which may be either its content or child text nodes, depending on the type of node. Elements are recursed and text nodes concatenated in document order with no intervening spaces.
--
-- ObjC selector: @- setStringValue:@
setStringValue :: (IsNSXMLNode nsxmlNode, IsNSString value) => nsxmlNode -> value -> IO ()
setStringValue nsxmlNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsxmlNode (mkSelector "setStringValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A node's index amongst its siblings.
--
-- ObjC selector: @- index@
index :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO CULong
index nsxmlNode  =
  sendMsg nsxmlNode (mkSelector "index") retCULong []

-- | The depth of the node within the tree. Documents and standalone nodes are level 0.
--
-- ObjC selector: @- level@
level :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO CULong
level nsxmlNode  =
  sendMsg nsxmlNode (mkSelector "level") retCULong []

-- | The encompassing document or nil.
--
-- ObjC selector: @- rootDocument@
rootDocument :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO (Id NSXMLDocument)
rootDocument nsxmlNode  =
  sendMsg nsxmlNode (mkSelector "rootDocument") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The parent of this node. Documents and standalone Nodes have a nil parent; there is not a 1-to-1 relationship between parent and children, eg a namespace cannot be a child but has a parent element.
--
-- ObjC selector: @- parent@
parent :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO (Id NSXMLNode)
parent nsxmlNode  =
  sendMsg nsxmlNode (mkSelector "parent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The amount of children, relevant for documents, elements, and document type declarations. Use this instead of [[self children] count].
--
-- ObjC selector: @- childCount@
childCount :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO CULong
childCount nsxmlNode  =
  sendMsg nsxmlNode (mkSelector "childCount") retCULong []

-- | An immutable array of child nodes. Relevant for documents, elements, and document type declarations.
--
-- ObjC selector: @- children@
children :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO (Id NSArray)
children nsxmlNode  =
  sendMsg nsxmlNode (mkSelector "children") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the previous sibling, or nil if there isn't one.
--
-- ObjC selector: @- previousSibling@
previousSibling :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO (Id NSXMLNode)
previousSibling nsxmlNode  =
  sendMsg nsxmlNode (mkSelector "previousSibling") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the next sibling, or nil if there isn't one.
--
-- ObjC selector: @- nextSibling@
nextSibling :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO (Id NSXMLNode)
nextSibling nsxmlNode  =
  sendMsg nsxmlNode (mkSelector "nextSibling") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the previous node in document order. This can be used to walk the tree backwards.
--
-- ObjC selector: @- previousNode@
previousNode :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO (Id NSXMLNode)
previousNode nsxmlNode  =
  sendMsg nsxmlNode (mkSelector "previousNode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the next node in document order. This can be used to walk the tree forwards.
--
-- ObjC selector: @- nextNode@
nextNode :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO (Id NSXMLNode)
nextNode nsxmlNode  =
  sendMsg nsxmlNode (mkSelector "nextNode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the XPath to this node, for example foo/bar[2]/baz.
--
-- ObjC selector: @- XPath@
xPath :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO (Id NSString)
xPath nsxmlNode  =
  sendMsg nsxmlNode (mkSelector "XPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the local name bar if this attribute or element's name is foo:bar
--
-- ObjC selector: @- localName@
localName :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO (Id NSString)
localName nsxmlNode  =
  sendMsg nsxmlNode (mkSelector "localName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the prefix foo if this attribute or element's name if foo:bar
--
-- ObjC selector: @- prefix@
prefix :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO (Id NSString)
prefix nsxmlNode  =
  sendMsg nsxmlNode (mkSelector "prefix") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Set the URI of this element, attribute, or document. For documents it is the URI of document origin. Getter returns the URI of this element, attribute, or document. For documents it is the URI of document origin and is automatically set when using initWithContentsOfURL.
--
-- ObjC selector: @- URI@
uri :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO (Id NSString)
uri nsxmlNode  =
  sendMsg nsxmlNode (mkSelector "URI") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Set the URI of this element, attribute, or document. For documents it is the URI of document origin. Getter returns the URI of this element, attribute, or document. For documents it is the URI of document origin and is automatically set when using initWithContentsOfURL.
--
-- ObjC selector: @- setURI:@
setURI :: (IsNSXMLNode nsxmlNode, IsNSString value) => nsxmlNode -> value -> IO ()
setURI nsxmlNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsxmlNode (mkSelector "setURI:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Used for debugging. May give more information than XMLString.
--
-- ObjC selector: @- description@
description :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO (Id NSString)
description nsxmlNode  =
  sendMsg nsxmlNode (mkSelector "description") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The representation of this node as it would appear in an XML document.
--
-- ObjC selector: @- XMLString@
xmlString :: IsNSXMLNode nsxmlNode => nsxmlNode -> IO (Id NSString)
xmlString nsxmlNode  =
  sendMsg nsxmlNode (mkSelector "XMLString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithKind:@
initWithKindSelector :: Selector
initWithKindSelector = mkSelector "initWithKind:"

-- | @Selector@ for @initWithKind:options:@
initWithKind_optionsSelector :: Selector
initWithKind_optionsSelector = mkSelector "initWithKind:options:"

-- | @Selector@ for @document@
documentSelector :: Selector
documentSelector = mkSelector "document"

-- | @Selector@ for @documentWithRootElement:@
documentWithRootElementSelector :: Selector
documentWithRootElementSelector = mkSelector "documentWithRootElement:"

-- | @Selector@ for @elementWithName:@
elementWithNameSelector :: Selector
elementWithNameSelector = mkSelector "elementWithName:"

-- | @Selector@ for @elementWithName:URI:@
elementWithName_URISelector :: Selector
elementWithName_URISelector = mkSelector "elementWithName:URI:"

-- | @Selector@ for @elementWithName:stringValue:@
elementWithName_stringValueSelector :: Selector
elementWithName_stringValueSelector = mkSelector "elementWithName:stringValue:"

-- | @Selector@ for @elementWithName:children:attributes:@
elementWithName_children_attributesSelector :: Selector
elementWithName_children_attributesSelector = mkSelector "elementWithName:children:attributes:"

-- | @Selector@ for @attributeWithName:stringValue:@
attributeWithName_stringValueSelector :: Selector
attributeWithName_stringValueSelector = mkSelector "attributeWithName:stringValue:"

-- | @Selector@ for @attributeWithName:URI:stringValue:@
attributeWithName_URI_stringValueSelector :: Selector
attributeWithName_URI_stringValueSelector = mkSelector "attributeWithName:URI:stringValue:"

-- | @Selector@ for @namespaceWithName:stringValue:@
namespaceWithName_stringValueSelector :: Selector
namespaceWithName_stringValueSelector = mkSelector "namespaceWithName:stringValue:"

-- | @Selector@ for @processingInstructionWithName:stringValue:@
processingInstructionWithName_stringValueSelector :: Selector
processingInstructionWithName_stringValueSelector = mkSelector "processingInstructionWithName:stringValue:"

-- | @Selector@ for @commentWithStringValue:@
commentWithStringValueSelector :: Selector
commentWithStringValueSelector = mkSelector "commentWithStringValue:"

-- | @Selector@ for @textWithStringValue:@
textWithStringValueSelector :: Selector
textWithStringValueSelector = mkSelector "textWithStringValue:"

-- | @Selector@ for @DTDNodeWithXMLString:@
dtdNodeWithXMLStringSelector :: Selector
dtdNodeWithXMLStringSelector = mkSelector "DTDNodeWithXMLString:"

-- | @Selector@ for @setStringValue:resolvingEntities:@
setStringValue_resolvingEntitiesSelector :: Selector
setStringValue_resolvingEntitiesSelector = mkSelector "setStringValue:resolvingEntities:"

-- | @Selector@ for @childAtIndex:@
childAtIndexSelector :: Selector
childAtIndexSelector = mkSelector "childAtIndex:"

-- | @Selector@ for @detach@
detachSelector :: Selector
detachSelector = mkSelector "detach"

-- | @Selector@ for @localNameForName:@
localNameForNameSelector :: Selector
localNameForNameSelector = mkSelector "localNameForName:"

-- | @Selector@ for @prefixForName:@
prefixForNameSelector :: Selector
prefixForNameSelector = mkSelector "prefixForName:"

-- | @Selector@ for @predefinedNamespaceForPrefix:@
predefinedNamespaceForPrefixSelector :: Selector
predefinedNamespaceForPrefixSelector = mkSelector "predefinedNamespaceForPrefix:"

-- | @Selector@ for @XMLStringWithOptions:@
xmlStringWithOptionsSelector :: Selector
xmlStringWithOptionsSelector = mkSelector "XMLStringWithOptions:"

-- | @Selector@ for @canonicalXMLStringPreservingComments:@
canonicalXMLStringPreservingCommentsSelector :: Selector
canonicalXMLStringPreservingCommentsSelector = mkSelector "canonicalXMLStringPreservingComments:"

-- | @Selector@ for @nodesForXPath:error:@
nodesForXPath_errorSelector :: Selector
nodesForXPath_errorSelector = mkSelector "nodesForXPath:error:"

-- | @Selector@ for @objectsForXQuery:constants:error:@
objectsForXQuery_constants_errorSelector :: Selector
objectsForXQuery_constants_errorSelector = mkSelector "objectsForXQuery:constants:error:"

-- | @Selector@ for @objectsForXQuery:error:@
objectsForXQuery_errorSelector :: Selector
objectsForXQuery_errorSelector = mkSelector "objectsForXQuery:error:"

-- | @Selector@ for @kind@
kindSelector :: Selector
kindSelector = mkSelector "kind"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @objectValue@
objectValueSelector :: Selector
objectValueSelector = mkSelector "objectValue"

-- | @Selector@ for @setObjectValue:@
setObjectValueSelector :: Selector
setObjectValueSelector = mkSelector "setObjectValue:"

-- | @Selector@ for @stringValue@
stringValueSelector :: Selector
stringValueSelector = mkSelector "stringValue"

-- | @Selector@ for @setStringValue:@
setStringValueSelector :: Selector
setStringValueSelector = mkSelector "setStringValue:"

-- | @Selector@ for @index@
indexSelector :: Selector
indexSelector = mkSelector "index"

-- | @Selector@ for @level@
levelSelector :: Selector
levelSelector = mkSelector "level"

-- | @Selector@ for @rootDocument@
rootDocumentSelector :: Selector
rootDocumentSelector = mkSelector "rootDocument"

-- | @Selector@ for @parent@
parentSelector :: Selector
parentSelector = mkSelector "parent"

-- | @Selector@ for @childCount@
childCountSelector :: Selector
childCountSelector = mkSelector "childCount"

-- | @Selector@ for @children@
childrenSelector :: Selector
childrenSelector = mkSelector "children"

-- | @Selector@ for @previousSibling@
previousSiblingSelector :: Selector
previousSiblingSelector = mkSelector "previousSibling"

-- | @Selector@ for @nextSibling@
nextSiblingSelector :: Selector
nextSiblingSelector = mkSelector "nextSibling"

-- | @Selector@ for @previousNode@
previousNodeSelector :: Selector
previousNodeSelector = mkSelector "previousNode"

-- | @Selector@ for @nextNode@
nextNodeSelector :: Selector
nextNodeSelector = mkSelector "nextNode"

-- | @Selector@ for @XPath@
xPathSelector :: Selector
xPathSelector = mkSelector "XPath"

-- | @Selector@ for @localName@
localNameSelector :: Selector
localNameSelector = mkSelector "localName"

-- | @Selector@ for @prefix@
prefixSelector :: Selector
prefixSelector = mkSelector "prefix"

-- | @Selector@ for @URI@
uriSelector :: Selector
uriSelector = mkSelector "URI"

-- | @Selector@ for @setURI:@
setURISelector :: Selector
setURISelector = mkSelector "setURI:"

-- | @Selector@ for @description@
descriptionSelector :: Selector
descriptionSelector = mkSelector "description"

-- | @Selector@ for @XMLString@
xmlStringSelector :: Selector
xmlStringSelector = mkSelector "XMLString"

