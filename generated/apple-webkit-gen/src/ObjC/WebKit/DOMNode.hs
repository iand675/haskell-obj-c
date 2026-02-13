{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMNode@.
module ObjC.WebKit.DOMNode
  ( DOMNode
  , IsDOMNode(..)
  , insertBefore_refChild
  , replaceChild_oldChild
  , removeChild
  , appendChild
  , hasChildNodes
  , cloneNode
  , normalize
  , isSupported_version
  , hasAttributes
  , isSameNode
  , isEqualNode
  , lookupPrefix
  , lookupNamespaceURI
  , isDefaultNamespace
  , compareDocumentPosition
  , contains
  , boundingBox
  , lineBoxRects
  , insertBefore
  , replaceChild
  , isSupported
  , nodeName
  , nodeValue
  , setNodeValue
  , nodeType
  , parentNode
  , childNodes
  , firstChild
  , lastChild
  , previousSibling
  , nextSibling
  , ownerDocument
  , namespaceURI
  , prefix
  , setPrefix
  , localName
  , attributes
  , baseURI
  , textContent
  , setTextContent
  , parentElement
  , isContentEditable
  , webArchive
  , appendChildSelector
  , attributesSelector
  , baseURISelector
  , boundingBoxSelector
  , childNodesSelector
  , cloneNodeSelector
  , compareDocumentPositionSelector
  , containsSelector
  , firstChildSelector
  , hasAttributesSelector
  , hasChildNodesSelector
  , insertBeforeSelector
  , insertBefore_refChildSelector
  , isContentEditableSelector
  , isDefaultNamespaceSelector
  , isEqualNodeSelector
  , isSameNodeSelector
  , isSupportedSelector
  , isSupported_versionSelector
  , lastChildSelector
  , lineBoxRectsSelector
  , localNameSelector
  , lookupNamespaceURISelector
  , lookupPrefixSelector
  , namespaceURISelector
  , nextSiblingSelector
  , nodeNameSelector
  , nodeTypeSelector
  , nodeValueSelector
  , normalizeSelector
  , ownerDocumentSelector
  , parentElementSelector
  , parentNodeSelector
  , prefixSelector
  , previousSiblingSelector
  , removeChildSelector
  , replaceChildSelector
  , replaceChild_oldChildSelector
  , setNodeValueSelector
  , setPrefixSelector
  , setTextContentSelector
  , textContentSelector
  , webArchiveSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- insertBefore:refChild:@
insertBefore_refChild :: (IsDOMNode domNode, IsDOMNode newChild, IsDOMNode refChild) => domNode -> newChild -> refChild -> IO (Id DOMNode)
insertBefore_refChild domNode newChild refChild =
  sendMessage domNode insertBefore_refChildSelector (toDOMNode newChild) (toDOMNode refChild)

-- | @- replaceChild:oldChild:@
replaceChild_oldChild :: (IsDOMNode domNode, IsDOMNode newChild, IsDOMNode oldChild) => domNode -> newChild -> oldChild -> IO (Id DOMNode)
replaceChild_oldChild domNode newChild oldChild =
  sendMessage domNode replaceChild_oldChildSelector (toDOMNode newChild) (toDOMNode oldChild)

-- | @- removeChild:@
removeChild :: (IsDOMNode domNode, IsDOMNode oldChild) => domNode -> oldChild -> IO (Id DOMNode)
removeChild domNode oldChild =
  sendMessage domNode removeChildSelector (toDOMNode oldChild)

-- | @- appendChild:@
appendChild :: (IsDOMNode domNode, IsDOMNode newChild) => domNode -> newChild -> IO (Id DOMNode)
appendChild domNode newChild =
  sendMessage domNode appendChildSelector (toDOMNode newChild)

-- | @- hasChildNodes@
hasChildNodes :: IsDOMNode domNode => domNode -> IO Bool
hasChildNodes domNode =
  sendMessage domNode hasChildNodesSelector

-- | @- cloneNode:@
cloneNode :: IsDOMNode domNode => domNode -> Bool -> IO (Id DOMNode)
cloneNode domNode deep =
  sendMessage domNode cloneNodeSelector deep

-- | @- normalize@
normalize :: IsDOMNode domNode => domNode -> IO ()
normalize domNode =
  sendMessage domNode normalizeSelector

-- | @- isSupported:version:@
isSupported_version :: (IsDOMNode domNode, IsNSString feature, IsNSString version) => domNode -> feature -> version -> IO Bool
isSupported_version domNode feature version =
  sendMessage domNode isSupported_versionSelector (toNSString feature) (toNSString version)

-- | @- hasAttributes@
hasAttributes :: IsDOMNode domNode => domNode -> IO Bool
hasAttributes domNode =
  sendMessage domNode hasAttributesSelector

-- | @- isSameNode:@
isSameNode :: (IsDOMNode domNode, IsDOMNode other) => domNode -> other -> IO Bool
isSameNode domNode other =
  sendMessage domNode isSameNodeSelector (toDOMNode other)

-- | @- isEqualNode:@
isEqualNode :: (IsDOMNode domNode, IsDOMNode other) => domNode -> other -> IO Bool
isEqualNode domNode other =
  sendMessage domNode isEqualNodeSelector (toDOMNode other)

-- | @- lookupPrefix:@
lookupPrefix :: (IsDOMNode domNode, IsNSString namespaceURI) => domNode -> namespaceURI -> IO (Id NSString)
lookupPrefix domNode namespaceURI =
  sendMessage domNode lookupPrefixSelector (toNSString namespaceURI)

-- | @- lookupNamespaceURI:@
lookupNamespaceURI :: (IsDOMNode domNode, IsNSString prefix) => domNode -> prefix -> IO (Id NSString)
lookupNamespaceURI domNode prefix =
  sendMessage domNode lookupNamespaceURISelector (toNSString prefix)

-- | @- isDefaultNamespace:@
isDefaultNamespace :: (IsDOMNode domNode, IsNSString namespaceURI) => domNode -> namespaceURI -> IO Bool
isDefaultNamespace domNode namespaceURI =
  sendMessage domNode isDefaultNamespaceSelector (toNSString namespaceURI)

-- | @- compareDocumentPosition:@
compareDocumentPosition :: (IsDOMNode domNode, IsDOMNode other) => domNode -> other -> IO CUShort
compareDocumentPosition domNode other =
  sendMessage domNode compareDocumentPositionSelector (toDOMNode other)

-- | @- contains:@
contains :: (IsDOMNode domNode, IsDOMNode other) => domNode -> other -> IO Bool
contains domNode other =
  sendMessage domNode containsSelector (toDOMNode other)

-- | @- boundingBox@
boundingBox :: IsDOMNode domNode => domNode -> IO NSRect
boundingBox domNode =
  sendMessage domNode boundingBoxSelector

-- | @- lineBoxRects@
lineBoxRects :: IsDOMNode domNode => domNode -> IO (Id NSArray)
lineBoxRects domNode =
  sendMessage domNode lineBoxRectsSelector

-- | @- insertBefore::@
insertBefore :: (IsDOMNode domNode, IsDOMNode newChild, IsDOMNode refChild) => domNode -> newChild -> refChild -> IO (Id DOMNode)
insertBefore domNode newChild refChild =
  sendMessage domNode insertBeforeSelector (toDOMNode newChild) (toDOMNode refChild)

-- | @- replaceChild::@
replaceChild :: (IsDOMNode domNode, IsDOMNode newChild, IsDOMNode oldChild) => domNode -> newChild -> oldChild -> IO (Id DOMNode)
replaceChild domNode newChild oldChild =
  sendMessage domNode replaceChildSelector (toDOMNode newChild) (toDOMNode oldChild)

-- | @- isSupported::@
isSupported :: (IsDOMNode domNode, IsNSString feature, IsNSString version) => domNode -> feature -> version -> IO Bool
isSupported domNode feature version =
  sendMessage domNode isSupportedSelector (toNSString feature) (toNSString version)

-- | @- nodeName@
nodeName :: IsDOMNode domNode => domNode -> IO (Id NSString)
nodeName domNode =
  sendMessage domNode nodeNameSelector

-- | @- nodeValue@
nodeValue :: IsDOMNode domNode => domNode -> IO (Id NSString)
nodeValue domNode =
  sendMessage domNode nodeValueSelector

-- | @- setNodeValue:@
setNodeValue :: (IsDOMNode domNode, IsNSString value) => domNode -> value -> IO ()
setNodeValue domNode value =
  sendMessage domNode setNodeValueSelector (toNSString value)

-- | @- nodeType@
nodeType :: IsDOMNode domNode => domNode -> IO CUShort
nodeType domNode =
  sendMessage domNode nodeTypeSelector

-- | @- parentNode@
parentNode :: IsDOMNode domNode => domNode -> IO (Id DOMNode)
parentNode domNode =
  sendMessage domNode parentNodeSelector

-- | @- childNodes@
childNodes :: IsDOMNode domNode => domNode -> IO (Id DOMNodeList)
childNodes domNode =
  sendMessage domNode childNodesSelector

-- | @- firstChild@
firstChild :: IsDOMNode domNode => domNode -> IO (Id DOMNode)
firstChild domNode =
  sendMessage domNode firstChildSelector

-- | @- lastChild@
lastChild :: IsDOMNode domNode => domNode -> IO (Id DOMNode)
lastChild domNode =
  sendMessage domNode lastChildSelector

-- | @- previousSibling@
previousSibling :: IsDOMNode domNode => domNode -> IO (Id DOMNode)
previousSibling domNode =
  sendMessage domNode previousSiblingSelector

-- | @- nextSibling@
nextSibling :: IsDOMNode domNode => domNode -> IO (Id DOMNode)
nextSibling domNode =
  sendMessage domNode nextSiblingSelector

-- | @- ownerDocument@
ownerDocument :: IsDOMNode domNode => domNode -> IO (Id DOMDocument)
ownerDocument domNode =
  sendMessage domNode ownerDocumentSelector

-- | @- namespaceURI@
namespaceURI :: IsDOMNode domNode => domNode -> IO (Id NSString)
namespaceURI domNode =
  sendMessage domNode namespaceURISelector

-- | @- prefix@
prefix :: IsDOMNode domNode => domNode -> IO (Id NSString)
prefix domNode =
  sendMessage domNode prefixSelector

-- | @- setPrefix:@
setPrefix :: (IsDOMNode domNode, IsNSString value) => domNode -> value -> IO ()
setPrefix domNode value =
  sendMessage domNode setPrefixSelector (toNSString value)

-- | @- localName@
localName :: IsDOMNode domNode => domNode -> IO (Id NSString)
localName domNode =
  sendMessage domNode localNameSelector

-- | @- attributes@
attributes :: IsDOMNode domNode => domNode -> IO (Id DOMNamedNodeMap)
attributes domNode =
  sendMessage domNode attributesSelector

-- | @- baseURI@
baseURI :: IsDOMNode domNode => domNode -> IO (Id NSString)
baseURI domNode =
  sendMessage domNode baseURISelector

-- | @- textContent@
textContent :: IsDOMNode domNode => domNode -> IO (Id NSString)
textContent domNode =
  sendMessage domNode textContentSelector

-- | @- setTextContent:@
setTextContent :: (IsDOMNode domNode, IsNSString value) => domNode -> value -> IO ()
setTextContent domNode value =
  sendMessage domNode setTextContentSelector (toNSString value)

-- | @- parentElement@
parentElement :: IsDOMNode domNode => domNode -> IO (Id DOMElement)
parentElement domNode =
  sendMessage domNode parentElementSelector

-- | @- isContentEditable@
isContentEditable :: IsDOMNode domNode => domNode -> IO Bool
isContentEditable domNode =
  sendMessage domNode isContentEditableSelector

-- | webArchive
--
-- A WebArchive representing the node and the children of the node.
--
-- ObjC selector: @- webArchive@
webArchive :: IsDOMNode domNode => domNode -> IO (Id WebArchive)
webArchive domNode =
  sendMessage domNode webArchiveSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @insertBefore:refChild:@
insertBefore_refChildSelector :: Selector '[Id DOMNode, Id DOMNode] (Id DOMNode)
insertBefore_refChildSelector = mkSelector "insertBefore:refChild:"

-- | @Selector@ for @replaceChild:oldChild:@
replaceChild_oldChildSelector :: Selector '[Id DOMNode, Id DOMNode] (Id DOMNode)
replaceChild_oldChildSelector = mkSelector "replaceChild:oldChild:"

-- | @Selector@ for @removeChild:@
removeChildSelector :: Selector '[Id DOMNode] (Id DOMNode)
removeChildSelector = mkSelector "removeChild:"

-- | @Selector@ for @appendChild:@
appendChildSelector :: Selector '[Id DOMNode] (Id DOMNode)
appendChildSelector = mkSelector "appendChild:"

-- | @Selector@ for @hasChildNodes@
hasChildNodesSelector :: Selector '[] Bool
hasChildNodesSelector = mkSelector "hasChildNodes"

-- | @Selector@ for @cloneNode:@
cloneNodeSelector :: Selector '[Bool] (Id DOMNode)
cloneNodeSelector = mkSelector "cloneNode:"

-- | @Selector@ for @normalize@
normalizeSelector :: Selector '[] ()
normalizeSelector = mkSelector "normalize"

-- | @Selector@ for @isSupported:version:@
isSupported_versionSelector :: Selector '[Id NSString, Id NSString] Bool
isSupported_versionSelector = mkSelector "isSupported:version:"

-- | @Selector@ for @hasAttributes@
hasAttributesSelector :: Selector '[] Bool
hasAttributesSelector = mkSelector "hasAttributes"

-- | @Selector@ for @isSameNode:@
isSameNodeSelector :: Selector '[Id DOMNode] Bool
isSameNodeSelector = mkSelector "isSameNode:"

-- | @Selector@ for @isEqualNode:@
isEqualNodeSelector :: Selector '[Id DOMNode] Bool
isEqualNodeSelector = mkSelector "isEqualNode:"

-- | @Selector@ for @lookupPrefix:@
lookupPrefixSelector :: Selector '[Id NSString] (Id NSString)
lookupPrefixSelector = mkSelector "lookupPrefix:"

-- | @Selector@ for @lookupNamespaceURI:@
lookupNamespaceURISelector :: Selector '[Id NSString] (Id NSString)
lookupNamespaceURISelector = mkSelector "lookupNamespaceURI:"

-- | @Selector@ for @isDefaultNamespace:@
isDefaultNamespaceSelector :: Selector '[Id NSString] Bool
isDefaultNamespaceSelector = mkSelector "isDefaultNamespace:"

-- | @Selector@ for @compareDocumentPosition:@
compareDocumentPositionSelector :: Selector '[Id DOMNode] CUShort
compareDocumentPositionSelector = mkSelector "compareDocumentPosition:"

-- | @Selector@ for @contains:@
containsSelector :: Selector '[Id DOMNode] Bool
containsSelector = mkSelector "contains:"

-- | @Selector@ for @boundingBox@
boundingBoxSelector :: Selector '[] NSRect
boundingBoxSelector = mkSelector "boundingBox"

-- | @Selector@ for @lineBoxRects@
lineBoxRectsSelector :: Selector '[] (Id NSArray)
lineBoxRectsSelector = mkSelector "lineBoxRects"

-- | @Selector@ for @insertBefore::@
insertBeforeSelector :: Selector '[Id DOMNode, Id DOMNode] (Id DOMNode)
insertBeforeSelector = mkSelector "insertBefore::"

-- | @Selector@ for @replaceChild::@
replaceChildSelector :: Selector '[Id DOMNode, Id DOMNode] (Id DOMNode)
replaceChildSelector = mkSelector "replaceChild::"

-- | @Selector@ for @isSupported::@
isSupportedSelector :: Selector '[Id NSString, Id NSString] Bool
isSupportedSelector = mkSelector "isSupported::"

-- | @Selector@ for @nodeName@
nodeNameSelector :: Selector '[] (Id NSString)
nodeNameSelector = mkSelector "nodeName"

-- | @Selector@ for @nodeValue@
nodeValueSelector :: Selector '[] (Id NSString)
nodeValueSelector = mkSelector "nodeValue"

-- | @Selector@ for @setNodeValue:@
setNodeValueSelector :: Selector '[Id NSString] ()
setNodeValueSelector = mkSelector "setNodeValue:"

-- | @Selector@ for @nodeType@
nodeTypeSelector :: Selector '[] CUShort
nodeTypeSelector = mkSelector "nodeType"

-- | @Selector@ for @parentNode@
parentNodeSelector :: Selector '[] (Id DOMNode)
parentNodeSelector = mkSelector "parentNode"

-- | @Selector@ for @childNodes@
childNodesSelector :: Selector '[] (Id DOMNodeList)
childNodesSelector = mkSelector "childNodes"

-- | @Selector@ for @firstChild@
firstChildSelector :: Selector '[] (Id DOMNode)
firstChildSelector = mkSelector "firstChild"

-- | @Selector@ for @lastChild@
lastChildSelector :: Selector '[] (Id DOMNode)
lastChildSelector = mkSelector "lastChild"

-- | @Selector@ for @previousSibling@
previousSiblingSelector :: Selector '[] (Id DOMNode)
previousSiblingSelector = mkSelector "previousSibling"

-- | @Selector@ for @nextSibling@
nextSiblingSelector :: Selector '[] (Id DOMNode)
nextSiblingSelector = mkSelector "nextSibling"

-- | @Selector@ for @ownerDocument@
ownerDocumentSelector :: Selector '[] (Id DOMDocument)
ownerDocumentSelector = mkSelector "ownerDocument"

-- | @Selector@ for @namespaceURI@
namespaceURISelector :: Selector '[] (Id NSString)
namespaceURISelector = mkSelector "namespaceURI"

-- | @Selector@ for @prefix@
prefixSelector :: Selector '[] (Id NSString)
prefixSelector = mkSelector "prefix"

-- | @Selector@ for @setPrefix:@
setPrefixSelector :: Selector '[Id NSString] ()
setPrefixSelector = mkSelector "setPrefix:"

-- | @Selector@ for @localName@
localNameSelector :: Selector '[] (Id NSString)
localNameSelector = mkSelector "localName"

-- | @Selector@ for @attributes@
attributesSelector :: Selector '[] (Id DOMNamedNodeMap)
attributesSelector = mkSelector "attributes"

-- | @Selector@ for @baseURI@
baseURISelector :: Selector '[] (Id NSString)
baseURISelector = mkSelector "baseURI"

-- | @Selector@ for @textContent@
textContentSelector :: Selector '[] (Id NSString)
textContentSelector = mkSelector "textContent"

-- | @Selector@ for @setTextContent:@
setTextContentSelector :: Selector '[Id NSString] ()
setTextContentSelector = mkSelector "setTextContent:"

-- | @Selector@ for @parentElement@
parentElementSelector :: Selector '[] (Id DOMElement)
parentElementSelector = mkSelector "parentElement"

-- | @Selector@ for @isContentEditable@
isContentEditableSelector :: Selector '[] Bool
isContentEditableSelector = mkSelector "isContentEditable"

-- | @Selector@ for @webArchive@
webArchiveSelector :: Selector '[] (Id WebArchive)
webArchiveSelector = mkSelector "webArchive"

