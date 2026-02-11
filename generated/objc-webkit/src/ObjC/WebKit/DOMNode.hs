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
  , insertBefore_refChildSelector
  , replaceChild_oldChildSelector
  , removeChildSelector
  , appendChildSelector
  , hasChildNodesSelector
  , cloneNodeSelector
  , normalizeSelector
  , isSupported_versionSelector
  , hasAttributesSelector
  , isSameNodeSelector
  , isEqualNodeSelector
  , lookupPrefixSelector
  , lookupNamespaceURISelector
  , isDefaultNamespaceSelector
  , compareDocumentPositionSelector
  , containsSelector
  , boundingBoxSelector
  , lineBoxRectsSelector
  , insertBeforeSelector
  , replaceChildSelector
  , isSupportedSelector
  , nodeNameSelector
  , nodeValueSelector
  , setNodeValueSelector
  , nodeTypeSelector
  , parentNodeSelector
  , childNodesSelector
  , firstChildSelector
  , lastChildSelector
  , previousSiblingSelector
  , nextSiblingSelector
  , ownerDocumentSelector
  , namespaceURISelector
  , prefixSelector
  , setPrefixSelector
  , localNameSelector
  , attributesSelector
  , baseURISelector
  , textContentSelector
  , setTextContentSelector
  , parentElementSelector
  , isContentEditableSelector
  , webArchiveSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- insertBefore:refChild:@
insertBefore_refChild :: (IsDOMNode domNode, IsDOMNode newChild, IsDOMNode refChild) => domNode -> newChild -> refChild -> IO (Id DOMNode)
insertBefore_refChild domNode  newChild refChild =
withObjCPtr newChild $ \raw_newChild ->
  withObjCPtr refChild $ \raw_refChild ->
      sendMsg domNode (mkSelector "insertBefore:refChild:") (retPtr retVoid) [argPtr (castPtr raw_newChild :: Ptr ()), argPtr (castPtr raw_refChild :: Ptr ())] >>= retainedObject . castPtr

-- | @- replaceChild:oldChild:@
replaceChild_oldChild :: (IsDOMNode domNode, IsDOMNode newChild, IsDOMNode oldChild) => domNode -> newChild -> oldChild -> IO (Id DOMNode)
replaceChild_oldChild domNode  newChild oldChild =
withObjCPtr newChild $ \raw_newChild ->
  withObjCPtr oldChild $ \raw_oldChild ->
      sendMsg domNode (mkSelector "replaceChild:oldChild:") (retPtr retVoid) [argPtr (castPtr raw_newChild :: Ptr ()), argPtr (castPtr raw_oldChild :: Ptr ())] >>= retainedObject . castPtr

-- | @- removeChild:@
removeChild :: (IsDOMNode domNode, IsDOMNode oldChild) => domNode -> oldChild -> IO (Id DOMNode)
removeChild domNode  oldChild =
withObjCPtr oldChild $ \raw_oldChild ->
    sendMsg domNode (mkSelector "removeChild:") (retPtr retVoid) [argPtr (castPtr raw_oldChild :: Ptr ())] >>= retainedObject . castPtr

-- | @- appendChild:@
appendChild :: (IsDOMNode domNode, IsDOMNode newChild) => domNode -> newChild -> IO (Id DOMNode)
appendChild domNode  newChild =
withObjCPtr newChild $ \raw_newChild ->
    sendMsg domNode (mkSelector "appendChild:") (retPtr retVoid) [argPtr (castPtr raw_newChild :: Ptr ())] >>= retainedObject . castPtr

-- | @- hasChildNodes@
hasChildNodes :: IsDOMNode domNode => domNode -> IO Bool
hasChildNodes domNode  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domNode (mkSelector "hasChildNodes") retCULong []

-- | @- cloneNode:@
cloneNode :: IsDOMNode domNode => domNode -> Bool -> IO (Id DOMNode)
cloneNode domNode  deep =
  sendMsg domNode (mkSelector "cloneNode:") (retPtr retVoid) [argCULong (if deep then 1 else 0)] >>= retainedObject . castPtr

-- | @- normalize@
normalize :: IsDOMNode domNode => domNode -> IO ()
normalize domNode  =
  sendMsg domNode (mkSelector "normalize") retVoid []

-- | @- isSupported:version:@
isSupported_version :: (IsDOMNode domNode, IsNSString feature, IsNSString version) => domNode -> feature -> version -> IO Bool
isSupported_version domNode  feature version =
withObjCPtr feature $ \raw_feature ->
  withObjCPtr version $ \raw_version ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg domNode (mkSelector "isSupported:version:") retCULong [argPtr (castPtr raw_feature :: Ptr ()), argPtr (castPtr raw_version :: Ptr ())]

-- | @- hasAttributes@
hasAttributes :: IsDOMNode domNode => domNode -> IO Bool
hasAttributes domNode  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domNode (mkSelector "hasAttributes") retCULong []

-- | @- isSameNode:@
isSameNode :: (IsDOMNode domNode, IsDOMNode other) => domNode -> other -> IO Bool
isSameNode domNode  other =
withObjCPtr other $ \raw_other ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg domNode (mkSelector "isSameNode:") retCULong [argPtr (castPtr raw_other :: Ptr ())]

-- | @- isEqualNode:@
isEqualNode :: (IsDOMNode domNode, IsDOMNode other) => domNode -> other -> IO Bool
isEqualNode domNode  other =
withObjCPtr other $ \raw_other ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg domNode (mkSelector "isEqualNode:") retCULong [argPtr (castPtr raw_other :: Ptr ())]

-- | @- lookupPrefix:@
lookupPrefix :: (IsDOMNode domNode, IsNSString namespaceURI) => domNode -> namespaceURI -> IO (Id NSString)
lookupPrefix domNode  namespaceURI =
withObjCPtr namespaceURI $ \raw_namespaceURI ->
    sendMsg domNode (mkSelector "lookupPrefix:") (retPtr retVoid) [argPtr (castPtr raw_namespaceURI :: Ptr ())] >>= retainedObject . castPtr

-- | @- lookupNamespaceURI:@
lookupNamespaceURI :: (IsDOMNode domNode, IsNSString prefix) => domNode -> prefix -> IO (Id NSString)
lookupNamespaceURI domNode  prefix =
withObjCPtr prefix $ \raw_prefix ->
    sendMsg domNode (mkSelector "lookupNamespaceURI:") (retPtr retVoid) [argPtr (castPtr raw_prefix :: Ptr ())] >>= retainedObject . castPtr

-- | @- isDefaultNamespace:@
isDefaultNamespace :: (IsDOMNode domNode, IsNSString namespaceURI) => domNode -> namespaceURI -> IO Bool
isDefaultNamespace domNode  namespaceURI =
withObjCPtr namespaceURI $ \raw_namespaceURI ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg domNode (mkSelector "isDefaultNamespace:") retCULong [argPtr (castPtr raw_namespaceURI :: Ptr ())]

-- | @- compareDocumentPosition:@
compareDocumentPosition :: (IsDOMNode domNode, IsDOMNode other) => domNode -> other -> IO CUShort
compareDocumentPosition domNode  other =
withObjCPtr other $ \raw_other ->
    fmap fromIntegral $ sendMsg domNode (mkSelector "compareDocumentPosition:") retCUInt [argPtr (castPtr raw_other :: Ptr ())]

-- | @- contains:@
contains :: (IsDOMNode domNode, IsDOMNode other) => domNode -> other -> IO Bool
contains domNode  other =
withObjCPtr other $ \raw_other ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg domNode (mkSelector "contains:") retCULong [argPtr (castPtr raw_other :: Ptr ())]

-- | @- boundingBox@
boundingBox :: IsDOMNode domNode => domNode -> IO NSRect
boundingBox domNode  =
  sendMsgStret domNode (mkSelector "boundingBox") retNSRect []

-- | @- lineBoxRects@
lineBoxRects :: IsDOMNode domNode => domNode -> IO (Id NSArray)
lineBoxRects domNode  =
  sendMsg domNode (mkSelector "lineBoxRects") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- insertBefore::@
insertBefore :: (IsDOMNode domNode, IsDOMNode newChild, IsDOMNode refChild) => domNode -> newChild -> refChild -> IO (Id DOMNode)
insertBefore domNode  newChild refChild =
withObjCPtr newChild $ \raw_newChild ->
  withObjCPtr refChild $ \raw_refChild ->
      sendMsg domNode (mkSelector "insertBefore::") (retPtr retVoid) [argPtr (castPtr raw_newChild :: Ptr ()), argPtr (castPtr raw_refChild :: Ptr ())] >>= retainedObject . castPtr

-- | @- replaceChild::@
replaceChild :: (IsDOMNode domNode, IsDOMNode newChild, IsDOMNode oldChild) => domNode -> newChild -> oldChild -> IO (Id DOMNode)
replaceChild domNode  newChild oldChild =
withObjCPtr newChild $ \raw_newChild ->
  withObjCPtr oldChild $ \raw_oldChild ->
      sendMsg domNode (mkSelector "replaceChild::") (retPtr retVoid) [argPtr (castPtr raw_newChild :: Ptr ()), argPtr (castPtr raw_oldChild :: Ptr ())] >>= retainedObject . castPtr

-- | @- isSupported::@
isSupported :: (IsDOMNode domNode, IsNSString feature, IsNSString version) => domNode -> feature -> version -> IO Bool
isSupported domNode  feature version =
withObjCPtr feature $ \raw_feature ->
  withObjCPtr version $ \raw_version ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg domNode (mkSelector "isSupported::") retCULong [argPtr (castPtr raw_feature :: Ptr ()), argPtr (castPtr raw_version :: Ptr ())]

-- | @- nodeName@
nodeName :: IsDOMNode domNode => domNode -> IO (Id NSString)
nodeName domNode  =
  sendMsg domNode (mkSelector "nodeName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- nodeValue@
nodeValue :: IsDOMNode domNode => domNode -> IO (Id NSString)
nodeValue domNode  =
  sendMsg domNode (mkSelector "nodeValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setNodeValue:@
setNodeValue :: (IsDOMNode domNode, IsNSString value) => domNode -> value -> IO ()
setNodeValue domNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg domNode (mkSelector "setNodeValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nodeType@
nodeType :: IsDOMNode domNode => domNode -> IO CUShort
nodeType domNode  =
  fmap fromIntegral $ sendMsg domNode (mkSelector "nodeType") retCUInt []

-- | @- parentNode@
parentNode :: IsDOMNode domNode => domNode -> IO (Id DOMNode)
parentNode domNode  =
  sendMsg domNode (mkSelector "parentNode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- childNodes@
childNodes :: IsDOMNode domNode => domNode -> IO (Id DOMNodeList)
childNodes domNode  =
  sendMsg domNode (mkSelector "childNodes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- firstChild@
firstChild :: IsDOMNode domNode => domNode -> IO (Id DOMNode)
firstChild domNode  =
  sendMsg domNode (mkSelector "firstChild") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- lastChild@
lastChild :: IsDOMNode domNode => domNode -> IO (Id DOMNode)
lastChild domNode  =
  sendMsg domNode (mkSelector "lastChild") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- previousSibling@
previousSibling :: IsDOMNode domNode => domNode -> IO (Id DOMNode)
previousSibling domNode  =
  sendMsg domNode (mkSelector "previousSibling") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- nextSibling@
nextSibling :: IsDOMNode domNode => domNode -> IO (Id DOMNode)
nextSibling domNode  =
  sendMsg domNode (mkSelector "nextSibling") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- ownerDocument@
ownerDocument :: IsDOMNode domNode => domNode -> IO (Id DOMDocument)
ownerDocument domNode  =
  sendMsg domNode (mkSelector "ownerDocument") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- namespaceURI@
namespaceURI :: IsDOMNode domNode => domNode -> IO (Id NSString)
namespaceURI domNode  =
  sendMsg domNode (mkSelector "namespaceURI") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- prefix@
prefix :: IsDOMNode domNode => domNode -> IO (Id NSString)
prefix domNode  =
  sendMsg domNode (mkSelector "prefix") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPrefix:@
setPrefix :: (IsDOMNode domNode, IsNSString value) => domNode -> value -> IO ()
setPrefix domNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg domNode (mkSelector "setPrefix:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- localName@
localName :: IsDOMNode domNode => domNode -> IO (Id NSString)
localName domNode  =
  sendMsg domNode (mkSelector "localName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- attributes@
attributes :: IsDOMNode domNode => domNode -> IO (Id DOMNamedNodeMap)
attributes domNode  =
  sendMsg domNode (mkSelector "attributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- baseURI@
baseURI :: IsDOMNode domNode => domNode -> IO (Id NSString)
baseURI domNode  =
  sendMsg domNode (mkSelector "baseURI") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- textContent@
textContent :: IsDOMNode domNode => domNode -> IO (Id NSString)
textContent domNode  =
  sendMsg domNode (mkSelector "textContent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTextContent:@
setTextContent :: (IsDOMNode domNode, IsNSString value) => domNode -> value -> IO ()
setTextContent domNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg domNode (mkSelector "setTextContent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- parentElement@
parentElement :: IsDOMNode domNode => domNode -> IO (Id DOMElement)
parentElement domNode  =
  sendMsg domNode (mkSelector "parentElement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- isContentEditable@
isContentEditable :: IsDOMNode domNode => domNode -> IO Bool
isContentEditable domNode  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domNode (mkSelector "isContentEditable") retCULong []

-- | webArchive
--
-- A WebArchive representing the node and the children of the node.
--
-- ObjC selector: @- webArchive@
webArchive :: IsDOMNode domNode => domNode -> IO (Id WebArchive)
webArchive domNode  =
  sendMsg domNode (mkSelector "webArchive") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @insertBefore:refChild:@
insertBefore_refChildSelector :: Selector
insertBefore_refChildSelector = mkSelector "insertBefore:refChild:"

-- | @Selector@ for @replaceChild:oldChild:@
replaceChild_oldChildSelector :: Selector
replaceChild_oldChildSelector = mkSelector "replaceChild:oldChild:"

-- | @Selector@ for @removeChild:@
removeChildSelector :: Selector
removeChildSelector = mkSelector "removeChild:"

-- | @Selector@ for @appendChild:@
appendChildSelector :: Selector
appendChildSelector = mkSelector "appendChild:"

-- | @Selector@ for @hasChildNodes@
hasChildNodesSelector :: Selector
hasChildNodesSelector = mkSelector "hasChildNodes"

-- | @Selector@ for @cloneNode:@
cloneNodeSelector :: Selector
cloneNodeSelector = mkSelector "cloneNode:"

-- | @Selector@ for @normalize@
normalizeSelector :: Selector
normalizeSelector = mkSelector "normalize"

-- | @Selector@ for @isSupported:version:@
isSupported_versionSelector :: Selector
isSupported_versionSelector = mkSelector "isSupported:version:"

-- | @Selector@ for @hasAttributes@
hasAttributesSelector :: Selector
hasAttributesSelector = mkSelector "hasAttributes"

-- | @Selector@ for @isSameNode:@
isSameNodeSelector :: Selector
isSameNodeSelector = mkSelector "isSameNode:"

-- | @Selector@ for @isEqualNode:@
isEqualNodeSelector :: Selector
isEqualNodeSelector = mkSelector "isEqualNode:"

-- | @Selector@ for @lookupPrefix:@
lookupPrefixSelector :: Selector
lookupPrefixSelector = mkSelector "lookupPrefix:"

-- | @Selector@ for @lookupNamespaceURI:@
lookupNamespaceURISelector :: Selector
lookupNamespaceURISelector = mkSelector "lookupNamespaceURI:"

-- | @Selector@ for @isDefaultNamespace:@
isDefaultNamespaceSelector :: Selector
isDefaultNamespaceSelector = mkSelector "isDefaultNamespace:"

-- | @Selector@ for @compareDocumentPosition:@
compareDocumentPositionSelector :: Selector
compareDocumentPositionSelector = mkSelector "compareDocumentPosition:"

-- | @Selector@ for @contains:@
containsSelector :: Selector
containsSelector = mkSelector "contains:"

-- | @Selector@ for @boundingBox@
boundingBoxSelector :: Selector
boundingBoxSelector = mkSelector "boundingBox"

-- | @Selector@ for @lineBoxRects@
lineBoxRectsSelector :: Selector
lineBoxRectsSelector = mkSelector "lineBoxRects"

-- | @Selector@ for @insertBefore::@
insertBeforeSelector :: Selector
insertBeforeSelector = mkSelector "insertBefore::"

-- | @Selector@ for @replaceChild::@
replaceChildSelector :: Selector
replaceChildSelector = mkSelector "replaceChild::"

-- | @Selector@ for @isSupported::@
isSupportedSelector :: Selector
isSupportedSelector = mkSelector "isSupported::"

-- | @Selector@ for @nodeName@
nodeNameSelector :: Selector
nodeNameSelector = mkSelector "nodeName"

-- | @Selector@ for @nodeValue@
nodeValueSelector :: Selector
nodeValueSelector = mkSelector "nodeValue"

-- | @Selector@ for @setNodeValue:@
setNodeValueSelector :: Selector
setNodeValueSelector = mkSelector "setNodeValue:"

-- | @Selector@ for @nodeType@
nodeTypeSelector :: Selector
nodeTypeSelector = mkSelector "nodeType"

-- | @Selector@ for @parentNode@
parentNodeSelector :: Selector
parentNodeSelector = mkSelector "parentNode"

-- | @Selector@ for @childNodes@
childNodesSelector :: Selector
childNodesSelector = mkSelector "childNodes"

-- | @Selector@ for @firstChild@
firstChildSelector :: Selector
firstChildSelector = mkSelector "firstChild"

-- | @Selector@ for @lastChild@
lastChildSelector :: Selector
lastChildSelector = mkSelector "lastChild"

-- | @Selector@ for @previousSibling@
previousSiblingSelector :: Selector
previousSiblingSelector = mkSelector "previousSibling"

-- | @Selector@ for @nextSibling@
nextSiblingSelector :: Selector
nextSiblingSelector = mkSelector "nextSibling"

-- | @Selector@ for @ownerDocument@
ownerDocumentSelector :: Selector
ownerDocumentSelector = mkSelector "ownerDocument"

-- | @Selector@ for @namespaceURI@
namespaceURISelector :: Selector
namespaceURISelector = mkSelector "namespaceURI"

-- | @Selector@ for @prefix@
prefixSelector :: Selector
prefixSelector = mkSelector "prefix"

-- | @Selector@ for @setPrefix:@
setPrefixSelector :: Selector
setPrefixSelector = mkSelector "setPrefix:"

-- | @Selector@ for @localName@
localNameSelector :: Selector
localNameSelector = mkSelector "localName"

-- | @Selector@ for @attributes@
attributesSelector :: Selector
attributesSelector = mkSelector "attributes"

-- | @Selector@ for @baseURI@
baseURISelector :: Selector
baseURISelector = mkSelector "baseURI"

-- | @Selector@ for @textContent@
textContentSelector :: Selector
textContentSelector = mkSelector "textContent"

-- | @Selector@ for @setTextContent:@
setTextContentSelector :: Selector
setTextContentSelector = mkSelector "setTextContent:"

-- | @Selector@ for @parentElement@
parentElementSelector :: Selector
parentElementSelector = mkSelector "parentElement"

-- | @Selector@ for @isContentEditable@
isContentEditableSelector :: Selector
isContentEditableSelector = mkSelector "isContentEditable"

-- | @Selector@ for @webArchive@
webArchiveSelector :: Selector
webArchiveSelector = mkSelector "webArchive"

