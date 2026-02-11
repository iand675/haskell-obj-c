{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMDocument@.
module ObjC.WebKit.DOMDocument
  ( DOMDocument
  , IsDOMDocument(..)
  , createElement
  , createDocumentFragment
  , createTextNode
  , createComment
  , createCDATASection
  , createProcessingInstruction_data
  , createAttribute
  , createEntityReference
  , getElementsByTagName
  , importNode_deep
  , createElementNS_qualifiedName
  , createAttributeNS_qualifiedName
  , getElementsByTagNameNS_localName
  , adoptNode
  , createEvent
  , createRange
  , createNodeIterator_whatToShow_filter_expandEntityReferences
  , createTreeWalker_whatToShow_filter_expandEntityReferences
  , getOverrideStyle_pseudoElement
  , createExpression_resolver
  , createNSResolver
  , evaluate_contextNode_resolver_type_inResult
  , execCommand_userInterface_value
  , execCommand_userInterface
  , execCommand
  , queryCommandEnabled
  , queryCommandIndeterm
  , queryCommandState
  , queryCommandSupported
  , queryCommandValue
  , getElementsByName
  , elementFromPoint_y
  , createCSSStyleDeclaration
  , getComputedStyle_pseudoElement
  , getMatchedCSSRules_pseudoElement
  , getMatchedCSSRules_pseudoElement_authorOnly
  , getElementsByClassName
  , hasFocus
  , webkitCancelFullScreen
  , getElementById
  , querySelector
  , querySelectorAll
  , urlWithAttributeString
  , createProcessingInstruction
  , importNode
  , createElementNS
  , createAttributeNS
  , getElementsByTagNameNS
  , createNodeIterator
  , createTreeWalker
  , getOverrideStyle
  , createExpression
  , evaluate
  , getComputedStyle
  , doctype
  , implementation
  , documentElement
  , inputEncoding
  , xmlEncoding
  , xmlVersion
  , setXmlVersion
  , xmlStandalone
  , setXmlStandalone
  , documentURI
  , setDocumentURI
  , defaultView
  , styleSheets
  , title
  , setTitle
  , referrer
  , domain
  , url
  , cookie
  , setCookie
  , body
  , setBody
  , images
  , applets
  , links
  , forms
  , anchors
  , lastModified
  , charset
  , setCharset
  , defaultCharset
  , readyState
  , characterSet
  , preferredStylesheetSet
  , selectedStylesheetSet
  , setSelectedStylesheetSet
  , activeElement
  , webFrame
  , createElementSelector
  , createDocumentFragmentSelector
  , createTextNodeSelector
  , createCommentSelector
  , createCDATASectionSelector
  , createProcessingInstruction_dataSelector
  , createAttributeSelector
  , createEntityReferenceSelector
  , getElementsByTagNameSelector
  , importNode_deepSelector
  , createElementNS_qualifiedNameSelector
  , createAttributeNS_qualifiedNameSelector
  , getElementsByTagNameNS_localNameSelector
  , adoptNodeSelector
  , createEventSelector
  , createRangeSelector
  , createNodeIterator_whatToShow_filter_expandEntityReferencesSelector
  , createTreeWalker_whatToShow_filter_expandEntityReferencesSelector
  , getOverrideStyle_pseudoElementSelector
  , createExpression_resolverSelector
  , createNSResolverSelector
  , evaluate_contextNode_resolver_type_inResultSelector
  , execCommand_userInterface_valueSelector
  , execCommand_userInterfaceSelector
  , execCommandSelector
  , queryCommandEnabledSelector
  , queryCommandIndetermSelector
  , queryCommandStateSelector
  , queryCommandSupportedSelector
  , queryCommandValueSelector
  , getElementsByNameSelector
  , elementFromPoint_ySelector
  , createCSSStyleDeclarationSelector
  , getComputedStyle_pseudoElementSelector
  , getMatchedCSSRules_pseudoElementSelector
  , getMatchedCSSRules_pseudoElement_authorOnlySelector
  , getElementsByClassNameSelector
  , hasFocusSelector
  , webkitCancelFullScreenSelector
  , getElementByIdSelector
  , querySelectorSelector
  , querySelectorAllSelector
  , urlWithAttributeStringSelector
  , createProcessingInstructionSelector
  , importNodeSelector
  , createElementNSSelector
  , createAttributeNSSelector
  , getElementsByTagNameNSSelector
  , createNodeIteratorSelector
  , createTreeWalkerSelector
  , getOverrideStyleSelector
  , createExpressionSelector
  , evaluateSelector
  , getComputedStyleSelector
  , doctypeSelector
  , implementationSelector
  , documentElementSelector
  , inputEncodingSelector
  , xmlEncodingSelector
  , xmlVersionSelector
  , setXmlVersionSelector
  , xmlStandaloneSelector
  , setXmlStandaloneSelector
  , documentURISelector
  , setDocumentURISelector
  , defaultViewSelector
  , styleSheetsSelector
  , titleSelector
  , setTitleSelector
  , referrerSelector
  , domainSelector
  , urlSelector
  , cookieSelector
  , setCookieSelector
  , bodySelector
  , setBodySelector
  , imagesSelector
  , appletsSelector
  , linksSelector
  , formsSelector
  , anchorsSelector
  , lastModifiedSelector
  , charsetSelector
  , setCharsetSelector
  , defaultCharsetSelector
  , readyStateSelector
  , characterSetSelector
  , preferredStylesheetSetSelector
  , selectedStylesheetSetSelector
  , setSelectedStylesheetSetSelector
  , activeElementSelector
  , webFrameSelector


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

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- createElement:@
createElement :: (IsDOMDocument domDocument, IsNSString tagName) => domDocument -> tagName -> IO (Id DOMElement)
createElement domDocument  tagName =
withObjCPtr tagName $ \raw_tagName ->
    sendMsg domDocument (mkSelector "createElement:") (retPtr retVoid) [argPtr (castPtr raw_tagName :: Ptr ())] >>= retainedObject . castPtr

-- | @- createDocumentFragment@
createDocumentFragment :: IsDOMDocument domDocument => domDocument -> IO (Id DOMDocumentFragment)
createDocumentFragment domDocument  =
  sendMsg domDocument (mkSelector "createDocumentFragment") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- createTextNode:@
createTextNode :: (IsDOMDocument domDocument, IsNSString data_) => domDocument -> data_ -> IO (Id DOMText)
createTextNode domDocument  data_ =
withObjCPtr data_ $ \raw_data_ ->
    sendMsg domDocument (mkSelector "createTextNode:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- createComment:@
createComment :: (IsDOMDocument domDocument, IsNSString data_) => domDocument -> data_ -> IO (Id DOMComment)
createComment domDocument  data_ =
withObjCPtr data_ $ \raw_data_ ->
    sendMsg domDocument (mkSelector "createComment:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- createCDATASection:@
createCDATASection :: (IsDOMDocument domDocument, IsNSString data_) => domDocument -> data_ -> IO (Id DOMCDATASection)
createCDATASection domDocument  data_ =
withObjCPtr data_ $ \raw_data_ ->
    sendMsg domDocument (mkSelector "createCDATASection:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- createProcessingInstruction:data:@
createProcessingInstruction_data :: (IsDOMDocument domDocument, IsNSString target, IsNSString data_) => domDocument -> target -> data_ -> IO (Id DOMProcessingInstruction)
createProcessingInstruction_data domDocument  target data_ =
withObjCPtr target $ \raw_target ->
  withObjCPtr data_ $ \raw_data_ ->
      sendMsg domDocument (mkSelector "createProcessingInstruction:data:") (retPtr retVoid) [argPtr (castPtr raw_target :: Ptr ()), argPtr (castPtr raw_data_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- createAttribute:@
createAttribute :: (IsDOMDocument domDocument, IsNSString name) => domDocument -> name -> IO (Id DOMAttr)
createAttribute domDocument  name =
withObjCPtr name $ \raw_name ->
    sendMsg domDocument (mkSelector "createAttribute:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @- createEntityReference:@
createEntityReference :: (IsDOMDocument domDocument, IsNSString name) => domDocument -> name -> IO (Id DOMEntityReference)
createEntityReference domDocument  name =
withObjCPtr name $ \raw_name ->
    sendMsg domDocument (mkSelector "createEntityReference:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @- getElementsByTagName:@
getElementsByTagName :: (IsDOMDocument domDocument, IsNSString tagname) => domDocument -> tagname -> IO (Id DOMNodeList)
getElementsByTagName domDocument  tagname =
withObjCPtr tagname $ \raw_tagname ->
    sendMsg domDocument (mkSelector "getElementsByTagName:") (retPtr retVoid) [argPtr (castPtr raw_tagname :: Ptr ())] >>= retainedObject . castPtr

-- | @- importNode:deep:@
importNode_deep :: (IsDOMDocument domDocument, IsDOMNode importedNode) => domDocument -> importedNode -> Bool -> IO (Id DOMNode)
importNode_deep domDocument  importedNode deep =
withObjCPtr importedNode $ \raw_importedNode ->
    sendMsg domDocument (mkSelector "importNode:deep:") (retPtr retVoid) [argPtr (castPtr raw_importedNode :: Ptr ()), argCULong (if deep then 1 else 0)] >>= retainedObject . castPtr

-- | @- createElementNS:qualifiedName:@
createElementNS_qualifiedName :: (IsDOMDocument domDocument, IsNSString namespaceURI, IsNSString qualifiedName) => domDocument -> namespaceURI -> qualifiedName -> IO (Id DOMElement)
createElementNS_qualifiedName domDocument  namespaceURI qualifiedName =
withObjCPtr namespaceURI $ \raw_namespaceURI ->
  withObjCPtr qualifiedName $ \raw_qualifiedName ->
      sendMsg domDocument (mkSelector "createElementNS:qualifiedName:") (retPtr retVoid) [argPtr (castPtr raw_namespaceURI :: Ptr ()), argPtr (castPtr raw_qualifiedName :: Ptr ())] >>= retainedObject . castPtr

-- | @- createAttributeNS:qualifiedName:@
createAttributeNS_qualifiedName :: (IsDOMDocument domDocument, IsNSString namespaceURI, IsNSString qualifiedName) => domDocument -> namespaceURI -> qualifiedName -> IO (Id DOMAttr)
createAttributeNS_qualifiedName domDocument  namespaceURI qualifiedName =
withObjCPtr namespaceURI $ \raw_namespaceURI ->
  withObjCPtr qualifiedName $ \raw_qualifiedName ->
      sendMsg domDocument (mkSelector "createAttributeNS:qualifiedName:") (retPtr retVoid) [argPtr (castPtr raw_namespaceURI :: Ptr ()), argPtr (castPtr raw_qualifiedName :: Ptr ())] >>= retainedObject . castPtr

-- | @- getElementsByTagNameNS:localName:@
getElementsByTagNameNS_localName :: (IsDOMDocument domDocument, IsNSString namespaceURI, IsNSString localName) => domDocument -> namespaceURI -> localName -> IO (Id DOMNodeList)
getElementsByTagNameNS_localName domDocument  namespaceURI localName =
withObjCPtr namespaceURI $ \raw_namespaceURI ->
  withObjCPtr localName $ \raw_localName ->
      sendMsg domDocument (mkSelector "getElementsByTagNameNS:localName:") (retPtr retVoid) [argPtr (castPtr raw_namespaceURI :: Ptr ()), argPtr (castPtr raw_localName :: Ptr ())] >>= retainedObject . castPtr

-- | @- adoptNode:@
adoptNode :: (IsDOMDocument domDocument, IsDOMNode source) => domDocument -> source -> IO (Id DOMNode)
adoptNode domDocument  source =
withObjCPtr source $ \raw_source ->
    sendMsg domDocument (mkSelector "adoptNode:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ())] >>= retainedObject . castPtr

-- | @- createEvent:@
createEvent :: (IsDOMDocument domDocument, IsNSString eventType) => domDocument -> eventType -> IO (Id DOMEvent)
createEvent domDocument  eventType =
withObjCPtr eventType $ \raw_eventType ->
    sendMsg domDocument (mkSelector "createEvent:") (retPtr retVoid) [argPtr (castPtr raw_eventType :: Ptr ())] >>= retainedObject . castPtr

-- | @- createRange@
createRange :: IsDOMDocument domDocument => domDocument -> IO (Id DOMRange)
createRange domDocument  =
  sendMsg domDocument (mkSelector "createRange") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- createNodeIterator:whatToShow:filter:expandEntityReferences:@
createNodeIterator_whatToShow_filter_expandEntityReferences :: (IsDOMDocument domDocument, IsDOMNode root) => domDocument -> root -> CUInt -> RawId -> Bool -> IO (Id DOMNodeIterator)
createNodeIterator_whatToShow_filter_expandEntityReferences domDocument  root whatToShow filter_ expandEntityReferences =
withObjCPtr root $ \raw_root ->
    sendMsg domDocument (mkSelector "createNodeIterator:whatToShow:filter:expandEntityReferences:") (retPtr retVoid) [argPtr (castPtr raw_root :: Ptr ()), argCUInt (fromIntegral whatToShow), argPtr (castPtr (unRawId filter_) :: Ptr ()), argCULong (if expandEntityReferences then 1 else 0)] >>= retainedObject . castPtr

-- | @- createTreeWalker:whatToShow:filter:expandEntityReferences:@
createTreeWalker_whatToShow_filter_expandEntityReferences :: (IsDOMDocument domDocument, IsDOMNode root) => domDocument -> root -> CUInt -> RawId -> Bool -> IO (Id DOMTreeWalker)
createTreeWalker_whatToShow_filter_expandEntityReferences domDocument  root whatToShow filter_ expandEntityReferences =
withObjCPtr root $ \raw_root ->
    sendMsg domDocument (mkSelector "createTreeWalker:whatToShow:filter:expandEntityReferences:") (retPtr retVoid) [argPtr (castPtr raw_root :: Ptr ()), argCUInt (fromIntegral whatToShow), argPtr (castPtr (unRawId filter_) :: Ptr ()), argCULong (if expandEntityReferences then 1 else 0)] >>= retainedObject . castPtr

-- | @- getOverrideStyle:pseudoElement:@
getOverrideStyle_pseudoElement :: (IsDOMDocument domDocument, IsDOMElement element, IsNSString pseudoElement) => domDocument -> element -> pseudoElement -> IO (Id DOMCSSStyleDeclaration)
getOverrideStyle_pseudoElement domDocument  element pseudoElement =
withObjCPtr element $ \raw_element ->
  withObjCPtr pseudoElement $ \raw_pseudoElement ->
      sendMsg domDocument (mkSelector "getOverrideStyle:pseudoElement:") (retPtr retVoid) [argPtr (castPtr raw_element :: Ptr ()), argPtr (castPtr raw_pseudoElement :: Ptr ())] >>= retainedObject . castPtr

-- | @- createExpression:resolver:@
createExpression_resolver :: (IsDOMDocument domDocument, IsNSString expression) => domDocument -> expression -> RawId -> IO (Id DOMXPathExpression)
createExpression_resolver domDocument  expression resolver =
withObjCPtr expression $ \raw_expression ->
    sendMsg domDocument (mkSelector "createExpression:resolver:") (retPtr retVoid) [argPtr (castPtr raw_expression :: Ptr ()), argPtr (castPtr (unRawId resolver) :: Ptr ())] >>= retainedObject . castPtr

-- | @- createNSResolver:@
createNSResolver :: (IsDOMDocument domDocument, IsDOMNode nodeResolver) => domDocument -> nodeResolver -> IO RawId
createNSResolver domDocument  nodeResolver =
withObjCPtr nodeResolver $ \raw_nodeResolver ->
    fmap (RawId . castPtr) $ sendMsg domDocument (mkSelector "createNSResolver:") (retPtr retVoid) [argPtr (castPtr raw_nodeResolver :: Ptr ())]

-- | @- evaluate:contextNode:resolver:type:inResult:@
evaluate_contextNode_resolver_type_inResult :: (IsDOMDocument domDocument, IsNSString expression, IsDOMNode contextNode, IsDOMXPathResult inResult) => domDocument -> expression -> contextNode -> RawId -> CUShort -> inResult -> IO (Id DOMXPathResult)
evaluate_contextNode_resolver_type_inResult domDocument  expression contextNode resolver type_ inResult =
withObjCPtr expression $ \raw_expression ->
  withObjCPtr contextNode $ \raw_contextNode ->
    withObjCPtr inResult $ \raw_inResult ->
        sendMsg domDocument (mkSelector "evaluate:contextNode:resolver:type:inResult:") (retPtr retVoid) [argPtr (castPtr raw_expression :: Ptr ()), argPtr (castPtr raw_contextNode :: Ptr ()), argPtr (castPtr (unRawId resolver) :: Ptr ()), argCUInt (fromIntegral type_), argPtr (castPtr raw_inResult :: Ptr ())] >>= retainedObject . castPtr

-- | @- execCommand:userInterface:value:@
execCommand_userInterface_value :: (IsDOMDocument domDocument, IsNSString command, IsNSString value) => domDocument -> command -> Bool -> value -> IO Bool
execCommand_userInterface_value domDocument  command userInterface value =
withObjCPtr command $ \raw_command ->
  withObjCPtr value $ \raw_value ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg domDocument (mkSelector "execCommand:userInterface:value:") retCULong [argPtr (castPtr raw_command :: Ptr ()), argCULong (if userInterface then 1 else 0), argPtr (castPtr raw_value :: Ptr ())]

-- | @- execCommand:userInterface:@
execCommand_userInterface :: (IsDOMDocument domDocument, IsNSString command) => domDocument -> command -> Bool -> IO Bool
execCommand_userInterface domDocument  command userInterface =
withObjCPtr command $ \raw_command ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg domDocument (mkSelector "execCommand:userInterface:") retCULong [argPtr (castPtr raw_command :: Ptr ()), argCULong (if userInterface then 1 else 0)]

-- | @- execCommand:@
execCommand :: (IsDOMDocument domDocument, IsNSString command) => domDocument -> command -> IO Bool
execCommand domDocument  command =
withObjCPtr command $ \raw_command ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg domDocument (mkSelector "execCommand:") retCULong [argPtr (castPtr raw_command :: Ptr ())]

-- | @- queryCommandEnabled:@
queryCommandEnabled :: (IsDOMDocument domDocument, IsNSString command) => domDocument -> command -> IO Bool
queryCommandEnabled domDocument  command =
withObjCPtr command $ \raw_command ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg domDocument (mkSelector "queryCommandEnabled:") retCULong [argPtr (castPtr raw_command :: Ptr ())]

-- | @- queryCommandIndeterm:@
queryCommandIndeterm :: (IsDOMDocument domDocument, IsNSString command) => domDocument -> command -> IO Bool
queryCommandIndeterm domDocument  command =
withObjCPtr command $ \raw_command ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg domDocument (mkSelector "queryCommandIndeterm:") retCULong [argPtr (castPtr raw_command :: Ptr ())]

-- | @- queryCommandState:@
queryCommandState :: (IsDOMDocument domDocument, IsNSString command) => domDocument -> command -> IO Bool
queryCommandState domDocument  command =
withObjCPtr command $ \raw_command ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg domDocument (mkSelector "queryCommandState:") retCULong [argPtr (castPtr raw_command :: Ptr ())]

-- | @- queryCommandSupported:@
queryCommandSupported :: (IsDOMDocument domDocument, IsNSString command) => domDocument -> command -> IO Bool
queryCommandSupported domDocument  command =
withObjCPtr command $ \raw_command ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg domDocument (mkSelector "queryCommandSupported:") retCULong [argPtr (castPtr raw_command :: Ptr ())]

-- | @- queryCommandValue:@
queryCommandValue :: (IsDOMDocument domDocument, IsNSString command) => domDocument -> command -> IO (Id NSString)
queryCommandValue domDocument  command =
withObjCPtr command $ \raw_command ->
    sendMsg domDocument (mkSelector "queryCommandValue:") (retPtr retVoid) [argPtr (castPtr raw_command :: Ptr ())] >>= retainedObject . castPtr

-- | @- getElementsByName:@
getElementsByName :: (IsDOMDocument domDocument, IsNSString elementName) => domDocument -> elementName -> IO (Id DOMNodeList)
getElementsByName domDocument  elementName =
withObjCPtr elementName $ \raw_elementName ->
    sendMsg domDocument (mkSelector "getElementsByName:") (retPtr retVoid) [argPtr (castPtr raw_elementName :: Ptr ())] >>= retainedObject . castPtr

-- | @- elementFromPoint:y:@
elementFromPoint_y :: IsDOMDocument domDocument => domDocument -> CInt -> CInt -> IO (Id DOMElement)
elementFromPoint_y domDocument  x y =
  sendMsg domDocument (mkSelector "elementFromPoint:y:") (retPtr retVoid) [argCInt (fromIntegral x), argCInt (fromIntegral y)] >>= retainedObject . castPtr

-- | @- createCSSStyleDeclaration@
createCSSStyleDeclaration :: IsDOMDocument domDocument => domDocument -> IO (Id DOMCSSStyleDeclaration)
createCSSStyleDeclaration domDocument  =
  sendMsg domDocument (mkSelector "createCSSStyleDeclaration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- getComputedStyle:pseudoElement:@
getComputedStyle_pseudoElement :: (IsDOMDocument domDocument, IsDOMElement element, IsNSString pseudoElement) => domDocument -> element -> pseudoElement -> IO (Id DOMCSSStyleDeclaration)
getComputedStyle_pseudoElement domDocument  element pseudoElement =
withObjCPtr element $ \raw_element ->
  withObjCPtr pseudoElement $ \raw_pseudoElement ->
      sendMsg domDocument (mkSelector "getComputedStyle:pseudoElement:") (retPtr retVoid) [argPtr (castPtr raw_element :: Ptr ()), argPtr (castPtr raw_pseudoElement :: Ptr ())] >>= retainedObject . castPtr

-- | @- getMatchedCSSRules:pseudoElement:@
getMatchedCSSRules_pseudoElement :: (IsDOMDocument domDocument, IsDOMElement element, IsNSString pseudoElement) => domDocument -> element -> pseudoElement -> IO (Id DOMCSSRuleList)
getMatchedCSSRules_pseudoElement domDocument  element pseudoElement =
withObjCPtr element $ \raw_element ->
  withObjCPtr pseudoElement $ \raw_pseudoElement ->
      sendMsg domDocument (mkSelector "getMatchedCSSRules:pseudoElement:") (retPtr retVoid) [argPtr (castPtr raw_element :: Ptr ()), argPtr (castPtr raw_pseudoElement :: Ptr ())] >>= retainedObject . castPtr

-- | @- getMatchedCSSRules:pseudoElement:authorOnly:@
getMatchedCSSRules_pseudoElement_authorOnly :: (IsDOMDocument domDocument, IsDOMElement element, IsNSString pseudoElement) => domDocument -> element -> pseudoElement -> Bool -> IO (Id DOMCSSRuleList)
getMatchedCSSRules_pseudoElement_authorOnly domDocument  element pseudoElement authorOnly =
withObjCPtr element $ \raw_element ->
  withObjCPtr pseudoElement $ \raw_pseudoElement ->
      sendMsg domDocument (mkSelector "getMatchedCSSRules:pseudoElement:authorOnly:") (retPtr retVoid) [argPtr (castPtr raw_element :: Ptr ()), argPtr (castPtr raw_pseudoElement :: Ptr ()), argCULong (if authorOnly then 1 else 0)] >>= retainedObject . castPtr

-- | @- getElementsByClassName:@
getElementsByClassName :: (IsDOMDocument domDocument, IsNSString classNames) => domDocument -> classNames -> IO (Id DOMNodeList)
getElementsByClassName domDocument  classNames =
withObjCPtr classNames $ \raw_classNames ->
    sendMsg domDocument (mkSelector "getElementsByClassName:") (retPtr retVoid) [argPtr (castPtr raw_classNames :: Ptr ())] >>= retainedObject . castPtr

-- | @- hasFocus@
hasFocus :: IsDOMDocument domDocument => domDocument -> IO Bool
hasFocus domDocument  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domDocument (mkSelector "hasFocus") retCULong []

-- | @- webkitCancelFullScreen@
webkitCancelFullScreen :: IsDOMDocument domDocument => domDocument -> IO ()
webkitCancelFullScreen domDocument  =
  sendMsg domDocument (mkSelector "webkitCancelFullScreen") retVoid []

-- | @- getElementById:@
getElementById :: (IsDOMDocument domDocument, IsNSString elementId) => domDocument -> elementId -> IO (Id DOMElement)
getElementById domDocument  elementId =
withObjCPtr elementId $ \raw_elementId ->
    sendMsg domDocument (mkSelector "getElementById:") (retPtr retVoid) [argPtr (castPtr raw_elementId :: Ptr ())] >>= retainedObject . castPtr

-- | @- querySelector:@
querySelector :: (IsDOMDocument domDocument, IsNSString selectors) => domDocument -> selectors -> IO (Id DOMElement)
querySelector domDocument  selectors =
withObjCPtr selectors $ \raw_selectors ->
    sendMsg domDocument (mkSelector "querySelector:") (retPtr retVoid) [argPtr (castPtr raw_selectors :: Ptr ())] >>= retainedObject . castPtr

-- | @- querySelectorAll:@
querySelectorAll :: (IsDOMDocument domDocument, IsNSString selectors) => domDocument -> selectors -> IO (Id DOMNodeList)
querySelectorAll domDocument  selectors =
withObjCPtr selectors $ \raw_selectors ->
    sendMsg domDocument (mkSelector "querySelectorAll:") (retPtr retVoid) [argPtr (castPtr raw_selectors :: Ptr ())] >>= retainedObject . castPtr

-- | URLWithAttributeString:
--
-- Constructs a URL given an attribute string.
--
-- This method constructs a URL given an attribute string just as WebKit does.     An attribute string is the value of an attribute of an element such as the href attribute on     the DOMHTMLAnchorElement class. This method is only applicable to attributes that refer to URLs.
--
-- ObjC selector: @- URLWithAttributeString:@
urlWithAttributeString :: (IsDOMDocument domDocument, IsNSString string) => domDocument -> string -> IO (Id NSURL)
urlWithAttributeString domDocument  string =
withObjCPtr string $ \raw_string ->
    sendMsg domDocument (mkSelector "URLWithAttributeString:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= retainedObject . castPtr

-- | @- createProcessingInstruction::@
createProcessingInstruction :: (IsDOMDocument domDocument, IsNSString target, IsNSString data_) => domDocument -> target -> data_ -> IO (Id DOMProcessingInstruction)
createProcessingInstruction domDocument  target data_ =
withObjCPtr target $ \raw_target ->
  withObjCPtr data_ $ \raw_data_ ->
      sendMsg domDocument (mkSelector "createProcessingInstruction::") (retPtr retVoid) [argPtr (castPtr raw_target :: Ptr ()), argPtr (castPtr raw_data_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- importNode::@
importNode :: (IsDOMDocument domDocument, IsDOMNode importedNode) => domDocument -> importedNode -> Bool -> IO (Id DOMNode)
importNode domDocument  importedNode deep =
withObjCPtr importedNode $ \raw_importedNode ->
    sendMsg domDocument (mkSelector "importNode::") (retPtr retVoid) [argPtr (castPtr raw_importedNode :: Ptr ()), argCULong (if deep then 1 else 0)] >>= retainedObject . castPtr

-- | @- createElementNS::@
createElementNS :: (IsDOMDocument domDocument, IsNSString namespaceURI, IsNSString qualifiedName) => domDocument -> namespaceURI -> qualifiedName -> IO (Id DOMElement)
createElementNS domDocument  namespaceURI qualifiedName =
withObjCPtr namespaceURI $ \raw_namespaceURI ->
  withObjCPtr qualifiedName $ \raw_qualifiedName ->
      sendMsg domDocument (mkSelector "createElementNS::") (retPtr retVoid) [argPtr (castPtr raw_namespaceURI :: Ptr ()), argPtr (castPtr raw_qualifiedName :: Ptr ())] >>= retainedObject . castPtr

-- | @- createAttributeNS::@
createAttributeNS :: (IsDOMDocument domDocument, IsNSString namespaceURI, IsNSString qualifiedName) => domDocument -> namespaceURI -> qualifiedName -> IO (Id DOMAttr)
createAttributeNS domDocument  namespaceURI qualifiedName =
withObjCPtr namespaceURI $ \raw_namespaceURI ->
  withObjCPtr qualifiedName $ \raw_qualifiedName ->
      sendMsg domDocument (mkSelector "createAttributeNS::") (retPtr retVoid) [argPtr (castPtr raw_namespaceURI :: Ptr ()), argPtr (castPtr raw_qualifiedName :: Ptr ())] >>= retainedObject . castPtr

-- | @- getElementsByTagNameNS::@
getElementsByTagNameNS :: (IsDOMDocument domDocument, IsNSString namespaceURI, IsNSString localName) => domDocument -> namespaceURI -> localName -> IO (Id DOMNodeList)
getElementsByTagNameNS domDocument  namespaceURI localName =
withObjCPtr namespaceURI $ \raw_namespaceURI ->
  withObjCPtr localName $ \raw_localName ->
      sendMsg domDocument (mkSelector "getElementsByTagNameNS::") (retPtr retVoid) [argPtr (castPtr raw_namespaceURI :: Ptr ()), argPtr (castPtr raw_localName :: Ptr ())] >>= retainedObject . castPtr

-- | @- createNodeIterator::::@
createNodeIterator :: (IsDOMDocument domDocument, IsDOMNode root) => domDocument -> root -> CUInt -> RawId -> Bool -> IO (Id DOMNodeIterator)
createNodeIterator domDocument  root whatToShow filter_ expandEntityReferences =
withObjCPtr root $ \raw_root ->
    sendMsg domDocument (mkSelector "createNodeIterator::::") (retPtr retVoid) [argPtr (castPtr raw_root :: Ptr ()), argCUInt (fromIntegral whatToShow), argPtr (castPtr (unRawId filter_) :: Ptr ()), argCULong (if expandEntityReferences then 1 else 0)] >>= retainedObject . castPtr

-- | @- createTreeWalker::::@
createTreeWalker :: (IsDOMDocument domDocument, IsDOMNode root) => domDocument -> root -> CUInt -> RawId -> Bool -> IO (Id DOMTreeWalker)
createTreeWalker domDocument  root whatToShow filter_ expandEntityReferences =
withObjCPtr root $ \raw_root ->
    sendMsg domDocument (mkSelector "createTreeWalker::::") (retPtr retVoid) [argPtr (castPtr raw_root :: Ptr ()), argCUInt (fromIntegral whatToShow), argPtr (castPtr (unRawId filter_) :: Ptr ()), argCULong (if expandEntityReferences then 1 else 0)] >>= retainedObject . castPtr

-- | @- getOverrideStyle::@
getOverrideStyle :: (IsDOMDocument domDocument, IsDOMElement element, IsNSString pseudoElement) => domDocument -> element -> pseudoElement -> IO (Id DOMCSSStyleDeclaration)
getOverrideStyle domDocument  element pseudoElement =
withObjCPtr element $ \raw_element ->
  withObjCPtr pseudoElement $ \raw_pseudoElement ->
      sendMsg domDocument (mkSelector "getOverrideStyle::") (retPtr retVoid) [argPtr (castPtr raw_element :: Ptr ()), argPtr (castPtr raw_pseudoElement :: Ptr ())] >>= retainedObject . castPtr

-- | @- createExpression::@
createExpression :: (IsDOMDocument domDocument, IsNSString expression) => domDocument -> expression -> RawId -> IO (Id DOMXPathExpression)
createExpression domDocument  expression resolver =
withObjCPtr expression $ \raw_expression ->
    sendMsg domDocument (mkSelector "createExpression::") (retPtr retVoid) [argPtr (castPtr raw_expression :: Ptr ()), argPtr (castPtr (unRawId resolver) :: Ptr ())] >>= retainedObject . castPtr

-- | @- evaluate:::::@
evaluate :: (IsDOMDocument domDocument, IsNSString expression, IsDOMNode contextNode, IsDOMXPathResult inResult) => domDocument -> expression -> contextNode -> RawId -> CUShort -> inResult -> IO (Id DOMXPathResult)
evaluate domDocument  expression contextNode resolver type_ inResult =
withObjCPtr expression $ \raw_expression ->
  withObjCPtr contextNode $ \raw_contextNode ->
    withObjCPtr inResult $ \raw_inResult ->
        sendMsg domDocument (mkSelector "evaluate:::::") (retPtr retVoid) [argPtr (castPtr raw_expression :: Ptr ()), argPtr (castPtr raw_contextNode :: Ptr ()), argPtr (castPtr (unRawId resolver) :: Ptr ()), argCUInt (fromIntegral type_), argPtr (castPtr raw_inResult :: Ptr ())] >>= retainedObject . castPtr

-- | @- getComputedStyle::@
getComputedStyle :: (IsDOMDocument domDocument, IsDOMElement element, IsNSString pseudoElement) => domDocument -> element -> pseudoElement -> IO (Id DOMCSSStyleDeclaration)
getComputedStyle domDocument  element pseudoElement =
withObjCPtr element $ \raw_element ->
  withObjCPtr pseudoElement $ \raw_pseudoElement ->
      sendMsg domDocument (mkSelector "getComputedStyle::") (retPtr retVoid) [argPtr (castPtr raw_element :: Ptr ()), argPtr (castPtr raw_pseudoElement :: Ptr ())] >>= retainedObject . castPtr

-- | @- doctype@
doctype :: IsDOMDocument domDocument => domDocument -> IO (Id DOMDocumentType)
doctype domDocument  =
  sendMsg domDocument (mkSelector "doctype") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- implementation@
implementation :: IsDOMDocument domDocument => domDocument -> IO (Id DOMImplementation)
implementation domDocument  =
  sendMsg domDocument (mkSelector "implementation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- documentElement@
documentElement :: IsDOMDocument domDocument => domDocument -> IO (Id DOMElement)
documentElement domDocument  =
  sendMsg domDocument (mkSelector "documentElement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- inputEncoding@
inputEncoding :: IsDOMDocument domDocument => domDocument -> IO (Id NSString)
inputEncoding domDocument  =
  sendMsg domDocument (mkSelector "inputEncoding") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- xmlEncoding@
xmlEncoding :: IsDOMDocument domDocument => domDocument -> IO (Id NSString)
xmlEncoding domDocument  =
  sendMsg domDocument (mkSelector "xmlEncoding") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- xmlVersion@
xmlVersion :: IsDOMDocument domDocument => domDocument -> IO (Id NSString)
xmlVersion domDocument  =
  sendMsg domDocument (mkSelector "xmlVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setXmlVersion:@
setXmlVersion :: (IsDOMDocument domDocument, IsNSString value) => domDocument -> value -> IO ()
setXmlVersion domDocument  value =
withObjCPtr value $ \raw_value ->
    sendMsg domDocument (mkSelector "setXmlVersion:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- xmlStandalone@
xmlStandalone :: IsDOMDocument domDocument => domDocument -> IO Bool
xmlStandalone domDocument  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domDocument (mkSelector "xmlStandalone") retCULong []

-- | @- setXmlStandalone:@
setXmlStandalone :: IsDOMDocument domDocument => domDocument -> Bool -> IO ()
setXmlStandalone domDocument  value =
  sendMsg domDocument (mkSelector "setXmlStandalone:") retVoid [argCULong (if value then 1 else 0)]

-- | @- documentURI@
documentURI :: IsDOMDocument domDocument => domDocument -> IO (Id NSString)
documentURI domDocument  =
  sendMsg domDocument (mkSelector "documentURI") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDocumentURI:@
setDocumentURI :: (IsDOMDocument domDocument, IsNSString value) => domDocument -> value -> IO ()
setDocumentURI domDocument  value =
withObjCPtr value $ \raw_value ->
    sendMsg domDocument (mkSelector "setDocumentURI:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- defaultView@
defaultView :: IsDOMDocument domDocument => domDocument -> IO (Id DOMAbstractView)
defaultView domDocument  =
  sendMsg domDocument (mkSelector "defaultView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- styleSheets@
styleSheets :: IsDOMDocument domDocument => domDocument -> IO (Id DOMStyleSheetList)
styleSheets domDocument  =
  sendMsg domDocument (mkSelector "styleSheets") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- title@
title :: IsDOMDocument domDocument => domDocument -> IO (Id NSString)
title domDocument  =
  sendMsg domDocument (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTitle:@
setTitle :: (IsDOMDocument domDocument, IsNSString value) => domDocument -> value -> IO ()
setTitle domDocument  value =
withObjCPtr value $ \raw_value ->
    sendMsg domDocument (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- referrer@
referrer :: IsDOMDocument domDocument => domDocument -> IO (Id NSString)
referrer domDocument  =
  sendMsg domDocument (mkSelector "referrer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- domain@
domain :: IsDOMDocument domDocument => domDocument -> IO (Id NSString)
domain domDocument  =
  sendMsg domDocument (mkSelector "domain") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- URL@
url :: IsDOMDocument domDocument => domDocument -> IO (Id NSString)
url domDocument  =
  sendMsg domDocument (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- cookie@
cookie :: IsDOMDocument domDocument => domDocument -> IO (Id NSString)
cookie domDocument  =
  sendMsg domDocument (mkSelector "cookie") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCookie:@
setCookie :: (IsDOMDocument domDocument, IsNSString value) => domDocument -> value -> IO ()
setCookie domDocument  value =
withObjCPtr value $ \raw_value ->
    sendMsg domDocument (mkSelector "setCookie:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- body@
body :: IsDOMDocument domDocument => domDocument -> IO (Id DOMHTMLElement)
body domDocument  =
  sendMsg domDocument (mkSelector "body") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBody:@
setBody :: (IsDOMDocument domDocument, IsDOMHTMLElement value) => domDocument -> value -> IO ()
setBody domDocument  value =
withObjCPtr value $ \raw_value ->
    sendMsg domDocument (mkSelector "setBody:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- images@
images :: IsDOMDocument domDocument => domDocument -> IO (Id DOMHTMLCollection)
images domDocument  =
  sendMsg domDocument (mkSelector "images") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- applets@
applets :: IsDOMDocument domDocument => domDocument -> IO (Id DOMHTMLCollection)
applets domDocument  =
  sendMsg domDocument (mkSelector "applets") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- links@
links :: IsDOMDocument domDocument => domDocument -> IO (Id DOMHTMLCollection)
links domDocument  =
  sendMsg domDocument (mkSelector "links") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- forms@
forms :: IsDOMDocument domDocument => domDocument -> IO (Id DOMHTMLCollection)
forms domDocument  =
  sendMsg domDocument (mkSelector "forms") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- anchors@
anchors :: IsDOMDocument domDocument => domDocument -> IO (Id DOMHTMLCollection)
anchors domDocument  =
  sendMsg domDocument (mkSelector "anchors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- lastModified@
lastModified :: IsDOMDocument domDocument => domDocument -> IO (Id NSString)
lastModified domDocument  =
  sendMsg domDocument (mkSelector "lastModified") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- charset@
charset :: IsDOMDocument domDocument => domDocument -> IO (Id NSString)
charset domDocument  =
  sendMsg domDocument (mkSelector "charset") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCharset:@
setCharset :: (IsDOMDocument domDocument, IsNSString value) => domDocument -> value -> IO ()
setCharset domDocument  value =
withObjCPtr value $ \raw_value ->
    sendMsg domDocument (mkSelector "setCharset:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- defaultCharset@
defaultCharset :: IsDOMDocument domDocument => domDocument -> IO (Id NSString)
defaultCharset domDocument  =
  sendMsg domDocument (mkSelector "defaultCharset") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- readyState@
readyState :: IsDOMDocument domDocument => domDocument -> IO (Id NSString)
readyState domDocument  =
  sendMsg domDocument (mkSelector "readyState") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- characterSet@
characterSet :: IsDOMDocument domDocument => domDocument -> IO (Id NSString)
characterSet domDocument  =
  sendMsg domDocument (mkSelector "characterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- preferredStylesheetSet@
preferredStylesheetSet :: IsDOMDocument domDocument => domDocument -> IO (Id NSString)
preferredStylesheetSet domDocument  =
  sendMsg domDocument (mkSelector "preferredStylesheetSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- selectedStylesheetSet@
selectedStylesheetSet :: IsDOMDocument domDocument => domDocument -> IO (Id NSString)
selectedStylesheetSet domDocument  =
  sendMsg domDocument (mkSelector "selectedStylesheetSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSelectedStylesheetSet:@
setSelectedStylesheetSet :: (IsDOMDocument domDocument, IsNSString value) => domDocument -> value -> IO ()
setSelectedStylesheetSet domDocument  value =
withObjCPtr value $ \raw_value ->
    sendMsg domDocument (mkSelector "setSelectedStylesheetSet:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- activeElement@
activeElement :: IsDOMDocument domDocument => domDocument -> IO (Id DOMElement)
activeElement domDocument  =
  sendMsg domDocument (mkSelector "activeElement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | webFrame
--
-- The frame of the DOM document.
--
-- ObjC selector: @- webFrame@
webFrame :: IsDOMDocument domDocument => domDocument -> IO (Id WebFrame)
webFrame domDocument  =
  sendMsg domDocument (mkSelector "webFrame") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @createElement:@
createElementSelector :: Selector
createElementSelector = mkSelector "createElement:"

-- | @Selector@ for @createDocumentFragment@
createDocumentFragmentSelector :: Selector
createDocumentFragmentSelector = mkSelector "createDocumentFragment"

-- | @Selector@ for @createTextNode:@
createTextNodeSelector :: Selector
createTextNodeSelector = mkSelector "createTextNode:"

-- | @Selector@ for @createComment:@
createCommentSelector :: Selector
createCommentSelector = mkSelector "createComment:"

-- | @Selector@ for @createCDATASection:@
createCDATASectionSelector :: Selector
createCDATASectionSelector = mkSelector "createCDATASection:"

-- | @Selector@ for @createProcessingInstruction:data:@
createProcessingInstruction_dataSelector :: Selector
createProcessingInstruction_dataSelector = mkSelector "createProcessingInstruction:data:"

-- | @Selector@ for @createAttribute:@
createAttributeSelector :: Selector
createAttributeSelector = mkSelector "createAttribute:"

-- | @Selector@ for @createEntityReference:@
createEntityReferenceSelector :: Selector
createEntityReferenceSelector = mkSelector "createEntityReference:"

-- | @Selector@ for @getElementsByTagName:@
getElementsByTagNameSelector :: Selector
getElementsByTagNameSelector = mkSelector "getElementsByTagName:"

-- | @Selector@ for @importNode:deep:@
importNode_deepSelector :: Selector
importNode_deepSelector = mkSelector "importNode:deep:"

-- | @Selector@ for @createElementNS:qualifiedName:@
createElementNS_qualifiedNameSelector :: Selector
createElementNS_qualifiedNameSelector = mkSelector "createElementNS:qualifiedName:"

-- | @Selector@ for @createAttributeNS:qualifiedName:@
createAttributeNS_qualifiedNameSelector :: Selector
createAttributeNS_qualifiedNameSelector = mkSelector "createAttributeNS:qualifiedName:"

-- | @Selector@ for @getElementsByTagNameNS:localName:@
getElementsByTagNameNS_localNameSelector :: Selector
getElementsByTagNameNS_localNameSelector = mkSelector "getElementsByTagNameNS:localName:"

-- | @Selector@ for @adoptNode:@
adoptNodeSelector :: Selector
adoptNodeSelector = mkSelector "adoptNode:"

-- | @Selector@ for @createEvent:@
createEventSelector :: Selector
createEventSelector = mkSelector "createEvent:"

-- | @Selector@ for @createRange@
createRangeSelector :: Selector
createRangeSelector = mkSelector "createRange"

-- | @Selector@ for @createNodeIterator:whatToShow:filter:expandEntityReferences:@
createNodeIterator_whatToShow_filter_expandEntityReferencesSelector :: Selector
createNodeIterator_whatToShow_filter_expandEntityReferencesSelector = mkSelector "createNodeIterator:whatToShow:filter:expandEntityReferences:"

-- | @Selector@ for @createTreeWalker:whatToShow:filter:expandEntityReferences:@
createTreeWalker_whatToShow_filter_expandEntityReferencesSelector :: Selector
createTreeWalker_whatToShow_filter_expandEntityReferencesSelector = mkSelector "createTreeWalker:whatToShow:filter:expandEntityReferences:"

-- | @Selector@ for @getOverrideStyle:pseudoElement:@
getOverrideStyle_pseudoElementSelector :: Selector
getOverrideStyle_pseudoElementSelector = mkSelector "getOverrideStyle:pseudoElement:"

-- | @Selector@ for @createExpression:resolver:@
createExpression_resolverSelector :: Selector
createExpression_resolverSelector = mkSelector "createExpression:resolver:"

-- | @Selector@ for @createNSResolver:@
createNSResolverSelector :: Selector
createNSResolverSelector = mkSelector "createNSResolver:"

-- | @Selector@ for @evaluate:contextNode:resolver:type:inResult:@
evaluate_contextNode_resolver_type_inResultSelector :: Selector
evaluate_contextNode_resolver_type_inResultSelector = mkSelector "evaluate:contextNode:resolver:type:inResult:"

-- | @Selector@ for @execCommand:userInterface:value:@
execCommand_userInterface_valueSelector :: Selector
execCommand_userInterface_valueSelector = mkSelector "execCommand:userInterface:value:"

-- | @Selector@ for @execCommand:userInterface:@
execCommand_userInterfaceSelector :: Selector
execCommand_userInterfaceSelector = mkSelector "execCommand:userInterface:"

-- | @Selector@ for @execCommand:@
execCommandSelector :: Selector
execCommandSelector = mkSelector "execCommand:"

-- | @Selector@ for @queryCommandEnabled:@
queryCommandEnabledSelector :: Selector
queryCommandEnabledSelector = mkSelector "queryCommandEnabled:"

-- | @Selector@ for @queryCommandIndeterm:@
queryCommandIndetermSelector :: Selector
queryCommandIndetermSelector = mkSelector "queryCommandIndeterm:"

-- | @Selector@ for @queryCommandState:@
queryCommandStateSelector :: Selector
queryCommandStateSelector = mkSelector "queryCommandState:"

-- | @Selector@ for @queryCommandSupported:@
queryCommandSupportedSelector :: Selector
queryCommandSupportedSelector = mkSelector "queryCommandSupported:"

-- | @Selector@ for @queryCommandValue:@
queryCommandValueSelector :: Selector
queryCommandValueSelector = mkSelector "queryCommandValue:"

-- | @Selector@ for @getElementsByName:@
getElementsByNameSelector :: Selector
getElementsByNameSelector = mkSelector "getElementsByName:"

-- | @Selector@ for @elementFromPoint:y:@
elementFromPoint_ySelector :: Selector
elementFromPoint_ySelector = mkSelector "elementFromPoint:y:"

-- | @Selector@ for @createCSSStyleDeclaration@
createCSSStyleDeclarationSelector :: Selector
createCSSStyleDeclarationSelector = mkSelector "createCSSStyleDeclaration"

-- | @Selector@ for @getComputedStyle:pseudoElement:@
getComputedStyle_pseudoElementSelector :: Selector
getComputedStyle_pseudoElementSelector = mkSelector "getComputedStyle:pseudoElement:"

-- | @Selector@ for @getMatchedCSSRules:pseudoElement:@
getMatchedCSSRules_pseudoElementSelector :: Selector
getMatchedCSSRules_pseudoElementSelector = mkSelector "getMatchedCSSRules:pseudoElement:"

-- | @Selector@ for @getMatchedCSSRules:pseudoElement:authorOnly:@
getMatchedCSSRules_pseudoElement_authorOnlySelector :: Selector
getMatchedCSSRules_pseudoElement_authorOnlySelector = mkSelector "getMatchedCSSRules:pseudoElement:authorOnly:"

-- | @Selector@ for @getElementsByClassName:@
getElementsByClassNameSelector :: Selector
getElementsByClassNameSelector = mkSelector "getElementsByClassName:"

-- | @Selector@ for @hasFocus@
hasFocusSelector :: Selector
hasFocusSelector = mkSelector "hasFocus"

-- | @Selector@ for @webkitCancelFullScreen@
webkitCancelFullScreenSelector :: Selector
webkitCancelFullScreenSelector = mkSelector "webkitCancelFullScreen"

-- | @Selector@ for @getElementById:@
getElementByIdSelector :: Selector
getElementByIdSelector = mkSelector "getElementById:"

-- | @Selector@ for @querySelector:@
querySelectorSelector :: Selector
querySelectorSelector = mkSelector "querySelector:"

-- | @Selector@ for @querySelectorAll:@
querySelectorAllSelector :: Selector
querySelectorAllSelector = mkSelector "querySelectorAll:"

-- | @Selector@ for @URLWithAttributeString:@
urlWithAttributeStringSelector :: Selector
urlWithAttributeStringSelector = mkSelector "URLWithAttributeString:"

-- | @Selector@ for @createProcessingInstruction::@
createProcessingInstructionSelector :: Selector
createProcessingInstructionSelector = mkSelector "createProcessingInstruction::"

-- | @Selector@ for @importNode::@
importNodeSelector :: Selector
importNodeSelector = mkSelector "importNode::"

-- | @Selector@ for @createElementNS::@
createElementNSSelector :: Selector
createElementNSSelector = mkSelector "createElementNS::"

-- | @Selector@ for @createAttributeNS::@
createAttributeNSSelector :: Selector
createAttributeNSSelector = mkSelector "createAttributeNS::"

-- | @Selector@ for @getElementsByTagNameNS::@
getElementsByTagNameNSSelector :: Selector
getElementsByTagNameNSSelector = mkSelector "getElementsByTagNameNS::"

-- | @Selector@ for @createNodeIterator::::@
createNodeIteratorSelector :: Selector
createNodeIteratorSelector = mkSelector "createNodeIterator::::"

-- | @Selector@ for @createTreeWalker::::@
createTreeWalkerSelector :: Selector
createTreeWalkerSelector = mkSelector "createTreeWalker::::"

-- | @Selector@ for @getOverrideStyle::@
getOverrideStyleSelector :: Selector
getOverrideStyleSelector = mkSelector "getOverrideStyle::"

-- | @Selector@ for @createExpression::@
createExpressionSelector :: Selector
createExpressionSelector = mkSelector "createExpression::"

-- | @Selector@ for @evaluate:::::@
evaluateSelector :: Selector
evaluateSelector = mkSelector "evaluate:::::"

-- | @Selector@ for @getComputedStyle::@
getComputedStyleSelector :: Selector
getComputedStyleSelector = mkSelector "getComputedStyle::"

-- | @Selector@ for @doctype@
doctypeSelector :: Selector
doctypeSelector = mkSelector "doctype"

-- | @Selector@ for @implementation@
implementationSelector :: Selector
implementationSelector = mkSelector "implementation"

-- | @Selector@ for @documentElement@
documentElementSelector :: Selector
documentElementSelector = mkSelector "documentElement"

-- | @Selector@ for @inputEncoding@
inputEncodingSelector :: Selector
inputEncodingSelector = mkSelector "inputEncoding"

-- | @Selector@ for @xmlEncoding@
xmlEncodingSelector :: Selector
xmlEncodingSelector = mkSelector "xmlEncoding"

-- | @Selector@ for @xmlVersion@
xmlVersionSelector :: Selector
xmlVersionSelector = mkSelector "xmlVersion"

-- | @Selector@ for @setXmlVersion:@
setXmlVersionSelector :: Selector
setXmlVersionSelector = mkSelector "setXmlVersion:"

-- | @Selector@ for @xmlStandalone@
xmlStandaloneSelector :: Selector
xmlStandaloneSelector = mkSelector "xmlStandalone"

-- | @Selector@ for @setXmlStandalone:@
setXmlStandaloneSelector :: Selector
setXmlStandaloneSelector = mkSelector "setXmlStandalone:"

-- | @Selector@ for @documentURI@
documentURISelector :: Selector
documentURISelector = mkSelector "documentURI"

-- | @Selector@ for @setDocumentURI:@
setDocumentURISelector :: Selector
setDocumentURISelector = mkSelector "setDocumentURI:"

-- | @Selector@ for @defaultView@
defaultViewSelector :: Selector
defaultViewSelector = mkSelector "defaultView"

-- | @Selector@ for @styleSheets@
styleSheetsSelector :: Selector
styleSheetsSelector = mkSelector "styleSheets"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @referrer@
referrerSelector :: Selector
referrerSelector = mkSelector "referrer"

-- | @Selector@ for @domain@
domainSelector :: Selector
domainSelector = mkSelector "domain"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

-- | @Selector@ for @cookie@
cookieSelector :: Selector
cookieSelector = mkSelector "cookie"

-- | @Selector@ for @setCookie:@
setCookieSelector :: Selector
setCookieSelector = mkSelector "setCookie:"

-- | @Selector@ for @body@
bodySelector :: Selector
bodySelector = mkSelector "body"

-- | @Selector@ for @setBody:@
setBodySelector :: Selector
setBodySelector = mkSelector "setBody:"

-- | @Selector@ for @images@
imagesSelector :: Selector
imagesSelector = mkSelector "images"

-- | @Selector@ for @applets@
appletsSelector :: Selector
appletsSelector = mkSelector "applets"

-- | @Selector@ for @links@
linksSelector :: Selector
linksSelector = mkSelector "links"

-- | @Selector@ for @forms@
formsSelector :: Selector
formsSelector = mkSelector "forms"

-- | @Selector@ for @anchors@
anchorsSelector :: Selector
anchorsSelector = mkSelector "anchors"

-- | @Selector@ for @lastModified@
lastModifiedSelector :: Selector
lastModifiedSelector = mkSelector "lastModified"

-- | @Selector@ for @charset@
charsetSelector :: Selector
charsetSelector = mkSelector "charset"

-- | @Selector@ for @setCharset:@
setCharsetSelector :: Selector
setCharsetSelector = mkSelector "setCharset:"

-- | @Selector@ for @defaultCharset@
defaultCharsetSelector :: Selector
defaultCharsetSelector = mkSelector "defaultCharset"

-- | @Selector@ for @readyState@
readyStateSelector :: Selector
readyStateSelector = mkSelector "readyState"

-- | @Selector@ for @characterSet@
characterSetSelector :: Selector
characterSetSelector = mkSelector "characterSet"

-- | @Selector@ for @preferredStylesheetSet@
preferredStylesheetSetSelector :: Selector
preferredStylesheetSetSelector = mkSelector "preferredStylesheetSet"

-- | @Selector@ for @selectedStylesheetSet@
selectedStylesheetSetSelector :: Selector
selectedStylesheetSetSelector = mkSelector "selectedStylesheetSet"

-- | @Selector@ for @setSelectedStylesheetSet:@
setSelectedStylesheetSetSelector :: Selector
setSelectedStylesheetSetSelector = mkSelector "setSelectedStylesheetSet:"

-- | @Selector@ for @activeElement@
activeElementSelector :: Selector
activeElementSelector = mkSelector "activeElement"

-- | @Selector@ for @webFrame@
webFrameSelector :: Selector
webFrameSelector = mkSelector "webFrame"

