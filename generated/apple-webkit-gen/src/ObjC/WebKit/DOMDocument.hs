{-# LANGUAGE DataKinds #-}
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
  , activeElementSelector
  , adoptNodeSelector
  , anchorsSelector
  , appletsSelector
  , bodySelector
  , characterSetSelector
  , charsetSelector
  , cookieSelector
  , createAttributeNSSelector
  , createAttributeNS_qualifiedNameSelector
  , createAttributeSelector
  , createCDATASectionSelector
  , createCSSStyleDeclarationSelector
  , createCommentSelector
  , createDocumentFragmentSelector
  , createElementNSSelector
  , createElementNS_qualifiedNameSelector
  , createElementSelector
  , createEntityReferenceSelector
  , createEventSelector
  , createExpressionSelector
  , createExpression_resolverSelector
  , createNSResolverSelector
  , createNodeIteratorSelector
  , createNodeIterator_whatToShow_filter_expandEntityReferencesSelector
  , createProcessingInstructionSelector
  , createProcessingInstruction_dataSelector
  , createRangeSelector
  , createTextNodeSelector
  , createTreeWalkerSelector
  , createTreeWalker_whatToShow_filter_expandEntityReferencesSelector
  , defaultCharsetSelector
  , defaultViewSelector
  , doctypeSelector
  , documentElementSelector
  , documentURISelector
  , domainSelector
  , elementFromPoint_ySelector
  , evaluateSelector
  , evaluate_contextNode_resolver_type_inResultSelector
  , execCommandSelector
  , execCommand_userInterfaceSelector
  , execCommand_userInterface_valueSelector
  , formsSelector
  , getComputedStyleSelector
  , getComputedStyle_pseudoElementSelector
  , getElementByIdSelector
  , getElementsByClassNameSelector
  , getElementsByNameSelector
  , getElementsByTagNameNSSelector
  , getElementsByTagNameNS_localNameSelector
  , getElementsByTagNameSelector
  , getMatchedCSSRules_pseudoElementSelector
  , getMatchedCSSRules_pseudoElement_authorOnlySelector
  , getOverrideStyleSelector
  , getOverrideStyle_pseudoElementSelector
  , hasFocusSelector
  , imagesSelector
  , implementationSelector
  , importNodeSelector
  , importNode_deepSelector
  , inputEncodingSelector
  , lastModifiedSelector
  , linksSelector
  , preferredStylesheetSetSelector
  , queryCommandEnabledSelector
  , queryCommandIndetermSelector
  , queryCommandStateSelector
  , queryCommandSupportedSelector
  , queryCommandValueSelector
  , querySelectorAllSelector
  , querySelectorSelector
  , readyStateSelector
  , referrerSelector
  , selectedStylesheetSetSelector
  , setBodySelector
  , setCharsetSelector
  , setCookieSelector
  , setDocumentURISelector
  , setSelectedStylesheetSetSelector
  , setTitleSelector
  , setXmlStandaloneSelector
  , setXmlVersionSelector
  , styleSheetsSelector
  , titleSelector
  , urlSelector
  , urlWithAttributeStringSelector
  , webFrameSelector
  , webkitCancelFullScreenSelector
  , xmlEncodingSelector
  , xmlStandaloneSelector
  , xmlVersionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- createElement:@
createElement :: (IsDOMDocument domDocument, IsNSString tagName) => domDocument -> tagName -> IO (Id DOMElement)
createElement domDocument tagName =
  sendMessage domDocument createElementSelector (toNSString tagName)

-- | @- createDocumentFragment@
createDocumentFragment :: IsDOMDocument domDocument => domDocument -> IO (Id DOMDocumentFragment)
createDocumentFragment domDocument =
  sendMessage domDocument createDocumentFragmentSelector

-- | @- createTextNode:@
createTextNode :: (IsDOMDocument domDocument, IsNSString data_) => domDocument -> data_ -> IO (Id DOMText)
createTextNode domDocument data_ =
  sendMessage domDocument createTextNodeSelector (toNSString data_)

-- | @- createComment:@
createComment :: (IsDOMDocument domDocument, IsNSString data_) => domDocument -> data_ -> IO (Id DOMComment)
createComment domDocument data_ =
  sendMessage domDocument createCommentSelector (toNSString data_)

-- | @- createCDATASection:@
createCDATASection :: (IsDOMDocument domDocument, IsNSString data_) => domDocument -> data_ -> IO (Id DOMCDATASection)
createCDATASection domDocument data_ =
  sendMessage domDocument createCDATASectionSelector (toNSString data_)

-- | @- createProcessingInstruction:data:@
createProcessingInstruction_data :: (IsDOMDocument domDocument, IsNSString target, IsNSString data_) => domDocument -> target -> data_ -> IO (Id DOMProcessingInstruction)
createProcessingInstruction_data domDocument target data_ =
  sendMessage domDocument createProcessingInstruction_dataSelector (toNSString target) (toNSString data_)

-- | @- createAttribute:@
createAttribute :: (IsDOMDocument domDocument, IsNSString name) => domDocument -> name -> IO (Id DOMAttr)
createAttribute domDocument name =
  sendMessage domDocument createAttributeSelector (toNSString name)

-- | @- createEntityReference:@
createEntityReference :: (IsDOMDocument domDocument, IsNSString name) => domDocument -> name -> IO (Id DOMEntityReference)
createEntityReference domDocument name =
  sendMessage domDocument createEntityReferenceSelector (toNSString name)

-- | @- getElementsByTagName:@
getElementsByTagName :: (IsDOMDocument domDocument, IsNSString tagname) => domDocument -> tagname -> IO (Id DOMNodeList)
getElementsByTagName domDocument tagname =
  sendMessage domDocument getElementsByTagNameSelector (toNSString tagname)

-- | @- importNode:deep:@
importNode_deep :: (IsDOMDocument domDocument, IsDOMNode importedNode) => domDocument -> importedNode -> Bool -> IO (Id DOMNode)
importNode_deep domDocument importedNode deep =
  sendMessage domDocument importNode_deepSelector (toDOMNode importedNode) deep

-- | @- createElementNS:qualifiedName:@
createElementNS_qualifiedName :: (IsDOMDocument domDocument, IsNSString namespaceURI, IsNSString qualifiedName) => domDocument -> namespaceURI -> qualifiedName -> IO (Id DOMElement)
createElementNS_qualifiedName domDocument namespaceURI qualifiedName =
  sendMessage domDocument createElementNS_qualifiedNameSelector (toNSString namespaceURI) (toNSString qualifiedName)

-- | @- createAttributeNS:qualifiedName:@
createAttributeNS_qualifiedName :: (IsDOMDocument domDocument, IsNSString namespaceURI, IsNSString qualifiedName) => domDocument -> namespaceURI -> qualifiedName -> IO (Id DOMAttr)
createAttributeNS_qualifiedName domDocument namespaceURI qualifiedName =
  sendMessage domDocument createAttributeNS_qualifiedNameSelector (toNSString namespaceURI) (toNSString qualifiedName)

-- | @- getElementsByTagNameNS:localName:@
getElementsByTagNameNS_localName :: (IsDOMDocument domDocument, IsNSString namespaceURI, IsNSString localName) => domDocument -> namespaceURI -> localName -> IO (Id DOMNodeList)
getElementsByTagNameNS_localName domDocument namespaceURI localName =
  sendMessage domDocument getElementsByTagNameNS_localNameSelector (toNSString namespaceURI) (toNSString localName)

-- | @- adoptNode:@
adoptNode :: (IsDOMDocument domDocument, IsDOMNode source) => domDocument -> source -> IO (Id DOMNode)
adoptNode domDocument source =
  sendMessage domDocument adoptNodeSelector (toDOMNode source)

-- | @- createEvent:@
createEvent :: (IsDOMDocument domDocument, IsNSString eventType) => domDocument -> eventType -> IO (Id DOMEvent)
createEvent domDocument eventType =
  sendMessage domDocument createEventSelector (toNSString eventType)

-- | @- createRange@
createRange :: IsDOMDocument domDocument => domDocument -> IO (Id DOMRange)
createRange domDocument =
  sendMessage domDocument createRangeSelector

-- | @- createNodeIterator:whatToShow:filter:expandEntityReferences:@
createNodeIterator_whatToShow_filter_expandEntityReferences :: (IsDOMDocument domDocument, IsDOMNode root) => domDocument -> root -> CUInt -> RawId -> Bool -> IO (Id DOMNodeIterator)
createNodeIterator_whatToShow_filter_expandEntityReferences domDocument root whatToShow filter_ expandEntityReferences =
  sendMessage domDocument createNodeIterator_whatToShow_filter_expandEntityReferencesSelector (toDOMNode root) whatToShow filter_ expandEntityReferences

-- | @- createTreeWalker:whatToShow:filter:expandEntityReferences:@
createTreeWalker_whatToShow_filter_expandEntityReferences :: (IsDOMDocument domDocument, IsDOMNode root) => domDocument -> root -> CUInt -> RawId -> Bool -> IO (Id DOMTreeWalker)
createTreeWalker_whatToShow_filter_expandEntityReferences domDocument root whatToShow filter_ expandEntityReferences =
  sendMessage domDocument createTreeWalker_whatToShow_filter_expandEntityReferencesSelector (toDOMNode root) whatToShow filter_ expandEntityReferences

-- | @- getOverrideStyle:pseudoElement:@
getOverrideStyle_pseudoElement :: (IsDOMDocument domDocument, IsDOMElement element, IsNSString pseudoElement) => domDocument -> element -> pseudoElement -> IO (Id DOMCSSStyleDeclaration)
getOverrideStyle_pseudoElement domDocument element pseudoElement =
  sendMessage domDocument getOverrideStyle_pseudoElementSelector (toDOMElement element) (toNSString pseudoElement)

-- | @- createExpression:resolver:@
createExpression_resolver :: (IsDOMDocument domDocument, IsNSString expression) => domDocument -> expression -> RawId -> IO (Id DOMXPathExpression)
createExpression_resolver domDocument expression resolver =
  sendMessage domDocument createExpression_resolverSelector (toNSString expression) resolver

-- | @- createNSResolver:@
createNSResolver :: (IsDOMDocument domDocument, IsDOMNode nodeResolver) => domDocument -> nodeResolver -> IO RawId
createNSResolver domDocument nodeResolver =
  sendMessage domDocument createNSResolverSelector (toDOMNode nodeResolver)

-- | @- evaluate:contextNode:resolver:type:inResult:@
evaluate_contextNode_resolver_type_inResult :: (IsDOMDocument domDocument, IsNSString expression, IsDOMNode contextNode, IsDOMXPathResult inResult) => domDocument -> expression -> contextNode -> RawId -> CUShort -> inResult -> IO (Id DOMXPathResult)
evaluate_contextNode_resolver_type_inResult domDocument expression contextNode resolver type_ inResult =
  sendMessage domDocument evaluate_contextNode_resolver_type_inResultSelector (toNSString expression) (toDOMNode contextNode) resolver type_ (toDOMXPathResult inResult)

-- | @- execCommand:userInterface:value:@
execCommand_userInterface_value :: (IsDOMDocument domDocument, IsNSString command, IsNSString value) => domDocument -> command -> Bool -> value -> IO Bool
execCommand_userInterface_value domDocument command userInterface value =
  sendMessage domDocument execCommand_userInterface_valueSelector (toNSString command) userInterface (toNSString value)

-- | @- execCommand:userInterface:@
execCommand_userInterface :: (IsDOMDocument domDocument, IsNSString command) => domDocument -> command -> Bool -> IO Bool
execCommand_userInterface domDocument command userInterface =
  sendMessage domDocument execCommand_userInterfaceSelector (toNSString command) userInterface

-- | @- execCommand:@
execCommand :: (IsDOMDocument domDocument, IsNSString command) => domDocument -> command -> IO Bool
execCommand domDocument command =
  sendMessage domDocument execCommandSelector (toNSString command)

-- | @- queryCommandEnabled:@
queryCommandEnabled :: (IsDOMDocument domDocument, IsNSString command) => domDocument -> command -> IO Bool
queryCommandEnabled domDocument command =
  sendMessage domDocument queryCommandEnabledSelector (toNSString command)

-- | @- queryCommandIndeterm:@
queryCommandIndeterm :: (IsDOMDocument domDocument, IsNSString command) => domDocument -> command -> IO Bool
queryCommandIndeterm domDocument command =
  sendMessage domDocument queryCommandIndetermSelector (toNSString command)

-- | @- queryCommandState:@
queryCommandState :: (IsDOMDocument domDocument, IsNSString command) => domDocument -> command -> IO Bool
queryCommandState domDocument command =
  sendMessage domDocument queryCommandStateSelector (toNSString command)

-- | @- queryCommandSupported:@
queryCommandSupported :: (IsDOMDocument domDocument, IsNSString command) => domDocument -> command -> IO Bool
queryCommandSupported domDocument command =
  sendMessage domDocument queryCommandSupportedSelector (toNSString command)

-- | @- queryCommandValue:@
queryCommandValue :: (IsDOMDocument domDocument, IsNSString command) => domDocument -> command -> IO (Id NSString)
queryCommandValue domDocument command =
  sendMessage domDocument queryCommandValueSelector (toNSString command)

-- | @- getElementsByName:@
getElementsByName :: (IsDOMDocument domDocument, IsNSString elementName) => domDocument -> elementName -> IO (Id DOMNodeList)
getElementsByName domDocument elementName =
  sendMessage domDocument getElementsByNameSelector (toNSString elementName)

-- | @- elementFromPoint:y:@
elementFromPoint_y :: IsDOMDocument domDocument => domDocument -> CInt -> CInt -> IO (Id DOMElement)
elementFromPoint_y domDocument x y =
  sendMessage domDocument elementFromPoint_ySelector x y

-- | @- createCSSStyleDeclaration@
createCSSStyleDeclaration :: IsDOMDocument domDocument => domDocument -> IO (Id DOMCSSStyleDeclaration)
createCSSStyleDeclaration domDocument =
  sendMessage domDocument createCSSStyleDeclarationSelector

-- | @- getComputedStyle:pseudoElement:@
getComputedStyle_pseudoElement :: (IsDOMDocument domDocument, IsDOMElement element, IsNSString pseudoElement) => domDocument -> element -> pseudoElement -> IO (Id DOMCSSStyleDeclaration)
getComputedStyle_pseudoElement domDocument element pseudoElement =
  sendMessage domDocument getComputedStyle_pseudoElementSelector (toDOMElement element) (toNSString pseudoElement)

-- | @- getMatchedCSSRules:pseudoElement:@
getMatchedCSSRules_pseudoElement :: (IsDOMDocument domDocument, IsDOMElement element, IsNSString pseudoElement) => domDocument -> element -> pseudoElement -> IO (Id DOMCSSRuleList)
getMatchedCSSRules_pseudoElement domDocument element pseudoElement =
  sendMessage domDocument getMatchedCSSRules_pseudoElementSelector (toDOMElement element) (toNSString pseudoElement)

-- | @- getMatchedCSSRules:pseudoElement:authorOnly:@
getMatchedCSSRules_pseudoElement_authorOnly :: (IsDOMDocument domDocument, IsDOMElement element, IsNSString pseudoElement) => domDocument -> element -> pseudoElement -> Bool -> IO (Id DOMCSSRuleList)
getMatchedCSSRules_pseudoElement_authorOnly domDocument element pseudoElement authorOnly =
  sendMessage domDocument getMatchedCSSRules_pseudoElement_authorOnlySelector (toDOMElement element) (toNSString pseudoElement) authorOnly

-- | @- getElementsByClassName:@
getElementsByClassName :: (IsDOMDocument domDocument, IsNSString classNames) => domDocument -> classNames -> IO (Id DOMNodeList)
getElementsByClassName domDocument classNames =
  sendMessage domDocument getElementsByClassNameSelector (toNSString classNames)

-- | @- hasFocus@
hasFocus :: IsDOMDocument domDocument => domDocument -> IO Bool
hasFocus domDocument =
  sendMessage domDocument hasFocusSelector

-- | @- webkitCancelFullScreen@
webkitCancelFullScreen :: IsDOMDocument domDocument => domDocument -> IO ()
webkitCancelFullScreen domDocument =
  sendMessage domDocument webkitCancelFullScreenSelector

-- | @- getElementById:@
getElementById :: (IsDOMDocument domDocument, IsNSString elementId) => domDocument -> elementId -> IO (Id DOMElement)
getElementById domDocument elementId =
  sendMessage domDocument getElementByIdSelector (toNSString elementId)

-- | @- querySelector:@
querySelector :: (IsDOMDocument domDocument, IsNSString selectors) => domDocument -> selectors -> IO (Id DOMElement)
querySelector domDocument selectors =
  sendMessage domDocument querySelectorSelector (toNSString selectors)

-- | @- querySelectorAll:@
querySelectorAll :: (IsDOMDocument domDocument, IsNSString selectors) => domDocument -> selectors -> IO (Id DOMNodeList)
querySelectorAll domDocument selectors =
  sendMessage domDocument querySelectorAllSelector (toNSString selectors)

-- | URLWithAttributeString:
--
-- Constructs a URL given an attribute string.
--
-- This method constructs a URL given an attribute string just as WebKit does.     An attribute string is the value of an attribute of an element such as the href attribute on     the DOMHTMLAnchorElement class. This method is only applicable to attributes that refer to URLs.
--
-- ObjC selector: @- URLWithAttributeString:@
urlWithAttributeString :: (IsDOMDocument domDocument, IsNSString string) => domDocument -> string -> IO (Id NSURL)
urlWithAttributeString domDocument string =
  sendMessage domDocument urlWithAttributeStringSelector (toNSString string)

-- | @- createProcessingInstruction::@
createProcessingInstruction :: (IsDOMDocument domDocument, IsNSString target, IsNSString data_) => domDocument -> target -> data_ -> IO (Id DOMProcessingInstruction)
createProcessingInstruction domDocument target data_ =
  sendMessage domDocument createProcessingInstructionSelector (toNSString target) (toNSString data_)

-- | @- importNode::@
importNode :: (IsDOMDocument domDocument, IsDOMNode importedNode) => domDocument -> importedNode -> Bool -> IO (Id DOMNode)
importNode domDocument importedNode deep =
  sendMessage domDocument importNodeSelector (toDOMNode importedNode) deep

-- | @- createElementNS::@
createElementNS :: (IsDOMDocument domDocument, IsNSString namespaceURI, IsNSString qualifiedName) => domDocument -> namespaceURI -> qualifiedName -> IO (Id DOMElement)
createElementNS domDocument namespaceURI qualifiedName =
  sendMessage domDocument createElementNSSelector (toNSString namespaceURI) (toNSString qualifiedName)

-- | @- createAttributeNS::@
createAttributeNS :: (IsDOMDocument domDocument, IsNSString namespaceURI, IsNSString qualifiedName) => domDocument -> namespaceURI -> qualifiedName -> IO (Id DOMAttr)
createAttributeNS domDocument namespaceURI qualifiedName =
  sendMessage domDocument createAttributeNSSelector (toNSString namespaceURI) (toNSString qualifiedName)

-- | @- getElementsByTagNameNS::@
getElementsByTagNameNS :: (IsDOMDocument domDocument, IsNSString namespaceURI, IsNSString localName) => domDocument -> namespaceURI -> localName -> IO (Id DOMNodeList)
getElementsByTagNameNS domDocument namespaceURI localName =
  sendMessage domDocument getElementsByTagNameNSSelector (toNSString namespaceURI) (toNSString localName)

-- | @- createNodeIterator::::@
createNodeIterator :: (IsDOMDocument domDocument, IsDOMNode root) => domDocument -> root -> CUInt -> RawId -> Bool -> IO (Id DOMNodeIterator)
createNodeIterator domDocument root whatToShow filter_ expandEntityReferences =
  sendMessage domDocument createNodeIteratorSelector (toDOMNode root) whatToShow filter_ expandEntityReferences

-- | @- createTreeWalker::::@
createTreeWalker :: (IsDOMDocument domDocument, IsDOMNode root) => domDocument -> root -> CUInt -> RawId -> Bool -> IO (Id DOMTreeWalker)
createTreeWalker domDocument root whatToShow filter_ expandEntityReferences =
  sendMessage domDocument createTreeWalkerSelector (toDOMNode root) whatToShow filter_ expandEntityReferences

-- | @- getOverrideStyle::@
getOverrideStyle :: (IsDOMDocument domDocument, IsDOMElement element, IsNSString pseudoElement) => domDocument -> element -> pseudoElement -> IO (Id DOMCSSStyleDeclaration)
getOverrideStyle domDocument element pseudoElement =
  sendMessage domDocument getOverrideStyleSelector (toDOMElement element) (toNSString pseudoElement)

-- | @- createExpression::@
createExpression :: (IsDOMDocument domDocument, IsNSString expression) => domDocument -> expression -> RawId -> IO (Id DOMXPathExpression)
createExpression domDocument expression resolver =
  sendMessage domDocument createExpressionSelector (toNSString expression) resolver

-- | @- evaluate:::::@
evaluate :: (IsDOMDocument domDocument, IsNSString expression, IsDOMNode contextNode, IsDOMXPathResult inResult) => domDocument -> expression -> contextNode -> RawId -> CUShort -> inResult -> IO (Id DOMXPathResult)
evaluate domDocument expression contextNode resolver type_ inResult =
  sendMessage domDocument evaluateSelector (toNSString expression) (toDOMNode contextNode) resolver type_ (toDOMXPathResult inResult)

-- | @- getComputedStyle::@
getComputedStyle :: (IsDOMDocument domDocument, IsDOMElement element, IsNSString pseudoElement) => domDocument -> element -> pseudoElement -> IO (Id DOMCSSStyleDeclaration)
getComputedStyle domDocument element pseudoElement =
  sendMessage domDocument getComputedStyleSelector (toDOMElement element) (toNSString pseudoElement)

-- | @- doctype@
doctype :: IsDOMDocument domDocument => domDocument -> IO (Id DOMDocumentType)
doctype domDocument =
  sendMessage domDocument doctypeSelector

-- | @- implementation@
implementation :: IsDOMDocument domDocument => domDocument -> IO (Id DOMImplementation)
implementation domDocument =
  sendMessage domDocument implementationSelector

-- | @- documentElement@
documentElement :: IsDOMDocument domDocument => domDocument -> IO (Id DOMElement)
documentElement domDocument =
  sendMessage domDocument documentElementSelector

-- | @- inputEncoding@
inputEncoding :: IsDOMDocument domDocument => domDocument -> IO (Id NSString)
inputEncoding domDocument =
  sendMessage domDocument inputEncodingSelector

-- | @- xmlEncoding@
xmlEncoding :: IsDOMDocument domDocument => domDocument -> IO (Id NSString)
xmlEncoding domDocument =
  sendMessage domDocument xmlEncodingSelector

-- | @- xmlVersion@
xmlVersion :: IsDOMDocument domDocument => domDocument -> IO (Id NSString)
xmlVersion domDocument =
  sendMessage domDocument xmlVersionSelector

-- | @- setXmlVersion:@
setXmlVersion :: (IsDOMDocument domDocument, IsNSString value) => domDocument -> value -> IO ()
setXmlVersion domDocument value =
  sendMessage domDocument setXmlVersionSelector (toNSString value)

-- | @- xmlStandalone@
xmlStandalone :: IsDOMDocument domDocument => domDocument -> IO Bool
xmlStandalone domDocument =
  sendMessage domDocument xmlStandaloneSelector

-- | @- setXmlStandalone:@
setXmlStandalone :: IsDOMDocument domDocument => domDocument -> Bool -> IO ()
setXmlStandalone domDocument value =
  sendMessage domDocument setXmlStandaloneSelector value

-- | @- documentURI@
documentURI :: IsDOMDocument domDocument => domDocument -> IO (Id NSString)
documentURI domDocument =
  sendMessage domDocument documentURISelector

-- | @- setDocumentURI:@
setDocumentURI :: (IsDOMDocument domDocument, IsNSString value) => domDocument -> value -> IO ()
setDocumentURI domDocument value =
  sendMessage domDocument setDocumentURISelector (toNSString value)

-- | @- defaultView@
defaultView :: IsDOMDocument domDocument => domDocument -> IO (Id DOMAbstractView)
defaultView domDocument =
  sendMessage domDocument defaultViewSelector

-- | @- styleSheets@
styleSheets :: IsDOMDocument domDocument => domDocument -> IO (Id DOMStyleSheetList)
styleSheets domDocument =
  sendMessage domDocument styleSheetsSelector

-- | @- title@
title :: IsDOMDocument domDocument => domDocument -> IO (Id NSString)
title domDocument =
  sendMessage domDocument titleSelector

-- | @- setTitle:@
setTitle :: (IsDOMDocument domDocument, IsNSString value) => domDocument -> value -> IO ()
setTitle domDocument value =
  sendMessage domDocument setTitleSelector (toNSString value)

-- | @- referrer@
referrer :: IsDOMDocument domDocument => domDocument -> IO (Id NSString)
referrer domDocument =
  sendMessage domDocument referrerSelector

-- | @- domain@
domain :: IsDOMDocument domDocument => domDocument -> IO (Id NSString)
domain domDocument =
  sendMessage domDocument domainSelector

-- | @- URL@
url :: IsDOMDocument domDocument => domDocument -> IO (Id NSString)
url domDocument =
  sendMessage domDocument urlSelector

-- | @- cookie@
cookie :: IsDOMDocument domDocument => domDocument -> IO (Id NSString)
cookie domDocument =
  sendMessage domDocument cookieSelector

-- | @- setCookie:@
setCookie :: (IsDOMDocument domDocument, IsNSString value) => domDocument -> value -> IO ()
setCookie domDocument value =
  sendMessage domDocument setCookieSelector (toNSString value)

-- | @- body@
body :: IsDOMDocument domDocument => domDocument -> IO (Id DOMHTMLElement)
body domDocument =
  sendMessage domDocument bodySelector

-- | @- setBody:@
setBody :: (IsDOMDocument domDocument, IsDOMHTMLElement value) => domDocument -> value -> IO ()
setBody domDocument value =
  sendMessage domDocument setBodySelector (toDOMHTMLElement value)

-- | @- images@
images :: IsDOMDocument domDocument => domDocument -> IO (Id DOMHTMLCollection)
images domDocument =
  sendMessage domDocument imagesSelector

-- | @- applets@
applets :: IsDOMDocument domDocument => domDocument -> IO (Id DOMHTMLCollection)
applets domDocument =
  sendMessage domDocument appletsSelector

-- | @- links@
links :: IsDOMDocument domDocument => domDocument -> IO (Id DOMHTMLCollection)
links domDocument =
  sendMessage domDocument linksSelector

-- | @- forms@
forms :: IsDOMDocument domDocument => domDocument -> IO (Id DOMHTMLCollection)
forms domDocument =
  sendMessage domDocument formsSelector

-- | @- anchors@
anchors :: IsDOMDocument domDocument => domDocument -> IO (Id DOMHTMLCollection)
anchors domDocument =
  sendMessage domDocument anchorsSelector

-- | @- lastModified@
lastModified :: IsDOMDocument domDocument => domDocument -> IO (Id NSString)
lastModified domDocument =
  sendMessage domDocument lastModifiedSelector

-- | @- charset@
charset :: IsDOMDocument domDocument => domDocument -> IO (Id NSString)
charset domDocument =
  sendMessage domDocument charsetSelector

-- | @- setCharset:@
setCharset :: (IsDOMDocument domDocument, IsNSString value) => domDocument -> value -> IO ()
setCharset domDocument value =
  sendMessage domDocument setCharsetSelector (toNSString value)

-- | @- defaultCharset@
defaultCharset :: IsDOMDocument domDocument => domDocument -> IO (Id NSString)
defaultCharset domDocument =
  sendMessage domDocument defaultCharsetSelector

-- | @- readyState@
readyState :: IsDOMDocument domDocument => domDocument -> IO (Id NSString)
readyState domDocument =
  sendMessage domDocument readyStateSelector

-- | @- characterSet@
characterSet :: IsDOMDocument domDocument => domDocument -> IO (Id NSString)
characterSet domDocument =
  sendMessage domDocument characterSetSelector

-- | @- preferredStylesheetSet@
preferredStylesheetSet :: IsDOMDocument domDocument => domDocument -> IO (Id NSString)
preferredStylesheetSet domDocument =
  sendMessage domDocument preferredStylesheetSetSelector

-- | @- selectedStylesheetSet@
selectedStylesheetSet :: IsDOMDocument domDocument => domDocument -> IO (Id NSString)
selectedStylesheetSet domDocument =
  sendMessage domDocument selectedStylesheetSetSelector

-- | @- setSelectedStylesheetSet:@
setSelectedStylesheetSet :: (IsDOMDocument domDocument, IsNSString value) => domDocument -> value -> IO ()
setSelectedStylesheetSet domDocument value =
  sendMessage domDocument setSelectedStylesheetSetSelector (toNSString value)

-- | @- activeElement@
activeElement :: IsDOMDocument domDocument => domDocument -> IO (Id DOMElement)
activeElement domDocument =
  sendMessage domDocument activeElementSelector

-- | webFrame
--
-- The frame of the DOM document.
--
-- ObjC selector: @- webFrame@
webFrame :: IsDOMDocument domDocument => domDocument -> IO (Id WebFrame)
webFrame domDocument =
  sendMessage domDocument webFrameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @createElement:@
createElementSelector :: Selector '[Id NSString] (Id DOMElement)
createElementSelector = mkSelector "createElement:"

-- | @Selector@ for @createDocumentFragment@
createDocumentFragmentSelector :: Selector '[] (Id DOMDocumentFragment)
createDocumentFragmentSelector = mkSelector "createDocumentFragment"

-- | @Selector@ for @createTextNode:@
createTextNodeSelector :: Selector '[Id NSString] (Id DOMText)
createTextNodeSelector = mkSelector "createTextNode:"

-- | @Selector@ for @createComment:@
createCommentSelector :: Selector '[Id NSString] (Id DOMComment)
createCommentSelector = mkSelector "createComment:"

-- | @Selector@ for @createCDATASection:@
createCDATASectionSelector :: Selector '[Id NSString] (Id DOMCDATASection)
createCDATASectionSelector = mkSelector "createCDATASection:"

-- | @Selector@ for @createProcessingInstruction:data:@
createProcessingInstruction_dataSelector :: Selector '[Id NSString, Id NSString] (Id DOMProcessingInstruction)
createProcessingInstruction_dataSelector = mkSelector "createProcessingInstruction:data:"

-- | @Selector@ for @createAttribute:@
createAttributeSelector :: Selector '[Id NSString] (Id DOMAttr)
createAttributeSelector = mkSelector "createAttribute:"

-- | @Selector@ for @createEntityReference:@
createEntityReferenceSelector :: Selector '[Id NSString] (Id DOMEntityReference)
createEntityReferenceSelector = mkSelector "createEntityReference:"

-- | @Selector@ for @getElementsByTagName:@
getElementsByTagNameSelector :: Selector '[Id NSString] (Id DOMNodeList)
getElementsByTagNameSelector = mkSelector "getElementsByTagName:"

-- | @Selector@ for @importNode:deep:@
importNode_deepSelector :: Selector '[Id DOMNode, Bool] (Id DOMNode)
importNode_deepSelector = mkSelector "importNode:deep:"

-- | @Selector@ for @createElementNS:qualifiedName:@
createElementNS_qualifiedNameSelector :: Selector '[Id NSString, Id NSString] (Id DOMElement)
createElementNS_qualifiedNameSelector = mkSelector "createElementNS:qualifiedName:"

-- | @Selector@ for @createAttributeNS:qualifiedName:@
createAttributeNS_qualifiedNameSelector :: Selector '[Id NSString, Id NSString] (Id DOMAttr)
createAttributeNS_qualifiedNameSelector = mkSelector "createAttributeNS:qualifiedName:"

-- | @Selector@ for @getElementsByTagNameNS:localName:@
getElementsByTagNameNS_localNameSelector :: Selector '[Id NSString, Id NSString] (Id DOMNodeList)
getElementsByTagNameNS_localNameSelector = mkSelector "getElementsByTagNameNS:localName:"

-- | @Selector@ for @adoptNode:@
adoptNodeSelector :: Selector '[Id DOMNode] (Id DOMNode)
adoptNodeSelector = mkSelector "adoptNode:"

-- | @Selector@ for @createEvent:@
createEventSelector :: Selector '[Id NSString] (Id DOMEvent)
createEventSelector = mkSelector "createEvent:"

-- | @Selector@ for @createRange@
createRangeSelector :: Selector '[] (Id DOMRange)
createRangeSelector = mkSelector "createRange"

-- | @Selector@ for @createNodeIterator:whatToShow:filter:expandEntityReferences:@
createNodeIterator_whatToShow_filter_expandEntityReferencesSelector :: Selector '[Id DOMNode, CUInt, RawId, Bool] (Id DOMNodeIterator)
createNodeIterator_whatToShow_filter_expandEntityReferencesSelector = mkSelector "createNodeIterator:whatToShow:filter:expandEntityReferences:"

-- | @Selector@ for @createTreeWalker:whatToShow:filter:expandEntityReferences:@
createTreeWalker_whatToShow_filter_expandEntityReferencesSelector :: Selector '[Id DOMNode, CUInt, RawId, Bool] (Id DOMTreeWalker)
createTreeWalker_whatToShow_filter_expandEntityReferencesSelector = mkSelector "createTreeWalker:whatToShow:filter:expandEntityReferences:"

-- | @Selector@ for @getOverrideStyle:pseudoElement:@
getOverrideStyle_pseudoElementSelector :: Selector '[Id DOMElement, Id NSString] (Id DOMCSSStyleDeclaration)
getOverrideStyle_pseudoElementSelector = mkSelector "getOverrideStyle:pseudoElement:"

-- | @Selector@ for @createExpression:resolver:@
createExpression_resolverSelector :: Selector '[Id NSString, RawId] (Id DOMXPathExpression)
createExpression_resolverSelector = mkSelector "createExpression:resolver:"

-- | @Selector@ for @createNSResolver:@
createNSResolverSelector :: Selector '[Id DOMNode] RawId
createNSResolverSelector = mkSelector "createNSResolver:"

-- | @Selector@ for @evaluate:contextNode:resolver:type:inResult:@
evaluate_contextNode_resolver_type_inResultSelector :: Selector '[Id NSString, Id DOMNode, RawId, CUShort, Id DOMXPathResult] (Id DOMXPathResult)
evaluate_contextNode_resolver_type_inResultSelector = mkSelector "evaluate:contextNode:resolver:type:inResult:"

-- | @Selector@ for @execCommand:userInterface:value:@
execCommand_userInterface_valueSelector :: Selector '[Id NSString, Bool, Id NSString] Bool
execCommand_userInterface_valueSelector = mkSelector "execCommand:userInterface:value:"

-- | @Selector@ for @execCommand:userInterface:@
execCommand_userInterfaceSelector :: Selector '[Id NSString, Bool] Bool
execCommand_userInterfaceSelector = mkSelector "execCommand:userInterface:"

-- | @Selector@ for @execCommand:@
execCommandSelector :: Selector '[Id NSString] Bool
execCommandSelector = mkSelector "execCommand:"

-- | @Selector@ for @queryCommandEnabled:@
queryCommandEnabledSelector :: Selector '[Id NSString] Bool
queryCommandEnabledSelector = mkSelector "queryCommandEnabled:"

-- | @Selector@ for @queryCommandIndeterm:@
queryCommandIndetermSelector :: Selector '[Id NSString] Bool
queryCommandIndetermSelector = mkSelector "queryCommandIndeterm:"

-- | @Selector@ for @queryCommandState:@
queryCommandStateSelector :: Selector '[Id NSString] Bool
queryCommandStateSelector = mkSelector "queryCommandState:"

-- | @Selector@ for @queryCommandSupported:@
queryCommandSupportedSelector :: Selector '[Id NSString] Bool
queryCommandSupportedSelector = mkSelector "queryCommandSupported:"

-- | @Selector@ for @queryCommandValue:@
queryCommandValueSelector :: Selector '[Id NSString] (Id NSString)
queryCommandValueSelector = mkSelector "queryCommandValue:"

-- | @Selector@ for @getElementsByName:@
getElementsByNameSelector :: Selector '[Id NSString] (Id DOMNodeList)
getElementsByNameSelector = mkSelector "getElementsByName:"

-- | @Selector@ for @elementFromPoint:y:@
elementFromPoint_ySelector :: Selector '[CInt, CInt] (Id DOMElement)
elementFromPoint_ySelector = mkSelector "elementFromPoint:y:"

-- | @Selector@ for @createCSSStyleDeclaration@
createCSSStyleDeclarationSelector :: Selector '[] (Id DOMCSSStyleDeclaration)
createCSSStyleDeclarationSelector = mkSelector "createCSSStyleDeclaration"

-- | @Selector@ for @getComputedStyle:pseudoElement:@
getComputedStyle_pseudoElementSelector :: Selector '[Id DOMElement, Id NSString] (Id DOMCSSStyleDeclaration)
getComputedStyle_pseudoElementSelector = mkSelector "getComputedStyle:pseudoElement:"

-- | @Selector@ for @getMatchedCSSRules:pseudoElement:@
getMatchedCSSRules_pseudoElementSelector :: Selector '[Id DOMElement, Id NSString] (Id DOMCSSRuleList)
getMatchedCSSRules_pseudoElementSelector = mkSelector "getMatchedCSSRules:pseudoElement:"

-- | @Selector@ for @getMatchedCSSRules:pseudoElement:authorOnly:@
getMatchedCSSRules_pseudoElement_authorOnlySelector :: Selector '[Id DOMElement, Id NSString, Bool] (Id DOMCSSRuleList)
getMatchedCSSRules_pseudoElement_authorOnlySelector = mkSelector "getMatchedCSSRules:pseudoElement:authorOnly:"

-- | @Selector@ for @getElementsByClassName:@
getElementsByClassNameSelector :: Selector '[Id NSString] (Id DOMNodeList)
getElementsByClassNameSelector = mkSelector "getElementsByClassName:"

-- | @Selector@ for @hasFocus@
hasFocusSelector :: Selector '[] Bool
hasFocusSelector = mkSelector "hasFocus"

-- | @Selector@ for @webkitCancelFullScreen@
webkitCancelFullScreenSelector :: Selector '[] ()
webkitCancelFullScreenSelector = mkSelector "webkitCancelFullScreen"

-- | @Selector@ for @getElementById:@
getElementByIdSelector :: Selector '[Id NSString] (Id DOMElement)
getElementByIdSelector = mkSelector "getElementById:"

-- | @Selector@ for @querySelector:@
querySelectorSelector :: Selector '[Id NSString] (Id DOMElement)
querySelectorSelector = mkSelector "querySelector:"

-- | @Selector@ for @querySelectorAll:@
querySelectorAllSelector :: Selector '[Id NSString] (Id DOMNodeList)
querySelectorAllSelector = mkSelector "querySelectorAll:"

-- | @Selector@ for @URLWithAttributeString:@
urlWithAttributeStringSelector :: Selector '[Id NSString] (Id NSURL)
urlWithAttributeStringSelector = mkSelector "URLWithAttributeString:"

-- | @Selector@ for @createProcessingInstruction::@
createProcessingInstructionSelector :: Selector '[Id NSString, Id NSString] (Id DOMProcessingInstruction)
createProcessingInstructionSelector = mkSelector "createProcessingInstruction::"

-- | @Selector@ for @importNode::@
importNodeSelector :: Selector '[Id DOMNode, Bool] (Id DOMNode)
importNodeSelector = mkSelector "importNode::"

-- | @Selector@ for @createElementNS::@
createElementNSSelector :: Selector '[Id NSString, Id NSString] (Id DOMElement)
createElementNSSelector = mkSelector "createElementNS::"

-- | @Selector@ for @createAttributeNS::@
createAttributeNSSelector :: Selector '[Id NSString, Id NSString] (Id DOMAttr)
createAttributeNSSelector = mkSelector "createAttributeNS::"

-- | @Selector@ for @getElementsByTagNameNS::@
getElementsByTagNameNSSelector :: Selector '[Id NSString, Id NSString] (Id DOMNodeList)
getElementsByTagNameNSSelector = mkSelector "getElementsByTagNameNS::"

-- | @Selector@ for @createNodeIterator::::@
createNodeIteratorSelector :: Selector '[Id DOMNode, CUInt, RawId, Bool] (Id DOMNodeIterator)
createNodeIteratorSelector = mkSelector "createNodeIterator::::"

-- | @Selector@ for @createTreeWalker::::@
createTreeWalkerSelector :: Selector '[Id DOMNode, CUInt, RawId, Bool] (Id DOMTreeWalker)
createTreeWalkerSelector = mkSelector "createTreeWalker::::"

-- | @Selector@ for @getOverrideStyle::@
getOverrideStyleSelector :: Selector '[Id DOMElement, Id NSString] (Id DOMCSSStyleDeclaration)
getOverrideStyleSelector = mkSelector "getOverrideStyle::"

-- | @Selector@ for @createExpression::@
createExpressionSelector :: Selector '[Id NSString, RawId] (Id DOMXPathExpression)
createExpressionSelector = mkSelector "createExpression::"

-- | @Selector@ for @evaluate:::::@
evaluateSelector :: Selector '[Id NSString, Id DOMNode, RawId, CUShort, Id DOMXPathResult] (Id DOMXPathResult)
evaluateSelector = mkSelector "evaluate:::::"

-- | @Selector@ for @getComputedStyle::@
getComputedStyleSelector :: Selector '[Id DOMElement, Id NSString] (Id DOMCSSStyleDeclaration)
getComputedStyleSelector = mkSelector "getComputedStyle::"

-- | @Selector@ for @doctype@
doctypeSelector :: Selector '[] (Id DOMDocumentType)
doctypeSelector = mkSelector "doctype"

-- | @Selector@ for @implementation@
implementationSelector :: Selector '[] (Id DOMImplementation)
implementationSelector = mkSelector "implementation"

-- | @Selector@ for @documentElement@
documentElementSelector :: Selector '[] (Id DOMElement)
documentElementSelector = mkSelector "documentElement"

-- | @Selector@ for @inputEncoding@
inputEncodingSelector :: Selector '[] (Id NSString)
inputEncodingSelector = mkSelector "inputEncoding"

-- | @Selector@ for @xmlEncoding@
xmlEncodingSelector :: Selector '[] (Id NSString)
xmlEncodingSelector = mkSelector "xmlEncoding"

-- | @Selector@ for @xmlVersion@
xmlVersionSelector :: Selector '[] (Id NSString)
xmlVersionSelector = mkSelector "xmlVersion"

-- | @Selector@ for @setXmlVersion:@
setXmlVersionSelector :: Selector '[Id NSString] ()
setXmlVersionSelector = mkSelector "setXmlVersion:"

-- | @Selector@ for @xmlStandalone@
xmlStandaloneSelector :: Selector '[] Bool
xmlStandaloneSelector = mkSelector "xmlStandalone"

-- | @Selector@ for @setXmlStandalone:@
setXmlStandaloneSelector :: Selector '[Bool] ()
setXmlStandaloneSelector = mkSelector "setXmlStandalone:"

-- | @Selector@ for @documentURI@
documentURISelector :: Selector '[] (Id NSString)
documentURISelector = mkSelector "documentURI"

-- | @Selector@ for @setDocumentURI:@
setDocumentURISelector :: Selector '[Id NSString] ()
setDocumentURISelector = mkSelector "setDocumentURI:"

-- | @Selector@ for @defaultView@
defaultViewSelector :: Selector '[] (Id DOMAbstractView)
defaultViewSelector = mkSelector "defaultView"

-- | @Selector@ for @styleSheets@
styleSheetsSelector :: Selector '[] (Id DOMStyleSheetList)
styleSheetsSelector = mkSelector "styleSheets"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @referrer@
referrerSelector :: Selector '[] (Id NSString)
referrerSelector = mkSelector "referrer"

-- | @Selector@ for @domain@
domainSelector :: Selector '[] (Id NSString)
domainSelector = mkSelector "domain"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSString)
urlSelector = mkSelector "URL"

-- | @Selector@ for @cookie@
cookieSelector :: Selector '[] (Id NSString)
cookieSelector = mkSelector "cookie"

-- | @Selector@ for @setCookie:@
setCookieSelector :: Selector '[Id NSString] ()
setCookieSelector = mkSelector "setCookie:"

-- | @Selector@ for @body@
bodySelector :: Selector '[] (Id DOMHTMLElement)
bodySelector = mkSelector "body"

-- | @Selector@ for @setBody:@
setBodySelector :: Selector '[Id DOMHTMLElement] ()
setBodySelector = mkSelector "setBody:"

-- | @Selector@ for @images@
imagesSelector :: Selector '[] (Id DOMHTMLCollection)
imagesSelector = mkSelector "images"

-- | @Selector@ for @applets@
appletsSelector :: Selector '[] (Id DOMHTMLCollection)
appletsSelector = mkSelector "applets"

-- | @Selector@ for @links@
linksSelector :: Selector '[] (Id DOMHTMLCollection)
linksSelector = mkSelector "links"

-- | @Selector@ for @forms@
formsSelector :: Selector '[] (Id DOMHTMLCollection)
formsSelector = mkSelector "forms"

-- | @Selector@ for @anchors@
anchorsSelector :: Selector '[] (Id DOMHTMLCollection)
anchorsSelector = mkSelector "anchors"

-- | @Selector@ for @lastModified@
lastModifiedSelector :: Selector '[] (Id NSString)
lastModifiedSelector = mkSelector "lastModified"

-- | @Selector@ for @charset@
charsetSelector :: Selector '[] (Id NSString)
charsetSelector = mkSelector "charset"

-- | @Selector@ for @setCharset:@
setCharsetSelector :: Selector '[Id NSString] ()
setCharsetSelector = mkSelector "setCharset:"

-- | @Selector@ for @defaultCharset@
defaultCharsetSelector :: Selector '[] (Id NSString)
defaultCharsetSelector = mkSelector "defaultCharset"

-- | @Selector@ for @readyState@
readyStateSelector :: Selector '[] (Id NSString)
readyStateSelector = mkSelector "readyState"

-- | @Selector@ for @characterSet@
characterSetSelector :: Selector '[] (Id NSString)
characterSetSelector = mkSelector "characterSet"

-- | @Selector@ for @preferredStylesheetSet@
preferredStylesheetSetSelector :: Selector '[] (Id NSString)
preferredStylesheetSetSelector = mkSelector "preferredStylesheetSet"

-- | @Selector@ for @selectedStylesheetSet@
selectedStylesheetSetSelector :: Selector '[] (Id NSString)
selectedStylesheetSetSelector = mkSelector "selectedStylesheetSet"

-- | @Selector@ for @setSelectedStylesheetSet:@
setSelectedStylesheetSetSelector :: Selector '[Id NSString] ()
setSelectedStylesheetSetSelector = mkSelector "setSelectedStylesheetSet:"

-- | @Selector@ for @activeElement@
activeElementSelector :: Selector '[] (Id DOMElement)
activeElementSelector = mkSelector "activeElement"

-- | @Selector@ for @webFrame@
webFrameSelector :: Selector '[] (Id WebFrame)
webFrameSelector = mkSelector "webFrame"

