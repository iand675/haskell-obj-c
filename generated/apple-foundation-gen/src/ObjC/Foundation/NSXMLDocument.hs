{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NSXMLDocument
--
-- An XML Document
--
-- Note: if the application of a method would result in more than one element in the children array, an exception is thrown. Trying to add a document, namespace, attribute, or node with a parent also throws an exception. To add a node with a parent first detach or create a copy of it.
--
-- Generated bindings for @NSXMLDocument@.
module ObjC.Foundation.NSXMLDocument
  ( NSXMLDocument
  , IsNSXMLDocument(..)
  , init_
  , initWithXMLString_options_error
  , initWithContentsOfURL_options_error
  , initWithData_options_error
  , initWithRootElement
  , replacementClassForClass
  , setRootElement
  , rootElement
  , insertChild_atIndex
  , insertChildren_atIndex
  , removeChildAtIndex
  , setChildren
  , addChild
  , replaceChildAtIndex_withNode
  , xmlDataWithOptions
  , objectByApplyingXSLT_arguments_error
  , objectByApplyingXSLTString_arguments_error
  , objectByApplyingXSLTAtURL_arguments_error
  , validateAndReturnError
  , characterEncoding
  , setCharacterEncoding
  , version
  , setVersion
  , standalone
  , setStandalone
  , documentContentKind
  , setDocumentContentKind
  , mimeType
  , setMIMEType
  , dtd
  , setDTD
  , xmlData
  , addChildSelector
  , characterEncodingSelector
  , documentContentKindSelector
  , dtdSelector
  , initSelector
  , initWithContentsOfURL_options_errorSelector
  , initWithData_options_errorSelector
  , initWithRootElementSelector
  , initWithXMLString_options_errorSelector
  , insertChild_atIndexSelector
  , insertChildren_atIndexSelector
  , mimeTypeSelector
  , objectByApplyingXSLTAtURL_arguments_errorSelector
  , objectByApplyingXSLTString_arguments_errorSelector
  , objectByApplyingXSLT_arguments_errorSelector
  , removeChildAtIndexSelector
  , replaceChildAtIndex_withNodeSelector
  , replacementClassForClassSelector
  , rootElementSelector
  , setCharacterEncodingSelector
  , setChildrenSelector
  , setDTDSelector
  , setDocumentContentKindSelector
  , setMIMETypeSelector
  , setRootElementSelector
  , setStandaloneSelector
  , setVersionSelector
  , standaloneSelector
  , validateAndReturnErrorSelector
  , versionSelector
  , xmlDataSelector
  , xmlDataWithOptionsSelector

  -- * Enum types
  , NSXMLDocumentContentKind(NSXMLDocumentContentKind)
  , pattern NSXMLDocumentXMLKind
  , pattern NSXMLDocumentXHTMLKind
  , pattern NSXMLDocumentHTMLKind
  , pattern NSXMLDocumentTextKind
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
init_ :: IsNSXMLDocument nsxmlDocument => nsxmlDocument -> IO (Id NSXMLDocument)
init_ nsxmlDocument =
  sendOwnedMessage nsxmlDocument initSelector

-- | initWithXMLString:options:error:
--
-- Returns a document created from either XML or HTML, if the HTMLTidy option is set. Parse errors are returned in error.
--
-- ObjC selector: @- initWithXMLString:options:error:@
initWithXMLString_options_error :: (IsNSXMLDocument nsxmlDocument, IsNSString string, IsNSError error_) => nsxmlDocument -> string -> NSXMLNodeOptions -> error_ -> IO (Id NSXMLDocument)
initWithXMLString_options_error nsxmlDocument string mask error_ =
  sendOwnedMessage nsxmlDocument initWithXMLString_options_errorSelector (toNSString string) mask (toNSError error_)

-- | initWithContentsOfURL:options:error:
--
-- Returns a document created from the contents of an XML or HTML URL. Connection problems such as 404, parse errors are returned in error.
--
-- ObjC selector: @- initWithContentsOfURL:options:error:@
initWithContentsOfURL_options_error :: (IsNSXMLDocument nsxmlDocument, IsNSURL url, IsNSError error_) => nsxmlDocument -> url -> NSXMLNodeOptions -> error_ -> IO (Id NSXMLDocument)
initWithContentsOfURL_options_error nsxmlDocument url mask error_ =
  sendOwnedMessage nsxmlDocument initWithContentsOfURL_options_errorSelector (toNSURL url) mask (toNSError error_)

-- | initWithData:options:error:
--
-- Returns a document created from data. Parse errors are returned in error.
--
-- ObjC selector: @- initWithData:options:error:@
initWithData_options_error :: (IsNSXMLDocument nsxmlDocument, IsNSData data_, IsNSError error_) => nsxmlDocument -> data_ -> NSXMLNodeOptions -> error_ -> IO (Id NSXMLDocument)
initWithData_options_error nsxmlDocument data_ mask error_ =
  sendOwnedMessage nsxmlDocument initWithData_options_errorSelector (toNSData data_) mask (toNSError error_)

-- | initWithRootElement:
--
-- Returns a document with a single child, the root element.
--
-- ObjC selector: @- initWithRootElement:@
initWithRootElement :: (IsNSXMLDocument nsxmlDocument, IsNSXMLElement element) => nsxmlDocument -> element -> IO (Id NSXMLDocument)
initWithRootElement nsxmlDocument element =
  sendOwnedMessage nsxmlDocument initWithRootElementSelector (toNSXMLElement element)

-- | @+ replacementClassForClass:@
replacementClassForClass :: Class -> IO Class
replacementClassForClass cls =
  do
    cls' <- getRequiredClass "NSXMLDocument"
    sendClassMessage cls' replacementClassForClassSelector cls

-- | setRootElement:
--
-- Set the root element. Removes all other children including comments and processing-instructions.
--
-- ObjC selector: @- setRootElement:@
setRootElement :: (IsNSXMLDocument nsxmlDocument, IsNSXMLElement root) => nsxmlDocument -> root -> IO ()
setRootElement nsxmlDocument root =
  sendMessage nsxmlDocument setRootElementSelector (toNSXMLElement root)

-- | rootElement
--
-- The root element.
--
-- ObjC selector: @- rootElement@
rootElement :: IsNSXMLDocument nsxmlDocument => nsxmlDocument -> IO (Id NSXMLElement)
rootElement nsxmlDocument =
  sendMessage nsxmlDocument rootElementSelector

-- | insertChild:atIndex:
--
-- Inserts a child at a particular index.
--
-- ObjC selector: @- insertChild:atIndex:@
insertChild_atIndex :: (IsNSXMLDocument nsxmlDocument, IsNSXMLNode child) => nsxmlDocument -> child -> CULong -> IO ()
insertChild_atIndex nsxmlDocument child index =
  sendMessage nsxmlDocument insertChild_atIndexSelector (toNSXMLNode child) index

-- | insertChildren:atIndex:
--
-- Insert several children at a particular index.
--
-- ObjC selector: @- insertChildren:atIndex:@
insertChildren_atIndex :: (IsNSXMLDocument nsxmlDocument, IsNSArray children) => nsxmlDocument -> children -> CULong -> IO ()
insertChildren_atIndex nsxmlDocument children index =
  sendMessage nsxmlDocument insertChildren_atIndexSelector (toNSArray children) index

-- | removeChildAtIndex:atIndex:
--
-- Removes a child at a particular index.
--
-- ObjC selector: @- removeChildAtIndex:@
removeChildAtIndex :: IsNSXMLDocument nsxmlDocument => nsxmlDocument -> CULong -> IO ()
removeChildAtIndex nsxmlDocument index =
  sendMessage nsxmlDocument removeChildAtIndexSelector index

-- | setChildren:
--
-- Removes all existing children and replaces them with the new children. Set children to nil to simply remove all children.
--
-- ObjC selector: @- setChildren:@
setChildren :: (IsNSXMLDocument nsxmlDocument, IsNSArray children) => nsxmlDocument -> children -> IO ()
setChildren nsxmlDocument children =
  sendMessage nsxmlDocument setChildrenSelector (toNSArray children)

-- | addChild:
--
-- Adds a child to the end of the existing children.
--
-- ObjC selector: @- addChild:@
addChild :: (IsNSXMLDocument nsxmlDocument, IsNSXMLNode child) => nsxmlDocument -> child -> IO ()
addChild nsxmlDocument child =
  sendMessage nsxmlDocument addChildSelector (toNSXMLNode child)

-- | replaceChildAtIndex:withNode:
--
-- Replaces a child at a particular index with another child.
--
-- ObjC selector: @- replaceChildAtIndex:withNode:@
replaceChildAtIndex_withNode :: (IsNSXMLDocument nsxmlDocument, IsNSXMLNode node) => nsxmlDocument -> CULong -> node -> IO ()
replaceChildAtIndex_withNode nsxmlDocument index node =
  sendMessage nsxmlDocument replaceChildAtIndex_withNodeSelector index (toNSXMLNode node)

-- | XMLDataWithOptions:
--
-- The representation of this node as it would appear in an XML document, encoded based on characterEncoding.
--
-- ObjC selector: @- XMLDataWithOptions:@
xmlDataWithOptions :: IsNSXMLDocument nsxmlDocument => nsxmlDocument -> NSXMLNodeOptions -> IO (Id NSData)
xmlDataWithOptions nsxmlDocument options =
  sendMessage nsxmlDocument xmlDataWithOptionsSelector options

-- | objectByApplyingXSLT:arguments:error:
--
-- Applies XSLT with arguments (NSString key/value pairs) to this document, returning a new document.
--
-- ObjC selector: @- objectByApplyingXSLT:arguments:error:@
objectByApplyingXSLT_arguments_error :: (IsNSXMLDocument nsxmlDocument, IsNSData xslt, IsNSDictionary arguments, IsNSError error_) => nsxmlDocument -> xslt -> arguments -> error_ -> IO RawId
objectByApplyingXSLT_arguments_error nsxmlDocument xslt arguments error_ =
  sendMessage nsxmlDocument objectByApplyingXSLT_arguments_errorSelector (toNSData xslt) (toNSDictionary arguments) (toNSError error_)

-- | objectByApplyingXSLTString:arguments:error:
--
-- Applies XSLT as expressed by a string with arguments (NSString key/value pairs) to this document, returning a new document.
--
-- ObjC selector: @- objectByApplyingXSLTString:arguments:error:@
objectByApplyingXSLTString_arguments_error :: (IsNSXMLDocument nsxmlDocument, IsNSString xslt, IsNSDictionary arguments, IsNSError error_) => nsxmlDocument -> xslt -> arguments -> error_ -> IO RawId
objectByApplyingXSLTString_arguments_error nsxmlDocument xslt arguments error_ =
  sendMessage nsxmlDocument objectByApplyingXSLTString_arguments_errorSelector (toNSString xslt) (toNSDictionary arguments) (toNSError error_)

-- | objectByApplyingXSLTAtURL:arguments:error:
--
-- Applies the XSLT at a URL with arguments (NSString key/value pairs) to this document, returning a new document. Error may contain a connection error from the URL.
--
-- ObjC selector: @- objectByApplyingXSLTAtURL:arguments:error:@
objectByApplyingXSLTAtURL_arguments_error :: (IsNSXMLDocument nsxmlDocument, IsNSURL xsltURL, IsNSDictionary argument, IsNSError error_) => nsxmlDocument -> xsltURL -> argument -> error_ -> IO RawId
objectByApplyingXSLTAtURL_arguments_error nsxmlDocument xsltURL argument error_ =
  sendMessage nsxmlDocument objectByApplyingXSLTAtURL_arguments_errorSelector (toNSURL xsltURL) (toNSDictionary argument) (toNSError error_)

-- | @- validateAndReturnError:@
validateAndReturnError :: (IsNSXMLDocument nsxmlDocument, IsNSError error_) => nsxmlDocument -> error_ -> IO Bool
validateAndReturnError nsxmlDocument error_ =
  sendMessage nsxmlDocument validateAndReturnErrorSelector (toNSError error_)

-- | Sets the character encoding to an IANA type.
--
-- ObjC selector: @- characterEncoding@
characterEncoding :: IsNSXMLDocument nsxmlDocument => nsxmlDocument -> IO (Id NSString)
characterEncoding nsxmlDocument =
  sendMessage nsxmlDocument characterEncodingSelector

-- | Sets the character encoding to an IANA type.
--
-- ObjC selector: @- setCharacterEncoding:@
setCharacterEncoding :: (IsNSXMLDocument nsxmlDocument, IsNSString value) => nsxmlDocument -> value -> IO ()
setCharacterEncoding nsxmlDocument value =
  sendMessage nsxmlDocument setCharacterEncodingSelector (toNSString value)

-- | Sets the XML version. Should be 1.0 or 1.1.
--
-- ObjC selector: @- version@
version :: IsNSXMLDocument nsxmlDocument => nsxmlDocument -> IO (Id NSString)
version nsxmlDocument =
  sendMessage nsxmlDocument versionSelector

-- | Sets the XML version. Should be 1.0 or 1.1.
--
-- ObjC selector: @- setVersion:@
setVersion :: (IsNSXMLDocument nsxmlDocument, IsNSString value) => nsxmlDocument -> value -> IO ()
setVersion nsxmlDocument value =
  sendMessage nsxmlDocument setVersionSelector (toNSString value)

-- | Set whether this document depends on an external DTD. If this option is set the standalone declaration will appear on output.
--
-- ObjC selector: @- standalone@
standalone :: IsNSXMLDocument nsxmlDocument => nsxmlDocument -> IO Bool
standalone nsxmlDocument =
  sendMessage nsxmlDocument standaloneSelector

-- | Set whether this document depends on an external DTD. If this option is set the standalone declaration will appear on output.
--
-- ObjC selector: @- setStandalone:@
setStandalone :: IsNSXMLDocument nsxmlDocument => nsxmlDocument -> Bool -> IO ()
setStandalone nsxmlDocument value =
  sendMessage nsxmlDocument setStandaloneSelector value

-- | The kind of document.
--
-- ObjC selector: @- documentContentKind@
documentContentKind :: IsNSXMLDocument nsxmlDocument => nsxmlDocument -> IO NSXMLDocumentContentKind
documentContentKind nsxmlDocument =
  sendMessage nsxmlDocument documentContentKindSelector

-- | The kind of document.
--
-- ObjC selector: @- setDocumentContentKind:@
setDocumentContentKind :: IsNSXMLDocument nsxmlDocument => nsxmlDocument -> NSXMLDocumentContentKind -> IO ()
setDocumentContentKind nsxmlDocument value =
  sendMessage nsxmlDocument setDocumentContentKindSelector value

-- | Set the MIME type, eg text/xml.
--
-- ObjC selector: @- MIMEType@
mimeType :: IsNSXMLDocument nsxmlDocument => nsxmlDocument -> IO (Id NSString)
mimeType nsxmlDocument =
  sendMessage nsxmlDocument mimeTypeSelector

-- | Set the MIME type, eg text/xml.
--
-- ObjC selector: @- setMIMEType:@
setMIMEType :: (IsNSXMLDocument nsxmlDocument, IsNSString value) => nsxmlDocument -> value -> IO ()
setMIMEType nsxmlDocument value =
  sendMessage nsxmlDocument setMIMETypeSelector (toNSString value)

-- | Set the associated DTD. This DTD will be output with the document.
--
-- ObjC selector: @- DTD@
dtd :: IsNSXMLDocument nsxmlDocument => nsxmlDocument -> IO (Id NSXMLDTD)
dtd nsxmlDocument =
  sendMessage nsxmlDocument dtdSelector

-- | Set the associated DTD. This DTD will be output with the document.
--
-- ObjC selector: @- setDTD:@
setDTD :: (IsNSXMLDocument nsxmlDocument, IsNSXMLDTD value) => nsxmlDocument -> value -> IO ()
setDTD nsxmlDocument value =
  sendMessage nsxmlDocument setDTDSelector (toNSXMLDTD value)

-- | Invokes XMLDataWithOptions with NSXMLNodeOptionsNone.
--
-- ObjC selector: @- XMLData@
xmlData :: IsNSXMLDocument nsxmlDocument => nsxmlDocument -> IO (Id NSData)
xmlData nsxmlDocument =
  sendMessage nsxmlDocument xmlDataSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSXMLDocument)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithXMLString:options:error:@
initWithXMLString_options_errorSelector :: Selector '[Id NSString, NSXMLNodeOptions, Id NSError] (Id NSXMLDocument)
initWithXMLString_options_errorSelector = mkSelector "initWithXMLString:options:error:"

-- | @Selector@ for @initWithContentsOfURL:options:error:@
initWithContentsOfURL_options_errorSelector :: Selector '[Id NSURL, NSXMLNodeOptions, Id NSError] (Id NSXMLDocument)
initWithContentsOfURL_options_errorSelector = mkSelector "initWithContentsOfURL:options:error:"

-- | @Selector@ for @initWithData:options:error:@
initWithData_options_errorSelector :: Selector '[Id NSData, NSXMLNodeOptions, Id NSError] (Id NSXMLDocument)
initWithData_options_errorSelector = mkSelector "initWithData:options:error:"

-- | @Selector@ for @initWithRootElement:@
initWithRootElementSelector :: Selector '[Id NSXMLElement] (Id NSXMLDocument)
initWithRootElementSelector = mkSelector "initWithRootElement:"

-- | @Selector@ for @replacementClassForClass:@
replacementClassForClassSelector :: Selector '[Class] Class
replacementClassForClassSelector = mkSelector "replacementClassForClass:"

-- | @Selector@ for @setRootElement:@
setRootElementSelector :: Selector '[Id NSXMLElement] ()
setRootElementSelector = mkSelector "setRootElement:"

-- | @Selector@ for @rootElement@
rootElementSelector :: Selector '[] (Id NSXMLElement)
rootElementSelector = mkSelector "rootElement"

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

-- | @Selector@ for @XMLDataWithOptions:@
xmlDataWithOptionsSelector :: Selector '[NSXMLNodeOptions] (Id NSData)
xmlDataWithOptionsSelector = mkSelector "XMLDataWithOptions:"

-- | @Selector@ for @objectByApplyingXSLT:arguments:error:@
objectByApplyingXSLT_arguments_errorSelector :: Selector '[Id NSData, Id NSDictionary, Id NSError] RawId
objectByApplyingXSLT_arguments_errorSelector = mkSelector "objectByApplyingXSLT:arguments:error:"

-- | @Selector@ for @objectByApplyingXSLTString:arguments:error:@
objectByApplyingXSLTString_arguments_errorSelector :: Selector '[Id NSString, Id NSDictionary, Id NSError] RawId
objectByApplyingXSLTString_arguments_errorSelector = mkSelector "objectByApplyingXSLTString:arguments:error:"

-- | @Selector@ for @objectByApplyingXSLTAtURL:arguments:error:@
objectByApplyingXSLTAtURL_arguments_errorSelector :: Selector '[Id NSURL, Id NSDictionary, Id NSError] RawId
objectByApplyingXSLTAtURL_arguments_errorSelector = mkSelector "objectByApplyingXSLTAtURL:arguments:error:"

-- | @Selector@ for @validateAndReturnError:@
validateAndReturnErrorSelector :: Selector '[Id NSError] Bool
validateAndReturnErrorSelector = mkSelector "validateAndReturnError:"

-- | @Selector@ for @characterEncoding@
characterEncodingSelector :: Selector '[] (Id NSString)
characterEncodingSelector = mkSelector "characterEncoding"

-- | @Selector@ for @setCharacterEncoding:@
setCharacterEncodingSelector :: Selector '[Id NSString] ()
setCharacterEncodingSelector = mkSelector "setCharacterEncoding:"

-- | @Selector@ for @version@
versionSelector :: Selector '[] (Id NSString)
versionSelector = mkSelector "version"

-- | @Selector@ for @setVersion:@
setVersionSelector :: Selector '[Id NSString] ()
setVersionSelector = mkSelector "setVersion:"

-- | @Selector@ for @standalone@
standaloneSelector :: Selector '[] Bool
standaloneSelector = mkSelector "standalone"

-- | @Selector@ for @setStandalone:@
setStandaloneSelector :: Selector '[Bool] ()
setStandaloneSelector = mkSelector "setStandalone:"

-- | @Selector@ for @documentContentKind@
documentContentKindSelector :: Selector '[] NSXMLDocumentContentKind
documentContentKindSelector = mkSelector "documentContentKind"

-- | @Selector@ for @setDocumentContentKind:@
setDocumentContentKindSelector :: Selector '[NSXMLDocumentContentKind] ()
setDocumentContentKindSelector = mkSelector "setDocumentContentKind:"

-- | @Selector@ for @MIMEType@
mimeTypeSelector :: Selector '[] (Id NSString)
mimeTypeSelector = mkSelector "MIMEType"

-- | @Selector@ for @setMIMEType:@
setMIMETypeSelector :: Selector '[Id NSString] ()
setMIMETypeSelector = mkSelector "setMIMEType:"

-- | @Selector@ for @DTD@
dtdSelector :: Selector '[] (Id NSXMLDTD)
dtdSelector = mkSelector "DTD"

-- | @Selector@ for @setDTD:@
setDTDSelector :: Selector '[Id NSXMLDTD] ()
setDTDSelector = mkSelector "setDTD:"

-- | @Selector@ for @XMLData@
xmlDataSelector :: Selector '[] (Id NSData)
xmlDataSelector = mkSelector "XMLData"

