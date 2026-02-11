{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithXMLString_options_errorSelector
  , initWithContentsOfURL_options_errorSelector
  , initWithData_options_errorSelector
  , initWithRootElementSelector
  , replacementClassForClassSelector
  , setRootElementSelector
  , rootElementSelector
  , insertChild_atIndexSelector
  , insertChildren_atIndexSelector
  , removeChildAtIndexSelector
  , setChildrenSelector
  , addChildSelector
  , replaceChildAtIndex_withNodeSelector
  , xmlDataWithOptionsSelector
  , objectByApplyingXSLT_arguments_errorSelector
  , objectByApplyingXSLTString_arguments_errorSelector
  , objectByApplyingXSLTAtURL_arguments_errorSelector
  , validateAndReturnErrorSelector
  , characterEncodingSelector
  , setCharacterEncodingSelector
  , versionSelector
  , setVersionSelector
  , standaloneSelector
  , setStandaloneSelector
  , documentContentKindSelector
  , setDocumentContentKindSelector
  , mimeTypeSelector
  , setMIMETypeSelector
  , dtdSelector
  , setDTDSelector
  , xmlDataSelector

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
init_ :: IsNSXMLDocument nsxmlDocument => nsxmlDocument -> IO (Id NSXMLDocument)
init_ nsxmlDocument  =
  sendMsg nsxmlDocument (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | initWithXMLString:options:error:
--
-- Returns a document created from either XML or HTML, if the HTMLTidy option is set. Parse errors are returned in error.
--
-- ObjC selector: @- initWithXMLString:options:error:@
initWithXMLString_options_error :: (IsNSXMLDocument nsxmlDocument, IsNSString string, IsNSError error_) => nsxmlDocument -> string -> NSXMLNodeOptions -> error_ -> IO (Id NSXMLDocument)
initWithXMLString_options_error nsxmlDocument  string mask error_ =
withObjCPtr string $ \raw_string ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg nsxmlDocument (mkSelector "initWithXMLString:options:error:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ()), argCULong (coerce mask), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | initWithContentsOfURL:options:error:
--
-- Returns a document created from the contents of an XML or HTML URL. Connection problems such as 404, parse errors are returned in error.
--
-- ObjC selector: @- initWithContentsOfURL:options:error:@
initWithContentsOfURL_options_error :: (IsNSXMLDocument nsxmlDocument, IsNSURL url, IsNSError error_) => nsxmlDocument -> url -> NSXMLNodeOptions -> error_ -> IO (Id NSXMLDocument)
initWithContentsOfURL_options_error nsxmlDocument  url mask error_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg nsxmlDocument (mkSelector "initWithContentsOfURL:options:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argCULong (coerce mask), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | initWithData:options:error:
--
-- Returns a document created from data. Parse errors are returned in error.
--
-- ObjC selector: @- initWithData:options:error:@
initWithData_options_error :: (IsNSXMLDocument nsxmlDocument, IsNSData data_, IsNSError error_) => nsxmlDocument -> data_ -> NSXMLNodeOptions -> error_ -> IO (Id NSXMLDocument)
initWithData_options_error nsxmlDocument  data_ mask error_ =
withObjCPtr data_ $ \raw_data_ ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg nsxmlDocument (mkSelector "initWithData:options:error:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argCULong (coerce mask), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | initWithRootElement:
--
-- Returns a document with a single child, the root element.
--
-- ObjC selector: @- initWithRootElement:@
initWithRootElement :: (IsNSXMLDocument nsxmlDocument, IsNSXMLElement element) => nsxmlDocument -> element -> IO (Id NSXMLDocument)
initWithRootElement nsxmlDocument  element =
withObjCPtr element $ \raw_element ->
    sendMsg nsxmlDocument (mkSelector "initWithRootElement:") (retPtr retVoid) [argPtr (castPtr raw_element :: Ptr ())] >>= ownedObject . castPtr

-- | @+ replacementClassForClass:@
replacementClassForClass :: Class -> IO Class
replacementClassForClass cls =
  do
    cls' <- getRequiredClass "NSXMLDocument"
    fmap (Class . castPtr) $ sendClassMsg cls' (mkSelector "replacementClassForClass:") (retPtr retVoid) [argPtr (unClass cls)]

-- | setRootElement:
--
-- Set the root element. Removes all other children including comments and processing-instructions.
--
-- ObjC selector: @- setRootElement:@
setRootElement :: (IsNSXMLDocument nsxmlDocument, IsNSXMLElement root) => nsxmlDocument -> root -> IO ()
setRootElement nsxmlDocument  root =
withObjCPtr root $ \raw_root ->
    sendMsg nsxmlDocument (mkSelector "setRootElement:") retVoid [argPtr (castPtr raw_root :: Ptr ())]

-- | rootElement
--
-- The root element.
--
-- ObjC selector: @- rootElement@
rootElement :: IsNSXMLDocument nsxmlDocument => nsxmlDocument -> IO (Id NSXMLElement)
rootElement nsxmlDocument  =
  sendMsg nsxmlDocument (mkSelector "rootElement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | insertChild:atIndex:
--
-- Inserts a child at a particular index.
--
-- ObjC selector: @- insertChild:atIndex:@
insertChild_atIndex :: (IsNSXMLDocument nsxmlDocument, IsNSXMLNode child) => nsxmlDocument -> child -> CULong -> IO ()
insertChild_atIndex nsxmlDocument  child index =
withObjCPtr child $ \raw_child ->
    sendMsg nsxmlDocument (mkSelector "insertChild:atIndex:") retVoid [argPtr (castPtr raw_child :: Ptr ()), argCULong (fromIntegral index)]

-- | insertChildren:atIndex:
--
-- Insert several children at a particular index.
--
-- ObjC selector: @- insertChildren:atIndex:@
insertChildren_atIndex :: (IsNSXMLDocument nsxmlDocument, IsNSArray children) => nsxmlDocument -> children -> CULong -> IO ()
insertChildren_atIndex nsxmlDocument  children index =
withObjCPtr children $ \raw_children ->
    sendMsg nsxmlDocument (mkSelector "insertChildren:atIndex:") retVoid [argPtr (castPtr raw_children :: Ptr ()), argCULong (fromIntegral index)]

-- | removeChildAtIndex:atIndex:
--
-- Removes a child at a particular index.
--
-- ObjC selector: @- removeChildAtIndex:@
removeChildAtIndex :: IsNSXMLDocument nsxmlDocument => nsxmlDocument -> CULong -> IO ()
removeChildAtIndex nsxmlDocument  index =
  sendMsg nsxmlDocument (mkSelector "removeChildAtIndex:") retVoid [argCULong (fromIntegral index)]

-- | setChildren:
--
-- Removes all existing children and replaces them with the new children. Set children to nil to simply remove all children.
--
-- ObjC selector: @- setChildren:@
setChildren :: (IsNSXMLDocument nsxmlDocument, IsNSArray children) => nsxmlDocument -> children -> IO ()
setChildren nsxmlDocument  children =
withObjCPtr children $ \raw_children ->
    sendMsg nsxmlDocument (mkSelector "setChildren:") retVoid [argPtr (castPtr raw_children :: Ptr ())]

-- | addChild:
--
-- Adds a child to the end of the existing children.
--
-- ObjC selector: @- addChild:@
addChild :: (IsNSXMLDocument nsxmlDocument, IsNSXMLNode child) => nsxmlDocument -> child -> IO ()
addChild nsxmlDocument  child =
withObjCPtr child $ \raw_child ->
    sendMsg nsxmlDocument (mkSelector "addChild:") retVoid [argPtr (castPtr raw_child :: Ptr ())]

-- | replaceChildAtIndex:withNode:
--
-- Replaces a child at a particular index with another child.
--
-- ObjC selector: @- replaceChildAtIndex:withNode:@
replaceChildAtIndex_withNode :: (IsNSXMLDocument nsxmlDocument, IsNSXMLNode node) => nsxmlDocument -> CULong -> node -> IO ()
replaceChildAtIndex_withNode nsxmlDocument  index node =
withObjCPtr node $ \raw_node ->
    sendMsg nsxmlDocument (mkSelector "replaceChildAtIndex:withNode:") retVoid [argCULong (fromIntegral index), argPtr (castPtr raw_node :: Ptr ())]

-- | XMLDataWithOptions:
--
-- The representation of this node as it would appear in an XML document, encoded based on characterEncoding.
--
-- ObjC selector: @- XMLDataWithOptions:@
xmlDataWithOptions :: IsNSXMLDocument nsxmlDocument => nsxmlDocument -> NSXMLNodeOptions -> IO (Id NSData)
xmlDataWithOptions nsxmlDocument  options =
  sendMsg nsxmlDocument (mkSelector "XMLDataWithOptions:") (retPtr retVoid) [argCULong (coerce options)] >>= retainedObject . castPtr

-- | objectByApplyingXSLT:arguments:error:
--
-- Applies XSLT with arguments (NSString key/value pairs) to this document, returning a new document.
--
-- ObjC selector: @- objectByApplyingXSLT:arguments:error:@
objectByApplyingXSLT_arguments_error :: (IsNSXMLDocument nsxmlDocument, IsNSData xslt, IsNSDictionary arguments, IsNSError error_) => nsxmlDocument -> xslt -> arguments -> error_ -> IO RawId
objectByApplyingXSLT_arguments_error nsxmlDocument  xslt arguments error_ =
withObjCPtr xslt $ \raw_xslt ->
  withObjCPtr arguments $ \raw_arguments ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap (RawId . castPtr) $ sendMsg nsxmlDocument (mkSelector "objectByApplyingXSLT:arguments:error:") (retPtr retVoid) [argPtr (castPtr raw_xslt :: Ptr ()), argPtr (castPtr raw_arguments :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | objectByApplyingXSLTString:arguments:error:
--
-- Applies XSLT as expressed by a string with arguments (NSString key/value pairs) to this document, returning a new document.
--
-- ObjC selector: @- objectByApplyingXSLTString:arguments:error:@
objectByApplyingXSLTString_arguments_error :: (IsNSXMLDocument nsxmlDocument, IsNSString xslt, IsNSDictionary arguments, IsNSError error_) => nsxmlDocument -> xslt -> arguments -> error_ -> IO RawId
objectByApplyingXSLTString_arguments_error nsxmlDocument  xslt arguments error_ =
withObjCPtr xslt $ \raw_xslt ->
  withObjCPtr arguments $ \raw_arguments ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap (RawId . castPtr) $ sendMsg nsxmlDocument (mkSelector "objectByApplyingXSLTString:arguments:error:") (retPtr retVoid) [argPtr (castPtr raw_xslt :: Ptr ()), argPtr (castPtr raw_arguments :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | objectByApplyingXSLTAtURL:arguments:error:
--
-- Applies the XSLT at a URL with arguments (NSString key/value pairs) to this document, returning a new document. Error may contain a connection error from the URL.
--
-- ObjC selector: @- objectByApplyingXSLTAtURL:arguments:error:@
objectByApplyingXSLTAtURL_arguments_error :: (IsNSXMLDocument nsxmlDocument, IsNSURL xsltURL, IsNSDictionary argument, IsNSError error_) => nsxmlDocument -> xsltURL -> argument -> error_ -> IO RawId
objectByApplyingXSLTAtURL_arguments_error nsxmlDocument  xsltURL argument error_ =
withObjCPtr xsltURL $ \raw_xsltURL ->
  withObjCPtr argument $ \raw_argument ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap (RawId . castPtr) $ sendMsg nsxmlDocument (mkSelector "objectByApplyingXSLTAtURL:arguments:error:") (retPtr retVoid) [argPtr (castPtr raw_xsltURL :: Ptr ()), argPtr (castPtr raw_argument :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- validateAndReturnError:@
validateAndReturnError :: (IsNSXMLDocument nsxmlDocument, IsNSError error_) => nsxmlDocument -> error_ -> IO Bool
validateAndReturnError nsxmlDocument  error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsxmlDocument (mkSelector "validateAndReturnError:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | Sets the character encoding to an IANA type.
--
-- ObjC selector: @- characterEncoding@
characterEncoding :: IsNSXMLDocument nsxmlDocument => nsxmlDocument -> IO (Id NSString)
characterEncoding nsxmlDocument  =
  sendMsg nsxmlDocument (mkSelector "characterEncoding") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Sets the character encoding to an IANA type.
--
-- ObjC selector: @- setCharacterEncoding:@
setCharacterEncoding :: (IsNSXMLDocument nsxmlDocument, IsNSString value) => nsxmlDocument -> value -> IO ()
setCharacterEncoding nsxmlDocument  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsxmlDocument (mkSelector "setCharacterEncoding:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Sets the XML version. Should be 1.0 or 1.1.
--
-- ObjC selector: @- version@
version :: IsNSXMLDocument nsxmlDocument => nsxmlDocument -> IO (Id NSString)
version nsxmlDocument  =
  sendMsg nsxmlDocument (mkSelector "version") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Sets the XML version. Should be 1.0 or 1.1.
--
-- ObjC selector: @- setVersion:@
setVersion :: (IsNSXMLDocument nsxmlDocument, IsNSString value) => nsxmlDocument -> value -> IO ()
setVersion nsxmlDocument  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsxmlDocument (mkSelector "setVersion:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Set whether this document depends on an external DTD. If this option is set the standalone declaration will appear on output.
--
-- ObjC selector: @- standalone@
standalone :: IsNSXMLDocument nsxmlDocument => nsxmlDocument -> IO Bool
standalone nsxmlDocument  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsxmlDocument (mkSelector "standalone") retCULong []

-- | Set whether this document depends on an external DTD. If this option is set the standalone declaration will appear on output.
--
-- ObjC selector: @- setStandalone:@
setStandalone :: IsNSXMLDocument nsxmlDocument => nsxmlDocument -> Bool -> IO ()
setStandalone nsxmlDocument  value =
  sendMsg nsxmlDocument (mkSelector "setStandalone:") retVoid [argCULong (if value then 1 else 0)]

-- | The kind of document.
--
-- ObjC selector: @- documentContentKind@
documentContentKind :: IsNSXMLDocument nsxmlDocument => nsxmlDocument -> IO NSXMLDocumentContentKind
documentContentKind nsxmlDocument  =
  fmap (coerce :: CULong -> NSXMLDocumentContentKind) $ sendMsg nsxmlDocument (mkSelector "documentContentKind") retCULong []

-- | The kind of document.
--
-- ObjC selector: @- setDocumentContentKind:@
setDocumentContentKind :: IsNSXMLDocument nsxmlDocument => nsxmlDocument -> NSXMLDocumentContentKind -> IO ()
setDocumentContentKind nsxmlDocument  value =
  sendMsg nsxmlDocument (mkSelector "setDocumentContentKind:") retVoid [argCULong (coerce value)]

-- | Set the MIME type, eg text/xml.
--
-- ObjC selector: @- MIMEType@
mimeType :: IsNSXMLDocument nsxmlDocument => nsxmlDocument -> IO (Id NSString)
mimeType nsxmlDocument  =
  sendMsg nsxmlDocument (mkSelector "MIMEType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Set the MIME type, eg text/xml.
--
-- ObjC selector: @- setMIMEType:@
setMIMEType :: (IsNSXMLDocument nsxmlDocument, IsNSString value) => nsxmlDocument -> value -> IO ()
setMIMEType nsxmlDocument  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsxmlDocument (mkSelector "setMIMEType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Set the associated DTD. This DTD will be output with the document.
--
-- ObjC selector: @- DTD@
dtd :: IsNSXMLDocument nsxmlDocument => nsxmlDocument -> IO (Id NSXMLDTD)
dtd nsxmlDocument  =
  sendMsg nsxmlDocument (mkSelector "DTD") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Set the associated DTD. This DTD will be output with the document.
--
-- ObjC selector: @- setDTD:@
setDTD :: (IsNSXMLDocument nsxmlDocument, IsNSXMLDTD value) => nsxmlDocument -> value -> IO ()
setDTD nsxmlDocument  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsxmlDocument (mkSelector "setDTD:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Invokes XMLDataWithOptions with NSXMLNodeOptionsNone.
--
-- ObjC selector: @- XMLData@
xmlData :: IsNSXMLDocument nsxmlDocument => nsxmlDocument -> IO (Id NSData)
xmlData nsxmlDocument  =
  sendMsg nsxmlDocument (mkSelector "XMLData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithXMLString:options:error:@
initWithXMLString_options_errorSelector :: Selector
initWithXMLString_options_errorSelector = mkSelector "initWithXMLString:options:error:"

-- | @Selector@ for @initWithContentsOfURL:options:error:@
initWithContentsOfURL_options_errorSelector :: Selector
initWithContentsOfURL_options_errorSelector = mkSelector "initWithContentsOfURL:options:error:"

-- | @Selector@ for @initWithData:options:error:@
initWithData_options_errorSelector :: Selector
initWithData_options_errorSelector = mkSelector "initWithData:options:error:"

-- | @Selector@ for @initWithRootElement:@
initWithRootElementSelector :: Selector
initWithRootElementSelector = mkSelector "initWithRootElement:"

-- | @Selector@ for @replacementClassForClass:@
replacementClassForClassSelector :: Selector
replacementClassForClassSelector = mkSelector "replacementClassForClass:"

-- | @Selector@ for @setRootElement:@
setRootElementSelector :: Selector
setRootElementSelector = mkSelector "setRootElement:"

-- | @Selector@ for @rootElement@
rootElementSelector :: Selector
rootElementSelector = mkSelector "rootElement"

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

-- | @Selector@ for @XMLDataWithOptions:@
xmlDataWithOptionsSelector :: Selector
xmlDataWithOptionsSelector = mkSelector "XMLDataWithOptions:"

-- | @Selector@ for @objectByApplyingXSLT:arguments:error:@
objectByApplyingXSLT_arguments_errorSelector :: Selector
objectByApplyingXSLT_arguments_errorSelector = mkSelector "objectByApplyingXSLT:arguments:error:"

-- | @Selector@ for @objectByApplyingXSLTString:arguments:error:@
objectByApplyingXSLTString_arguments_errorSelector :: Selector
objectByApplyingXSLTString_arguments_errorSelector = mkSelector "objectByApplyingXSLTString:arguments:error:"

-- | @Selector@ for @objectByApplyingXSLTAtURL:arguments:error:@
objectByApplyingXSLTAtURL_arguments_errorSelector :: Selector
objectByApplyingXSLTAtURL_arguments_errorSelector = mkSelector "objectByApplyingXSLTAtURL:arguments:error:"

-- | @Selector@ for @validateAndReturnError:@
validateAndReturnErrorSelector :: Selector
validateAndReturnErrorSelector = mkSelector "validateAndReturnError:"

-- | @Selector@ for @characterEncoding@
characterEncodingSelector :: Selector
characterEncodingSelector = mkSelector "characterEncoding"

-- | @Selector@ for @setCharacterEncoding:@
setCharacterEncodingSelector :: Selector
setCharacterEncodingSelector = mkSelector "setCharacterEncoding:"

-- | @Selector@ for @version@
versionSelector :: Selector
versionSelector = mkSelector "version"

-- | @Selector@ for @setVersion:@
setVersionSelector :: Selector
setVersionSelector = mkSelector "setVersion:"

-- | @Selector@ for @standalone@
standaloneSelector :: Selector
standaloneSelector = mkSelector "standalone"

-- | @Selector@ for @setStandalone:@
setStandaloneSelector :: Selector
setStandaloneSelector = mkSelector "setStandalone:"

-- | @Selector@ for @documentContentKind@
documentContentKindSelector :: Selector
documentContentKindSelector = mkSelector "documentContentKind"

-- | @Selector@ for @setDocumentContentKind:@
setDocumentContentKindSelector :: Selector
setDocumentContentKindSelector = mkSelector "setDocumentContentKind:"

-- | @Selector@ for @MIMEType@
mimeTypeSelector :: Selector
mimeTypeSelector = mkSelector "MIMEType"

-- | @Selector@ for @setMIMEType:@
setMIMETypeSelector :: Selector
setMIMETypeSelector = mkSelector "setMIMEType:"

-- | @Selector@ for @DTD@
dtdSelector :: Selector
dtdSelector = mkSelector "DTD"

-- | @Selector@ for @setDTD:@
setDTDSelector :: Selector
setDTDSelector = mkSelector "setDTD:"

-- | @Selector@ for @XMLData@
xmlDataSelector :: Selector
xmlDataSelector = mkSelector "XMLData"

