{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMImplementation@.
module ObjC.WebKit.DOMImplementation
  ( DOMImplementation
  , IsDOMImplementation(..)
  , hasFeature_version
  , createDocumentType_publicId_systemId
  , createDocument_qualifiedName_doctype
  , createCSSStyleSheet_media
  , createHTMLDocument
  , hasFeature
  , createDocumentType
  , createDocument
  , createCSSStyleSheet
  , createCSSStyleSheetSelector
  , createCSSStyleSheet_mediaSelector
  , createDocumentSelector
  , createDocumentTypeSelector
  , createDocumentType_publicId_systemIdSelector
  , createDocument_qualifiedName_doctypeSelector
  , createHTMLDocumentSelector
  , hasFeatureSelector
  , hasFeature_versionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- hasFeature:version:@
hasFeature_version :: (IsDOMImplementation domImplementation, IsNSString feature, IsNSString version) => domImplementation -> feature -> version -> IO Bool
hasFeature_version domImplementation feature version =
  sendMessage domImplementation hasFeature_versionSelector (toNSString feature) (toNSString version)

-- | @- createDocumentType:publicId:systemId:@
createDocumentType_publicId_systemId :: (IsDOMImplementation domImplementation, IsNSString qualifiedName, IsNSString publicId, IsNSString systemId) => domImplementation -> qualifiedName -> publicId -> systemId -> IO (Id DOMDocumentType)
createDocumentType_publicId_systemId domImplementation qualifiedName publicId systemId =
  sendMessage domImplementation createDocumentType_publicId_systemIdSelector (toNSString qualifiedName) (toNSString publicId) (toNSString systemId)

-- | @- createDocument:qualifiedName:doctype:@
createDocument_qualifiedName_doctype :: (IsDOMImplementation domImplementation, IsNSString namespaceURI, IsNSString qualifiedName, IsDOMDocumentType doctype) => domImplementation -> namespaceURI -> qualifiedName -> doctype -> IO (Id DOMDocument)
createDocument_qualifiedName_doctype domImplementation namespaceURI qualifiedName doctype =
  sendMessage domImplementation createDocument_qualifiedName_doctypeSelector (toNSString namespaceURI) (toNSString qualifiedName) (toDOMDocumentType doctype)

-- | @- createCSSStyleSheet:media:@
createCSSStyleSheet_media :: (IsDOMImplementation domImplementation, IsNSString title, IsNSString media) => domImplementation -> title -> media -> IO (Id DOMCSSStyleSheet)
createCSSStyleSheet_media domImplementation title media =
  sendMessage domImplementation createCSSStyleSheet_mediaSelector (toNSString title) (toNSString media)

-- | @- createHTMLDocument:@
createHTMLDocument :: (IsDOMImplementation domImplementation, IsNSString title) => domImplementation -> title -> IO (Id DOMHTMLDocument)
createHTMLDocument domImplementation title =
  sendMessage domImplementation createHTMLDocumentSelector (toNSString title)

-- | @- hasFeature::@
hasFeature :: (IsDOMImplementation domImplementation, IsNSString feature, IsNSString version) => domImplementation -> feature -> version -> IO Bool
hasFeature domImplementation feature version =
  sendMessage domImplementation hasFeatureSelector (toNSString feature) (toNSString version)

-- | @- createDocumentType:::@
createDocumentType :: (IsDOMImplementation domImplementation, IsNSString qualifiedName, IsNSString publicId, IsNSString systemId) => domImplementation -> qualifiedName -> publicId -> systemId -> IO (Id DOMDocumentType)
createDocumentType domImplementation qualifiedName publicId systemId =
  sendMessage domImplementation createDocumentTypeSelector (toNSString qualifiedName) (toNSString publicId) (toNSString systemId)

-- | @- createDocument:::@
createDocument :: (IsDOMImplementation domImplementation, IsNSString namespaceURI, IsNSString qualifiedName, IsDOMDocumentType doctype) => domImplementation -> namespaceURI -> qualifiedName -> doctype -> IO (Id DOMDocument)
createDocument domImplementation namespaceURI qualifiedName doctype =
  sendMessage domImplementation createDocumentSelector (toNSString namespaceURI) (toNSString qualifiedName) (toDOMDocumentType doctype)

-- | @- createCSSStyleSheet::@
createCSSStyleSheet :: (IsDOMImplementation domImplementation, IsNSString title, IsNSString media) => domImplementation -> title -> media -> IO (Id DOMCSSStyleSheet)
createCSSStyleSheet domImplementation title media =
  sendMessage domImplementation createCSSStyleSheetSelector (toNSString title) (toNSString media)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @hasFeature:version:@
hasFeature_versionSelector :: Selector '[Id NSString, Id NSString] Bool
hasFeature_versionSelector = mkSelector "hasFeature:version:"

-- | @Selector@ for @createDocumentType:publicId:systemId:@
createDocumentType_publicId_systemIdSelector :: Selector '[Id NSString, Id NSString, Id NSString] (Id DOMDocumentType)
createDocumentType_publicId_systemIdSelector = mkSelector "createDocumentType:publicId:systemId:"

-- | @Selector@ for @createDocument:qualifiedName:doctype:@
createDocument_qualifiedName_doctypeSelector :: Selector '[Id NSString, Id NSString, Id DOMDocumentType] (Id DOMDocument)
createDocument_qualifiedName_doctypeSelector = mkSelector "createDocument:qualifiedName:doctype:"

-- | @Selector@ for @createCSSStyleSheet:media:@
createCSSStyleSheet_mediaSelector :: Selector '[Id NSString, Id NSString] (Id DOMCSSStyleSheet)
createCSSStyleSheet_mediaSelector = mkSelector "createCSSStyleSheet:media:"

-- | @Selector@ for @createHTMLDocument:@
createHTMLDocumentSelector :: Selector '[Id NSString] (Id DOMHTMLDocument)
createHTMLDocumentSelector = mkSelector "createHTMLDocument:"

-- | @Selector@ for @hasFeature::@
hasFeatureSelector :: Selector '[Id NSString, Id NSString] Bool
hasFeatureSelector = mkSelector "hasFeature::"

-- | @Selector@ for @createDocumentType:::@
createDocumentTypeSelector :: Selector '[Id NSString, Id NSString, Id NSString] (Id DOMDocumentType)
createDocumentTypeSelector = mkSelector "createDocumentType:::"

-- | @Selector@ for @createDocument:::@
createDocumentSelector :: Selector '[Id NSString, Id NSString, Id DOMDocumentType] (Id DOMDocument)
createDocumentSelector = mkSelector "createDocument:::"

-- | @Selector@ for @createCSSStyleSheet::@
createCSSStyleSheetSelector :: Selector '[Id NSString, Id NSString] (Id DOMCSSStyleSheet)
createCSSStyleSheetSelector = mkSelector "createCSSStyleSheet::"

