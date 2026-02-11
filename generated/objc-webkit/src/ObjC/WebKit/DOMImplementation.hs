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
  , hasFeature_versionSelector
  , createDocumentType_publicId_systemIdSelector
  , createDocument_qualifiedName_doctypeSelector
  , createCSSStyleSheet_mediaSelector
  , createHTMLDocumentSelector
  , hasFeatureSelector
  , createDocumentTypeSelector
  , createDocumentSelector
  , createCSSStyleSheetSelector


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

-- | @- hasFeature:version:@
hasFeature_version :: (IsDOMImplementation domImplementation, IsNSString feature, IsNSString version) => domImplementation -> feature -> version -> IO Bool
hasFeature_version domImplementation  feature version =
withObjCPtr feature $ \raw_feature ->
  withObjCPtr version $ \raw_version ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg domImplementation (mkSelector "hasFeature:version:") retCULong [argPtr (castPtr raw_feature :: Ptr ()), argPtr (castPtr raw_version :: Ptr ())]

-- | @- createDocumentType:publicId:systemId:@
createDocumentType_publicId_systemId :: (IsDOMImplementation domImplementation, IsNSString qualifiedName, IsNSString publicId, IsNSString systemId) => domImplementation -> qualifiedName -> publicId -> systemId -> IO (Id DOMDocumentType)
createDocumentType_publicId_systemId domImplementation  qualifiedName publicId systemId =
withObjCPtr qualifiedName $ \raw_qualifiedName ->
  withObjCPtr publicId $ \raw_publicId ->
    withObjCPtr systemId $ \raw_systemId ->
        sendMsg domImplementation (mkSelector "createDocumentType:publicId:systemId:") (retPtr retVoid) [argPtr (castPtr raw_qualifiedName :: Ptr ()), argPtr (castPtr raw_publicId :: Ptr ()), argPtr (castPtr raw_systemId :: Ptr ())] >>= retainedObject . castPtr

-- | @- createDocument:qualifiedName:doctype:@
createDocument_qualifiedName_doctype :: (IsDOMImplementation domImplementation, IsNSString namespaceURI, IsNSString qualifiedName, IsDOMDocumentType doctype) => domImplementation -> namespaceURI -> qualifiedName -> doctype -> IO (Id DOMDocument)
createDocument_qualifiedName_doctype domImplementation  namespaceURI qualifiedName doctype =
withObjCPtr namespaceURI $ \raw_namespaceURI ->
  withObjCPtr qualifiedName $ \raw_qualifiedName ->
    withObjCPtr doctype $ \raw_doctype ->
        sendMsg domImplementation (mkSelector "createDocument:qualifiedName:doctype:") (retPtr retVoid) [argPtr (castPtr raw_namespaceURI :: Ptr ()), argPtr (castPtr raw_qualifiedName :: Ptr ()), argPtr (castPtr raw_doctype :: Ptr ())] >>= retainedObject . castPtr

-- | @- createCSSStyleSheet:media:@
createCSSStyleSheet_media :: (IsDOMImplementation domImplementation, IsNSString title, IsNSString media) => domImplementation -> title -> media -> IO (Id DOMCSSStyleSheet)
createCSSStyleSheet_media domImplementation  title media =
withObjCPtr title $ \raw_title ->
  withObjCPtr media $ \raw_media ->
      sendMsg domImplementation (mkSelector "createCSSStyleSheet:media:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr raw_media :: Ptr ())] >>= retainedObject . castPtr

-- | @- createHTMLDocument:@
createHTMLDocument :: (IsDOMImplementation domImplementation, IsNSString title) => domImplementation -> title -> IO (Id DOMHTMLDocument)
createHTMLDocument domImplementation  title =
withObjCPtr title $ \raw_title ->
    sendMsg domImplementation (mkSelector "createHTMLDocument:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ())] >>= retainedObject . castPtr

-- | @- hasFeature::@
hasFeature :: (IsDOMImplementation domImplementation, IsNSString feature, IsNSString version) => domImplementation -> feature -> version -> IO Bool
hasFeature domImplementation  feature version =
withObjCPtr feature $ \raw_feature ->
  withObjCPtr version $ \raw_version ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg domImplementation (mkSelector "hasFeature::") retCULong [argPtr (castPtr raw_feature :: Ptr ()), argPtr (castPtr raw_version :: Ptr ())]

-- | @- createDocumentType:::@
createDocumentType :: (IsDOMImplementation domImplementation, IsNSString qualifiedName, IsNSString publicId, IsNSString systemId) => domImplementation -> qualifiedName -> publicId -> systemId -> IO (Id DOMDocumentType)
createDocumentType domImplementation  qualifiedName publicId systemId =
withObjCPtr qualifiedName $ \raw_qualifiedName ->
  withObjCPtr publicId $ \raw_publicId ->
    withObjCPtr systemId $ \raw_systemId ->
        sendMsg domImplementation (mkSelector "createDocumentType:::") (retPtr retVoid) [argPtr (castPtr raw_qualifiedName :: Ptr ()), argPtr (castPtr raw_publicId :: Ptr ()), argPtr (castPtr raw_systemId :: Ptr ())] >>= retainedObject . castPtr

-- | @- createDocument:::@
createDocument :: (IsDOMImplementation domImplementation, IsNSString namespaceURI, IsNSString qualifiedName, IsDOMDocumentType doctype) => domImplementation -> namespaceURI -> qualifiedName -> doctype -> IO (Id DOMDocument)
createDocument domImplementation  namespaceURI qualifiedName doctype =
withObjCPtr namespaceURI $ \raw_namespaceURI ->
  withObjCPtr qualifiedName $ \raw_qualifiedName ->
    withObjCPtr doctype $ \raw_doctype ->
        sendMsg domImplementation (mkSelector "createDocument:::") (retPtr retVoid) [argPtr (castPtr raw_namespaceURI :: Ptr ()), argPtr (castPtr raw_qualifiedName :: Ptr ()), argPtr (castPtr raw_doctype :: Ptr ())] >>= retainedObject . castPtr

-- | @- createCSSStyleSheet::@
createCSSStyleSheet :: (IsDOMImplementation domImplementation, IsNSString title, IsNSString media) => domImplementation -> title -> media -> IO (Id DOMCSSStyleSheet)
createCSSStyleSheet domImplementation  title media =
withObjCPtr title $ \raw_title ->
  withObjCPtr media $ \raw_media ->
      sendMsg domImplementation (mkSelector "createCSSStyleSheet::") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr raw_media :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @hasFeature:version:@
hasFeature_versionSelector :: Selector
hasFeature_versionSelector = mkSelector "hasFeature:version:"

-- | @Selector@ for @createDocumentType:publicId:systemId:@
createDocumentType_publicId_systemIdSelector :: Selector
createDocumentType_publicId_systemIdSelector = mkSelector "createDocumentType:publicId:systemId:"

-- | @Selector@ for @createDocument:qualifiedName:doctype:@
createDocument_qualifiedName_doctypeSelector :: Selector
createDocument_qualifiedName_doctypeSelector = mkSelector "createDocument:qualifiedName:doctype:"

-- | @Selector@ for @createCSSStyleSheet:media:@
createCSSStyleSheet_mediaSelector :: Selector
createCSSStyleSheet_mediaSelector = mkSelector "createCSSStyleSheet:media:"

-- | @Selector@ for @createHTMLDocument:@
createHTMLDocumentSelector :: Selector
createHTMLDocumentSelector = mkSelector "createHTMLDocument:"

-- | @Selector@ for @hasFeature::@
hasFeatureSelector :: Selector
hasFeatureSelector = mkSelector "hasFeature::"

-- | @Selector@ for @createDocumentType:::@
createDocumentTypeSelector :: Selector
createDocumentTypeSelector = mkSelector "createDocumentType:::"

-- | @Selector@ for @createDocument:::@
createDocumentSelector :: Selector
createDocumentSelector = mkSelector "createDocument:::"

-- | @Selector@ for @createCSSStyleSheet::@
createCSSStyleSheetSelector :: Selector
createCSSStyleSheetSelector = mkSelector "createCSSStyleSheet::"

