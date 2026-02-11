{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NSXMLDTDNode
--
-- The nodes that are exclusive to a DTD
--
-- Every DTD node has a name. Object value is defined as follows:		Entity declaration - the string that that entity resolves to eg "&lt;"		Attribute declaration - the default value, if any		Element declaration - the validation string		Notation declaration - no objectValue
--
-- Generated bindings for @NSXMLDTDNode@.
module ObjC.Foundation.NSXMLDTDNode
  ( NSXMLDTDNode
  , IsNSXMLDTDNode(..)
  , initWithXMLString
  , initWithKind_options
  , init_
  , dtdKind
  , setDTDKind
  , external
  , publicID
  , setPublicID
  , systemID
  , setSystemID
  , notationName
  , setNotationName
  , initWithXMLStringSelector
  , initWithKind_optionsSelector
  , initSelector
  , dtdKindSelector
  , setDTDKindSelector
  , externalSelector
  , publicIDSelector
  , setPublicIDSelector
  , systemIDSelector
  , setSystemIDSelector
  , notationNameSelector
  , setNotationNameSelector

  -- * Enum types
  , NSXMLDTDNodeKind(NSXMLDTDNodeKind)
  , pattern NSXMLEntityGeneralKind
  , pattern NSXMLEntityParsedKind
  , pattern NSXMLEntityUnparsedKind
  , pattern NSXMLEntityParameterKind
  , pattern NSXMLEntityPredefined
  , pattern NSXMLAttributeCDATAKind
  , pattern NSXMLAttributeIDKind
  , pattern NSXMLAttributeIDRefKind
  , pattern NSXMLAttributeIDRefsKind
  , pattern NSXMLAttributeEntityKind
  , pattern NSXMLAttributeEntitiesKind
  , pattern NSXMLAttributeNMTokenKind
  , pattern NSXMLAttributeNMTokensKind
  , pattern NSXMLAttributeEnumerationKind
  , pattern NSXMLAttributeNotationKind
  , pattern NSXMLElementDeclarationUndefinedKind
  , pattern NSXMLElementDeclarationEmptyKind
  , pattern NSXMLElementDeclarationAnyKind
  , pattern NSXMLElementDeclarationMixedKind
  , pattern NSXMLElementDeclarationElementKind
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

-- | initWithXMLString:
--
-- Returns an element, attribute, entity, or notation DTD node based on the full XML string.
--
-- ObjC selector: @- initWithXMLString:@
initWithXMLString :: (IsNSXMLDTDNode nsxmldtdNode, IsNSString string) => nsxmldtdNode -> string -> IO (Id NSXMLDTDNode)
initWithXMLString nsxmldtdNode  string =
withObjCPtr string $ \raw_string ->
    sendMsg nsxmldtdNode (mkSelector "initWithXMLString:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithKind:options:@
initWithKind_options :: IsNSXMLDTDNode nsxmldtdNode => nsxmldtdNode -> NSXMLNodeKind -> NSXMLNodeOptions -> IO (Id NSXMLDTDNode)
initWithKind_options nsxmldtdNode  kind options =
  sendMsg nsxmldtdNode (mkSelector "initWithKind:options:") (retPtr retVoid) [argCULong (coerce kind), argCULong (coerce options)] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsNSXMLDTDNode nsxmldtdNode => nsxmldtdNode -> IO (Id NSXMLDTDNode)
init_ nsxmldtdNode  =
  sendMsg nsxmldtdNode (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Sets the DTD sub kind.
--
-- ObjC selector: @- DTDKind@
dtdKind :: IsNSXMLDTDNode nsxmldtdNode => nsxmldtdNode -> IO NSXMLDTDNodeKind
dtdKind nsxmldtdNode  =
  fmap (coerce :: CULong -> NSXMLDTDNodeKind) $ sendMsg nsxmldtdNode (mkSelector "DTDKind") retCULong []

-- | Sets the DTD sub kind.
--
-- ObjC selector: @- setDTDKind:@
setDTDKind :: IsNSXMLDTDNode nsxmldtdNode => nsxmldtdNode -> NSXMLDTDNodeKind -> IO ()
setDTDKind nsxmldtdNode  value =
  sendMsg nsxmldtdNode (mkSelector "setDTDKind:") retVoid [argCULong (coerce value)]

-- | True if the system id is set. Valid for entities and notations.
--
-- ObjC selector: @- external@
external :: IsNSXMLDTDNode nsxmldtdNode => nsxmldtdNode -> IO Bool
external nsxmldtdNode  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsxmldtdNode (mkSelector "external") retCULong []

-- | Sets the public id. This identifier should be in the default catalog in /etc/xml/catalog or in a path specified by the environment variable XML_CATALOG_FILES. When the public id is set the system id must also be set. Valid for entities and notations.
--
-- ObjC selector: @- publicID@
publicID :: IsNSXMLDTDNode nsxmldtdNode => nsxmldtdNode -> IO (Id NSString)
publicID nsxmldtdNode  =
  sendMsg nsxmldtdNode (mkSelector "publicID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Sets the public id. This identifier should be in the default catalog in /etc/xml/catalog or in a path specified by the environment variable XML_CATALOG_FILES. When the public id is set the system id must also be set. Valid for entities and notations.
--
-- ObjC selector: @- setPublicID:@
setPublicID :: (IsNSXMLDTDNode nsxmldtdNode, IsNSString value) => nsxmldtdNode -> value -> IO ()
setPublicID nsxmldtdNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsxmldtdNode (mkSelector "setPublicID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Sets the system id. This should be a URL that points to a valid DTD. Valid for entities and notations.
--
-- ObjC selector: @- systemID@
systemID :: IsNSXMLDTDNode nsxmldtdNode => nsxmldtdNode -> IO (Id NSString)
systemID nsxmldtdNode  =
  sendMsg nsxmldtdNode (mkSelector "systemID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Sets the system id. This should be a URL that points to a valid DTD. Valid for entities and notations.
--
-- ObjC selector: @- setSystemID:@
setSystemID :: (IsNSXMLDTDNode nsxmldtdNode, IsNSString value) => nsxmldtdNode -> value -> IO ()
setSystemID nsxmldtdNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsxmldtdNode (mkSelector "setSystemID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Set the notation name. Valid for entities only.
--
-- ObjC selector: @- notationName@
notationName :: IsNSXMLDTDNode nsxmldtdNode => nsxmldtdNode -> IO (Id NSString)
notationName nsxmldtdNode  =
  sendMsg nsxmldtdNode (mkSelector "notationName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Set the notation name. Valid for entities only.
--
-- ObjC selector: @- setNotationName:@
setNotationName :: (IsNSXMLDTDNode nsxmldtdNode, IsNSString value) => nsxmldtdNode -> value -> IO ()
setNotationName nsxmldtdNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsxmldtdNode (mkSelector "setNotationName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithXMLString:@
initWithXMLStringSelector :: Selector
initWithXMLStringSelector = mkSelector "initWithXMLString:"

-- | @Selector@ for @initWithKind:options:@
initWithKind_optionsSelector :: Selector
initWithKind_optionsSelector = mkSelector "initWithKind:options:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @DTDKind@
dtdKindSelector :: Selector
dtdKindSelector = mkSelector "DTDKind"

-- | @Selector@ for @setDTDKind:@
setDTDKindSelector :: Selector
setDTDKindSelector = mkSelector "setDTDKind:"

-- | @Selector@ for @external@
externalSelector :: Selector
externalSelector = mkSelector "external"

-- | @Selector@ for @publicID@
publicIDSelector :: Selector
publicIDSelector = mkSelector "publicID"

-- | @Selector@ for @setPublicID:@
setPublicIDSelector :: Selector
setPublicIDSelector = mkSelector "setPublicID:"

-- | @Selector@ for @systemID@
systemIDSelector :: Selector
systemIDSelector = mkSelector "systemID"

-- | @Selector@ for @setSystemID:@
setSystemIDSelector :: Selector
setSystemIDSelector = mkSelector "setSystemID:"

-- | @Selector@ for @notationName@
notationNameSelector :: Selector
notationNameSelector = mkSelector "notationName"

-- | @Selector@ for @setNotationName:@
setNotationNameSelector :: Selector
setNotationNameSelector = mkSelector "setNotationName:"

