{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NSXMLDTD
--
-- Defines the order, repetition, and allowable values for a document
--
-- Generated bindings for @NSXMLDTD@.
module ObjC.Foundation.NSXMLDTD
  ( NSXMLDTD
  , IsNSXMLDTD(..)
  , init_
  , initWithKind_options
  , initWithContentsOfURL_options_error
  , initWithData_options_error
  , insertChild_atIndex
  , insertChildren_atIndex
  , removeChildAtIndex
  , setChildren
  , addChild
  , replaceChildAtIndex_withNode
  , entityDeclarationForName
  , notationDeclarationForName
  , elementDeclarationForName
  , attributeDeclarationForName_elementName
  , predefinedEntityDeclarationForName
  , publicID
  , setPublicID
  , systemID
  , setSystemID
  , addChildSelector
  , attributeDeclarationForName_elementNameSelector
  , elementDeclarationForNameSelector
  , entityDeclarationForNameSelector
  , initSelector
  , initWithContentsOfURL_options_errorSelector
  , initWithData_options_errorSelector
  , initWithKind_optionsSelector
  , insertChild_atIndexSelector
  , insertChildren_atIndexSelector
  , notationDeclarationForNameSelector
  , predefinedEntityDeclarationForNameSelector
  , publicIDSelector
  , removeChildAtIndexSelector
  , replaceChildAtIndex_withNodeSelector
  , setChildrenSelector
  , setPublicIDSelector
  , setSystemIDSelector
  , systemIDSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- init@
init_ :: IsNSXMLDTD nsxmldtd => nsxmldtd -> IO (Id NSXMLDTD)
init_ nsxmldtd =
  sendOwnedMessage nsxmldtd initSelector

-- | @- initWithKind:options:@
initWithKind_options :: IsNSXMLDTD nsxmldtd => nsxmldtd -> NSXMLNodeKind -> NSXMLNodeOptions -> IO (Id NSXMLDTD)
initWithKind_options nsxmldtd kind options =
  sendOwnedMessage nsxmldtd initWithKind_optionsSelector kind options

-- | @- initWithContentsOfURL:options:error:@
initWithContentsOfURL_options_error :: (IsNSXMLDTD nsxmldtd, IsNSURL url, IsNSError error_) => nsxmldtd -> url -> NSXMLNodeOptions -> error_ -> IO (Id NSXMLDTD)
initWithContentsOfURL_options_error nsxmldtd url mask error_ =
  sendOwnedMessage nsxmldtd initWithContentsOfURL_options_errorSelector (toNSURL url) mask (toNSError error_)

-- | @- initWithData:options:error:@
initWithData_options_error :: (IsNSXMLDTD nsxmldtd, IsNSData data_, IsNSError error_) => nsxmldtd -> data_ -> NSXMLNodeOptions -> error_ -> IO (Id NSXMLDTD)
initWithData_options_error nsxmldtd data_ mask error_ =
  sendOwnedMessage nsxmldtd initWithData_options_errorSelector (toNSData data_) mask (toNSError error_)

-- | insertChild:atIndex:
--
-- Inserts a child at a particular index.
--
-- ObjC selector: @- insertChild:atIndex:@
insertChild_atIndex :: (IsNSXMLDTD nsxmldtd, IsNSXMLNode child) => nsxmldtd -> child -> CULong -> IO ()
insertChild_atIndex nsxmldtd child index =
  sendMessage nsxmldtd insertChild_atIndexSelector (toNSXMLNode child) index

-- | insertChildren:atIndex:
--
-- Insert several children at a particular index.
--
-- ObjC selector: @- insertChildren:atIndex:@
insertChildren_atIndex :: (IsNSXMLDTD nsxmldtd, IsNSArray children) => nsxmldtd -> children -> CULong -> IO ()
insertChildren_atIndex nsxmldtd children index =
  sendMessage nsxmldtd insertChildren_atIndexSelector (toNSArray children) index

-- | removeChildAtIndex:
--
-- Removes a child at a particular index.
--
-- ObjC selector: @- removeChildAtIndex:@
removeChildAtIndex :: IsNSXMLDTD nsxmldtd => nsxmldtd -> CULong -> IO ()
removeChildAtIndex nsxmldtd index =
  sendMessage nsxmldtd removeChildAtIndexSelector index

-- | setChildren:
--
-- Removes all existing children and replaces them with the new children. Set children to nil to simply remove all children.
--
-- ObjC selector: @- setChildren:@
setChildren :: (IsNSXMLDTD nsxmldtd, IsNSArray children) => nsxmldtd -> children -> IO ()
setChildren nsxmldtd children =
  sendMessage nsxmldtd setChildrenSelector (toNSArray children)

-- | addChild:
--
-- Adds a child to the end of the existing children.
--
-- ObjC selector: @- addChild:@
addChild :: (IsNSXMLDTD nsxmldtd, IsNSXMLNode child) => nsxmldtd -> child -> IO ()
addChild nsxmldtd child =
  sendMessage nsxmldtd addChildSelector (toNSXMLNode child)

-- | replaceChildAtIndex:withNode:
--
-- Replaces a child at a particular index with another child.
--
-- ObjC selector: @- replaceChildAtIndex:withNode:@
replaceChildAtIndex_withNode :: (IsNSXMLDTD nsxmldtd, IsNSXMLNode node) => nsxmldtd -> CULong -> node -> IO ()
replaceChildAtIndex_withNode nsxmldtd index node =
  sendMessage nsxmldtd replaceChildAtIndex_withNodeSelector index (toNSXMLNode node)

-- | entityDeclarationForName:
--
-- Returns the entity declaration matching this name.
--
-- ObjC selector: @- entityDeclarationForName:@
entityDeclarationForName :: (IsNSXMLDTD nsxmldtd, IsNSString name) => nsxmldtd -> name -> IO (Id NSXMLDTDNode)
entityDeclarationForName nsxmldtd name =
  sendMessage nsxmldtd entityDeclarationForNameSelector (toNSString name)

-- | notationDeclarationForName:
--
-- Returns the notation declaration matching this name.
--
-- ObjC selector: @- notationDeclarationForName:@
notationDeclarationForName :: (IsNSXMLDTD nsxmldtd, IsNSString name) => nsxmldtd -> name -> IO (Id NSXMLDTDNode)
notationDeclarationForName nsxmldtd name =
  sendMessage nsxmldtd notationDeclarationForNameSelector (toNSString name)

-- | elementDeclarationForName:
--
-- Returns the element declaration matching this name.
--
-- ObjC selector: @- elementDeclarationForName:@
elementDeclarationForName :: (IsNSXMLDTD nsxmldtd, IsNSString name) => nsxmldtd -> name -> IO (Id NSXMLDTDNode)
elementDeclarationForName nsxmldtd name =
  sendMessage nsxmldtd elementDeclarationForNameSelector (toNSString name)

-- | attributeDeclarationForName:
--
-- Returns the attribute declaration matching this name.
--
-- ObjC selector: @- attributeDeclarationForName:elementName:@
attributeDeclarationForName_elementName :: (IsNSXMLDTD nsxmldtd, IsNSString name, IsNSString elementName) => nsxmldtd -> name -> elementName -> IO (Id NSXMLDTDNode)
attributeDeclarationForName_elementName nsxmldtd name elementName =
  sendMessage nsxmldtd attributeDeclarationForName_elementNameSelector (toNSString name) (toNSString elementName)

-- | predefinedEntityDeclarationForName:
--
-- Returns the predefined entity declaration matching this name.
--
-- The five predefined entities are	&lt; - <&gt; - >&amp; - &&quot; - "&apos; - &
--
-- ObjC selector: @+ predefinedEntityDeclarationForName:@
predefinedEntityDeclarationForName :: IsNSString name => name -> IO (Id NSXMLDTDNode)
predefinedEntityDeclarationForName name =
  do
    cls' <- getRequiredClass "NSXMLDTD"
    sendClassMessage cls' predefinedEntityDeclarationForNameSelector (toNSString name)

-- | Sets the public id. This identifier should be in the default catalog in /etc/xml/catalog or in a path specified by the environment variable XML_CATALOG_FILES. When the public id is set the system id must also be set.
--
-- ObjC selector: @- publicID@
publicID :: IsNSXMLDTD nsxmldtd => nsxmldtd -> IO (Id NSString)
publicID nsxmldtd =
  sendMessage nsxmldtd publicIDSelector

-- | Sets the public id. This identifier should be in the default catalog in /etc/xml/catalog or in a path specified by the environment variable XML_CATALOG_FILES. When the public id is set the system id must also be set.
--
-- ObjC selector: @- setPublicID:@
setPublicID :: (IsNSXMLDTD nsxmldtd, IsNSString value) => nsxmldtd -> value -> IO ()
setPublicID nsxmldtd value =
  sendMessage nsxmldtd setPublicIDSelector (toNSString value)

-- | Sets the system id. This should be a URL that points to a valid DTD.
--
-- ObjC selector: @- systemID@
systemID :: IsNSXMLDTD nsxmldtd => nsxmldtd -> IO (Id NSString)
systemID nsxmldtd =
  sendMessage nsxmldtd systemIDSelector

-- | Sets the system id. This should be a URL that points to a valid DTD.
--
-- ObjC selector: @- setSystemID:@
setSystemID :: (IsNSXMLDTD nsxmldtd, IsNSString value) => nsxmldtd -> value -> IO ()
setSystemID nsxmldtd value =
  sendMessage nsxmldtd setSystemIDSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSXMLDTD)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithKind:options:@
initWithKind_optionsSelector :: Selector '[NSXMLNodeKind, NSXMLNodeOptions] (Id NSXMLDTD)
initWithKind_optionsSelector = mkSelector "initWithKind:options:"

-- | @Selector@ for @initWithContentsOfURL:options:error:@
initWithContentsOfURL_options_errorSelector :: Selector '[Id NSURL, NSXMLNodeOptions, Id NSError] (Id NSXMLDTD)
initWithContentsOfURL_options_errorSelector = mkSelector "initWithContentsOfURL:options:error:"

-- | @Selector@ for @initWithData:options:error:@
initWithData_options_errorSelector :: Selector '[Id NSData, NSXMLNodeOptions, Id NSError] (Id NSXMLDTD)
initWithData_options_errorSelector = mkSelector "initWithData:options:error:"

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

-- | @Selector@ for @entityDeclarationForName:@
entityDeclarationForNameSelector :: Selector '[Id NSString] (Id NSXMLDTDNode)
entityDeclarationForNameSelector = mkSelector "entityDeclarationForName:"

-- | @Selector@ for @notationDeclarationForName:@
notationDeclarationForNameSelector :: Selector '[Id NSString] (Id NSXMLDTDNode)
notationDeclarationForNameSelector = mkSelector "notationDeclarationForName:"

-- | @Selector@ for @elementDeclarationForName:@
elementDeclarationForNameSelector :: Selector '[Id NSString] (Id NSXMLDTDNode)
elementDeclarationForNameSelector = mkSelector "elementDeclarationForName:"

-- | @Selector@ for @attributeDeclarationForName:elementName:@
attributeDeclarationForName_elementNameSelector :: Selector '[Id NSString, Id NSString] (Id NSXMLDTDNode)
attributeDeclarationForName_elementNameSelector = mkSelector "attributeDeclarationForName:elementName:"

-- | @Selector@ for @predefinedEntityDeclarationForName:@
predefinedEntityDeclarationForNameSelector :: Selector '[Id NSString] (Id NSXMLDTDNode)
predefinedEntityDeclarationForNameSelector = mkSelector "predefinedEntityDeclarationForName:"

-- | @Selector@ for @publicID@
publicIDSelector :: Selector '[] (Id NSString)
publicIDSelector = mkSelector "publicID"

-- | @Selector@ for @setPublicID:@
setPublicIDSelector :: Selector '[Id NSString] ()
setPublicIDSelector = mkSelector "setPublicID:"

-- | @Selector@ for @systemID@
systemIDSelector :: Selector '[] (Id NSString)
systemIDSelector = mkSelector "systemID"

-- | @Selector@ for @setSystemID:@
setSystemIDSelector :: Selector '[Id NSString] ()
setSystemIDSelector = mkSelector "setSystemID:"

