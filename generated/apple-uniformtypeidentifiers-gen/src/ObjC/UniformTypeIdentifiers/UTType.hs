{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class representing a type in a type hierarchy.
--
-- Types may represent files on disk, abstract data types with no on-disk	representation, or even entirely unrelated hierarchical classification	systems such as hardware.
--
-- Older API that does not use @UTType@ typically uses an untyped @NSString@	or @CFStringRef@ to refer to a type by its identifier. To get the	identifier of a type for use with these APIs, use the @identifier@ property	of this class.
--
-- https://developer.apple.com/library/archive/documentation/FileManagement/Conceptual/understanding_utis/
--
-- Generated bindings for @UTType@.
module ObjC.UniformTypeIdentifiers.UTType
  ( UTType
  , IsUTType(..)
  , new
  , init_
  , typeWithIdentifier
  , typeWithFilenameExtension
  , typeWithFilenameExtension_conformingToType
  , typeWithMIMEType
  , typeWithMIMEType_conformingToType
  , exportedTypeWithIdentifier
  , exportedTypeWithIdentifier_conformingToType
  , importedTypeWithIdentifier
  , importedTypeWithIdentifier_conformingToType
  , typeWithTag_tagClass_conformingToType
  , typesWithTag_tagClass_conformingToType
  , conformsToType
  , isSupertypeOfType
  , isSubtypeOfType
  , identifier
  , preferredFilenameExtension
  , preferredMIMEType
  , localizedDescription
  , version
  , referenceURL
  , dynamic
  , declared
  , publicType
  , tags
  , supertypes
  , conformsToTypeSelector
  , declaredSelector
  , dynamicSelector
  , exportedTypeWithIdentifierSelector
  , exportedTypeWithIdentifier_conformingToTypeSelector
  , identifierSelector
  , importedTypeWithIdentifierSelector
  , importedTypeWithIdentifier_conformingToTypeSelector
  , initSelector
  , isSubtypeOfTypeSelector
  , isSupertypeOfTypeSelector
  , localizedDescriptionSelector
  , newSelector
  , preferredFilenameExtensionSelector
  , preferredMIMETypeSelector
  , publicTypeSelector
  , referenceURLSelector
  , supertypesSelector
  , tagsSelector
  , typeWithFilenameExtensionSelector
  , typeWithFilenameExtension_conformingToTypeSelector
  , typeWithIdentifierSelector
  , typeWithMIMETypeSelector
  , typeWithMIMEType_conformingToTypeSelector
  , typeWithTag_tagClass_conformingToTypeSelector
  , typesWithTag_tagClass_conformingToTypeSelector
  , versionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.UniformTypeIdentifiers.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id UTType)
new  =
  do
    cls' <- getRequiredClass "UTType"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsUTType utType => utType -> IO (Id UTType)
init_ utType =
  sendOwnedMessage utType initSelector

-- | Create a type given a type identifier.
--
-- @identifier@ — The type identifier.
--
-- Returns: A type, or @nil@ if the type identifier is not known to the system.
--
-- ObjC selector: @+ typeWithIdentifier:@
typeWithIdentifier :: IsNSString identifier => identifier -> IO (Id UTType)
typeWithIdentifier identifier =
  do
    cls' <- getRequiredClass "UTType"
    sendClassMessage cls' typeWithIdentifierSelector (toNSString identifier)

-- | Create a type given a filename extension that conforms to		@UTTypeData.@
--
-- @filenameExtension@ — The filename extension for which a type is desired.
--
-- Returns: A type. If no types are known to the system with the specified		filename extension and conformance but the inputs were otherwise valid,		a dynamic type may be provided. If the inputs were not valid, returns		@nil.@
--
-- This method is equivalent to:
--
-- [UTType typeWithTag:filenameExtension tagClass:UTTagClassFilenameExtension conformingToType:UTTypeData]
--
-- To get the type of a file on disk, use the @NSURLContentTypeKey@ property.	You should not attempt to derive the type of a file system object based	solely on its path extension.
--
-- ObjC selector: @+ typeWithFilenameExtension:@
typeWithFilenameExtension :: IsNSString filenameExtension => filenameExtension -> IO (Id UTType)
typeWithFilenameExtension filenameExtension =
  do
    cls' <- getRequiredClass "UTType"
    sendClassMessage cls' typeWithFilenameExtensionSelector (toNSString filenameExtension)

-- | Create a type given a filename extension.
--
-- @filenameExtension@ — The filename extension for which a type is desired.
--
-- @supertype@ — Another type that the resulting type must conform to.		 Typically, you would pass @UTTypeData@ or @UTTypePackage.@
--
-- Returns: A type. If no types are known to the system with the specified		filename extension and conformance but the inputs were otherwise valid,		a dynamic type may be provided. If the inputs were not valid, returns		@nil.@
--
-- This method is equivalent to:
--
-- [UTType typeWithTag:filenameExtension tagClass:UTTagClassFilenameExtension conformingToType:supertype]
--
-- To get the type of a file on disk, use the @NSURLContentTypeKey@ property.	You should not attempt to derive the type of a file system object based	solely on its path extension.
--
-- ObjC selector: @+ typeWithFilenameExtension:conformingToType:@
typeWithFilenameExtension_conformingToType :: (IsNSString filenameExtension, IsUTType supertype) => filenameExtension -> supertype -> IO (Id UTType)
typeWithFilenameExtension_conformingToType filenameExtension supertype =
  do
    cls' <- getRequiredClass "UTType"
    sendClassMessage cls' typeWithFilenameExtension_conformingToTypeSelector (toNSString filenameExtension) (toUTType supertype)

-- | Create a type given a MIME type that conforms to @UTTypeData.@
--
-- @mimeType@ — The MIME type for which a type is desired.
--
-- Returns: A type. If no types are known to the system with the specified		MIME type and conformance but the inputs were otherwise valid, a dynamic		type may be provided. If the inputs were not valid, returns @nil.@
--
-- This method is equivalent to:
--
-- [UTType typeWithTag:mimeType tagClass:UTTagClassMIMEType conformingToType:UTTypeData]
--
-- ObjC selector: @+ typeWithMIMEType:@
typeWithMIMEType :: IsNSString mimeType => mimeType -> IO (Id UTType)
typeWithMIMEType mimeType =
  do
    cls' <- getRequiredClass "UTType"
    sendClassMessage cls' typeWithMIMETypeSelector (toNSString mimeType)

-- | Create a type given a MIME type.
--
-- @mimeType@ — The MIME type for which a type is desired.
--
-- @supertype@ — Another type that the resulting type must conform to.		Typically, you would pass @UTTypeData.@
--
-- Returns: A type. If no types are known to the system with the specified		MIME type and conformance but the inputs were otherwise valid, a dynamic		type may be provided. If the inputs were not valid, returns @nil.@
--
-- This method is equivalent to:
--
-- [UTType typeWithTag:mimeType tagClass:UTTagClassMIMEType conformingToType:supertype]
--
-- ObjC selector: @+ typeWithMIMEType:conformingToType:@
typeWithMIMEType_conformingToType :: (IsNSString mimeType, IsUTType supertype) => mimeType -> supertype -> IO (Id UTType)
typeWithMIMEType_conformingToType mimeType supertype =
  do
    cls' <- getRequiredClass "UTType"
    sendClassMessage cls' typeWithMIMEType_conformingToTypeSelector (toNSString mimeType) (toUTType supertype)

-- | Gets an active @UTType@ corresponding to a type that is declared as		"exported" by the current process.
--
-- @identifier@ — The type identifier for which a type is desired.
--
-- Returns: A type.
--
-- Use this method to get types that are exported by your application. If	identifier does not correspond to any type known to the system, the	result is undefined.
--
-- Conformance to either @UTTypeData@ or @UTTypePackage@ is assumed.
--
-- You would generally use this method with @dispatch_once():@
--
-- UTType *GetMyFileFormat(void) {
-- static UTType *result = nil;
--
-- static dispatch_once_t once;
-- dispatch_once(&once, ^ {
-- result = [UTType exportedTypeWithIdentifier:@"com.example.myfileformat"];
-- });
--
-- return result;
-- }
--
-- ObjC selector: @+ exportedTypeWithIdentifier:@
exportedTypeWithIdentifier :: IsNSString identifier => identifier -> IO (Id UTType)
exportedTypeWithIdentifier identifier =
  do
    cls' <- getRequiredClass "UTType"
    sendClassMessage cls' exportedTypeWithIdentifierSelector (toNSString identifier)

-- | Gets an active @UTType@ corresponding to a type that is declared as		"exported" by the current process.
--
-- @identifier@ — The type identifier for which a type is desired.
--
-- @parentType@ — A parent type that the resulting type is expected to		conform to.
--
-- Returns: A type.
--
-- Use this method to get types that are exported by your application. If	identifier does not correspond to any type known to the system, the	result is undefined.
--
-- You would generally use this method with @dispatch_once():@
--
-- UTType *GetMyFileFormat(void) {
-- static UTType *result = nil;
--
-- static dispatch_once_t once;
-- dispatch_once(&once, ^ {
-- result = [UTType exportedTypeWithIdentifier:@"com.example.myfileformat" conformingToType:UTTypeData];
-- });
--
-- return result;
-- }
--
-- ObjC selector: @+ exportedTypeWithIdentifier:conformingToType:@
exportedTypeWithIdentifier_conformingToType :: (IsNSString identifier, IsUTType parentType) => identifier -> parentType -> IO (Id UTType)
exportedTypeWithIdentifier_conformingToType identifier parentType =
  do
    cls' <- getRequiredClass "UTType"
    sendClassMessage cls' exportedTypeWithIdentifier_conformingToTypeSelector (toNSString identifier) (toUTType parentType)

-- | Gets an active @UTType@ corresponding to a type that is declared as		"imported" by the current process.
--
-- @identifier@ — The type identifier for which a type is desired.
--
-- Returns: A type whose identifier may or may not be equal to identifier,		but which is functionally equivalent.
--
-- Use this method to get types that are imported by your application. If	identifier does not correspond to any type known to the system, the	result is undefined.
--
-- Conformance to either @UTTypeData@ or @UTTypePackage@ is assumed.
--
-- You would generally use this method in the body of a funcion or method and	would /not/ use @dispatch_once()@ as the type can change over time:
--
-- UTType *GetCompetitorFileFormat(void) {
-- return [UTType importedTypeWithIdentifier:@"com.example.competitorfileformat"];
-- }
--
-- In the general case, this method returns a type with the same identifier,	but if that type has a preferred filename extension and /another/ type is	the preferred type for that extension, then that /other/ type is	substituted.
--
-- ObjC selector: @+ importedTypeWithIdentifier:@
importedTypeWithIdentifier :: IsNSString identifier => identifier -> IO (Id UTType)
importedTypeWithIdentifier identifier =
  do
    cls' <- getRequiredClass "UTType"
    sendClassMessage cls' importedTypeWithIdentifierSelector (toNSString identifier)

-- | Gets an active @UTType@ corresponding to a type that is declared as		"imported" by the current process.
--
-- @identifier@ — The type identifier for which a type is desired.
--
-- @parentType@ — A parent type that the resulting type is expected to		conform to.
--
-- Returns: A type whose identifier may or may not be equal to identifier,		but which is functionally equivalent.
--
-- Use this method to get types that are imported by your application. If	identifier does not correspond to any type known to the system, the	result is undefined.
--
-- You would generally use this method in the body of a funcion or method and	would /not/ use @dispatch_once()@ as the type can change over time:
--
-- UTType *GetCompetitorFileFormat(void) {
-- return [UTType importedTypeWithIdentifier:@"com.example.competitorfileformat" conformingToType:UTTypeData];
-- }
--
-- In the general case, this method returns a type with the same identifier,	but if that type has a preferred filename extension and /another/ type is	the preferred type for that extension, then that /other/ type is	substituted.
--
-- ObjC selector: @+ importedTypeWithIdentifier:conformingToType:@
importedTypeWithIdentifier_conformingToType :: (IsNSString identifier, IsUTType parentType) => identifier -> parentType -> IO (Id UTType)
importedTypeWithIdentifier_conformingToType identifier parentType =
  do
    cls' <- getRequiredClass "UTType"
    sendClassMessage cls' importedTypeWithIdentifier_conformingToTypeSelector (toNSString identifier) (toUTType parentType)

-- | Create a type given a type tag.
--
-- @tag@ — The tag, such as the path extension, for which a type is desired.
--
-- @tagClass@ — The class of the tag, such as @UTTagClassFilenameExtension.@
--
-- @supertype@ — Another type that the resulting type must conform to. If		@nil,@ no conformance is required.
--
-- Returns: A type. If no types are known to the system with the specified tag		but the inputs were otherwise valid, a dynamic type may be provided. If		the inputs were not valid, returns @nil.@
--
-- ObjC selector: @+ typeWithTag:tagClass:conformingToType:@
typeWithTag_tagClass_conformingToType :: (IsNSString tag, IsNSString tagClass, IsUTType supertype) => tag -> tagClass -> supertype -> IO (Id UTType)
typeWithTag_tagClass_conformingToType tag tagClass supertype =
  do
    cls' <- getRequiredClass "UTType"
    sendClassMessage cls' typeWithTag_tagClass_conformingToTypeSelector (toNSString tag) (toNSString tagClass) (toUTType supertype)

-- | Create an array of types given a type tag.
--
-- @tag@ — The tag, such as the path extension, for which a set of types is		desired.
--
-- @tagClass@ — The class of the tag, such as @UTTagClassFilenameExtension.@
--
-- @supertype@ — Another type that the resulting types must conform to. If		@nil,@ no conformance is required.
--
-- Returns: An array of types, or the empty array if no such types were		available. If no types are known to the system with the specified tag		but the inputs were otherwise valid, a dynamic type may be provided.
--
-- ObjC selector: @+ typesWithTag:tagClass:conformingToType:@
typesWithTag_tagClass_conformingToType :: (IsNSString tag, IsNSString tagClass, IsUTType supertype) => tag -> tagClass -> supertype -> IO (Id NSArray)
typesWithTag_tagClass_conformingToType tag tagClass supertype =
  do
    cls' <- getRequiredClass "UTType"
    sendClassMessage cls' typesWithTag_tagClass_conformingToTypeSelector (toNSString tag) (toNSString tagClass) (toUTType supertype)

-- | Tests for a conformance relationship between the receiver and another		type.
--
-- @type@ — The type against which conformance should be tested.
--
-- Returns: If the two types are equal, returns @YES.@ If the receiver		conforms, directly or indirectly, to type, returns @YES.@ Otherwise,		returns @NO.@
--
-- -isSupertypeOfType:
--
-- -isSubtypeOfType:
--
-- ObjC selector: @- conformsToType:@
conformsToType :: (IsUTType utType, IsUTType type_) => utType -> type_ -> IO Bool
conformsToType utType type_ =
  sendMessage utType conformsToTypeSelector (toUTType type_)

-- | Tests if the receiver is a supertype of another type.
--
-- @type@ — The type against which conformance should be tested.
--
-- Returns: If type conforms, directly or indirectly, to the receiver and is		not equal to it, returns @YES.@ Otherwise, returns @NO.@
--
-- -conformsToType:
--
-- -isSubtypeOfType:
--
-- ObjC selector: @- isSupertypeOfType:@
isSupertypeOfType :: (IsUTType utType, IsUTType type_) => utType -> type_ -> IO Bool
isSupertypeOfType utType type_ =
  sendMessage utType isSupertypeOfTypeSelector (toUTType type_)

-- | Tests if the receiver is a subtype of another type.
--
-- @type@ — The type against which conformance should be tested.
--
-- Returns: If the receiver conforms, directly or indirectly, to type and is		not equal to it, returns @YES.@ Otherwise, returns @NO.@
--
-- -conformsToType:
--
-- -isSupertypeOfType:
--
-- ObjC selector: @- isSubtypeOfType:@
isSubtypeOfType :: (IsUTType utType, IsUTType type_) => utType -> type_ -> IO Bool
isSubtypeOfType utType type_ =
  sendMessage utType isSubtypeOfTypeSelector (toUTType type_)

-- | The receiver's identifier.
--
-- A type is /identified/ /by/ its Uniform Type Identifier (UTI), a	reverse-DNS string such as @"public.jpeg"@ or @"com.adobe.pdf".@ The type	itself /has/ a UTI, but is not itself the UTI. This terminology is not	consistently used across Apple's documentation.
--
-- ObjC selector: @- identifier@
identifier :: IsUTType utType => utType -> IO (Id NSString)
identifier utType =
  sendMessage utType identifierSelector

-- | If available, the preferred (first available) tag of class		@UTTagClassFilenameExtension.@
--
-- Many uses of types require the generation of a filename (e.g. when saving a	file to disk.) If not @nil,@ the value of this property is the best	available filename extension for the given type. The value of this property	is equivalent to, but more efficient than:
--
-- type.tags[UTTagClassFilenameExtension].firstObject
--
-- ObjC selector: @- preferredFilenameExtension@
preferredFilenameExtension :: IsUTType utType => utType -> IO (Id NSString)
preferredFilenameExtension utType =
  sendMessage utType preferredFilenameExtensionSelector

-- | If available, the preferred (first available) tag of class		@UTTagClassMIMEType.@
--
-- If not @nil,@ the value of this property is the best available MIME type	for the given type, according to its declaration. The value of this property	is equivalent to, but more efficient than:
--
-- type.tags[UTTagClassMIMEType].firstObject
--
-- ObjC selector: @- preferredMIMEType@
preferredMIMEType :: IsUTType utType => utType -> IO (Id NSString)
preferredMIMEType utType =
  sendMessage utType preferredMIMETypeSelector

-- | The localized description of the type.
--
-- If the type does not provide a description, the system may search its	supertypes for one. Dynamic types never have localized descriptions even if	their supertypes do.
--
-- ObjC selector: @- localizedDescription@
localizedDescription :: IsUTType utType => utType -> IO (Id NSString)
localizedDescription utType =
  sendMessage utType localizedDescriptionSelector

-- | The type's version.
--
-- Most types do not specify a version.
--
-- ObjC selector: @- version@
version :: IsUTType utType => utType -> IO (Id NSNumber)
version utType =
  sendMessage utType versionSelector

-- | The reference URL of the type.
--
-- A reference URL is a human-readable document describing a type. Most types	do not specify reference URLs.
--
-- Warning: This URL is not validated in any way by the system, nor is its		scheme or structure guaranteed in any way.
--
-- ObjC selector: @- referenceURL@
referenceURL :: IsUTType utType => utType -> IO (Id NSURL)
referenceURL utType =
  sendMessage utType referenceURLSelector

-- | Whether or not the receiver is a dynamically generated type.
--
-- Dynamic types are recognized by the system, but may not be directly declared	or claimed by an application. They are used when a file is encountered whose	metadata has no corresponding type known to the system.
--
-- A type cannot be both declared /and/ dynamic.
--
-- ObjC selector: @- dynamic@
dynamic :: IsUTType utType => utType -> IO Bool
dynamic utType =
  sendMessage utType dynamicSelector

-- | Whether or not the receiver is a type known to the system.
--
-- A type cannot be both declared /and/ dynamic.
--
-- ObjC selector: @- declared@
declared :: IsUTType utType => utType -> IO Bool
declared utType =
  sendMessage utType declaredSelector

-- | Whether or not the type is in the public domain.
--
-- Types in the public domain have identifiers starting with @"public."@ and	are generally defined by a standards body or by convention. They are never	dynamic.
--
-- ObjC selector: @- publicType@
publicType :: IsUTType utType => utType -> IO Bool
publicType utType =
  sendMessage utType publicTypeSelector

-- | The tag specification dictionary of the type.
--
-- The system does not store tag information for non-standard tag classes. It	normalizes string values into arrays containing those strings. For instance,	a value of:
--
-- {
-- "public.mime-type": "x/y",
-- "nonstandard-tag-class": "abc",
-- }
--
-- Is normalized to:
--
-- {
-- "public.mime-type": [ "x/y" ]
-- }
--
-- If you are simply looking for the preferred filename extension or MIME	type of a type, it is more efficient for you to use the	@preferredFilenameExtension@ and @preferredMIMEType@ properties	respectively.
--
-- ObjC selector: @- tags@
tags :: IsUTType utType => utType -> IO (Id NSDictionary)
tags utType =
  sendMessage utType tagsSelector

-- | The set of types to which the receiving type conforms, directly or		indirectly.
--
-- If you are just interested in checking if one type conforms to another, it	is more efficient to use @-conformsToType:@ than this property.
--
-- ObjC selector: @- supertypes@
supertypes :: IsUTType utType => utType -> IO (Id NSSet)
supertypes utType =
  sendMessage utType supertypesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id UTType)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id UTType)
initSelector = mkSelector "init"

-- | @Selector@ for @typeWithIdentifier:@
typeWithIdentifierSelector :: Selector '[Id NSString] (Id UTType)
typeWithIdentifierSelector = mkSelector "typeWithIdentifier:"

-- | @Selector@ for @typeWithFilenameExtension:@
typeWithFilenameExtensionSelector :: Selector '[Id NSString] (Id UTType)
typeWithFilenameExtensionSelector = mkSelector "typeWithFilenameExtension:"

-- | @Selector@ for @typeWithFilenameExtension:conformingToType:@
typeWithFilenameExtension_conformingToTypeSelector :: Selector '[Id NSString, Id UTType] (Id UTType)
typeWithFilenameExtension_conformingToTypeSelector = mkSelector "typeWithFilenameExtension:conformingToType:"

-- | @Selector@ for @typeWithMIMEType:@
typeWithMIMETypeSelector :: Selector '[Id NSString] (Id UTType)
typeWithMIMETypeSelector = mkSelector "typeWithMIMEType:"

-- | @Selector@ for @typeWithMIMEType:conformingToType:@
typeWithMIMEType_conformingToTypeSelector :: Selector '[Id NSString, Id UTType] (Id UTType)
typeWithMIMEType_conformingToTypeSelector = mkSelector "typeWithMIMEType:conformingToType:"

-- | @Selector@ for @exportedTypeWithIdentifier:@
exportedTypeWithIdentifierSelector :: Selector '[Id NSString] (Id UTType)
exportedTypeWithIdentifierSelector = mkSelector "exportedTypeWithIdentifier:"

-- | @Selector@ for @exportedTypeWithIdentifier:conformingToType:@
exportedTypeWithIdentifier_conformingToTypeSelector :: Selector '[Id NSString, Id UTType] (Id UTType)
exportedTypeWithIdentifier_conformingToTypeSelector = mkSelector "exportedTypeWithIdentifier:conformingToType:"

-- | @Selector@ for @importedTypeWithIdentifier:@
importedTypeWithIdentifierSelector :: Selector '[Id NSString] (Id UTType)
importedTypeWithIdentifierSelector = mkSelector "importedTypeWithIdentifier:"

-- | @Selector@ for @importedTypeWithIdentifier:conformingToType:@
importedTypeWithIdentifier_conformingToTypeSelector :: Selector '[Id NSString, Id UTType] (Id UTType)
importedTypeWithIdentifier_conformingToTypeSelector = mkSelector "importedTypeWithIdentifier:conformingToType:"

-- | @Selector@ for @typeWithTag:tagClass:conformingToType:@
typeWithTag_tagClass_conformingToTypeSelector :: Selector '[Id NSString, Id NSString, Id UTType] (Id UTType)
typeWithTag_tagClass_conformingToTypeSelector = mkSelector "typeWithTag:tagClass:conformingToType:"

-- | @Selector@ for @typesWithTag:tagClass:conformingToType:@
typesWithTag_tagClass_conformingToTypeSelector :: Selector '[Id NSString, Id NSString, Id UTType] (Id NSArray)
typesWithTag_tagClass_conformingToTypeSelector = mkSelector "typesWithTag:tagClass:conformingToType:"

-- | @Selector@ for @conformsToType:@
conformsToTypeSelector :: Selector '[Id UTType] Bool
conformsToTypeSelector = mkSelector "conformsToType:"

-- | @Selector@ for @isSupertypeOfType:@
isSupertypeOfTypeSelector :: Selector '[Id UTType] Bool
isSupertypeOfTypeSelector = mkSelector "isSupertypeOfType:"

-- | @Selector@ for @isSubtypeOfType:@
isSubtypeOfTypeSelector :: Selector '[Id UTType] Bool
isSubtypeOfTypeSelector = mkSelector "isSubtypeOfType:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @preferredFilenameExtension@
preferredFilenameExtensionSelector :: Selector '[] (Id NSString)
preferredFilenameExtensionSelector = mkSelector "preferredFilenameExtension"

-- | @Selector@ for @preferredMIMEType@
preferredMIMETypeSelector :: Selector '[] (Id NSString)
preferredMIMETypeSelector = mkSelector "preferredMIMEType"

-- | @Selector@ for @localizedDescription@
localizedDescriptionSelector :: Selector '[] (Id NSString)
localizedDescriptionSelector = mkSelector "localizedDescription"

-- | @Selector@ for @version@
versionSelector :: Selector '[] (Id NSNumber)
versionSelector = mkSelector "version"

-- | @Selector@ for @referenceURL@
referenceURLSelector :: Selector '[] (Id NSURL)
referenceURLSelector = mkSelector "referenceURL"

-- | @Selector@ for @dynamic@
dynamicSelector :: Selector '[] Bool
dynamicSelector = mkSelector "dynamic"

-- | @Selector@ for @declared@
declaredSelector :: Selector '[] Bool
declaredSelector = mkSelector "declared"

-- | @Selector@ for @publicType@
publicTypeSelector :: Selector '[] Bool
publicTypeSelector = mkSelector "publicType"

-- | @Selector@ for @tags@
tagsSelector :: Selector '[] (Id NSDictionary)
tagsSelector = mkSelector "tags"

-- | @Selector@ for @supertypes@
supertypesSelector :: Selector '[] (Id NSSet)
supertypesSelector = mkSelector "supertypes"

