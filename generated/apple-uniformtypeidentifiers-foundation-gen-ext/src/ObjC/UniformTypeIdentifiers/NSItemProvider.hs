{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSItemProvider@.
module ObjC.UniformTypeIdentifiers.NSItemProvider
  ( NSItemProvider
  , IsNSItemProvider(..)
  , initWithContentsOfURL_contentType_openInPlace_coordinated_visibility
  , registerDataRepresentationForContentType_visibility_loadHandler
  , registerFileRepresentationForContentType_visibility_openInPlace_loadHandler
  , registeredContentTypesConformingToContentType
  , loadDataRepresentationForContentType_completionHandler
  , loadFileRepresentationForContentType_openInPlace_completionHandler
  , registeredContentTypes
  , registeredContentTypesForOpenInPlace
  , initWithContentsOfURL_contentType_openInPlace_coordinated_visibilitySelector
  , loadDataRepresentationForContentType_completionHandlerSelector
  , loadFileRepresentationForContentType_openInPlace_completionHandlerSelector
  , registerDataRepresentationForContentType_visibility_loadHandlerSelector
  , registerFileRepresentationForContentType_visibility_openInPlace_loadHandlerSelector
  , registeredContentTypesConformingToContentTypeSelector
  , registeredContentTypesForOpenInPlaceSelector
  , registeredContentTypesSelector

  -- * Enum types
  , NSItemProviderRepresentationVisibility(NSItemProviderRepresentationVisibility)
  , pattern NSItemProviderRepresentationVisibilityAll
  , pattern NSItemProviderRepresentationVisibilityTeam
  , pattern NSItemProviderRepresentationVisibilityGroup
  , pattern NSItemProviderRepresentationVisibilityOwnProcess

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.UniformTypeIdentifiers.Internal.Classes
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Initialize this instance with the contents of a URL
--
-- The filename of the URL is copied into the @suggestedName@ property
--
-- @fileURL@ — The URL of the file.
--
-- @contentType@ — The content type associated with this file, or @nil@ to deduce the content type from the                    file extension.
--
-- @openInPlace@ — Pass @YES@ to allow this file to be opened in place.
--
-- @coordinated@ — Pass @YES@ to use file coordination to access this file, even if it is not opened in place.                    If @openInPlace@ is set to @YES@ file coordination will be used and this parameter is ignored.
--
-- @visibility@ — The visibility of this representation.
--
-- ObjC selector: @- initWithContentsOfURL:contentType:openInPlace:coordinated:visibility:@
initWithContentsOfURL_contentType_openInPlace_coordinated_visibility :: (IsNSItemProvider nsItemProvider, IsNSURL fileURL, IsUTType contentType) => nsItemProvider -> fileURL -> contentType -> Bool -> Bool -> NSItemProviderRepresentationVisibility -> IO (Id NSItemProvider)
initWithContentsOfURL_contentType_openInPlace_coordinated_visibility nsItemProvider fileURL contentType openInPlace coordinated visibility =
  sendOwnedMessage nsItemProvider initWithContentsOfURL_contentType_openInPlace_coordinated_visibilitySelector (toNSURL fileURL) (toUTType contentType) openInPlace coordinated visibility

-- | Register a representation backed by an @NSData@
--
-- The load handler must call the completion block when loading is complete. Pass either a non-nil data object, or a non-nil error. If the load handler returns a non-nil progress object, it should report loading progress and respond to cancelation.
--
-- @contentType@ — The content type associated with the data representation.
--
-- @visibility@ — Specifies which processes have access to this representation.
--
-- @loadHandler@ — A block called to provide the data representation.
--
-- ObjC selector: @- registerDataRepresentationForContentType:visibility:loadHandler:@
registerDataRepresentationForContentType_visibility_loadHandler :: (IsNSItemProvider nsItemProvider, IsUTType contentType) => nsItemProvider -> contentType -> NSItemProviderRepresentationVisibility -> Ptr () -> IO ()
registerDataRepresentationForContentType_visibility_loadHandler nsItemProvider contentType visibility loadHandler =
  sendMessage nsItemProvider registerDataRepresentationForContentType_visibility_loadHandlerSelector (toUTType contentType) visibility loadHandler

-- | Register a representation backed by a file
--
-- It is permissible to provide a URL pointing to a folder. A folder requested as @NSData@ will yield a data object containing a zip archive holding a copy of the source folder tree.
--
-- The load handler must call the completion block when loading is complete. Pass either a non-nil url, or a non-nil error. Pass @YES@ to @coordinated@ if the file should be accessed using file coordination even if it is not opened in-place. Files registered as open-in-place are assumed to need coordination, and this parameter will be ignored in those cases. If the load handler returns a non-nil progress object, it should report loading progress and respond to cancelation.
--
-- Note: Not all files specified as openable in place can be opened in place by the destination. System security or       privacy policies may restrict which files can be opened in place.
--
-- @contentType@ — The content type associated with the file representation.
--
-- @visibility@ — Specifies which processes have access to this representation.
--
-- @openInPlace@ — Specifies whether the file should be openable in place.
--
-- @loadHandler@ — A block called to provide the file representation.
--
-- ObjC selector: @- registerFileRepresentationForContentType:visibility:openInPlace:loadHandler:@
registerFileRepresentationForContentType_visibility_openInPlace_loadHandler :: (IsNSItemProvider nsItemProvider, IsUTType contentType) => nsItemProvider -> contentType -> NSItemProviderRepresentationVisibility -> Bool -> Ptr () -> IO ()
registerFileRepresentationForContentType_visibility_openInPlace_loadHandler nsItemProvider contentType visibility openInPlace loadHandler =
  sendMessage nsItemProvider registerFileRepresentationForContentType_visibility_openInPlace_loadHandlerSelector (toUTType contentType) visibility openInPlace loadHandler

-- | Return an array of registered content types that conform to a given content type.
--
-- The returned content types are given in order of fidelity. Prefer content types that appear earlier in the array.
--
-- ObjC selector: @- registeredContentTypesConformingToContentType:@
registeredContentTypesConformingToContentType :: (IsNSItemProvider nsItemProvider, IsUTType contentType) => nsItemProvider -> contentType -> IO (Id NSArray)
registeredContentTypesConformingToContentType nsItemProvider contentType =
  sendMessage nsItemProvider registeredContentTypesConformingToContentTypeSelector (toUTType contentType)

-- | Load a representation as data
--
-- If the requested representation was registered as a file, an @NSData@ with the contents of the file will be provided. If the registered URL points to a folder, an @NSData@ containing a zip archive containing that folder will be provided.
--
-- Note: The completion handler may be scheduled on an arbitrary queue.
--
-- @contentType@ — Content type of the representation to load. Must conform to one of the content types returned                    by @registeredContentTypes@
--
-- @completionHandler@ — A block that will be called when loading is complete. It will either have a non-nil                          @data@ or a non-nil @error@ parameter.
--
-- Returns: A progress object. Use it to monitor loading progress, or to cancel loading.
--
-- ObjC selector: @- loadDataRepresentationForContentType:completionHandler:@
loadDataRepresentationForContentType_completionHandler :: (IsNSItemProvider nsItemProvider, IsUTType contentType) => nsItemProvider -> contentType -> Ptr () -> IO (Id NSProgress)
loadDataRepresentationForContentType_completionHandler nsItemProvider contentType completionHandler =
  sendMessage nsItemProvider loadDataRepresentationForContentType_completionHandlerSelector (toUTType contentType) completionHandler

-- | Load a representation as a file
--
-- Except for files registered as open-in-place, a temporary file containing a copy of the original will be provided to your completion handler. This temporary file will be deleted once your completion handler returns. To keep a copy of this file, move or copy it into another directory before returning from the completion handler.
--
-- If the representation was registered as @NSData@ its contents will be written to a temporary file.
--
-- If @suggestedName@ is non-nil, an attempt will be made to use it as the file name, with an appropriate file extension based on the content type. Otherwise, a suitable name and file extension will be chosen based on the content type.
--
-- Note: The completion handler may be scheduled on an arbitrary queue
--
-- @contentType@ — Content type of the representation to load. Must conform to one of the content types returned                    by @registeredContentTypes@
--
-- @openInPlace@ — Pass @YES@ to attempt to open a file representation in place
--
-- @completionHandler@ — A block that will be called when loading is complete. It will either have a non-nil                          @URL@ or a non-nill @error@ parameter. The @openInPlace@ parameter will be set to                          @YES@ if the file was successfully opened in place, or @NO@ if a copy of the file was                          created in a temporary directory.
--
-- Returns: A progress object. Use it to monitor loading progress, or to cancel loading.
--
-- ObjC selector: @- loadFileRepresentationForContentType:openInPlace:completionHandler:@
loadFileRepresentationForContentType_openInPlace_completionHandler :: (IsNSItemProvider nsItemProvider, IsUTType contentType) => nsItemProvider -> contentType -> Bool -> Ptr () -> IO (Id NSProgress)
loadFileRepresentationForContentType_openInPlace_completionHandler nsItemProvider contentType openInPlace completionHandler =
  sendMessage nsItemProvider loadFileRepresentationForContentType_openInPlace_completionHandlerSelector (toUTType contentType) openInPlace completionHandler

-- | Registered content types, in the order they were registered
--
-- Content types should be registered in order of fidelity. Prefer using content types that appear earlier in the array.
--
-- ObjC selector: @- registeredContentTypes@
registeredContentTypes :: IsNSItemProvider nsItemProvider => nsItemProvider -> IO (Id NSArray)
registeredContentTypes nsItemProvider =
  sendMessage nsItemProvider registeredContentTypesSelector

-- | Registered content types that can be loaded as files opened in place
--
-- ObjC selector: @- registeredContentTypesForOpenInPlace@
registeredContentTypesForOpenInPlace :: IsNSItemProvider nsItemProvider => nsItemProvider -> IO (Id NSArray)
registeredContentTypesForOpenInPlace nsItemProvider =
  sendMessage nsItemProvider registeredContentTypesForOpenInPlaceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithContentsOfURL:contentType:openInPlace:coordinated:visibility:@
initWithContentsOfURL_contentType_openInPlace_coordinated_visibilitySelector :: Selector '[Id NSURL, Id UTType, Bool, Bool, NSItemProviderRepresentationVisibility] (Id NSItemProvider)
initWithContentsOfURL_contentType_openInPlace_coordinated_visibilitySelector = mkSelector "initWithContentsOfURL:contentType:openInPlace:coordinated:visibility:"

-- | @Selector@ for @registerDataRepresentationForContentType:visibility:loadHandler:@
registerDataRepresentationForContentType_visibility_loadHandlerSelector :: Selector '[Id UTType, NSItemProviderRepresentationVisibility, Ptr ()] ()
registerDataRepresentationForContentType_visibility_loadHandlerSelector = mkSelector "registerDataRepresentationForContentType:visibility:loadHandler:"

-- | @Selector@ for @registerFileRepresentationForContentType:visibility:openInPlace:loadHandler:@
registerFileRepresentationForContentType_visibility_openInPlace_loadHandlerSelector :: Selector '[Id UTType, NSItemProviderRepresentationVisibility, Bool, Ptr ()] ()
registerFileRepresentationForContentType_visibility_openInPlace_loadHandlerSelector = mkSelector "registerFileRepresentationForContentType:visibility:openInPlace:loadHandler:"

-- | @Selector@ for @registeredContentTypesConformingToContentType:@
registeredContentTypesConformingToContentTypeSelector :: Selector '[Id UTType] (Id NSArray)
registeredContentTypesConformingToContentTypeSelector = mkSelector "registeredContentTypesConformingToContentType:"

-- | @Selector@ for @loadDataRepresentationForContentType:completionHandler:@
loadDataRepresentationForContentType_completionHandlerSelector :: Selector '[Id UTType, Ptr ()] (Id NSProgress)
loadDataRepresentationForContentType_completionHandlerSelector = mkSelector "loadDataRepresentationForContentType:completionHandler:"

-- | @Selector@ for @loadFileRepresentationForContentType:openInPlace:completionHandler:@
loadFileRepresentationForContentType_openInPlace_completionHandlerSelector :: Selector '[Id UTType, Bool, Ptr ()] (Id NSProgress)
loadFileRepresentationForContentType_openInPlace_completionHandlerSelector = mkSelector "loadFileRepresentationForContentType:openInPlace:completionHandler:"

-- | @Selector@ for @registeredContentTypes@
registeredContentTypesSelector :: Selector '[] (Id NSArray)
registeredContentTypesSelector = mkSelector "registeredContentTypes"

-- | @Selector@ for @registeredContentTypesForOpenInPlace@
registeredContentTypesForOpenInPlaceSelector :: Selector '[] (Id NSArray)
registeredContentTypesForOpenInPlaceSelector = mkSelector "registeredContentTypesForOpenInPlace"

