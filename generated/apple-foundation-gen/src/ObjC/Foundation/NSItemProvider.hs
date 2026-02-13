{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSItemProvider@.
module ObjC.Foundation.NSItemProvider
  ( NSItemProvider
  , IsNSItemProvider(..)
  , init_
  , registerDataRepresentationForTypeIdentifier_visibility_loadHandler
  , registerFileRepresentationForTypeIdentifier_fileOptions_visibility_loadHandler
  , registeredTypeIdentifiersWithFileOptions
  , hasItemConformingToTypeIdentifier
  , hasRepresentationConformingToTypeIdentifier_fileOptions
  , loadDataRepresentationForTypeIdentifier_completionHandler
  , loadFileRepresentationForTypeIdentifier_completionHandler
  , loadInPlaceFileRepresentationForTypeIdentifier_completionHandler
  , initWithObject
  , registerObject_visibility
  , canLoadObjectOfClass
  , initWithItem_typeIdentifier
  , initWithContentsOfURL
  , registerItemForTypeIdentifier_loadHandler
  , loadItemForTypeIdentifier_options_completionHandler
  , loadPreviewImageWithOptions_completionHandler
  , registeredTypeIdentifiers
  , suggestedName
  , setSuggestedName
  , previewImageHandler
  , setPreviewImageHandler
  , canLoadObjectOfClassSelector
  , hasItemConformingToTypeIdentifierSelector
  , hasRepresentationConformingToTypeIdentifier_fileOptionsSelector
  , initSelector
  , initWithContentsOfURLSelector
  , initWithItem_typeIdentifierSelector
  , initWithObjectSelector
  , loadDataRepresentationForTypeIdentifier_completionHandlerSelector
  , loadFileRepresentationForTypeIdentifier_completionHandlerSelector
  , loadInPlaceFileRepresentationForTypeIdentifier_completionHandlerSelector
  , loadItemForTypeIdentifier_options_completionHandlerSelector
  , loadPreviewImageWithOptions_completionHandlerSelector
  , previewImageHandlerSelector
  , registerDataRepresentationForTypeIdentifier_visibility_loadHandlerSelector
  , registerFileRepresentationForTypeIdentifier_fileOptions_visibility_loadHandlerSelector
  , registerItemForTypeIdentifier_loadHandlerSelector
  , registerObject_visibilitySelector
  , registeredTypeIdentifiersSelector
  , registeredTypeIdentifiersWithFileOptionsSelector
  , setPreviewImageHandlerSelector
  , setSuggestedNameSelector
  , suggestedNameSelector

  -- * Enum types
  , NSItemProviderFileOptions(NSItemProviderFileOptions)
  , pattern NSItemProviderFileOptionOpenInPlace
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

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- init@
init_ :: IsNSItemProvider nsItemProvider => nsItemProvider -> IO (Id NSItemProvider)
init_ nsItemProvider =
  sendOwnedMessage nsItemProvider initSelector

-- | @- registerDataRepresentationForTypeIdentifier:visibility:loadHandler:@
registerDataRepresentationForTypeIdentifier_visibility_loadHandler :: (IsNSItemProvider nsItemProvider, IsNSString typeIdentifier) => nsItemProvider -> typeIdentifier -> NSItemProviderRepresentationVisibility -> Ptr () -> IO ()
registerDataRepresentationForTypeIdentifier_visibility_loadHandler nsItemProvider typeIdentifier visibility loadHandler =
  sendMessage nsItemProvider registerDataRepresentationForTypeIdentifier_visibility_loadHandlerSelector (toNSString typeIdentifier) visibility loadHandler

-- | @- registerFileRepresentationForTypeIdentifier:fileOptions:visibility:loadHandler:@
registerFileRepresentationForTypeIdentifier_fileOptions_visibility_loadHandler :: (IsNSItemProvider nsItemProvider, IsNSString typeIdentifier) => nsItemProvider -> typeIdentifier -> NSItemProviderFileOptions -> NSItemProviderRepresentationVisibility -> Ptr () -> IO ()
registerFileRepresentationForTypeIdentifier_fileOptions_visibility_loadHandler nsItemProvider typeIdentifier fileOptions visibility loadHandler =
  sendMessage nsItemProvider registerFileRepresentationForTypeIdentifier_fileOptions_visibility_loadHandlerSelector (toNSString typeIdentifier) fileOptions visibility loadHandler

-- | @- registeredTypeIdentifiersWithFileOptions:@
registeredTypeIdentifiersWithFileOptions :: IsNSItemProvider nsItemProvider => nsItemProvider -> NSItemProviderFileOptions -> IO (Id NSArray)
registeredTypeIdentifiersWithFileOptions nsItemProvider fileOptions =
  sendMessage nsItemProvider registeredTypeIdentifiersWithFileOptionsSelector fileOptions

-- | @- hasItemConformingToTypeIdentifier:@
hasItemConformingToTypeIdentifier :: (IsNSItemProvider nsItemProvider, IsNSString typeIdentifier) => nsItemProvider -> typeIdentifier -> IO Bool
hasItemConformingToTypeIdentifier nsItemProvider typeIdentifier =
  sendMessage nsItemProvider hasItemConformingToTypeIdentifierSelector (toNSString typeIdentifier)

-- | @- hasRepresentationConformingToTypeIdentifier:fileOptions:@
hasRepresentationConformingToTypeIdentifier_fileOptions :: (IsNSItemProvider nsItemProvider, IsNSString typeIdentifier) => nsItemProvider -> typeIdentifier -> NSItemProviderFileOptions -> IO Bool
hasRepresentationConformingToTypeIdentifier_fileOptions nsItemProvider typeIdentifier fileOptions =
  sendMessage nsItemProvider hasRepresentationConformingToTypeIdentifier_fileOptionsSelector (toNSString typeIdentifier) fileOptions

-- | @- loadDataRepresentationForTypeIdentifier:completionHandler:@
loadDataRepresentationForTypeIdentifier_completionHandler :: (IsNSItemProvider nsItemProvider, IsNSString typeIdentifier) => nsItemProvider -> typeIdentifier -> Ptr () -> IO (Id NSProgress)
loadDataRepresentationForTypeIdentifier_completionHandler nsItemProvider typeIdentifier completionHandler =
  sendMessage nsItemProvider loadDataRepresentationForTypeIdentifier_completionHandlerSelector (toNSString typeIdentifier) completionHandler

-- | @- loadFileRepresentationForTypeIdentifier:completionHandler:@
loadFileRepresentationForTypeIdentifier_completionHandler :: (IsNSItemProvider nsItemProvider, IsNSString typeIdentifier) => nsItemProvider -> typeIdentifier -> Ptr () -> IO (Id NSProgress)
loadFileRepresentationForTypeIdentifier_completionHandler nsItemProvider typeIdentifier completionHandler =
  sendMessage nsItemProvider loadFileRepresentationForTypeIdentifier_completionHandlerSelector (toNSString typeIdentifier) completionHandler

-- | @- loadInPlaceFileRepresentationForTypeIdentifier:completionHandler:@
loadInPlaceFileRepresentationForTypeIdentifier_completionHandler :: (IsNSItemProvider nsItemProvider, IsNSString typeIdentifier) => nsItemProvider -> typeIdentifier -> Ptr () -> IO (Id NSProgress)
loadInPlaceFileRepresentationForTypeIdentifier_completionHandler nsItemProvider typeIdentifier completionHandler =
  sendMessage nsItemProvider loadInPlaceFileRepresentationForTypeIdentifier_completionHandlerSelector (toNSString typeIdentifier) completionHandler

-- | @- initWithObject:@
initWithObject :: IsNSItemProvider nsItemProvider => nsItemProvider -> RawId -> IO (Id NSItemProvider)
initWithObject nsItemProvider object =
  sendOwnedMessage nsItemProvider initWithObjectSelector object

-- | @- registerObject:visibility:@
registerObject_visibility :: IsNSItemProvider nsItemProvider => nsItemProvider -> RawId -> NSItemProviderRepresentationVisibility -> IO ()
registerObject_visibility nsItemProvider object visibility =
  sendMessage nsItemProvider registerObject_visibilitySelector object visibility

-- | @- canLoadObjectOfClass:@
canLoadObjectOfClass :: IsNSItemProvider nsItemProvider => nsItemProvider -> Class -> IO Bool
canLoadObjectOfClass nsItemProvider aClass =
  sendMessage nsItemProvider canLoadObjectOfClassSelector aClass

-- | @- initWithItem:typeIdentifier:@
initWithItem_typeIdentifier :: (IsNSItemProvider nsItemProvider, IsNSString typeIdentifier) => nsItemProvider -> RawId -> typeIdentifier -> IO (Id NSItemProvider)
initWithItem_typeIdentifier nsItemProvider item typeIdentifier =
  sendOwnedMessage nsItemProvider initWithItem_typeIdentifierSelector item (toNSString typeIdentifier)

-- | @- initWithContentsOfURL:@
initWithContentsOfURL :: (IsNSItemProvider nsItemProvider, IsNSURL fileURL) => nsItemProvider -> fileURL -> IO (Id NSItemProvider)
initWithContentsOfURL nsItemProvider fileURL =
  sendOwnedMessage nsItemProvider initWithContentsOfURLSelector (toNSURL fileURL)

-- | @- registerItemForTypeIdentifier:loadHandler:@
registerItemForTypeIdentifier_loadHandler :: (IsNSItemProvider nsItemProvider, IsNSString typeIdentifier) => nsItemProvider -> typeIdentifier -> Ptr () -> IO ()
registerItemForTypeIdentifier_loadHandler nsItemProvider typeIdentifier loadHandler =
  sendMessage nsItemProvider registerItemForTypeIdentifier_loadHandlerSelector (toNSString typeIdentifier) loadHandler

-- | @- loadItemForTypeIdentifier:options:completionHandler:@
loadItemForTypeIdentifier_options_completionHandler :: (IsNSItemProvider nsItemProvider, IsNSString typeIdentifier, IsNSDictionary options) => nsItemProvider -> typeIdentifier -> options -> Ptr () -> IO ()
loadItemForTypeIdentifier_options_completionHandler nsItemProvider typeIdentifier options completionHandler =
  sendMessage nsItemProvider loadItemForTypeIdentifier_options_completionHandlerSelector (toNSString typeIdentifier) (toNSDictionary options) completionHandler

-- | @- loadPreviewImageWithOptions:completionHandler:@
loadPreviewImageWithOptions_completionHandler :: (IsNSItemProvider nsItemProvider, IsNSDictionary options) => nsItemProvider -> options -> Ptr () -> IO ()
loadPreviewImageWithOptions_completionHandler nsItemProvider options completionHandler =
  sendMessage nsItemProvider loadPreviewImageWithOptions_completionHandlerSelector (toNSDictionary options) completionHandler

-- | @- registeredTypeIdentifiers@
registeredTypeIdentifiers :: IsNSItemProvider nsItemProvider => nsItemProvider -> IO (Id NSArray)
registeredTypeIdentifiers nsItemProvider =
  sendMessage nsItemProvider registeredTypeIdentifiersSelector

-- | @- suggestedName@
suggestedName :: IsNSItemProvider nsItemProvider => nsItemProvider -> IO (Id NSString)
suggestedName nsItemProvider =
  sendMessage nsItemProvider suggestedNameSelector

-- | @- setSuggestedName:@
setSuggestedName :: (IsNSItemProvider nsItemProvider, IsNSString value) => nsItemProvider -> value -> IO ()
setSuggestedName nsItemProvider value =
  sendMessage nsItemProvider setSuggestedNameSelector (toNSString value)

-- | @- previewImageHandler@
previewImageHandler :: IsNSItemProvider nsItemProvider => nsItemProvider -> IO (Ptr ())
previewImageHandler nsItemProvider =
  sendMessage nsItemProvider previewImageHandlerSelector

-- | @- setPreviewImageHandler:@
setPreviewImageHandler :: IsNSItemProvider nsItemProvider => nsItemProvider -> Ptr () -> IO ()
setPreviewImageHandler nsItemProvider value =
  sendMessage nsItemProvider setPreviewImageHandlerSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSItemProvider)
initSelector = mkSelector "init"

-- | @Selector@ for @registerDataRepresentationForTypeIdentifier:visibility:loadHandler:@
registerDataRepresentationForTypeIdentifier_visibility_loadHandlerSelector :: Selector '[Id NSString, NSItemProviderRepresentationVisibility, Ptr ()] ()
registerDataRepresentationForTypeIdentifier_visibility_loadHandlerSelector = mkSelector "registerDataRepresentationForTypeIdentifier:visibility:loadHandler:"

-- | @Selector@ for @registerFileRepresentationForTypeIdentifier:fileOptions:visibility:loadHandler:@
registerFileRepresentationForTypeIdentifier_fileOptions_visibility_loadHandlerSelector :: Selector '[Id NSString, NSItemProviderFileOptions, NSItemProviderRepresentationVisibility, Ptr ()] ()
registerFileRepresentationForTypeIdentifier_fileOptions_visibility_loadHandlerSelector = mkSelector "registerFileRepresentationForTypeIdentifier:fileOptions:visibility:loadHandler:"

-- | @Selector@ for @registeredTypeIdentifiersWithFileOptions:@
registeredTypeIdentifiersWithFileOptionsSelector :: Selector '[NSItemProviderFileOptions] (Id NSArray)
registeredTypeIdentifiersWithFileOptionsSelector = mkSelector "registeredTypeIdentifiersWithFileOptions:"

-- | @Selector@ for @hasItemConformingToTypeIdentifier:@
hasItemConformingToTypeIdentifierSelector :: Selector '[Id NSString] Bool
hasItemConformingToTypeIdentifierSelector = mkSelector "hasItemConformingToTypeIdentifier:"

-- | @Selector@ for @hasRepresentationConformingToTypeIdentifier:fileOptions:@
hasRepresentationConformingToTypeIdentifier_fileOptionsSelector :: Selector '[Id NSString, NSItemProviderFileOptions] Bool
hasRepresentationConformingToTypeIdentifier_fileOptionsSelector = mkSelector "hasRepresentationConformingToTypeIdentifier:fileOptions:"

-- | @Selector@ for @loadDataRepresentationForTypeIdentifier:completionHandler:@
loadDataRepresentationForTypeIdentifier_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] (Id NSProgress)
loadDataRepresentationForTypeIdentifier_completionHandlerSelector = mkSelector "loadDataRepresentationForTypeIdentifier:completionHandler:"

-- | @Selector@ for @loadFileRepresentationForTypeIdentifier:completionHandler:@
loadFileRepresentationForTypeIdentifier_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] (Id NSProgress)
loadFileRepresentationForTypeIdentifier_completionHandlerSelector = mkSelector "loadFileRepresentationForTypeIdentifier:completionHandler:"

-- | @Selector@ for @loadInPlaceFileRepresentationForTypeIdentifier:completionHandler:@
loadInPlaceFileRepresentationForTypeIdentifier_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] (Id NSProgress)
loadInPlaceFileRepresentationForTypeIdentifier_completionHandlerSelector = mkSelector "loadInPlaceFileRepresentationForTypeIdentifier:completionHandler:"

-- | @Selector@ for @initWithObject:@
initWithObjectSelector :: Selector '[RawId] (Id NSItemProvider)
initWithObjectSelector = mkSelector "initWithObject:"

-- | @Selector@ for @registerObject:visibility:@
registerObject_visibilitySelector :: Selector '[RawId, NSItemProviderRepresentationVisibility] ()
registerObject_visibilitySelector = mkSelector "registerObject:visibility:"

-- | @Selector@ for @canLoadObjectOfClass:@
canLoadObjectOfClassSelector :: Selector '[Class] Bool
canLoadObjectOfClassSelector = mkSelector "canLoadObjectOfClass:"

-- | @Selector@ for @initWithItem:typeIdentifier:@
initWithItem_typeIdentifierSelector :: Selector '[RawId, Id NSString] (Id NSItemProvider)
initWithItem_typeIdentifierSelector = mkSelector "initWithItem:typeIdentifier:"

-- | @Selector@ for @initWithContentsOfURL:@
initWithContentsOfURLSelector :: Selector '[Id NSURL] (Id NSItemProvider)
initWithContentsOfURLSelector = mkSelector "initWithContentsOfURL:"

-- | @Selector@ for @registerItemForTypeIdentifier:loadHandler:@
registerItemForTypeIdentifier_loadHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
registerItemForTypeIdentifier_loadHandlerSelector = mkSelector "registerItemForTypeIdentifier:loadHandler:"

-- | @Selector@ for @loadItemForTypeIdentifier:options:completionHandler:@
loadItemForTypeIdentifier_options_completionHandlerSelector :: Selector '[Id NSString, Id NSDictionary, Ptr ()] ()
loadItemForTypeIdentifier_options_completionHandlerSelector = mkSelector "loadItemForTypeIdentifier:options:completionHandler:"

-- | @Selector@ for @loadPreviewImageWithOptions:completionHandler:@
loadPreviewImageWithOptions_completionHandlerSelector :: Selector '[Id NSDictionary, Ptr ()] ()
loadPreviewImageWithOptions_completionHandlerSelector = mkSelector "loadPreviewImageWithOptions:completionHandler:"

-- | @Selector@ for @registeredTypeIdentifiers@
registeredTypeIdentifiersSelector :: Selector '[] (Id NSArray)
registeredTypeIdentifiersSelector = mkSelector "registeredTypeIdentifiers"

-- | @Selector@ for @suggestedName@
suggestedNameSelector :: Selector '[] (Id NSString)
suggestedNameSelector = mkSelector "suggestedName"

-- | @Selector@ for @setSuggestedName:@
setSuggestedNameSelector :: Selector '[Id NSString] ()
setSuggestedNameSelector = mkSelector "setSuggestedName:"

-- | @Selector@ for @previewImageHandler@
previewImageHandlerSelector :: Selector '[] (Ptr ())
previewImageHandlerSelector = mkSelector "previewImageHandler"

-- | @Selector@ for @setPreviewImageHandler:@
setPreviewImageHandlerSelector :: Selector '[Ptr ()] ()
setPreviewImageHandlerSelector = mkSelector "setPreviewImageHandler:"

