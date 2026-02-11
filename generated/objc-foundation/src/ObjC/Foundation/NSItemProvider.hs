{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , registerDataRepresentationForTypeIdentifier_visibility_loadHandlerSelector
  , registerFileRepresentationForTypeIdentifier_fileOptions_visibility_loadHandlerSelector
  , registeredTypeIdentifiersWithFileOptionsSelector
  , hasItemConformingToTypeIdentifierSelector
  , hasRepresentationConformingToTypeIdentifier_fileOptionsSelector
  , loadDataRepresentationForTypeIdentifier_completionHandlerSelector
  , loadFileRepresentationForTypeIdentifier_completionHandlerSelector
  , loadInPlaceFileRepresentationForTypeIdentifier_completionHandlerSelector
  , initWithObjectSelector
  , registerObject_visibilitySelector
  , canLoadObjectOfClassSelector
  , initWithItem_typeIdentifierSelector
  , initWithContentsOfURLSelector
  , registerItemForTypeIdentifier_loadHandlerSelector
  , loadItemForTypeIdentifier_options_completionHandlerSelector
  , loadPreviewImageWithOptions_completionHandlerSelector
  , registeredTypeIdentifiersSelector
  , suggestedNameSelector
  , setSuggestedNameSelector
  , previewImageHandlerSelector
  , setPreviewImageHandlerSelector

  -- * Enum types
  , NSItemProviderFileOptions(NSItemProviderFileOptions)
  , pattern NSItemProviderFileOptionOpenInPlace
  , NSItemProviderRepresentationVisibility(NSItemProviderRepresentationVisibility)
  , pattern NSItemProviderRepresentationVisibilityAll
  , pattern NSItemProviderRepresentationVisibilityTeam
  , pattern NSItemProviderRepresentationVisibilityGroup
  , pattern NSItemProviderRepresentationVisibilityOwnProcess

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
init_ :: IsNSItemProvider nsItemProvider => nsItemProvider -> IO (Id NSItemProvider)
init_ nsItemProvider  =
  sendMsg nsItemProvider (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- registerDataRepresentationForTypeIdentifier:visibility:loadHandler:@
registerDataRepresentationForTypeIdentifier_visibility_loadHandler :: (IsNSItemProvider nsItemProvider, IsNSString typeIdentifier) => nsItemProvider -> typeIdentifier -> NSItemProviderRepresentationVisibility -> Ptr () -> IO ()
registerDataRepresentationForTypeIdentifier_visibility_loadHandler nsItemProvider  typeIdentifier visibility loadHandler =
withObjCPtr typeIdentifier $ \raw_typeIdentifier ->
    sendMsg nsItemProvider (mkSelector "registerDataRepresentationForTypeIdentifier:visibility:loadHandler:") retVoid [argPtr (castPtr raw_typeIdentifier :: Ptr ()), argCLong (coerce visibility), argPtr (castPtr loadHandler :: Ptr ())]

-- | @- registerFileRepresentationForTypeIdentifier:fileOptions:visibility:loadHandler:@
registerFileRepresentationForTypeIdentifier_fileOptions_visibility_loadHandler :: (IsNSItemProvider nsItemProvider, IsNSString typeIdentifier) => nsItemProvider -> typeIdentifier -> NSItemProviderFileOptions -> NSItemProviderRepresentationVisibility -> Ptr () -> IO ()
registerFileRepresentationForTypeIdentifier_fileOptions_visibility_loadHandler nsItemProvider  typeIdentifier fileOptions visibility loadHandler =
withObjCPtr typeIdentifier $ \raw_typeIdentifier ->
    sendMsg nsItemProvider (mkSelector "registerFileRepresentationForTypeIdentifier:fileOptions:visibility:loadHandler:") retVoid [argPtr (castPtr raw_typeIdentifier :: Ptr ()), argCLong (coerce fileOptions), argCLong (coerce visibility), argPtr (castPtr loadHandler :: Ptr ())]

-- | @- registeredTypeIdentifiersWithFileOptions:@
registeredTypeIdentifiersWithFileOptions :: IsNSItemProvider nsItemProvider => nsItemProvider -> NSItemProviderFileOptions -> IO (Id NSArray)
registeredTypeIdentifiersWithFileOptions nsItemProvider  fileOptions =
  sendMsg nsItemProvider (mkSelector "registeredTypeIdentifiersWithFileOptions:") (retPtr retVoid) [argCLong (coerce fileOptions)] >>= retainedObject . castPtr

-- | @- hasItemConformingToTypeIdentifier:@
hasItemConformingToTypeIdentifier :: (IsNSItemProvider nsItemProvider, IsNSString typeIdentifier) => nsItemProvider -> typeIdentifier -> IO Bool
hasItemConformingToTypeIdentifier nsItemProvider  typeIdentifier =
withObjCPtr typeIdentifier $ \raw_typeIdentifier ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsItemProvider (mkSelector "hasItemConformingToTypeIdentifier:") retCULong [argPtr (castPtr raw_typeIdentifier :: Ptr ())]

-- | @- hasRepresentationConformingToTypeIdentifier:fileOptions:@
hasRepresentationConformingToTypeIdentifier_fileOptions :: (IsNSItemProvider nsItemProvider, IsNSString typeIdentifier) => nsItemProvider -> typeIdentifier -> NSItemProviderFileOptions -> IO Bool
hasRepresentationConformingToTypeIdentifier_fileOptions nsItemProvider  typeIdentifier fileOptions =
withObjCPtr typeIdentifier $ \raw_typeIdentifier ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsItemProvider (mkSelector "hasRepresentationConformingToTypeIdentifier:fileOptions:") retCULong [argPtr (castPtr raw_typeIdentifier :: Ptr ()), argCLong (coerce fileOptions)]

-- | @- loadDataRepresentationForTypeIdentifier:completionHandler:@
loadDataRepresentationForTypeIdentifier_completionHandler :: (IsNSItemProvider nsItemProvider, IsNSString typeIdentifier) => nsItemProvider -> typeIdentifier -> Ptr () -> IO (Id NSProgress)
loadDataRepresentationForTypeIdentifier_completionHandler nsItemProvider  typeIdentifier completionHandler =
withObjCPtr typeIdentifier $ \raw_typeIdentifier ->
    sendMsg nsItemProvider (mkSelector "loadDataRepresentationForTypeIdentifier:completionHandler:") (retPtr retVoid) [argPtr (castPtr raw_typeIdentifier :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= retainedObject . castPtr

-- | @- loadFileRepresentationForTypeIdentifier:completionHandler:@
loadFileRepresentationForTypeIdentifier_completionHandler :: (IsNSItemProvider nsItemProvider, IsNSString typeIdentifier) => nsItemProvider -> typeIdentifier -> Ptr () -> IO (Id NSProgress)
loadFileRepresentationForTypeIdentifier_completionHandler nsItemProvider  typeIdentifier completionHandler =
withObjCPtr typeIdentifier $ \raw_typeIdentifier ->
    sendMsg nsItemProvider (mkSelector "loadFileRepresentationForTypeIdentifier:completionHandler:") (retPtr retVoid) [argPtr (castPtr raw_typeIdentifier :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= retainedObject . castPtr

-- | @- loadInPlaceFileRepresentationForTypeIdentifier:completionHandler:@
loadInPlaceFileRepresentationForTypeIdentifier_completionHandler :: (IsNSItemProvider nsItemProvider, IsNSString typeIdentifier) => nsItemProvider -> typeIdentifier -> Ptr () -> IO (Id NSProgress)
loadInPlaceFileRepresentationForTypeIdentifier_completionHandler nsItemProvider  typeIdentifier completionHandler =
withObjCPtr typeIdentifier $ \raw_typeIdentifier ->
    sendMsg nsItemProvider (mkSelector "loadInPlaceFileRepresentationForTypeIdentifier:completionHandler:") (retPtr retVoid) [argPtr (castPtr raw_typeIdentifier :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithObject:@
initWithObject :: IsNSItemProvider nsItemProvider => nsItemProvider -> RawId -> IO (Id NSItemProvider)
initWithObject nsItemProvider  object =
  sendMsg nsItemProvider (mkSelector "initWithObject:") (retPtr retVoid) [argPtr (castPtr (unRawId object) :: Ptr ())] >>= ownedObject . castPtr

-- | @- registerObject:visibility:@
registerObject_visibility :: IsNSItemProvider nsItemProvider => nsItemProvider -> RawId -> NSItemProviderRepresentationVisibility -> IO ()
registerObject_visibility nsItemProvider  object visibility =
  sendMsg nsItemProvider (mkSelector "registerObject:visibility:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ()), argCLong (coerce visibility)]

-- | @- canLoadObjectOfClass:@
canLoadObjectOfClass :: IsNSItemProvider nsItemProvider => nsItemProvider -> Class -> IO Bool
canLoadObjectOfClass nsItemProvider  aClass =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsItemProvider (mkSelector "canLoadObjectOfClass:") retCULong [argPtr (unClass aClass)]

-- | @- initWithItem:typeIdentifier:@
initWithItem_typeIdentifier :: (IsNSItemProvider nsItemProvider, IsNSString typeIdentifier) => nsItemProvider -> RawId -> typeIdentifier -> IO (Id NSItemProvider)
initWithItem_typeIdentifier nsItemProvider  item typeIdentifier =
withObjCPtr typeIdentifier $ \raw_typeIdentifier ->
    sendMsg nsItemProvider (mkSelector "initWithItem:typeIdentifier:") (retPtr retVoid) [argPtr (castPtr (unRawId item) :: Ptr ()), argPtr (castPtr raw_typeIdentifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithContentsOfURL:@
initWithContentsOfURL :: (IsNSItemProvider nsItemProvider, IsNSURL fileURL) => nsItemProvider -> fileURL -> IO (Id NSItemProvider)
initWithContentsOfURL nsItemProvider  fileURL =
withObjCPtr fileURL $ \raw_fileURL ->
    sendMsg nsItemProvider (mkSelector "initWithContentsOfURL:") (retPtr retVoid) [argPtr (castPtr raw_fileURL :: Ptr ())] >>= ownedObject . castPtr

-- | @- registerItemForTypeIdentifier:loadHandler:@
registerItemForTypeIdentifier_loadHandler :: (IsNSItemProvider nsItemProvider, IsNSString typeIdentifier) => nsItemProvider -> typeIdentifier -> Ptr () -> IO ()
registerItemForTypeIdentifier_loadHandler nsItemProvider  typeIdentifier loadHandler =
withObjCPtr typeIdentifier $ \raw_typeIdentifier ->
    sendMsg nsItemProvider (mkSelector "registerItemForTypeIdentifier:loadHandler:") retVoid [argPtr (castPtr raw_typeIdentifier :: Ptr ()), argPtr (castPtr loadHandler :: Ptr ())]

-- | @- loadItemForTypeIdentifier:options:completionHandler:@
loadItemForTypeIdentifier_options_completionHandler :: (IsNSItemProvider nsItemProvider, IsNSString typeIdentifier, IsNSDictionary options) => nsItemProvider -> typeIdentifier -> options -> Ptr () -> IO ()
loadItemForTypeIdentifier_options_completionHandler nsItemProvider  typeIdentifier options completionHandler =
withObjCPtr typeIdentifier $ \raw_typeIdentifier ->
  withObjCPtr options $ \raw_options ->
      sendMsg nsItemProvider (mkSelector "loadItemForTypeIdentifier:options:completionHandler:") retVoid [argPtr (castPtr raw_typeIdentifier :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- loadPreviewImageWithOptions:completionHandler:@
loadPreviewImageWithOptions_completionHandler :: (IsNSItemProvider nsItemProvider, IsNSDictionary options) => nsItemProvider -> options -> Ptr () -> IO ()
loadPreviewImageWithOptions_completionHandler nsItemProvider  options completionHandler =
withObjCPtr options $ \raw_options ->
    sendMsg nsItemProvider (mkSelector "loadPreviewImageWithOptions:completionHandler:") retVoid [argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- registeredTypeIdentifiers@
registeredTypeIdentifiers :: IsNSItemProvider nsItemProvider => nsItemProvider -> IO (Id NSArray)
registeredTypeIdentifiers nsItemProvider  =
  sendMsg nsItemProvider (mkSelector "registeredTypeIdentifiers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- suggestedName@
suggestedName :: IsNSItemProvider nsItemProvider => nsItemProvider -> IO (Id NSString)
suggestedName nsItemProvider  =
  sendMsg nsItemProvider (mkSelector "suggestedName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSuggestedName:@
setSuggestedName :: (IsNSItemProvider nsItemProvider, IsNSString value) => nsItemProvider -> value -> IO ()
setSuggestedName nsItemProvider  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsItemProvider (mkSelector "setSuggestedName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- previewImageHandler@
previewImageHandler :: IsNSItemProvider nsItemProvider => nsItemProvider -> IO (Ptr ())
previewImageHandler nsItemProvider  =
  fmap castPtr $ sendMsg nsItemProvider (mkSelector "previewImageHandler") (retPtr retVoid) []

-- | @- setPreviewImageHandler:@
setPreviewImageHandler :: IsNSItemProvider nsItemProvider => nsItemProvider -> Ptr () -> IO ()
setPreviewImageHandler nsItemProvider  value =
  sendMsg nsItemProvider (mkSelector "setPreviewImageHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @registerDataRepresentationForTypeIdentifier:visibility:loadHandler:@
registerDataRepresentationForTypeIdentifier_visibility_loadHandlerSelector :: Selector
registerDataRepresentationForTypeIdentifier_visibility_loadHandlerSelector = mkSelector "registerDataRepresentationForTypeIdentifier:visibility:loadHandler:"

-- | @Selector@ for @registerFileRepresentationForTypeIdentifier:fileOptions:visibility:loadHandler:@
registerFileRepresentationForTypeIdentifier_fileOptions_visibility_loadHandlerSelector :: Selector
registerFileRepresentationForTypeIdentifier_fileOptions_visibility_loadHandlerSelector = mkSelector "registerFileRepresentationForTypeIdentifier:fileOptions:visibility:loadHandler:"

-- | @Selector@ for @registeredTypeIdentifiersWithFileOptions:@
registeredTypeIdentifiersWithFileOptionsSelector :: Selector
registeredTypeIdentifiersWithFileOptionsSelector = mkSelector "registeredTypeIdentifiersWithFileOptions:"

-- | @Selector@ for @hasItemConformingToTypeIdentifier:@
hasItemConformingToTypeIdentifierSelector :: Selector
hasItemConformingToTypeIdentifierSelector = mkSelector "hasItemConformingToTypeIdentifier:"

-- | @Selector@ for @hasRepresentationConformingToTypeIdentifier:fileOptions:@
hasRepresentationConformingToTypeIdentifier_fileOptionsSelector :: Selector
hasRepresentationConformingToTypeIdentifier_fileOptionsSelector = mkSelector "hasRepresentationConformingToTypeIdentifier:fileOptions:"

-- | @Selector@ for @loadDataRepresentationForTypeIdentifier:completionHandler:@
loadDataRepresentationForTypeIdentifier_completionHandlerSelector :: Selector
loadDataRepresentationForTypeIdentifier_completionHandlerSelector = mkSelector "loadDataRepresentationForTypeIdentifier:completionHandler:"

-- | @Selector@ for @loadFileRepresentationForTypeIdentifier:completionHandler:@
loadFileRepresentationForTypeIdentifier_completionHandlerSelector :: Selector
loadFileRepresentationForTypeIdentifier_completionHandlerSelector = mkSelector "loadFileRepresentationForTypeIdentifier:completionHandler:"

-- | @Selector@ for @loadInPlaceFileRepresentationForTypeIdentifier:completionHandler:@
loadInPlaceFileRepresentationForTypeIdentifier_completionHandlerSelector :: Selector
loadInPlaceFileRepresentationForTypeIdentifier_completionHandlerSelector = mkSelector "loadInPlaceFileRepresentationForTypeIdentifier:completionHandler:"

-- | @Selector@ for @initWithObject:@
initWithObjectSelector :: Selector
initWithObjectSelector = mkSelector "initWithObject:"

-- | @Selector@ for @registerObject:visibility:@
registerObject_visibilitySelector :: Selector
registerObject_visibilitySelector = mkSelector "registerObject:visibility:"

-- | @Selector@ for @canLoadObjectOfClass:@
canLoadObjectOfClassSelector :: Selector
canLoadObjectOfClassSelector = mkSelector "canLoadObjectOfClass:"

-- | @Selector@ for @initWithItem:typeIdentifier:@
initWithItem_typeIdentifierSelector :: Selector
initWithItem_typeIdentifierSelector = mkSelector "initWithItem:typeIdentifier:"

-- | @Selector@ for @initWithContentsOfURL:@
initWithContentsOfURLSelector :: Selector
initWithContentsOfURLSelector = mkSelector "initWithContentsOfURL:"

-- | @Selector@ for @registerItemForTypeIdentifier:loadHandler:@
registerItemForTypeIdentifier_loadHandlerSelector :: Selector
registerItemForTypeIdentifier_loadHandlerSelector = mkSelector "registerItemForTypeIdentifier:loadHandler:"

-- | @Selector@ for @loadItemForTypeIdentifier:options:completionHandler:@
loadItemForTypeIdentifier_options_completionHandlerSelector :: Selector
loadItemForTypeIdentifier_options_completionHandlerSelector = mkSelector "loadItemForTypeIdentifier:options:completionHandler:"

-- | @Selector@ for @loadPreviewImageWithOptions:completionHandler:@
loadPreviewImageWithOptions_completionHandlerSelector :: Selector
loadPreviewImageWithOptions_completionHandlerSelector = mkSelector "loadPreviewImageWithOptions:completionHandler:"

-- | @Selector@ for @registeredTypeIdentifiers@
registeredTypeIdentifiersSelector :: Selector
registeredTypeIdentifiersSelector = mkSelector "registeredTypeIdentifiers"

-- | @Selector@ for @suggestedName@
suggestedNameSelector :: Selector
suggestedNameSelector = mkSelector "suggestedName"

-- | @Selector@ for @setSuggestedName:@
setSuggestedNameSelector :: Selector
setSuggestedNameSelector = mkSelector "setSuggestedName:"

-- | @Selector@ for @previewImageHandler@
previewImageHandlerSelector :: Selector
previewImageHandlerSelector = mkSelector "previewImageHandler"

-- | @Selector@ for @setPreviewImageHandler:@
setPreviewImageHandlerSelector :: Selector
setPreviewImageHandlerSelector = mkSelector "setPreviewImageHandler:"

