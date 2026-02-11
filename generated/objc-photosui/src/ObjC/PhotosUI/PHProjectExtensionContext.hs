{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | When a Photos project extension is initialized, it is handed a PHProjectExtensionContext object. This object provides the extension access to the underlying project as well as the photo library from which assets can be fetched.
--
-- Generated bindings for @PHProjectExtensionContext@.
module ObjC.PhotosUI.PHProjectExtensionContext
  ( PHProjectExtensionContext
  , IsPHProjectExtensionContext(..)
  , showEditorForAsset
  , updatedProjectInfoFromProjectInfo_completion
  , photoLibrary
  , project
  , showEditorForAssetSelector
  , updatedProjectInfoFromProjectInfo_completionSelector
  , photoLibrarySelector
  , projectSelector


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

import ObjC.PhotosUI.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.Photos.Internal.Classes

-- | Invokes the Photos Editor for the given asset.
--
-- @asset@ — The asset to edit.
--
-- Note: The extension should observe library changes to get notified when assets are changed/edited.
--
-- See: PHPhotoLibraryChangeObserver
--
-- ObjC selector: @- showEditorForAsset:@
showEditorForAsset :: (IsPHProjectExtensionContext phProjectExtensionContext, IsPHAsset asset) => phProjectExtensionContext -> asset -> IO ()
showEditorForAsset phProjectExtensionContext  asset =
withObjCPtr asset $ \raw_asset ->
    sendMsg phProjectExtensionContext (mkSelector "showEditorForAsset:") retVoid [argPtr (castPtr raw_asset :: Ptr ())]

-- | Creates an updated PHProjectInfo from the given projectInfo and the current assets in the PHProject. If the existingProjectInfo is not nil the extension sections will be update to reflect any deletions from the photo library and a new section is appended for any assets in the project which weren't referenced in existingProjectInfo.
--
-- @existingProjectInfo@ — PHProjectInfo to update.                            If existingProjectInfo is nil a new PHProjectInfo will be created from all assets in the PHProject.
--
-- @completion@ — Completion block that is called with the update result.                            updatedProjectInfo is the updated project info, if the update was cancelled it might be nil.
--
-- Returns: NSProgress which can be observed, if it's canceled the original project info is returned.
--
-- ObjC selector: @- updatedProjectInfoFromProjectInfo:completion:@
updatedProjectInfoFromProjectInfo_completion :: (IsPHProjectExtensionContext phProjectExtensionContext, IsPHProjectInfo existingProjectInfo) => phProjectExtensionContext -> existingProjectInfo -> Ptr () -> IO (Id NSProgress)
updatedProjectInfoFromProjectInfo_completion phProjectExtensionContext  existingProjectInfo completion =
withObjCPtr existingProjectInfo $ \raw_existingProjectInfo ->
    sendMsg phProjectExtensionContext (mkSelector "updatedProjectInfoFromProjectInfo:completion:") (retPtr retVoid) [argPtr (castPtr raw_existingProjectInfo :: Ptr ()), argPtr (castPtr completion :: Ptr ())] >>= retainedObject . castPtr

-- | @- photoLibrary@
photoLibrary :: IsPHProjectExtensionContext phProjectExtensionContext => phProjectExtensionContext -> IO (Id PHPhotoLibrary)
photoLibrary phProjectExtensionContext  =
  sendMsg phProjectExtensionContext (mkSelector "photoLibrary") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- project@
project :: IsPHProjectExtensionContext phProjectExtensionContext => phProjectExtensionContext -> IO (Id PHProject)
project phProjectExtensionContext  =
  sendMsg phProjectExtensionContext (mkSelector "project") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @showEditorForAsset:@
showEditorForAssetSelector :: Selector
showEditorForAssetSelector = mkSelector "showEditorForAsset:"

-- | @Selector@ for @updatedProjectInfoFromProjectInfo:completion:@
updatedProjectInfoFromProjectInfo_completionSelector :: Selector
updatedProjectInfoFromProjectInfo_completionSelector = mkSelector "updatedProjectInfoFromProjectInfo:completion:"

-- | @Selector@ for @photoLibrary@
photoLibrarySelector :: Selector
photoLibrarySelector = mkSelector "photoLibrary"

-- | @Selector@ for @project@
projectSelector :: Selector
projectSelector = mkSelector "project"

