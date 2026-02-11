{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHAssetChangeRequest@.
module ObjC.Photos.PHAssetChangeRequest
  ( PHAssetChangeRequest
  , IsPHAssetChangeRequest(..)
  , creationRequestForAssetFromImage
  , creationRequestForAssetFromImageAtFileURL
  , creationRequestForAssetFromVideoAtFileURL
  , deleteAssets
  , changeRequestForAsset
  , revertAssetContentToOriginal
  , placeholderForCreatedAsset
  , creationDate
  , setCreationDate
  , favorite
  , setFavorite
  , hidden
  , setHidden
  , contentEditingOutput
  , setContentEditingOutput
  , creationRequestForAssetFromImageSelector
  , creationRequestForAssetFromImageAtFileURLSelector
  , creationRequestForAssetFromVideoAtFileURLSelector
  , deleteAssetsSelector
  , changeRequestForAssetSelector
  , revertAssetContentToOriginalSelector
  , placeholderForCreatedAssetSelector
  , creationDateSelector
  , setCreationDateSelector
  , favoriteSelector
  , setFavoriteSelector
  , hiddenSelector
  , setHiddenSelector
  , contentEditingOutputSelector
  , setContentEditingOutputSelector


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

import ObjC.Photos.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ creationRequestForAssetFromImage:@
creationRequestForAssetFromImage :: IsNSImage image => image -> IO (Id PHAssetChangeRequest)
creationRequestForAssetFromImage image =
  do
    cls' <- getRequiredClass "PHAssetChangeRequest"
    withObjCPtr image $ \raw_image ->
      sendClassMsg cls' (mkSelector "creationRequestForAssetFromImage:") (retPtr retVoid) [argPtr (castPtr raw_image :: Ptr ())] >>= retainedObject . castPtr

-- | @+ creationRequestForAssetFromImageAtFileURL:@
creationRequestForAssetFromImageAtFileURL :: IsNSURL fileURL => fileURL -> IO (Id PHAssetChangeRequest)
creationRequestForAssetFromImageAtFileURL fileURL =
  do
    cls' <- getRequiredClass "PHAssetChangeRequest"
    withObjCPtr fileURL $ \raw_fileURL ->
      sendClassMsg cls' (mkSelector "creationRequestForAssetFromImageAtFileURL:") (retPtr retVoid) [argPtr (castPtr raw_fileURL :: Ptr ())] >>= retainedObject . castPtr

-- | @+ creationRequestForAssetFromVideoAtFileURL:@
creationRequestForAssetFromVideoAtFileURL :: IsNSURL fileURL => fileURL -> IO (Id PHAssetChangeRequest)
creationRequestForAssetFromVideoAtFileURL fileURL =
  do
    cls' <- getRequiredClass "PHAssetChangeRequest"
    withObjCPtr fileURL $ \raw_fileURL ->
      sendClassMsg cls' (mkSelector "creationRequestForAssetFromVideoAtFileURL:") (retPtr retVoid) [argPtr (castPtr raw_fileURL :: Ptr ())] >>= retainedObject . castPtr

-- | @+ deleteAssets:@
deleteAssets :: RawId -> IO ()
deleteAssets assets =
  do
    cls' <- getRequiredClass "PHAssetChangeRequest"
    sendClassMsg cls' (mkSelector "deleteAssets:") retVoid [argPtr (castPtr (unRawId assets) :: Ptr ())]

-- | @+ changeRequestForAsset:@
changeRequestForAsset :: IsPHAsset asset => asset -> IO (Id PHAssetChangeRequest)
changeRequestForAsset asset =
  do
    cls' <- getRequiredClass "PHAssetChangeRequest"
    withObjCPtr asset $ \raw_asset ->
      sendClassMsg cls' (mkSelector "changeRequestForAsset:") (retPtr retVoid) [argPtr (castPtr raw_asset :: Ptr ())] >>= retainedObject . castPtr

-- | @- revertAssetContentToOriginal@
revertAssetContentToOriginal :: IsPHAssetChangeRequest phAssetChangeRequest => phAssetChangeRequest -> IO ()
revertAssetContentToOriginal phAssetChangeRequest  =
  sendMsg phAssetChangeRequest (mkSelector "revertAssetContentToOriginal") retVoid []

-- | @- placeholderForCreatedAsset@
placeholderForCreatedAsset :: IsPHAssetChangeRequest phAssetChangeRequest => phAssetChangeRequest -> IO (Id PHObjectPlaceholder)
placeholderForCreatedAsset phAssetChangeRequest  =
  sendMsg phAssetChangeRequest (mkSelector "placeholderForCreatedAsset") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- creationDate@
creationDate :: IsPHAssetChangeRequest phAssetChangeRequest => phAssetChangeRequest -> IO (Id NSDate)
creationDate phAssetChangeRequest  =
  sendMsg phAssetChangeRequest (mkSelector "creationDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCreationDate:@
setCreationDate :: (IsPHAssetChangeRequest phAssetChangeRequest, IsNSDate value) => phAssetChangeRequest -> value -> IO ()
setCreationDate phAssetChangeRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg phAssetChangeRequest (mkSelector "setCreationDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- favorite@
favorite :: IsPHAssetChangeRequest phAssetChangeRequest => phAssetChangeRequest -> IO Bool
favorite phAssetChangeRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg phAssetChangeRequest (mkSelector "favorite") retCULong []

-- | @- setFavorite:@
setFavorite :: IsPHAssetChangeRequest phAssetChangeRequest => phAssetChangeRequest -> Bool -> IO ()
setFavorite phAssetChangeRequest  value =
  sendMsg phAssetChangeRequest (mkSelector "setFavorite:") retVoid [argCULong (if value then 1 else 0)]

-- | @- hidden@
hidden :: IsPHAssetChangeRequest phAssetChangeRequest => phAssetChangeRequest -> IO Bool
hidden phAssetChangeRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg phAssetChangeRequest (mkSelector "hidden") retCULong []

-- | @- setHidden:@
setHidden :: IsPHAssetChangeRequest phAssetChangeRequest => phAssetChangeRequest -> Bool -> IO ()
setHidden phAssetChangeRequest  value =
  sendMsg phAssetChangeRequest (mkSelector "setHidden:") retVoid [argCULong (if value then 1 else 0)]

-- | @- contentEditingOutput@
contentEditingOutput :: IsPHAssetChangeRequest phAssetChangeRequest => phAssetChangeRequest -> IO (Id PHContentEditingOutput)
contentEditingOutput phAssetChangeRequest  =
  sendMsg phAssetChangeRequest (mkSelector "contentEditingOutput") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContentEditingOutput:@
setContentEditingOutput :: (IsPHAssetChangeRequest phAssetChangeRequest, IsPHContentEditingOutput value) => phAssetChangeRequest -> value -> IO ()
setContentEditingOutput phAssetChangeRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg phAssetChangeRequest (mkSelector "setContentEditingOutput:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @creationRequestForAssetFromImage:@
creationRequestForAssetFromImageSelector :: Selector
creationRequestForAssetFromImageSelector = mkSelector "creationRequestForAssetFromImage:"

-- | @Selector@ for @creationRequestForAssetFromImageAtFileURL:@
creationRequestForAssetFromImageAtFileURLSelector :: Selector
creationRequestForAssetFromImageAtFileURLSelector = mkSelector "creationRequestForAssetFromImageAtFileURL:"

-- | @Selector@ for @creationRequestForAssetFromVideoAtFileURL:@
creationRequestForAssetFromVideoAtFileURLSelector :: Selector
creationRequestForAssetFromVideoAtFileURLSelector = mkSelector "creationRequestForAssetFromVideoAtFileURL:"

-- | @Selector@ for @deleteAssets:@
deleteAssetsSelector :: Selector
deleteAssetsSelector = mkSelector "deleteAssets:"

-- | @Selector@ for @changeRequestForAsset:@
changeRequestForAssetSelector :: Selector
changeRequestForAssetSelector = mkSelector "changeRequestForAsset:"

-- | @Selector@ for @revertAssetContentToOriginal@
revertAssetContentToOriginalSelector :: Selector
revertAssetContentToOriginalSelector = mkSelector "revertAssetContentToOriginal"

-- | @Selector@ for @placeholderForCreatedAsset@
placeholderForCreatedAssetSelector :: Selector
placeholderForCreatedAssetSelector = mkSelector "placeholderForCreatedAsset"

-- | @Selector@ for @creationDate@
creationDateSelector :: Selector
creationDateSelector = mkSelector "creationDate"

-- | @Selector@ for @setCreationDate:@
setCreationDateSelector :: Selector
setCreationDateSelector = mkSelector "setCreationDate:"

-- | @Selector@ for @favorite@
favoriteSelector :: Selector
favoriteSelector = mkSelector "favorite"

-- | @Selector@ for @setFavorite:@
setFavoriteSelector :: Selector
setFavoriteSelector = mkSelector "setFavorite:"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector
hiddenSelector = mkSelector "hidden"

-- | @Selector@ for @setHidden:@
setHiddenSelector :: Selector
setHiddenSelector = mkSelector "setHidden:"

-- | @Selector@ for @contentEditingOutput@
contentEditingOutputSelector :: Selector
contentEditingOutputSelector = mkSelector "contentEditingOutput"

-- | @Selector@ for @setContentEditingOutput:@
setContentEditingOutputSelector :: Selector
setContentEditingOutputSelector = mkSelector "setContentEditingOutput:"

