{-# LANGUAGE DataKinds #-}
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
  , changeRequestForAssetSelector
  , contentEditingOutputSelector
  , creationDateSelector
  , creationRequestForAssetFromImageAtFileURLSelector
  , creationRequestForAssetFromImageSelector
  , creationRequestForAssetFromVideoAtFileURLSelector
  , deleteAssetsSelector
  , favoriteSelector
  , hiddenSelector
  , placeholderForCreatedAssetSelector
  , revertAssetContentToOriginalSelector
  , setContentEditingOutputSelector
  , setCreationDateSelector
  , setFavoriteSelector
  , setHiddenSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' creationRequestForAssetFromImageSelector (toNSImage image)

-- | @+ creationRequestForAssetFromImageAtFileURL:@
creationRequestForAssetFromImageAtFileURL :: IsNSURL fileURL => fileURL -> IO (Id PHAssetChangeRequest)
creationRequestForAssetFromImageAtFileURL fileURL =
  do
    cls' <- getRequiredClass "PHAssetChangeRequest"
    sendClassMessage cls' creationRequestForAssetFromImageAtFileURLSelector (toNSURL fileURL)

-- | @+ creationRequestForAssetFromVideoAtFileURL:@
creationRequestForAssetFromVideoAtFileURL :: IsNSURL fileURL => fileURL -> IO (Id PHAssetChangeRequest)
creationRequestForAssetFromVideoAtFileURL fileURL =
  do
    cls' <- getRequiredClass "PHAssetChangeRequest"
    sendClassMessage cls' creationRequestForAssetFromVideoAtFileURLSelector (toNSURL fileURL)

-- | @+ deleteAssets:@
deleteAssets :: RawId -> IO ()
deleteAssets assets =
  do
    cls' <- getRequiredClass "PHAssetChangeRequest"
    sendClassMessage cls' deleteAssetsSelector assets

-- | @+ changeRequestForAsset:@
changeRequestForAsset :: IsPHAsset asset => asset -> IO (Id PHAssetChangeRequest)
changeRequestForAsset asset =
  do
    cls' <- getRequiredClass "PHAssetChangeRequest"
    sendClassMessage cls' changeRequestForAssetSelector (toPHAsset asset)

-- | @- revertAssetContentToOriginal@
revertAssetContentToOriginal :: IsPHAssetChangeRequest phAssetChangeRequest => phAssetChangeRequest -> IO ()
revertAssetContentToOriginal phAssetChangeRequest =
  sendMessage phAssetChangeRequest revertAssetContentToOriginalSelector

-- | @- placeholderForCreatedAsset@
placeholderForCreatedAsset :: IsPHAssetChangeRequest phAssetChangeRequest => phAssetChangeRequest -> IO (Id PHObjectPlaceholder)
placeholderForCreatedAsset phAssetChangeRequest =
  sendMessage phAssetChangeRequest placeholderForCreatedAssetSelector

-- | @- creationDate@
creationDate :: IsPHAssetChangeRequest phAssetChangeRequest => phAssetChangeRequest -> IO (Id NSDate)
creationDate phAssetChangeRequest =
  sendMessage phAssetChangeRequest creationDateSelector

-- | @- setCreationDate:@
setCreationDate :: (IsPHAssetChangeRequest phAssetChangeRequest, IsNSDate value) => phAssetChangeRequest -> value -> IO ()
setCreationDate phAssetChangeRequest value =
  sendMessage phAssetChangeRequest setCreationDateSelector (toNSDate value)

-- | @- favorite@
favorite :: IsPHAssetChangeRequest phAssetChangeRequest => phAssetChangeRequest -> IO Bool
favorite phAssetChangeRequest =
  sendMessage phAssetChangeRequest favoriteSelector

-- | @- setFavorite:@
setFavorite :: IsPHAssetChangeRequest phAssetChangeRequest => phAssetChangeRequest -> Bool -> IO ()
setFavorite phAssetChangeRequest value =
  sendMessage phAssetChangeRequest setFavoriteSelector value

-- | @- hidden@
hidden :: IsPHAssetChangeRequest phAssetChangeRequest => phAssetChangeRequest -> IO Bool
hidden phAssetChangeRequest =
  sendMessage phAssetChangeRequest hiddenSelector

-- | @- setHidden:@
setHidden :: IsPHAssetChangeRequest phAssetChangeRequest => phAssetChangeRequest -> Bool -> IO ()
setHidden phAssetChangeRequest value =
  sendMessage phAssetChangeRequest setHiddenSelector value

-- | @- contentEditingOutput@
contentEditingOutput :: IsPHAssetChangeRequest phAssetChangeRequest => phAssetChangeRequest -> IO (Id PHContentEditingOutput)
contentEditingOutput phAssetChangeRequest =
  sendMessage phAssetChangeRequest contentEditingOutputSelector

-- | @- setContentEditingOutput:@
setContentEditingOutput :: (IsPHAssetChangeRequest phAssetChangeRequest, IsPHContentEditingOutput value) => phAssetChangeRequest -> value -> IO ()
setContentEditingOutput phAssetChangeRequest value =
  sendMessage phAssetChangeRequest setContentEditingOutputSelector (toPHContentEditingOutput value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @creationRequestForAssetFromImage:@
creationRequestForAssetFromImageSelector :: Selector '[Id NSImage] (Id PHAssetChangeRequest)
creationRequestForAssetFromImageSelector = mkSelector "creationRequestForAssetFromImage:"

-- | @Selector@ for @creationRequestForAssetFromImageAtFileURL:@
creationRequestForAssetFromImageAtFileURLSelector :: Selector '[Id NSURL] (Id PHAssetChangeRequest)
creationRequestForAssetFromImageAtFileURLSelector = mkSelector "creationRequestForAssetFromImageAtFileURL:"

-- | @Selector@ for @creationRequestForAssetFromVideoAtFileURL:@
creationRequestForAssetFromVideoAtFileURLSelector :: Selector '[Id NSURL] (Id PHAssetChangeRequest)
creationRequestForAssetFromVideoAtFileURLSelector = mkSelector "creationRequestForAssetFromVideoAtFileURL:"

-- | @Selector@ for @deleteAssets:@
deleteAssetsSelector :: Selector '[RawId] ()
deleteAssetsSelector = mkSelector "deleteAssets:"

-- | @Selector@ for @changeRequestForAsset:@
changeRequestForAssetSelector :: Selector '[Id PHAsset] (Id PHAssetChangeRequest)
changeRequestForAssetSelector = mkSelector "changeRequestForAsset:"

-- | @Selector@ for @revertAssetContentToOriginal@
revertAssetContentToOriginalSelector :: Selector '[] ()
revertAssetContentToOriginalSelector = mkSelector "revertAssetContentToOriginal"

-- | @Selector@ for @placeholderForCreatedAsset@
placeholderForCreatedAssetSelector :: Selector '[] (Id PHObjectPlaceholder)
placeholderForCreatedAssetSelector = mkSelector "placeholderForCreatedAsset"

-- | @Selector@ for @creationDate@
creationDateSelector :: Selector '[] (Id NSDate)
creationDateSelector = mkSelector "creationDate"

-- | @Selector@ for @setCreationDate:@
setCreationDateSelector :: Selector '[Id NSDate] ()
setCreationDateSelector = mkSelector "setCreationDate:"

-- | @Selector@ for @favorite@
favoriteSelector :: Selector '[] Bool
favoriteSelector = mkSelector "favorite"

-- | @Selector@ for @setFavorite:@
setFavoriteSelector :: Selector '[Bool] ()
setFavoriteSelector = mkSelector "setFavorite:"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector '[] Bool
hiddenSelector = mkSelector "hidden"

-- | @Selector@ for @setHidden:@
setHiddenSelector :: Selector '[Bool] ()
setHiddenSelector = mkSelector "setHidden:"

-- | @Selector@ for @contentEditingOutput@
contentEditingOutputSelector :: Selector '[] (Id PHContentEditingOutput)
contentEditingOutputSelector = mkSelector "contentEditingOutput"

-- | @Selector@ for @setContentEditingOutput:@
setContentEditingOutputSelector :: Selector '[Id PHContentEditingOutput] ()
setContentEditingOutputSelector = mkSelector "setContentEditingOutput:"

