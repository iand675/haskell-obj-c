{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHCachingImageManager@.
module ObjC.Photos.PHCachingImageManager
  ( PHCachingImageManager
  , IsPHCachingImageManager(..)
  , stopCachingImagesForAllAssets
  , allowsCachingHighQualityImages
  , setAllowsCachingHighQualityImages
  , allowsCachingHighQualityImagesSelector
  , setAllowsCachingHighQualityImagesSelector
  , stopCachingImagesForAllAssetsSelector

  -- * Enum types
  , PHImageContentMode(PHImageContentMode)
  , pattern PHImageContentModeAspectFit
  , pattern PHImageContentModeAspectFill
  , pattern PHImageContentModeDefault

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Photos.Internal.Classes
import ObjC.Photos.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- stopCachingImagesForAllAssets@
stopCachingImagesForAllAssets :: IsPHCachingImageManager phCachingImageManager => phCachingImageManager -> IO ()
stopCachingImagesForAllAssets phCachingImageManager =
  sendMessage phCachingImageManager stopCachingImagesForAllAssetsSelector

-- | @- allowsCachingHighQualityImages@
allowsCachingHighQualityImages :: IsPHCachingImageManager phCachingImageManager => phCachingImageManager -> IO Bool
allowsCachingHighQualityImages phCachingImageManager =
  sendMessage phCachingImageManager allowsCachingHighQualityImagesSelector

-- | @- setAllowsCachingHighQualityImages:@
setAllowsCachingHighQualityImages :: IsPHCachingImageManager phCachingImageManager => phCachingImageManager -> Bool -> IO ()
setAllowsCachingHighQualityImages phCachingImageManager value =
  sendMessage phCachingImageManager setAllowsCachingHighQualityImagesSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stopCachingImagesForAllAssets@
stopCachingImagesForAllAssetsSelector :: Selector '[] ()
stopCachingImagesForAllAssetsSelector = mkSelector "stopCachingImagesForAllAssets"

-- | @Selector@ for @allowsCachingHighQualityImages@
allowsCachingHighQualityImagesSelector :: Selector '[] Bool
allowsCachingHighQualityImagesSelector = mkSelector "allowsCachingHighQualityImages"

-- | @Selector@ for @setAllowsCachingHighQualityImages:@
setAllowsCachingHighQualityImagesSelector :: Selector '[Bool] ()
setAllowsCachingHighQualityImagesSelector = mkSelector "setAllowsCachingHighQualityImages:"

