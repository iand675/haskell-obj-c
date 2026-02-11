{-# LANGUAGE PatternSynonyms #-}
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
  , stopCachingImagesForAllAssetsSelector
  , allowsCachingHighQualityImagesSelector
  , setAllowsCachingHighQualityImagesSelector

  -- * Enum types
  , PHImageContentMode(PHImageContentMode)
  , pattern PHImageContentModeAspectFit
  , pattern PHImageContentModeAspectFill
  , pattern PHImageContentModeDefault

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
import ObjC.Photos.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- stopCachingImagesForAllAssets@
stopCachingImagesForAllAssets :: IsPHCachingImageManager phCachingImageManager => phCachingImageManager -> IO ()
stopCachingImagesForAllAssets phCachingImageManager  =
  sendMsg phCachingImageManager (mkSelector "stopCachingImagesForAllAssets") retVoid []

-- | @- allowsCachingHighQualityImages@
allowsCachingHighQualityImages :: IsPHCachingImageManager phCachingImageManager => phCachingImageManager -> IO Bool
allowsCachingHighQualityImages phCachingImageManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg phCachingImageManager (mkSelector "allowsCachingHighQualityImages") retCULong []

-- | @- setAllowsCachingHighQualityImages:@
setAllowsCachingHighQualityImages :: IsPHCachingImageManager phCachingImageManager => phCachingImageManager -> Bool -> IO ()
setAllowsCachingHighQualityImages phCachingImageManager  value =
  sendMsg phCachingImageManager (mkSelector "setAllowsCachingHighQualityImages:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stopCachingImagesForAllAssets@
stopCachingImagesForAllAssetsSelector :: Selector
stopCachingImagesForAllAssetsSelector = mkSelector "stopCachingImagesForAllAssets"

-- | @Selector@ for @allowsCachingHighQualityImages@
allowsCachingHighQualityImagesSelector :: Selector
allowsCachingHighQualityImagesSelector = mkSelector "allowsCachingHighQualityImages"

-- | @Selector@ for @setAllowsCachingHighQualityImages:@
setAllowsCachingHighQualityImagesSelector :: Selector
setAllowsCachingHighQualityImagesSelector = mkSelector "setAllowsCachingHighQualityImages:"

