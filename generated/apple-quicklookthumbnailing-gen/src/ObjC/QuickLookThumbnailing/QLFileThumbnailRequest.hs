{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This class contains information about the thumbnail that should be provided.
--
-- Generated bindings for @QLFileThumbnailRequest@.
module ObjC.QuickLookThumbnailing.QLFileThumbnailRequest
  ( QLFileThumbnailRequest
  , IsQLFileThumbnailRequest(..)
  , scale
  , fileURL
  , fileURLSelector
  , scaleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuickLookThumbnailing.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The scale of the requested thumbnail.
--
-- ObjC selector: @- scale@
scale :: IsQLFileThumbnailRequest qlFileThumbnailRequest => qlFileThumbnailRequest -> IO CDouble
scale qlFileThumbnailRequest =
  sendMessage qlFileThumbnailRequest scaleSelector

-- | The url of the file for which a thumbnail is being requested.
--
-- ObjC selector: @- fileURL@
fileURL :: IsQLFileThumbnailRequest qlFileThumbnailRequest => qlFileThumbnailRequest -> IO (Id NSURL)
fileURL qlFileThumbnailRequest =
  sendMessage qlFileThumbnailRequest fileURLSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @scale@
scaleSelector :: Selector '[] CDouble
scaleSelector = mkSelector "scale"

-- | @Selector@ for @fileURL@
fileURLSelector :: Selector '[] (Id NSURL)
fileURLSelector = mkSelector "fileURL"

