{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHLivePhoto@.
module ObjC.Photos.PHLivePhoto
  ( PHLivePhoto
  , IsPHLivePhoto(..)
  , init_
  , cancelLivePhotoRequestWithRequestID
  , cancelLivePhotoRequestWithRequestIDSelector
  , initSelector

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
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHLivePhoto phLivePhoto => phLivePhoto -> IO (Id PHLivePhoto)
init_ phLivePhoto =
  sendOwnedMessage phLivePhoto initSelector

-- | Cancels the loading of a PHLivePhoto. The request's completion handler will be called.
--
-- ObjC selector: @+ cancelLivePhotoRequestWithRequestID:@
cancelLivePhotoRequestWithRequestID :: CInt -> IO ()
cancelLivePhotoRequestWithRequestID requestID =
  do
    cls' <- getRequiredClass "PHLivePhoto"
    sendClassMessage cls' cancelLivePhotoRequestWithRequestIDSelector requestID

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHLivePhoto)
initSelector = mkSelector "init"

-- | @Selector@ for @cancelLivePhotoRequestWithRequestID:@
cancelLivePhotoRequestWithRequestIDSelector :: Selector '[CInt] ()
cancelLivePhotoRequestWithRequestIDSelector = mkSelector "cancelLivePhotoRequestWithRequestID:"

