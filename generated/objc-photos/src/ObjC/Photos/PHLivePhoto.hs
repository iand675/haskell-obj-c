{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHLivePhoto@.
module ObjC.Photos.PHLivePhoto
  ( PHLivePhoto
  , IsPHLivePhoto(..)
  , init_
  , cancelLivePhotoRequestWithRequestID
  , initSelector
  , cancelLivePhotoRequestWithRequestIDSelector

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
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Photos.Internal.Classes
import ObjC.Photos.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHLivePhoto phLivePhoto => phLivePhoto -> IO (Id PHLivePhoto)
init_ phLivePhoto  =
  sendMsg phLivePhoto (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Cancels the loading of a PHLivePhoto. The request's completion handler will be called.
--
-- ObjC selector: @+ cancelLivePhotoRequestWithRequestID:@
cancelLivePhotoRequestWithRequestID :: CInt -> IO ()
cancelLivePhotoRequestWithRequestID requestID =
  do
    cls' <- getRequiredClass "PHLivePhoto"
    sendClassMsg cls' (mkSelector "cancelLivePhotoRequestWithRequestID:") retVoid [argCInt (fromIntegral requestID)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @cancelLivePhotoRequestWithRequestID:@
cancelLivePhotoRequestWithRequestIDSelector :: Selector
cancelLivePhotoRequestWithRequestIDSelector = mkSelector "cancelLivePhotoRequestWithRequestID:"

