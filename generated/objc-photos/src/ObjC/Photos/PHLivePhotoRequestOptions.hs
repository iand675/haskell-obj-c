{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHLivePhotoRequestOptions@.
module ObjC.Photos.PHLivePhotoRequestOptions
  ( PHLivePhotoRequestOptions
  , IsPHLivePhotoRequestOptions(..)
  , version
  , setVersion
  , deliveryMode
  , setDeliveryMode
  , networkAccessAllowed
  , setNetworkAccessAllowed
  , progressHandler
  , setProgressHandler
  , versionSelector
  , setVersionSelector
  , deliveryModeSelector
  , setDeliveryModeSelector
  , networkAccessAllowedSelector
  , setNetworkAccessAllowedSelector
  , progressHandlerSelector
  , setProgressHandlerSelector

  -- * Enum types
  , PHImageRequestOptionsDeliveryMode(PHImageRequestOptionsDeliveryMode)
  , pattern PHImageRequestOptionsDeliveryModeOpportunistic
  , pattern PHImageRequestOptionsDeliveryModeHighQualityFormat
  , pattern PHImageRequestOptionsDeliveryModeFastFormat
  , PHImageRequestOptionsVersion(PHImageRequestOptionsVersion)
  , pattern PHImageRequestOptionsVersionCurrent
  , pattern PHImageRequestOptionsVersionUnadjusted
  , pattern PHImageRequestOptionsVersionOriginal

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

-- | @- version@
version :: IsPHLivePhotoRequestOptions phLivePhotoRequestOptions => phLivePhotoRequestOptions -> IO PHImageRequestOptionsVersion
version phLivePhotoRequestOptions  =
  fmap (coerce :: CLong -> PHImageRequestOptionsVersion) $ sendMsg phLivePhotoRequestOptions (mkSelector "version") retCLong []

-- | @- setVersion:@
setVersion :: IsPHLivePhotoRequestOptions phLivePhotoRequestOptions => phLivePhotoRequestOptions -> PHImageRequestOptionsVersion -> IO ()
setVersion phLivePhotoRequestOptions  value =
  sendMsg phLivePhotoRequestOptions (mkSelector "setVersion:") retVoid [argCLong (coerce value)]

-- | @- deliveryMode@
deliveryMode :: IsPHLivePhotoRequestOptions phLivePhotoRequestOptions => phLivePhotoRequestOptions -> IO PHImageRequestOptionsDeliveryMode
deliveryMode phLivePhotoRequestOptions  =
  fmap (coerce :: CLong -> PHImageRequestOptionsDeliveryMode) $ sendMsg phLivePhotoRequestOptions (mkSelector "deliveryMode") retCLong []

-- | @- setDeliveryMode:@
setDeliveryMode :: IsPHLivePhotoRequestOptions phLivePhotoRequestOptions => phLivePhotoRequestOptions -> PHImageRequestOptionsDeliveryMode -> IO ()
setDeliveryMode phLivePhotoRequestOptions  value =
  sendMsg phLivePhotoRequestOptions (mkSelector "setDeliveryMode:") retVoid [argCLong (coerce value)]

-- | @- networkAccessAllowed@
networkAccessAllowed :: IsPHLivePhotoRequestOptions phLivePhotoRequestOptions => phLivePhotoRequestOptions -> IO Bool
networkAccessAllowed phLivePhotoRequestOptions  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg phLivePhotoRequestOptions (mkSelector "networkAccessAllowed") retCULong []

-- | @- setNetworkAccessAllowed:@
setNetworkAccessAllowed :: IsPHLivePhotoRequestOptions phLivePhotoRequestOptions => phLivePhotoRequestOptions -> Bool -> IO ()
setNetworkAccessAllowed phLivePhotoRequestOptions  value =
  sendMsg phLivePhotoRequestOptions (mkSelector "setNetworkAccessAllowed:") retVoid [argCULong (if value then 1 else 0)]

-- | @- progressHandler@
progressHandler :: IsPHLivePhotoRequestOptions phLivePhotoRequestOptions => phLivePhotoRequestOptions -> IO (Ptr ())
progressHandler phLivePhotoRequestOptions  =
  fmap castPtr $ sendMsg phLivePhotoRequestOptions (mkSelector "progressHandler") (retPtr retVoid) []

-- | @- setProgressHandler:@
setProgressHandler :: IsPHLivePhotoRequestOptions phLivePhotoRequestOptions => phLivePhotoRequestOptions -> Ptr () -> IO ()
setProgressHandler phLivePhotoRequestOptions  value =
  sendMsg phLivePhotoRequestOptions (mkSelector "setProgressHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @version@
versionSelector :: Selector
versionSelector = mkSelector "version"

-- | @Selector@ for @setVersion:@
setVersionSelector :: Selector
setVersionSelector = mkSelector "setVersion:"

-- | @Selector@ for @deliveryMode@
deliveryModeSelector :: Selector
deliveryModeSelector = mkSelector "deliveryMode"

-- | @Selector@ for @setDeliveryMode:@
setDeliveryModeSelector :: Selector
setDeliveryModeSelector = mkSelector "setDeliveryMode:"

-- | @Selector@ for @networkAccessAllowed@
networkAccessAllowedSelector :: Selector
networkAccessAllowedSelector = mkSelector "networkAccessAllowed"

-- | @Selector@ for @setNetworkAccessAllowed:@
setNetworkAccessAllowedSelector :: Selector
setNetworkAccessAllowedSelector = mkSelector "setNetworkAccessAllowed:"

-- | @Selector@ for @progressHandler@
progressHandlerSelector :: Selector
progressHandlerSelector = mkSelector "progressHandler"

-- | @Selector@ for @setProgressHandler:@
setProgressHandlerSelector :: Selector
setProgressHandlerSelector = mkSelector "setProgressHandler:"

