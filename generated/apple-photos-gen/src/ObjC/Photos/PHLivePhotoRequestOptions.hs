{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , deliveryModeSelector
  , networkAccessAllowedSelector
  , progressHandlerSelector
  , setDeliveryModeSelector
  , setNetworkAccessAllowedSelector
  , setProgressHandlerSelector
  , setVersionSelector
  , versionSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Photos.Internal.Classes
import ObjC.Photos.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- version@
version :: IsPHLivePhotoRequestOptions phLivePhotoRequestOptions => phLivePhotoRequestOptions -> IO PHImageRequestOptionsVersion
version phLivePhotoRequestOptions =
  sendMessage phLivePhotoRequestOptions versionSelector

-- | @- setVersion:@
setVersion :: IsPHLivePhotoRequestOptions phLivePhotoRequestOptions => phLivePhotoRequestOptions -> PHImageRequestOptionsVersion -> IO ()
setVersion phLivePhotoRequestOptions value =
  sendMessage phLivePhotoRequestOptions setVersionSelector value

-- | @- deliveryMode@
deliveryMode :: IsPHLivePhotoRequestOptions phLivePhotoRequestOptions => phLivePhotoRequestOptions -> IO PHImageRequestOptionsDeliveryMode
deliveryMode phLivePhotoRequestOptions =
  sendMessage phLivePhotoRequestOptions deliveryModeSelector

-- | @- setDeliveryMode:@
setDeliveryMode :: IsPHLivePhotoRequestOptions phLivePhotoRequestOptions => phLivePhotoRequestOptions -> PHImageRequestOptionsDeliveryMode -> IO ()
setDeliveryMode phLivePhotoRequestOptions value =
  sendMessage phLivePhotoRequestOptions setDeliveryModeSelector value

-- | @- networkAccessAllowed@
networkAccessAllowed :: IsPHLivePhotoRequestOptions phLivePhotoRequestOptions => phLivePhotoRequestOptions -> IO Bool
networkAccessAllowed phLivePhotoRequestOptions =
  sendMessage phLivePhotoRequestOptions networkAccessAllowedSelector

-- | @- setNetworkAccessAllowed:@
setNetworkAccessAllowed :: IsPHLivePhotoRequestOptions phLivePhotoRequestOptions => phLivePhotoRequestOptions -> Bool -> IO ()
setNetworkAccessAllowed phLivePhotoRequestOptions value =
  sendMessage phLivePhotoRequestOptions setNetworkAccessAllowedSelector value

-- | @- progressHandler@
progressHandler :: IsPHLivePhotoRequestOptions phLivePhotoRequestOptions => phLivePhotoRequestOptions -> IO (Ptr ())
progressHandler phLivePhotoRequestOptions =
  sendMessage phLivePhotoRequestOptions progressHandlerSelector

-- | @- setProgressHandler:@
setProgressHandler :: IsPHLivePhotoRequestOptions phLivePhotoRequestOptions => phLivePhotoRequestOptions -> Ptr () -> IO ()
setProgressHandler phLivePhotoRequestOptions value =
  sendMessage phLivePhotoRequestOptions setProgressHandlerSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @version@
versionSelector :: Selector '[] PHImageRequestOptionsVersion
versionSelector = mkSelector "version"

-- | @Selector@ for @setVersion:@
setVersionSelector :: Selector '[PHImageRequestOptionsVersion] ()
setVersionSelector = mkSelector "setVersion:"

-- | @Selector@ for @deliveryMode@
deliveryModeSelector :: Selector '[] PHImageRequestOptionsDeliveryMode
deliveryModeSelector = mkSelector "deliveryMode"

-- | @Selector@ for @setDeliveryMode:@
setDeliveryModeSelector :: Selector '[PHImageRequestOptionsDeliveryMode] ()
setDeliveryModeSelector = mkSelector "setDeliveryMode:"

-- | @Selector@ for @networkAccessAllowed@
networkAccessAllowedSelector :: Selector '[] Bool
networkAccessAllowedSelector = mkSelector "networkAccessAllowed"

-- | @Selector@ for @setNetworkAccessAllowed:@
setNetworkAccessAllowedSelector :: Selector '[Bool] ()
setNetworkAccessAllowedSelector = mkSelector "setNetworkAccessAllowed:"

-- | @Selector@ for @progressHandler@
progressHandlerSelector :: Selector '[] (Ptr ())
progressHandlerSelector = mkSelector "progressHandler"

-- | @Selector@ for @setProgressHandler:@
setProgressHandlerSelector :: Selector '[Ptr ()] ()
setProgressHandlerSelector = mkSelector "setProgressHandler:"

