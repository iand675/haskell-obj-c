{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHVideoRequestOptions@.
module ObjC.Photos.PHVideoRequestOptions
  ( PHVideoRequestOptions
  , IsPHVideoRequestOptions(..)
  , networkAccessAllowed
  , setNetworkAccessAllowed
  , version
  , setVersion
  , deliveryMode
  , setDeliveryMode
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
  , PHVideoRequestOptionsDeliveryMode(PHVideoRequestOptionsDeliveryMode)
  , pattern PHVideoRequestOptionsDeliveryModeAutomatic
  , pattern PHVideoRequestOptionsDeliveryModeHighQualityFormat
  , pattern PHVideoRequestOptionsDeliveryModeMediumQualityFormat
  , pattern PHVideoRequestOptionsDeliveryModeFastFormat
  , PHVideoRequestOptionsVersion(PHVideoRequestOptionsVersion)
  , pattern PHVideoRequestOptionsVersionCurrent
  , pattern PHVideoRequestOptionsVersionOriginal

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

-- | @- networkAccessAllowed@
networkAccessAllowed :: IsPHVideoRequestOptions phVideoRequestOptions => phVideoRequestOptions -> IO Bool
networkAccessAllowed phVideoRequestOptions =
  sendMessage phVideoRequestOptions networkAccessAllowedSelector

-- | @- setNetworkAccessAllowed:@
setNetworkAccessAllowed :: IsPHVideoRequestOptions phVideoRequestOptions => phVideoRequestOptions -> Bool -> IO ()
setNetworkAccessAllowed phVideoRequestOptions value =
  sendMessage phVideoRequestOptions setNetworkAccessAllowedSelector value

-- | @- version@
version :: IsPHVideoRequestOptions phVideoRequestOptions => phVideoRequestOptions -> IO PHVideoRequestOptionsVersion
version phVideoRequestOptions =
  sendMessage phVideoRequestOptions versionSelector

-- | @- setVersion:@
setVersion :: IsPHVideoRequestOptions phVideoRequestOptions => phVideoRequestOptions -> PHVideoRequestOptionsVersion -> IO ()
setVersion phVideoRequestOptions value =
  sendMessage phVideoRequestOptions setVersionSelector value

-- | @- deliveryMode@
deliveryMode :: IsPHVideoRequestOptions phVideoRequestOptions => phVideoRequestOptions -> IO PHVideoRequestOptionsDeliveryMode
deliveryMode phVideoRequestOptions =
  sendMessage phVideoRequestOptions deliveryModeSelector

-- | @- setDeliveryMode:@
setDeliveryMode :: IsPHVideoRequestOptions phVideoRequestOptions => phVideoRequestOptions -> PHVideoRequestOptionsDeliveryMode -> IO ()
setDeliveryMode phVideoRequestOptions value =
  sendMessage phVideoRequestOptions setDeliveryModeSelector value

-- | @- progressHandler@
progressHandler :: IsPHVideoRequestOptions phVideoRequestOptions => phVideoRequestOptions -> IO (Ptr ())
progressHandler phVideoRequestOptions =
  sendMessage phVideoRequestOptions progressHandlerSelector

-- | @- setProgressHandler:@
setProgressHandler :: IsPHVideoRequestOptions phVideoRequestOptions => phVideoRequestOptions -> Ptr () -> IO ()
setProgressHandler phVideoRequestOptions value =
  sendMessage phVideoRequestOptions setProgressHandlerSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @networkAccessAllowed@
networkAccessAllowedSelector :: Selector '[] Bool
networkAccessAllowedSelector = mkSelector "networkAccessAllowed"

-- | @Selector@ for @setNetworkAccessAllowed:@
setNetworkAccessAllowedSelector :: Selector '[Bool] ()
setNetworkAccessAllowedSelector = mkSelector "setNetworkAccessAllowed:"

-- | @Selector@ for @version@
versionSelector :: Selector '[] PHVideoRequestOptionsVersion
versionSelector = mkSelector "version"

-- | @Selector@ for @setVersion:@
setVersionSelector :: Selector '[PHVideoRequestOptionsVersion] ()
setVersionSelector = mkSelector "setVersion:"

-- | @Selector@ for @deliveryMode@
deliveryModeSelector :: Selector '[] PHVideoRequestOptionsDeliveryMode
deliveryModeSelector = mkSelector "deliveryMode"

-- | @Selector@ for @setDeliveryMode:@
setDeliveryModeSelector :: Selector '[PHVideoRequestOptionsDeliveryMode] ()
setDeliveryModeSelector = mkSelector "setDeliveryMode:"

-- | @Selector@ for @progressHandler@
progressHandlerSelector :: Selector '[] (Ptr ())
progressHandlerSelector = mkSelector "progressHandler"

-- | @Selector@ for @setProgressHandler:@
setProgressHandlerSelector :: Selector '[Ptr ()] ()
setProgressHandlerSelector = mkSelector "setProgressHandler:"

