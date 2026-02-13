{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHImageRequestOptions@.
module ObjC.Photos.PHImageRequestOptions
  ( PHImageRequestOptions
  , IsPHImageRequestOptions(..)
  , version
  , setVersion
  , deliveryMode
  , setDeliveryMode
  , resizeMode
  , setResizeMode
  , networkAccessAllowed
  , setNetworkAccessAllowed
  , synchronous
  , setSynchronous
  , progressHandler
  , setProgressHandler
  , allowSecondaryDegradedImage
  , setAllowSecondaryDegradedImage
  , allowSecondaryDegradedImageSelector
  , deliveryModeSelector
  , networkAccessAllowedSelector
  , progressHandlerSelector
  , resizeModeSelector
  , setAllowSecondaryDegradedImageSelector
  , setDeliveryModeSelector
  , setNetworkAccessAllowedSelector
  , setProgressHandlerSelector
  , setResizeModeSelector
  , setSynchronousSelector
  , setVersionSelector
  , synchronousSelector
  , versionSelector

  -- * Enum types
  , PHImageRequestOptionsDeliveryMode(PHImageRequestOptionsDeliveryMode)
  , pattern PHImageRequestOptionsDeliveryModeOpportunistic
  , pattern PHImageRequestOptionsDeliveryModeHighQualityFormat
  , pattern PHImageRequestOptionsDeliveryModeFastFormat
  , PHImageRequestOptionsResizeMode(PHImageRequestOptionsResizeMode)
  , pattern PHImageRequestOptionsResizeModeNone
  , pattern PHImageRequestOptionsResizeModeFast
  , pattern PHImageRequestOptionsResizeModeExact
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
version :: IsPHImageRequestOptions phImageRequestOptions => phImageRequestOptions -> IO PHImageRequestOptionsVersion
version phImageRequestOptions =
  sendMessage phImageRequestOptions versionSelector

-- | @- setVersion:@
setVersion :: IsPHImageRequestOptions phImageRequestOptions => phImageRequestOptions -> PHImageRequestOptionsVersion -> IO ()
setVersion phImageRequestOptions value =
  sendMessage phImageRequestOptions setVersionSelector value

-- | @- deliveryMode@
deliveryMode :: IsPHImageRequestOptions phImageRequestOptions => phImageRequestOptions -> IO PHImageRequestOptionsDeliveryMode
deliveryMode phImageRequestOptions =
  sendMessage phImageRequestOptions deliveryModeSelector

-- | @- setDeliveryMode:@
setDeliveryMode :: IsPHImageRequestOptions phImageRequestOptions => phImageRequestOptions -> PHImageRequestOptionsDeliveryMode -> IO ()
setDeliveryMode phImageRequestOptions value =
  sendMessage phImageRequestOptions setDeliveryModeSelector value

-- | @- resizeMode@
resizeMode :: IsPHImageRequestOptions phImageRequestOptions => phImageRequestOptions -> IO PHImageRequestOptionsResizeMode
resizeMode phImageRequestOptions =
  sendMessage phImageRequestOptions resizeModeSelector

-- | @- setResizeMode:@
setResizeMode :: IsPHImageRequestOptions phImageRequestOptions => phImageRequestOptions -> PHImageRequestOptionsResizeMode -> IO ()
setResizeMode phImageRequestOptions value =
  sendMessage phImageRequestOptions setResizeModeSelector value

-- | @- networkAccessAllowed@
networkAccessAllowed :: IsPHImageRequestOptions phImageRequestOptions => phImageRequestOptions -> IO Bool
networkAccessAllowed phImageRequestOptions =
  sendMessage phImageRequestOptions networkAccessAllowedSelector

-- | @- setNetworkAccessAllowed:@
setNetworkAccessAllowed :: IsPHImageRequestOptions phImageRequestOptions => phImageRequestOptions -> Bool -> IO ()
setNetworkAccessAllowed phImageRequestOptions value =
  sendMessage phImageRequestOptions setNetworkAccessAllowedSelector value

-- | @- synchronous@
synchronous :: IsPHImageRequestOptions phImageRequestOptions => phImageRequestOptions -> IO Bool
synchronous phImageRequestOptions =
  sendMessage phImageRequestOptions synchronousSelector

-- | @- setSynchronous:@
setSynchronous :: IsPHImageRequestOptions phImageRequestOptions => phImageRequestOptions -> Bool -> IO ()
setSynchronous phImageRequestOptions value =
  sendMessage phImageRequestOptions setSynchronousSelector value

-- | @- progressHandler@
progressHandler :: IsPHImageRequestOptions phImageRequestOptions => phImageRequestOptions -> IO (Ptr ())
progressHandler phImageRequestOptions =
  sendMessage phImageRequestOptions progressHandlerSelector

-- | @- setProgressHandler:@
setProgressHandler :: IsPHImageRequestOptions phImageRequestOptions => phImageRequestOptions -> Ptr () -> IO ()
setProgressHandler phImageRequestOptions value =
  sendMessage phImageRequestOptions setProgressHandlerSelector value

-- | @- allowSecondaryDegradedImage@
allowSecondaryDegradedImage :: IsPHImageRequestOptions phImageRequestOptions => phImageRequestOptions -> IO Bool
allowSecondaryDegradedImage phImageRequestOptions =
  sendMessage phImageRequestOptions allowSecondaryDegradedImageSelector

-- | @- setAllowSecondaryDegradedImage:@
setAllowSecondaryDegradedImage :: IsPHImageRequestOptions phImageRequestOptions => phImageRequestOptions -> Bool -> IO ()
setAllowSecondaryDegradedImage phImageRequestOptions value =
  sendMessage phImageRequestOptions setAllowSecondaryDegradedImageSelector value

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

-- | @Selector@ for @resizeMode@
resizeModeSelector :: Selector '[] PHImageRequestOptionsResizeMode
resizeModeSelector = mkSelector "resizeMode"

-- | @Selector@ for @setResizeMode:@
setResizeModeSelector :: Selector '[PHImageRequestOptionsResizeMode] ()
setResizeModeSelector = mkSelector "setResizeMode:"

-- | @Selector@ for @networkAccessAllowed@
networkAccessAllowedSelector :: Selector '[] Bool
networkAccessAllowedSelector = mkSelector "networkAccessAllowed"

-- | @Selector@ for @setNetworkAccessAllowed:@
setNetworkAccessAllowedSelector :: Selector '[Bool] ()
setNetworkAccessAllowedSelector = mkSelector "setNetworkAccessAllowed:"

-- | @Selector@ for @synchronous@
synchronousSelector :: Selector '[] Bool
synchronousSelector = mkSelector "synchronous"

-- | @Selector@ for @setSynchronous:@
setSynchronousSelector :: Selector '[Bool] ()
setSynchronousSelector = mkSelector "setSynchronous:"

-- | @Selector@ for @progressHandler@
progressHandlerSelector :: Selector '[] (Ptr ())
progressHandlerSelector = mkSelector "progressHandler"

-- | @Selector@ for @setProgressHandler:@
setProgressHandlerSelector :: Selector '[Ptr ()] ()
setProgressHandlerSelector = mkSelector "setProgressHandler:"

-- | @Selector@ for @allowSecondaryDegradedImage@
allowSecondaryDegradedImageSelector :: Selector '[] Bool
allowSecondaryDegradedImageSelector = mkSelector "allowSecondaryDegradedImage"

-- | @Selector@ for @setAllowSecondaryDegradedImage:@
setAllowSecondaryDegradedImageSelector :: Selector '[Bool] ()
setAllowSecondaryDegradedImageSelector = mkSelector "setAllowSecondaryDegradedImage:"

