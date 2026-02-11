{-# LANGUAGE PatternSynonyms #-}
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
  , versionSelector
  , setVersionSelector
  , deliveryModeSelector
  , setDeliveryModeSelector
  , resizeModeSelector
  , setResizeModeSelector
  , networkAccessAllowedSelector
  , setNetworkAccessAllowedSelector
  , synchronousSelector
  , setSynchronousSelector
  , progressHandlerSelector
  , setProgressHandlerSelector
  , allowSecondaryDegradedImageSelector
  , setAllowSecondaryDegradedImageSelector

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
import ObjC.Foundation.Internal.Classes

-- | @- version@
version :: IsPHImageRequestOptions phImageRequestOptions => phImageRequestOptions -> IO PHImageRequestOptionsVersion
version phImageRequestOptions  =
  fmap (coerce :: CLong -> PHImageRequestOptionsVersion) $ sendMsg phImageRequestOptions (mkSelector "version") retCLong []

-- | @- setVersion:@
setVersion :: IsPHImageRequestOptions phImageRequestOptions => phImageRequestOptions -> PHImageRequestOptionsVersion -> IO ()
setVersion phImageRequestOptions  value =
  sendMsg phImageRequestOptions (mkSelector "setVersion:") retVoid [argCLong (coerce value)]

-- | @- deliveryMode@
deliveryMode :: IsPHImageRequestOptions phImageRequestOptions => phImageRequestOptions -> IO PHImageRequestOptionsDeliveryMode
deliveryMode phImageRequestOptions  =
  fmap (coerce :: CLong -> PHImageRequestOptionsDeliveryMode) $ sendMsg phImageRequestOptions (mkSelector "deliveryMode") retCLong []

-- | @- setDeliveryMode:@
setDeliveryMode :: IsPHImageRequestOptions phImageRequestOptions => phImageRequestOptions -> PHImageRequestOptionsDeliveryMode -> IO ()
setDeliveryMode phImageRequestOptions  value =
  sendMsg phImageRequestOptions (mkSelector "setDeliveryMode:") retVoid [argCLong (coerce value)]

-- | @- resizeMode@
resizeMode :: IsPHImageRequestOptions phImageRequestOptions => phImageRequestOptions -> IO PHImageRequestOptionsResizeMode
resizeMode phImageRequestOptions  =
  fmap (coerce :: CLong -> PHImageRequestOptionsResizeMode) $ sendMsg phImageRequestOptions (mkSelector "resizeMode") retCLong []

-- | @- setResizeMode:@
setResizeMode :: IsPHImageRequestOptions phImageRequestOptions => phImageRequestOptions -> PHImageRequestOptionsResizeMode -> IO ()
setResizeMode phImageRequestOptions  value =
  sendMsg phImageRequestOptions (mkSelector "setResizeMode:") retVoid [argCLong (coerce value)]

-- | @- networkAccessAllowed@
networkAccessAllowed :: IsPHImageRequestOptions phImageRequestOptions => phImageRequestOptions -> IO Bool
networkAccessAllowed phImageRequestOptions  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg phImageRequestOptions (mkSelector "networkAccessAllowed") retCULong []

-- | @- setNetworkAccessAllowed:@
setNetworkAccessAllowed :: IsPHImageRequestOptions phImageRequestOptions => phImageRequestOptions -> Bool -> IO ()
setNetworkAccessAllowed phImageRequestOptions  value =
  sendMsg phImageRequestOptions (mkSelector "setNetworkAccessAllowed:") retVoid [argCULong (if value then 1 else 0)]

-- | @- synchronous@
synchronous :: IsPHImageRequestOptions phImageRequestOptions => phImageRequestOptions -> IO Bool
synchronous phImageRequestOptions  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg phImageRequestOptions (mkSelector "synchronous") retCULong []

-- | @- setSynchronous:@
setSynchronous :: IsPHImageRequestOptions phImageRequestOptions => phImageRequestOptions -> Bool -> IO ()
setSynchronous phImageRequestOptions  value =
  sendMsg phImageRequestOptions (mkSelector "setSynchronous:") retVoid [argCULong (if value then 1 else 0)]

-- | @- progressHandler@
progressHandler :: IsPHImageRequestOptions phImageRequestOptions => phImageRequestOptions -> IO (Ptr ())
progressHandler phImageRequestOptions  =
  fmap castPtr $ sendMsg phImageRequestOptions (mkSelector "progressHandler") (retPtr retVoid) []

-- | @- setProgressHandler:@
setProgressHandler :: IsPHImageRequestOptions phImageRequestOptions => phImageRequestOptions -> Ptr () -> IO ()
setProgressHandler phImageRequestOptions  value =
  sendMsg phImageRequestOptions (mkSelector "setProgressHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | @- allowSecondaryDegradedImage@
allowSecondaryDegradedImage :: IsPHImageRequestOptions phImageRequestOptions => phImageRequestOptions -> IO Bool
allowSecondaryDegradedImage phImageRequestOptions  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg phImageRequestOptions (mkSelector "allowSecondaryDegradedImage") retCULong []

-- | @- setAllowSecondaryDegradedImage:@
setAllowSecondaryDegradedImage :: IsPHImageRequestOptions phImageRequestOptions => phImageRequestOptions -> Bool -> IO ()
setAllowSecondaryDegradedImage phImageRequestOptions  value =
  sendMsg phImageRequestOptions (mkSelector "setAllowSecondaryDegradedImage:") retVoid [argCULong (if value then 1 else 0)]

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

-- | @Selector@ for @resizeMode@
resizeModeSelector :: Selector
resizeModeSelector = mkSelector "resizeMode"

-- | @Selector@ for @setResizeMode:@
setResizeModeSelector :: Selector
setResizeModeSelector = mkSelector "setResizeMode:"

-- | @Selector@ for @networkAccessAllowed@
networkAccessAllowedSelector :: Selector
networkAccessAllowedSelector = mkSelector "networkAccessAllowed"

-- | @Selector@ for @setNetworkAccessAllowed:@
setNetworkAccessAllowedSelector :: Selector
setNetworkAccessAllowedSelector = mkSelector "setNetworkAccessAllowed:"

-- | @Selector@ for @synchronous@
synchronousSelector :: Selector
synchronousSelector = mkSelector "synchronous"

-- | @Selector@ for @setSynchronous:@
setSynchronousSelector :: Selector
setSynchronousSelector = mkSelector "setSynchronous:"

-- | @Selector@ for @progressHandler@
progressHandlerSelector :: Selector
progressHandlerSelector = mkSelector "progressHandler"

-- | @Selector@ for @setProgressHandler:@
setProgressHandlerSelector :: Selector
setProgressHandlerSelector = mkSelector "setProgressHandler:"

-- | @Selector@ for @allowSecondaryDegradedImage@
allowSecondaryDegradedImageSelector :: Selector
allowSecondaryDegradedImageSelector = mkSelector "allowSecondaryDegradedImage"

-- | @Selector@ for @setAllowSecondaryDegradedImage:@
setAllowSecondaryDegradedImageSelector :: Selector
setAllowSecondaryDegradedImageSelector = mkSelector "setAllowSecondaryDegradedImage:"

