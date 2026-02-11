{-# LANGUAGE PatternSynonyms #-}
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
  , networkAccessAllowedSelector
  , setNetworkAccessAllowedSelector
  , versionSelector
  , setVersionSelector
  , deliveryModeSelector
  , setDeliveryModeSelector
  , progressHandlerSelector
  , setProgressHandlerSelector

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

-- | @- networkAccessAllowed@
networkAccessAllowed :: IsPHVideoRequestOptions phVideoRequestOptions => phVideoRequestOptions -> IO Bool
networkAccessAllowed phVideoRequestOptions  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg phVideoRequestOptions (mkSelector "networkAccessAllowed") retCULong []

-- | @- setNetworkAccessAllowed:@
setNetworkAccessAllowed :: IsPHVideoRequestOptions phVideoRequestOptions => phVideoRequestOptions -> Bool -> IO ()
setNetworkAccessAllowed phVideoRequestOptions  value =
  sendMsg phVideoRequestOptions (mkSelector "setNetworkAccessAllowed:") retVoid [argCULong (if value then 1 else 0)]

-- | @- version@
version :: IsPHVideoRequestOptions phVideoRequestOptions => phVideoRequestOptions -> IO PHVideoRequestOptionsVersion
version phVideoRequestOptions  =
  fmap (coerce :: CLong -> PHVideoRequestOptionsVersion) $ sendMsg phVideoRequestOptions (mkSelector "version") retCLong []

-- | @- setVersion:@
setVersion :: IsPHVideoRequestOptions phVideoRequestOptions => phVideoRequestOptions -> PHVideoRequestOptionsVersion -> IO ()
setVersion phVideoRequestOptions  value =
  sendMsg phVideoRequestOptions (mkSelector "setVersion:") retVoid [argCLong (coerce value)]

-- | @- deliveryMode@
deliveryMode :: IsPHVideoRequestOptions phVideoRequestOptions => phVideoRequestOptions -> IO PHVideoRequestOptionsDeliveryMode
deliveryMode phVideoRequestOptions  =
  fmap (coerce :: CLong -> PHVideoRequestOptionsDeliveryMode) $ sendMsg phVideoRequestOptions (mkSelector "deliveryMode") retCLong []

-- | @- setDeliveryMode:@
setDeliveryMode :: IsPHVideoRequestOptions phVideoRequestOptions => phVideoRequestOptions -> PHVideoRequestOptionsDeliveryMode -> IO ()
setDeliveryMode phVideoRequestOptions  value =
  sendMsg phVideoRequestOptions (mkSelector "setDeliveryMode:") retVoid [argCLong (coerce value)]

-- | @- progressHandler@
progressHandler :: IsPHVideoRequestOptions phVideoRequestOptions => phVideoRequestOptions -> IO (Ptr ())
progressHandler phVideoRequestOptions  =
  fmap castPtr $ sendMsg phVideoRequestOptions (mkSelector "progressHandler") (retPtr retVoid) []

-- | @- setProgressHandler:@
setProgressHandler :: IsPHVideoRequestOptions phVideoRequestOptions => phVideoRequestOptions -> Ptr () -> IO ()
setProgressHandler phVideoRequestOptions  value =
  sendMsg phVideoRequestOptions (mkSelector "setProgressHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @networkAccessAllowed@
networkAccessAllowedSelector :: Selector
networkAccessAllowedSelector = mkSelector "networkAccessAllowed"

-- | @Selector@ for @setNetworkAccessAllowed:@
setNetworkAccessAllowedSelector :: Selector
setNetworkAccessAllowedSelector = mkSelector "setNetworkAccessAllowed:"

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

-- | @Selector@ for @progressHandler@
progressHandlerSelector :: Selector
progressHandlerSelector = mkSelector "progressHandler"

-- | @Selector@ for @setProgressHandler:@
setProgressHandlerSelector :: Selector
setProgressHandlerSelector = mkSelector "setProgressHandler:"

