{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHContentEditingInputRequestOptions@.
module ObjC.Photos.PHContentEditingInputRequestOptions
  ( PHContentEditingInputRequestOptions
  , IsPHContentEditingInputRequestOptions(..)
  , canHandleAdjustmentData
  , setCanHandleAdjustmentData
  , networkAccessAllowed
  , setNetworkAccessAllowed
  , progressHandler
  , setProgressHandler
  , canHandleAdjustmentDataSelector
  , setCanHandleAdjustmentDataSelector
  , networkAccessAllowedSelector
  , setNetworkAccessAllowedSelector
  , progressHandlerSelector
  , setProgressHandlerSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- canHandleAdjustmentData@
canHandleAdjustmentData :: IsPHContentEditingInputRequestOptions phContentEditingInputRequestOptions => phContentEditingInputRequestOptions -> IO (Ptr ())
canHandleAdjustmentData phContentEditingInputRequestOptions  =
  fmap castPtr $ sendMsg phContentEditingInputRequestOptions (mkSelector "canHandleAdjustmentData") (retPtr retVoid) []

-- | @- setCanHandleAdjustmentData:@
setCanHandleAdjustmentData :: IsPHContentEditingInputRequestOptions phContentEditingInputRequestOptions => phContentEditingInputRequestOptions -> Ptr () -> IO ()
setCanHandleAdjustmentData phContentEditingInputRequestOptions  value =
  sendMsg phContentEditingInputRequestOptions (mkSelector "setCanHandleAdjustmentData:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | @- networkAccessAllowed@
networkAccessAllowed :: IsPHContentEditingInputRequestOptions phContentEditingInputRequestOptions => phContentEditingInputRequestOptions -> IO Bool
networkAccessAllowed phContentEditingInputRequestOptions  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg phContentEditingInputRequestOptions (mkSelector "networkAccessAllowed") retCULong []

-- | @- setNetworkAccessAllowed:@
setNetworkAccessAllowed :: IsPHContentEditingInputRequestOptions phContentEditingInputRequestOptions => phContentEditingInputRequestOptions -> Bool -> IO ()
setNetworkAccessAllowed phContentEditingInputRequestOptions  value =
  sendMsg phContentEditingInputRequestOptions (mkSelector "setNetworkAccessAllowed:") retVoid [argCULong (if value then 1 else 0)]

-- | @- progressHandler@
progressHandler :: IsPHContentEditingInputRequestOptions phContentEditingInputRequestOptions => phContentEditingInputRequestOptions -> IO (Ptr ())
progressHandler phContentEditingInputRequestOptions  =
  fmap castPtr $ sendMsg phContentEditingInputRequestOptions (mkSelector "progressHandler") (retPtr retVoid) []

-- | @- setProgressHandler:@
setProgressHandler :: IsPHContentEditingInputRequestOptions phContentEditingInputRequestOptions => phContentEditingInputRequestOptions -> Ptr () -> IO ()
setProgressHandler phContentEditingInputRequestOptions  value =
  sendMsg phContentEditingInputRequestOptions (mkSelector "setProgressHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @canHandleAdjustmentData@
canHandleAdjustmentDataSelector :: Selector
canHandleAdjustmentDataSelector = mkSelector "canHandleAdjustmentData"

-- | @Selector@ for @setCanHandleAdjustmentData:@
setCanHandleAdjustmentDataSelector :: Selector
setCanHandleAdjustmentDataSelector = mkSelector "setCanHandleAdjustmentData:"

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

