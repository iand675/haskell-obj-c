{-# LANGUAGE DataKinds #-}
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
  , networkAccessAllowedSelector
  , progressHandlerSelector
  , setCanHandleAdjustmentDataSelector
  , setNetworkAccessAllowedSelector
  , setProgressHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Photos.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- canHandleAdjustmentData@
canHandleAdjustmentData :: IsPHContentEditingInputRequestOptions phContentEditingInputRequestOptions => phContentEditingInputRequestOptions -> IO (Ptr ())
canHandleAdjustmentData phContentEditingInputRequestOptions =
  sendMessage phContentEditingInputRequestOptions canHandleAdjustmentDataSelector

-- | @- setCanHandleAdjustmentData:@
setCanHandleAdjustmentData :: IsPHContentEditingInputRequestOptions phContentEditingInputRequestOptions => phContentEditingInputRequestOptions -> Ptr () -> IO ()
setCanHandleAdjustmentData phContentEditingInputRequestOptions value =
  sendMessage phContentEditingInputRequestOptions setCanHandleAdjustmentDataSelector value

-- | @- networkAccessAllowed@
networkAccessAllowed :: IsPHContentEditingInputRequestOptions phContentEditingInputRequestOptions => phContentEditingInputRequestOptions -> IO Bool
networkAccessAllowed phContentEditingInputRequestOptions =
  sendMessage phContentEditingInputRequestOptions networkAccessAllowedSelector

-- | @- setNetworkAccessAllowed:@
setNetworkAccessAllowed :: IsPHContentEditingInputRequestOptions phContentEditingInputRequestOptions => phContentEditingInputRequestOptions -> Bool -> IO ()
setNetworkAccessAllowed phContentEditingInputRequestOptions value =
  sendMessage phContentEditingInputRequestOptions setNetworkAccessAllowedSelector value

-- | @- progressHandler@
progressHandler :: IsPHContentEditingInputRequestOptions phContentEditingInputRequestOptions => phContentEditingInputRequestOptions -> IO (Ptr ())
progressHandler phContentEditingInputRequestOptions =
  sendMessage phContentEditingInputRequestOptions progressHandlerSelector

-- | @- setProgressHandler:@
setProgressHandler :: IsPHContentEditingInputRequestOptions phContentEditingInputRequestOptions => phContentEditingInputRequestOptions -> Ptr () -> IO ()
setProgressHandler phContentEditingInputRequestOptions value =
  sendMessage phContentEditingInputRequestOptions setProgressHandlerSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @canHandleAdjustmentData@
canHandleAdjustmentDataSelector :: Selector '[] (Ptr ())
canHandleAdjustmentDataSelector = mkSelector "canHandleAdjustmentData"

-- | @Selector@ for @setCanHandleAdjustmentData:@
setCanHandleAdjustmentDataSelector :: Selector '[Ptr ()] ()
setCanHandleAdjustmentDataSelector = mkSelector "setCanHandleAdjustmentData:"

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

