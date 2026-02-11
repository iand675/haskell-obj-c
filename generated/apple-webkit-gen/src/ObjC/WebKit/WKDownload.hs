{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @WKDownload@.
module ObjC.WebKit.WKDownload
  ( WKDownload
  , IsWKDownload(..)
  , cancel
  , originalRequest
  , webView
  , delegate
  , setDelegate
  , userInitiated
  , originatingFrame
  , cancelSelector
  , originalRequestSelector
  , webViewSelector
  , delegateSelector
  , setDelegateSelector
  , userInitiatedSelector
  , originatingFrameSelector


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

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- cancel:@
cancel :: IsWKDownload wkDownload => wkDownload -> Ptr () -> IO ()
cancel wkDownload  completionHandler =
    sendMsg wkDownload (mkSelector "cancel:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- originalRequest@
originalRequest :: IsWKDownload wkDownload => wkDownload -> IO (Id NSURLRequest)
originalRequest wkDownload  =
    sendMsg wkDownload (mkSelector "originalRequest") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- webView@
webView :: IsWKDownload wkDownload => wkDownload -> IO (Id WKWebView)
webView wkDownload  =
    sendMsg wkDownload (mkSelector "webView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- delegate@
delegate :: IsWKDownload wkDownload => wkDownload -> IO RawId
delegate wkDownload  =
    fmap (RawId . castPtr) $ sendMsg wkDownload (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsWKDownload wkDownload => wkDownload -> RawId -> IO ()
setDelegate wkDownload  value =
    sendMsg wkDownload (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- userInitiated@
userInitiated :: IsWKDownload wkDownload => wkDownload -> IO Bool
userInitiated wkDownload  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkDownload (mkSelector "userInitiated") retCULong []

-- | @- originatingFrame@
originatingFrame :: IsWKDownload wkDownload => wkDownload -> IO (Id WKFrameInfo)
originatingFrame wkDownload  =
    sendMsg wkDownload (mkSelector "originatingFrame") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cancel:@
cancelSelector :: Selector
cancelSelector = mkSelector "cancel:"

-- | @Selector@ for @originalRequest@
originalRequestSelector :: Selector
originalRequestSelector = mkSelector "originalRequest"

-- | @Selector@ for @webView@
webViewSelector :: Selector
webViewSelector = mkSelector "webView"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @userInitiated@
userInitiatedSelector :: Selector
userInitiatedSelector = mkSelector "userInitiated"

-- | @Selector@ for @originatingFrame@
originatingFrameSelector :: Selector
originatingFrameSelector = mkSelector "originatingFrame"

