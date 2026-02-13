{-# LANGUAGE DataKinds #-}
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
  , delegateSelector
  , originalRequestSelector
  , originatingFrameSelector
  , setDelegateSelector
  , userInitiatedSelector
  , webViewSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- cancel:@
cancel :: IsWKDownload wkDownload => wkDownload -> Ptr () -> IO ()
cancel wkDownload completionHandler =
  sendMessage wkDownload cancelSelector completionHandler

-- | @- originalRequest@
originalRequest :: IsWKDownload wkDownload => wkDownload -> IO (Id NSURLRequest)
originalRequest wkDownload =
  sendMessage wkDownload originalRequestSelector

-- | @- webView@
webView :: IsWKDownload wkDownload => wkDownload -> IO (Id WKWebView)
webView wkDownload =
  sendMessage wkDownload webViewSelector

-- | @- delegate@
delegate :: IsWKDownload wkDownload => wkDownload -> IO RawId
delegate wkDownload =
  sendMessage wkDownload delegateSelector

-- | @- setDelegate:@
setDelegate :: IsWKDownload wkDownload => wkDownload -> RawId -> IO ()
setDelegate wkDownload value =
  sendMessage wkDownload setDelegateSelector value

-- | @- userInitiated@
userInitiated :: IsWKDownload wkDownload => wkDownload -> IO Bool
userInitiated wkDownload =
  sendMessage wkDownload userInitiatedSelector

-- | @- originatingFrame@
originatingFrame :: IsWKDownload wkDownload => wkDownload -> IO (Id WKFrameInfo)
originatingFrame wkDownload =
  sendMessage wkDownload originatingFrameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cancel:@
cancelSelector :: Selector '[Ptr ()] ()
cancelSelector = mkSelector "cancel:"

-- | @Selector@ for @originalRequest@
originalRequestSelector :: Selector '[] (Id NSURLRequest)
originalRequestSelector = mkSelector "originalRequest"

-- | @Selector@ for @webView@
webViewSelector :: Selector '[] (Id WKWebView)
webViewSelector = mkSelector "webView"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @userInitiated@
userInitiatedSelector :: Selector '[] Bool
userInitiatedSelector = mkSelector "userInitiated"

-- | @Selector@ for @originatingFrame@
originatingFrameSelector :: Selector '[] (Id WKFrameInfo)
originatingFrameSelector = mkSelector "originatingFrame"

