{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASWebAuthenticationSessionWebBrowserSessionManager@.
module ObjC.AuthenticationServices.ASWebAuthenticationSessionWebBrowserSessionManager
  ( ASWebAuthenticationSessionWebBrowserSessionManager
  , IsASWebAuthenticationSessionWebBrowserSessionManager(..)
  , sharedManager
  , sessionHandler
  , setSessionHandler
  , wasLaunchedByAuthenticationServices
  , sharedManagerSelector
  , sessionHandlerSelector
  , setSessionHandlerSelector
  , wasLaunchedByAuthenticationServicesSelector


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

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ sharedManager@
sharedManager :: IO (Id ASWebAuthenticationSessionWebBrowserSessionManager)
sharedManager  =
  do
    cls' <- getRequiredClass "ASWebAuthenticationSessionWebBrowserSessionManager"
    sendClassMsg cls' (mkSelector "sharedManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sessionHandler@
sessionHandler :: IsASWebAuthenticationSessionWebBrowserSessionManager asWebAuthenticationSessionWebBrowserSessionManager => asWebAuthenticationSessionWebBrowserSessionManager -> IO RawId
sessionHandler asWebAuthenticationSessionWebBrowserSessionManager  =
    fmap (RawId . castPtr) $ sendMsg asWebAuthenticationSessionWebBrowserSessionManager (mkSelector "sessionHandler") (retPtr retVoid) []

-- | @- setSessionHandler:@
setSessionHandler :: IsASWebAuthenticationSessionWebBrowserSessionManager asWebAuthenticationSessionWebBrowserSessionManager => asWebAuthenticationSessionWebBrowserSessionManager -> RawId -> IO ()
setSessionHandler asWebAuthenticationSessionWebBrowserSessionManager  value =
    sendMsg asWebAuthenticationSessionWebBrowserSessionManager (mkSelector "setSessionHandler:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- wasLaunchedByAuthenticationServices@
wasLaunchedByAuthenticationServices :: IsASWebAuthenticationSessionWebBrowserSessionManager asWebAuthenticationSessionWebBrowserSessionManager => asWebAuthenticationSessionWebBrowserSessionManager -> IO Bool
wasLaunchedByAuthenticationServices asWebAuthenticationSessionWebBrowserSessionManager  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg asWebAuthenticationSessionWebBrowserSessionManager (mkSelector "wasLaunchedByAuthenticationServices") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedManager@
sharedManagerSelector :: Selector
sharedManagerSelector = mkSelector "sharedManager"

-- | @Selector@ for @sessionHandler@
sessionHandlerSelector :: Selector
sessionHandlerSelector = mkSelector "sessionHandler"

-- | @Selector@ for @setSessionHandler:@
setSessionHandlerSelector :: Selector
setSessionHandlerSelector = mkSelector "setSessionHandler:"

-- | @Selector@ for @wasLaunchedByAuthenticationServices@
wasLaunchedByAuthenticationServicesSelector :: Selector
wasLaunchedByAuthenticationServicesSelector = mkSelector "wasLaunchedByAuthenticationServices"

