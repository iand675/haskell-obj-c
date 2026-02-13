{-# LANGUAGE DataKinds #-}
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
  , sessionHandlerSelector
  , setSessionHandlerSelector
  , sharedManagerSelector
  , wasLaunchedByAuthenticationServicesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ sharedManager@
sharedManager :: IO (Id ASWebAuthenticationSessionWebBrowserSessionManager)
sharedManager  =
  do
    cls' <- getRequiredClass "ASWebAuthenticationSessionWebBrowserSessionManager"
    sendClassMessage cls' sharedManagerSelector

-- | @- sessionHandler@
sessionHandler :: IsASWebAuthenticationSessionWebBrowserSessionManager asWebAuthenticationSessionWebBrowserSessionManager => asWebAuthenticationSessionWebBrowserSessionManager -> IO RawId
sessionHandler asWebAuthenticationSessionWebBrowserSessionManager =
  sendMessage asWebAuthenticationSessionWebBrowserSessionManager sessionHandlerSelector

-- | @- setSessionHandler:@
setSessionHandler :: IsASWebAuthenticationSessionWebBrowserSessionManager asWebAuthenticationSessionWebBrowserSessionManager => asWebAuthenticationSessionWebBrowserSessionManager -> RawId -> IO ()
setSessionHandler asWebAuthenticationSessionWebBrowserSessionManager value =
  sendMessage asWebAuthenticationSessionWebBrowserSessionManager setSessionHandlerSelector value

-- | @- wasLaunchedByAuthenticationServices@
wasLaunchedByAuthenticationServices :: IsASWebAuthenticationSessionWebBrowserSessionManager asWebAuthenticationSessionWebBrowserSessionManager => asWebAuthenticationSessionWebBrowserSessionManager -> IO Bool
wasLaunchedByAuthenticationServices asWebAuthenticationSessionWebBrowserSessionManager =
  sendMessage asWebAuthenticationSessionWebBrowserSessionManager wasLaunchedByAuthenticationServicesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedManager@
sharedManagerSelector :: Selector '[] (Id ASWebAuthenticationSessionWebBrowserSessionManager)
sharedManagerSelector = mkSelector "sharedManager"

-- | @Selector@ for @sessionHandler@
sessionHandlerSelector :: Selector '[] RawId
sessionHandlerSelector = mkSelector "sessionHandler"

-- | @Selector@ for @setSessionHandler:@
setSessionHandlerSelector :: Selector '[RawId] ()
setSessionHandlerSelector = mkSelector "setSessionHandler:"

-- | @Selector@ for @wasLaunchedByAuthenticationServices@
wasLaunchedByAuthenticationServicesSelector :: Selector '[] Bool
wasLaunchedByAuthenticationServicesSelector = mkSelector "wasLaunchedByAuthenticationServices"

