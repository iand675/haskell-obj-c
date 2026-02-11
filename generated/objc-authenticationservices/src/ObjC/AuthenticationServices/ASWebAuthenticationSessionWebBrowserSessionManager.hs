{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASWebAuthenticationSessionWebBrowserSessionManager@.
module ObjC.AuthenticationServices.ASWebAuthenticationSessionWebBrowserSessionManager
  ( ASWebAuthenticationSessionWebBrowserSessionManager
  , IsASWebAuthenticationSessionWebBrowserSessionManager(..)
  , sharedManager
  , wasLaunchedByAuthenticationServices
  , sharedManagerSelector
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

-- | @Selector@ for @wasLaunchedByAuthenticationServices@
wasLaunchedByAuthenticationServicesSelector :: Selector
wasLaunchedByAuthenticationServicesSelector = mkSelector "wasLaunchedByAuthenticationServices"

