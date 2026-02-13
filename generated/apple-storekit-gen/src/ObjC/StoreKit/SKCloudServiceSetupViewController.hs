{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKCloudServiceSetupViewController@.
module ObjC.StoreKit.SKCloudServiceSetupViewController
  ( SKCloudServiceSetupViewController
  , IsSKCloudServiceSetupViewController(..)
  , loadWithOptions_completionHandler
  , delegate
  , setDelegate
  , delegateSelector
  , loadWithOptions_completionHandlerSelector
  , setDelegateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.StoreKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Load cloud service setup view with the given options. Block is invoked on the main thread when the load finishes.
--
-- ObjC selector: @- loadWithOptions:completionHandler:@
loadWithOptions_completionHandler :: (IsSKCloudServiceSetupViewController skCloudServiceSetupViewController, IsNSDictionary options) => skCloudServiceSetupViewController -> options -> Ptr () -> IO ()
loadWithOptions_completionHandler skCloudServiceSetupViewController options completionHandler =
  sendMessage skCloudServiceSetupViewController loadWithOptions_completionHandlerSelector (toNSDictionary options) completionHandler

-- | Optional delegate.
--
-- ObjC selector: @- delegate@
delegate :: IsSKCloudServiceSetupViewController skCloudServiceSetupViewController => skCloudServiceSetupViewController -> IO RawId
delegate skCloudServiceSetupViewController =
  sendMessage skCloudServiceSetupViewController delegateSelector

-- | Optional delegate.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsSKCloudServiceSetupViewController skCloudServiceSetupViewController => skCloudServiceSetupViewController -> RawId -> IO ()
setDelegate skCloudServiceSetupViewController value =
  sendMessage skCloudServiceSetupViewController setDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @loadWithOptions:completionHandler:@
loadWithOptions_completionHandlerSelector :: Selector '[Id NSDictionary, Ptr ()] ()
loadWithOptions_completionHandlerSelector = mkSelector "loadWithOptions:completionHandler:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

