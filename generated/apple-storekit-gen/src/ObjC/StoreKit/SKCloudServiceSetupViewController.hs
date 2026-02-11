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
  , loadWithOptions_completionHandlerSelector
  , delegateSelector
  , setDelegateSelector


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

import ObjC.StoreKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Load cloud service setup view with the given options. Block is invoked on the main thread when the load finishes.
--
-- ObjC selector: @- loadWithOptions:completionHandler:@
loadWithOptions_completionHandler :: (IsSKCloudServiceSetupViewController skCloudServiceSetupViewController, IsNSDictionary options) => skCloudServiceSetupViewController -> options -> Ptr () -> IO ()
loadWithOptions_completionHandler skCloudServiceSetupViewController  options completionHandler =
  withObjCPtr options $ \raw_options ->
      sendMsg skCloudServiceSetupViewController (mkSelector "loadWithOptions:completionHandler:") retVoid [argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Optional delegate.
--
-- ObjC selector: @- delegate@
delegate :: IsSKCloudServiceSetupViewController skCloudServiceSetupViewController => skCloudServiceSetupViewController -> IO RawId
delegate skCloudServiceSetupViewController  =
    fmap (RawId . castPtr) $ sendMsg skCloudServiceSetupViewController (mkSelector "delegate") (retPtr retVoid) []

-- | Optional delegate.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsSKCloudServiceSetupViewController skCloudServiceSetupViewController => skCloudServiceSetupViewController -> RawId -> IO ()
setDelegate skCloudServiceSetupViewController  value =
    sendMsg skCloudServiceSetupViewController (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @loadWithOptions:completionHandler:@
loadWithOptions_completionHandlerSelector :: Selector
loadWithOptions_completionHandlerSelector = mkSelector "loadWithOptions:completionHandler:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

