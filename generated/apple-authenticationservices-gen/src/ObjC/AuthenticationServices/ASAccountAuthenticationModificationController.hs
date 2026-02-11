{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASAccountAuthenticationModificationController@.
module ObjC.AuthenticationServices.ASAccountAuthenticationModificationController
  ( ASAccountAuthenticationModificationController
  , IsASAccountAuthenticationModificationController(..)
  , performRequest
  , delegate
  , setDelegate
  , presentationContextProvider
  , setPresentationContextProvider
  , performRequestSelector
  , delegateSelector
  , setDelegateSelector
  , presentationContextProviderSelector
  , setPresentationContextProviderSelector


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

-- | Perform an upgrade request, one at a time. Any requests initiated with a request already in progress will fail immediately.
--
-- ObjC selector: @- performRequest:@
performRequest :: (IsASAccountAuthenticationModificationController asAccountAuthenticationModificationController, IsASAccountAuthenticationModificationRequest request) => asAccountAuthenticationModificationController -> request -> IO ()
performRequest asAccountAuthenticationModificationController  request =
  withObjCPtr request $ \raw_request ->
      sendMsg asAccountAuthenticationModificationController (mkSelector "performRequest:") retVoid [argPtr (castPtr raw_request :: Ptr ())]

-- | This delegate will be notified upon completion of the upgrade to report success or failure.
--
-- ObjC selector: @- delegate@
delegate :: IsASAccountAuthenticationModificationController asAccountAuthenticationModificationController => asAccountAuthenticationModificationController -> IO RawId
delegate asAccountAuthenticationModificationController  =
    fmap (RawId . castPtr) $ sendMsg asAccountAuthenticationModificationController (mkSelector "delegate") (retPtr retVoid) []

-- | This delegate will be notified upon completion of the upgrade to report success or failure.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsASAccountAuthenticationModificationController asAccountAuthenticationModificationController => asAccountAuthenticationModificationController -> RawId -> IO ()
setDelegate asAccountAuthenticationModificationController  value =
    sendMsg asAccountAuthenticationModificationController (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | This will be used to provide a presentation context to display authorization UI.
--
-- ObjC selector: @- presentationContextProvider@
presentationContextProvider :: IsASAccountAuthenticationModificationController asAccountAuthenticationModificationController => asAccountAuthenticationModificationController -> IO RawId
presentationContextProvider asAccountAuthenticationModificationController  =
    fmap (RawId . castPtr) $ sendMsg asAccountAuthenticationModificationController (mkSelector "presentationContextProvider") (retPtr retVoid) []

-- | This will be used to provide a presentation context to display authorization UI.
--
-- ObjC selector: @- setPresentationContextProvider:@
setPresentationContextProvider :: IsASAccountAuthenticationModificationController asAccountAuthenticationModificationController => asAccountAuthenticationModificationController -> RawId -> IO ()
setPresentationContextProvider asAccountAuthenticationModificationController  value =
    sendMsg asAccountAuthenticationModificationController (mkSelector "setPresentationContextProvider:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @performRequest:@
performRequestSelector :: Selector
performRequestSelector = mkSelector "performRequest:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @presentationContextProvider@
presentationContextProviderSelector :: Selector
presentationContextProviderSelector = mkSelector "presentationContextProvider"

-- | @Selector@ for @setPresentationContextProvider:@
setPresentationContextProviderSelector :: Selector
setPresentationContextProviderSelector = mkSelector "setPresentationContextProvider:"

