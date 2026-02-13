{-# LANGUAGE DataKinds #-}
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
  , delegateSelector
  , performRequestSelector
  , presentationContextProviderSelector
  , setDelegateSelector
  , setPresentationContextProviderSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Perform an upgrade request, one at a time. Any requests initiated with a request already in progress will fail immediately.
--
-- ObjC selector: @- performRequest:@
performRequest :: (IsASAccountAuthenticationModificationController asAccountAuthenticationModificationController, IsASAccountAuthenticationModificationRequest request) => asAccountAuthenticationModificationController -> request -> IO ()
performRequest asAccountAuthenticationModificationController request =
  sendMessage asAccountAuthenticationModificationController performRequestSelector (toASAccountAuthenticationModificationRequest request)

-- | This delegate will be notified upon completion of the upgrade to report success or failure.
--
-- ObjC selector: @- delegate@
delegate :: IsASAccountAuthenticationModificationController asAccountAuthenticationModificationController => asAccountAuthenticationModificationController -> IO RawId
delegate asAccountAuthenticationModificationController =
  sendMessage asAccountAuthenticationModificationController delegateSelector

-- | This delegate will be notified upon completion of the upgrade to report success or failure.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsASAccountAuthenticationModificationController asAccountAuthenticationModificationController => asAccountAuthenticationModificationController -> RawId -> IO ()
setDelegate asAccountAuthenticationModificationController value =
  sendMessage asAccountAuthenticationModificationController setDelegateSelector value

-- | This will be used to provide a presentation context to display authorization UI.
--
-- ObjC selector: @- presentationContextProvider@
presentationContextProvider :: IsASAccountAuthenticationModificationController asAccountAuthenticationModificationController => asAccountAuthenticationModificationController -> IO RawId
presentationContextProvider asAccountAuthenticationModificationController =
  sendMessage asAccountAuthenticationModificationController presentationContextProviderSelector

-- | This will be used to provide a presentation context to display authorization UI.
--
-- ObjC selector: @- setPresentationContextProvider:@
setPresentationContextProvider :: IsASAccountAuthenticationModificationController asAccountAuthenticationModificationController => asAccountAuthenticationModificationController -> RawId -> IO ()
setPresentationContextProvider asAccountAuthenticationModificationController value =
  sendMessage asAccountAuthenticationModificationController setPresentationContextProviderSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @performRequest:@
performRequestSelector :: Selector '[Id ASAccountAuthenticationModificationRequest] ()
performRequestSelector = mkSelector "performRequest:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @presentationContextProvider@
presentationContextProviderSelector :: Selector '[] RawId
presentationContextProviderSelector = mkSelector "presentationContextProvider"

-- | @Selector@ for @setPresentationContextProvider:@
setPresentationContextProviderSelector :: Selector '[RawId] ()
setPresentationContextProviderSelector = mkSelector "setPresentationContextProvider:"

