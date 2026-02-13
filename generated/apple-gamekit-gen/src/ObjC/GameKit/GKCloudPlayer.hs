{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GKCloudPlayer@.
module ObjC.GameKit.GKCloudPlayer
  ( GKCloudPlayer
  , IsGKCloudPlayer(..)
  , getCurrentSignedInPlayerForContainer_completionHandler
  , getCurrentSignedInPlayerForContainer_completionHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Retrieve a player instance representing the active iCloud account for a given iCloud container. Returns nil and an error if the user is not signed in to iCloud or the container is invalid.
--
-- ObjC selector: @+ getCurrentSignedInPlayerForContainer:completionHandler:@
getCurrentSignedInPlayerForContainer_completionHandler :: IsNSString containerName => containerName -> Ptr () -> IO ()
getCurrentSignedInPlayerForContainer_completionHandler containerName handler =
  do
    cls' <- getRequiredClass "GKCloudPlayer"
    sendClassMessage cls' getCurrentSignedInPlayerForContainer_completionHandlerSelector (toNSString containerName) handler

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getCurrentSignedInPlayerForContainer:completionHandler:@
getCurrentSignedInPlayerForContainer_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
getCurrentSignedInPlayerForContainer_completionHandlerSelector = mkSelector "getCurrentSignedInPlayerForContainer:completionHandler:"

