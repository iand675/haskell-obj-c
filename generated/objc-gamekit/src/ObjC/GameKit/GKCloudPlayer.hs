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

import ObjC.GameKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Retrieve a player instance representing the active iCloud account for a given iCloud container. Returns nil and an error if the user is not signed in to iCloud or the container is invalid.
--
-- ObjC selector: @+ getCurrentSignedInPlayerForContainer:completionHandler:@
getCurrentSignedInPlayerForContainer_completionHandler :: IsNSString containerName => containerName -> Ptr () -> IO ()
getCurrentSignedInPlayerForContainer_completionHandler containerName handler =
  do
    cls' <- getRequiredClass "GKCloudPlayer"
    withObjCPtr containerName $ \raw_containerName ->
      sendClassMsg cls' (mkSelector "getCurrentSignedInPlayerForContainer:completionHandler:") retVoid [argPtr (castPtr raw_containerName :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getCurrentSignedInPlayerForContainer:completionHandler:@
getCurrentSignedInPlayerForContainer_completionHandlerSelector :: Selector
getCurrentSignedInPlayerForContainer_completionHandlerSelector = mkSelector "getCurrentSignedInPlayerForContainer:completionHandler:"

