{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SFSafariExtensionManager@.
module ObjC.SafariServices.SFSafariExtensionManager
  ( SFSafariExtensionManager
  , IsSFSafariExtensionManager(..)
  , getStateOfSafariExtensionWithIdentifier_completionHandler
  , getStateOfSafariExtensionWithIdentifier_completionHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SafariServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ getStateOfSafariExtensionWithIdentifier:completionHandler:@
getStateOfSafariExtensionWithIdentifier_completionHandler :: IsNSString identifier => identifier -> Ptr () -> IO ()
getStateOfSafariExtensionWithIdentifier_completionHandler identifier completionHandler =
  do
    cls' <- getRequiredClass "SFSafariExtensionManager"
    sendClassMessage cls' getStateOfSafariExtensionWithIdentifier_completionHandlerSelector (toNSString identifier) completionHandler

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getStateOfSafariExtensionWithIdentifier:completionHandler:@
getStateOfSafariExtensionWithIdentifier_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
getStateOfSafariExtensionWithIdentifier_completionHandlerSelector = mkSelector "getStateOfSafariExtensionWithIdentifier:completionHandler:"

