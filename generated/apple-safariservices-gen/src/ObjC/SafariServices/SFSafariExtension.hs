{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SFSafariExtension@.
module ObjC.SafariServices.SFSafariExtension
  ( SFSafariExtension
  , IsSFSafariExtension(..)
  , new
  , init_
  , getBaseURIWithCompletionHandler
  , getBaseURIWithCompletionHandlerSelector
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SafariServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id SFSafariExtension)
new  =
  do
    cls' <- getRequiredClass "SFSafariExtension"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsSFSafariExtension sfSafariExtension => sfSafariExtension -> IO (Id SFSafariExtension)
init_ sfSafariExtension =
  sendOwnedMessage sfSafariExtension initSelector

-- | Calls the completion handler with the base URI of the extension.
--
-- ObjC selector: @+ getBaseURIWithCompletionHandler:@
getBaseURIWithCompletionHandler :: Ptr () -> IO ()
getBaseURIWithCompletionHandler completionHandler =
  do
    cls' <- getRequiredClass "SFSafariExtension"
    sendClassMessage cls' getBaseURIWithCompletionHandlerSelector completionHandler

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SFSafariExtension)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SFSafariExtension)
initSelector = mkSelector "init"

-- | @Selector@ for @getBaseURIWithCompletionHandler:@
getBaseURIWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
getBaseURIWithCompletionHandlerSelector = mkSelector "getBaseURIWithCompletionHandler:"

