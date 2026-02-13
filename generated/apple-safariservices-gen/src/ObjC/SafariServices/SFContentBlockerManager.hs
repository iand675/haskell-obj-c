{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SFContentBlockerManager@.
module ObjC.SafariServices.SFContentBlockerManager
  ( SFContentBlockerManager
  , IsSFContentBlockerManager(..)
  , reloadContentBlockerWithIdentifier_completionHandler
  , getStateOfContentBlockerWithIdentifier_completionHandler
  , getStateOfContentBlockerWithIdentifier_completionHandlerSelector
  , reloadContentBlockerWithIdentifier_completionHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SafariServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ reloadContentBlockerWithIdentifier:completionHandler:@
reloadContentBlockerWithIdentifier_completionHandler :: IsNSString identifier => identifier -> Ptr () -> IO ()
reloadContentBlockerWithIdentifier_completionHandler identifier completionHandler =
  do
    cls' <- getRequiredClass "SFContentBlockerManager"
    sendClassMessage cls' reloadContentBlockerWithIdentifier_completionHandlerSelector (toNSString identifier) completionHandler

-- | @+ getStateOfContentBlockerWithIdentifier:completionHandler:@
getStateOfContentBlockerWithIdentifier_completionHandler :: IsNSString identifier => identifier -> Ptr () -> IO ()
getStateOfContentBlockerWithIdentifier_completionHandler identifier completionHandler =
  do
    cls' <- getRequiredClass "SFContentBlockerManager"
    sendClassMessage cls' getStateOfContentBlockerWithIdentifier_completionHandlerSelector (toNSString identifier) completionHandler

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reloadContentBlockerWithIdentifier:completionHandler:@
reloadContentBlockerWithIdentifier_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
reloadContentBlockerWithIdentifier_completionHandlerSelector = mkSelector "reloadContentBlockerWithIdentifier:completionHandler:"

-- | @Selector@ for @getStateOfContentBlockerWithIdentifier:completionHandler:@
getStateOfContentBlockerWithIdentifier_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
getStateOfContentBlockerWithIdentifier_completionHandlerSelector = mkSelector "getStateOfContentBlockerWithIdentifier:completionHandler:"

