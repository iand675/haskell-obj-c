{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NEURLFilter@.
module ObjC.NetworkExtension.NEURLFilter
  ( NEURLFilter
  , IsNEURLFilter(..)
  , verdictForURL_completionHandler
  , init_
  , initSelector
  , verdictForURL_completionHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | This method determines if the specified URL should be allowed or denied.  The returned Allow or Deny verdict should be honored to prevent communication with restricted or malicious Internet sites. - Parameters:   - url: url to be validated   - completionHandler: A block that will be called when validation is completed. A NEURLFilterVerdict verdict will be returned to indicate   whether the specified URL should be allowed or denied.  If verdict is Deny, caller should fail the URL request.
--
-- ObjC selector: @+ verdictForURL:completionHandler:@
verdictForURL_completionHandler :: IsNSURL url => url -> Ptr () -> IO ()
verdictForURL_completionHandler url completionHandler =
  do
    cls' <- getRequiredClass "NEURLFilter"
    sendClassMessage cls' verdictForURL_completionHandlerSelector (toNSURL url) completionHandler

-- | @- init@
init_ :: IsNEURLFilter neurlFilter => neurlFilter -> IO (Id NEURLFilter)
init_ neurlFilter =
  sendOwnedMessage neurlFilter initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @verdictForURL:completionHandler:@
verdictForURL_completionHandlerSelector :: Selector '[Id NSURL, Ptr ()] ()
verdictForURL_completionHandlerSelector = mkSelector "verdictForURL:completionHandler:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NEURLFilter)
initSelector = mkSelector "init"

