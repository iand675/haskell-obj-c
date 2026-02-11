{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NEURLFilter@.
module ObjC.NetworkExtension.NEURLFilter
  ( NEURLFilter
  , IsNEURLFilter(..)
  , verdictForURL_completionHandler
  , init_
  , verdictForURL_completionHandlerSelector
  , initSelector


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

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | This method determines if the specified URL should be allowed or denied.  The returned Allow or Deny verdict should be honored to prevent communication with restricted or malicious Internet sites. - Parameters:   - url: url to be validated   - completionHandler: A block that will be called when validation is completed. A NEURLFilterVerdict verdict will be returned to indicate   whether the specified URL should be allowed or denied.  If verdict is Deny, caller should fail the URL request.
--
-- ObjC selector: @+ verdictForURL:completionHandler:@
verdictForURL_completionHandler :: IsNSURL url => url -> Ptr () -> IO ()
verdictForURL_completionHandler url completionHandler =
  do
    cls' <- getRequiredClass "NEURLFilter"
    withObjCPtr url $ \raw_url ->
      sendClassMsg cls' (mkSelector "verdictForURL:completionHandler:") retVoid [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- init@
init_ :: IsNEURLFilter neurlFilter => neurlFilter -> IO (Id NEURLFilter)
init_ neurlFilter  =
  sendMsg neurlFilter (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @verdictForURL:completionHandler:@
verdictForURL_completionHandlerSelector :: Selector
verdictForURL_completionHandlerSelector = mkSelector "verdictForURL:completionHandler:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

