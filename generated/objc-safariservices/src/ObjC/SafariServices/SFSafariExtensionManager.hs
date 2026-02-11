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

import ObjC.SafariServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ getStateOfSafariExtensionWithIdentifier:completionHandler:@
getStateOfSafariExtensionWithIdentifier_completionHandler :: IsNSString identifier => identifier -> Ptr () -> IO ()
getStateOfSafariExtensionWithIdentifier_completionHandler identifier completionHandler =
  do
    cls' <- getRequiredClass "SFSafariExtensionManager"
    withObjCPtr identifier $ \raw_identifier ->
      sendClassMsg cls' (mkSelector "getStateOfSafariExtensionWithIdentifier:completionHandler:") retVoid [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getStateOfSafariExtensionWithIdentifier:completionHandler:@
getStateOfSafariExtensionWithIdentifier_completionHandlerSelector :: Selector
getStateOfSafariExtensionWithIdentifier_completionHandlerSelector = mkSelector "getStateOfSafariExtensionWithIdentifier:completionHandler:"

