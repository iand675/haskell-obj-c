{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SFContentBlockerManager@.
module ObjC.SafariServices.SFContentBlockerManager
  ( SFContentBlockerManager
  , IsSFContentBlockerManager(..)
  , reloadContentBlockerWithIdentifier_completionHandler
  , getStateOfContentBlockerWithIdentifier_completionHandler
  , reloadContentBlockerWithIdentifier_completionHandlerSelector
  , getStateOfContentBlockerWithIdentifier_completionHandlerSelector


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

-- | @+ reloadContentBlockerWithIdentifier:completionHandler:@
reloadContentBlockerWithIdentifier_completionHandler :: IsNSString identifier => identifier -> Ptr () -> IO ()
reloadContentBlockerWithIdentifier_completionHandler identifier completionHandler =
  do
    cls' <- getRequiredClass "SFContentBlockerManager"
    withObjCPtr identifier $ \raw_identifier ->
      sendClassMsg cls' (mkSelector "reloadContentBlockerWithIdentifier:completionHandler:") retVoid [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @+ getStateOfContentBlockerWithIdentifier:completionHandler:@
getStateOfContentBlockerWithIdentifier_completionHandler :: IsNSString identifier => identifier -> Ptr () -> IO ()
getStateOfContentBlockerWithIdentifier_completionHandler identifier completionHandler =
  do
    cls' <- getRequiredClass "SFContentBlockerManager"
    withObjCPtr identifier $ \raw_identifier ->
      sendClassMsg cls' (mkSelector "getStateOfContentBlockerWithIdentifier:completionHandler:") retVoid [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reloadContentBlockerWithIdentifier:completionHandler:@
reloadContentBlockerWithIdentifier_completionHandlerSelector :: Selector
reloadContentBlockerWithIdentifier_completionHandlerSelector = mkSelector "reloadContentBlockerWithIdentifier:completionHandler:"

-- | @Selector@ for @getStateOfContentBlockerWithIdentifier:completionHandler:@
getStateOfContentBlockerWithIdentifier_completionHandlerSelector :: Selector
getStateOfContentBlockerWithIdentifier_completionHandlerSelector = mkSelector "getStateOfContentBlockerWithIdentifier:completionHandler:"

