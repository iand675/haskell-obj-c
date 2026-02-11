{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUserAppleScriptTask@.
module ObjC.Foundation.NSUserAppleScriptTask
  ( NSUserAppleScriptTask
  , IsNSUserAppleScriptTask(..)
  , executeWithAppleEvent_completionHandler
  , executeWithAppleEvent_completionHandlerSelector


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

import ObjC.Foundation.Internal.Classes

-- | @- executeWithAppleEvent:completionHandler:@
executeWithAppleEvent_completionHandler :: (IsNSUserAppleScriptTask nsUserAppleScriptTask, IsNSAppleEventDescriptor event) => nsUserAppleScriptTask -> event -> Ptr () -> IO ()
executeWithAppleEvent_completionHandler nsUserAppleScriptTask  event handler =
withObjCPtr event $ \raw_event ->
    sendMsg nsUserAppleScriptTask (mkSelector "executeWithAppleEvent:completionHandler:") retVoid [argPtr (castPtr raw_event :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @executeWithAppleEvent:completionHandler:@
executeWithAppleEvent_completionHandlerSelector :: Selector
executeWithAppleEvent_completionHandlerSelector = mkSelector "executeWithAppleEvent:completionHandler:"

