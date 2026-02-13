{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- executeWithAppleEvent:completionHandler:@
executeWithAppleEvent_completionHandler :: (IsNSUserAppleScriptTask nsUserAppleScriptTask, IsNSAppleEventDescriptor event) => nsUserAppleScriptTask -> event -> Ptr () -> IO ()
executeWithAppleEvent_completionHandler nsUserAppleScriptTask event handler =
  sendMessage nsUserAppleScriptTask executeWithAppleEvent_completionHandlerSelector (toNSAppleEventDescriptor event) handler

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @executeWithAppleEvent:completionHandler:@
executeWithAppleEvent_completionHandlerSelector :: Selector '[Id NSAppleEventDescriptor, Ptr ()] ()
executeWithAppleEvent_completionHandlerSelector = mkSelector "executeWithAppleEvent:completionHandler:"

