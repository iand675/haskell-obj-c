{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUserAutomatorTask@.
module ObjC.Foundation.NSUserAutomatorTask
  ( NSUserAutomatorTask
  , IsNSUserAutomatorTask(..)
  , executeWithInput_completionHandler
  , variables
  , setVariables
  , executeWithInput_completionHandlerSelector
  , setVariablesSelector
  , variablesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- executeWithInput:completionHandler:@
executeWithInput_completionHandler :: IsNSUserAutomatorTask nsUserAutomatorTask => nsUserAutomatorTask -> RawId -> Ptr () -> IO ()
executeWithInput_completionHandler nsUserAutomatorTask input handler =
  sendMessage nsUserAutomatorTask executeWithInput_completionHandlerSelector input handler

-- | @- variables@
variables :: IsNSUserAutomatorTask nsUserAutomatorTask => nsUserAutomatorTask -> IO (Id NSDictionary)
variables nsUserAutomatorTask =
  sendMessage nsUserAutomatorTask variablesSelector

-- | @- setVariables:@
setVariables :: (IsNSUserAutomatorTask nsUserAutomatorTask, IsNSDictionary value) => nsUserAutomatorTask -> value -> IO ()
setVariables nsUserAutomatorTask value =
  sendMessage nsUserAutomatorTask setVariablesSelector (toNSDictionary value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @executeWithInput:completionHandler:@
executeWithInput_completionHandlerSelector :: Selector '[RawId, Ptr ()] ()
executeWithInput_completionHandlerSelector = mkSelector "executeWithInput:completionHandler:"

-- | @Selector@ for @variables@
variablesSelector :: Selector '[] (Id NSDictionary)
variablesSelector = mkSelector "variables"

-- | @Selector@ for @setVariables:@
setVariablesSelector :: Selector '[Id NSDictionary] ()
setVariablesSelector = mkSelector "setVariables:"

