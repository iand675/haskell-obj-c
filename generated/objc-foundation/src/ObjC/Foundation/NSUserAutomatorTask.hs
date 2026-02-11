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
  , variablesSelector
  , setVariablesSelector


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

-- | @- executeWithInput:completionHandler:@
executeWithInput_completionHandler :: IsNSUserAutomatorTask nsUserAutomatorTask => nsUserAutomatorTask -> RawId -> Ptr () -> IO ()
executeWithInput_completionHandler nsUserAutomatorTask  input handler =
  sendMsg nsUserAutomatorTask (mkSelector "executeWithInput:completionHandler:") retVoid [argPtr (castPtr (unRawId input) :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | @- variables@
variables :: IsNSUserAutomatorTask nsUserAutomatorTask => nsUserAutomatorTask -> IO (Id NSDictionary)
variables nsUserAutomatorTask  =
  sendMsg nsUserAutomatorTask (mkSelector "variables") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setVariables:@
setVariables :: (IsNSUserAutomatorTask nsUserAutomatorTask, IsNSDictionary value) => nsUserAutomatorTask -> value -> IO ()
setVariables nsUserAutomatorTask  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsUserAutomatorTask (mkSelector "setVariables:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @executeWithInput:completionHandler:@
executeWithInput_completionHandlerSelector :: Selector
executeWithInput_completionHandlerSelector = mkSelector "executeWithInput:completionHandler:"

-- | @Selector@ for @variables@
variablesSelector :: Selector
variablesSelector = mkSelector "variables"

-- | @Selector@ for @setVariables:@
setVariablesSelector :: Selector
setVariablesSelector = mkSelector "setVariables:"

