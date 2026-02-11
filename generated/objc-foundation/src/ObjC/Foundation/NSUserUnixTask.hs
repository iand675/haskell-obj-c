{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUserUnixTask@.
module ObjC.Foundation.NSUserUnixTask
  ( NSUserUnixTask
  , IsNSUserUnixTask(..)
  , executeWithArguments_completionHandler
  , standardInput
  , setStandardInput
  , standardOutput
  , setStandardOutput
  , standardError
  , setStandardError
  , executeWithArguments_completionHandlerSelector
  , standardInputSelector
  , setStandardInputSelector
  , standardOutputSelector
  , setStandardOutputSelector
  , standardErrorSelector
  , setStandardErrorSelector


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

-- | @- executeWithArguments:completionHandler:@
executeWithArguments_completionHandler :: (IsNSUserUnixTask nsUserUnixTask, IsNSArray arguments) => nsUserUnixTask -> arguments -> Ptr () -> IO ()
executeWithArguments_completionHandler nsUserUnixTask  arguments handler =
withObjCPtr arguments $ \raw_arguments ->
    sendMsg nsUserUnixTask (mkSelector "executeWithArguments:completionHandler:") retVoid [argPtr (castPtr raw_arguments :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | @- standardInput@
standardInput :: IsNSUserUnixTask nsUserUnixTask => nsUserUnixTask -> IO (Id NSFileHandle)
standardInput nsUserUnixTask  =
  sendMsg nsUserUnixTask (mkSelector "standardInput") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStandardInput:@
setStandardInput :: (IsNSUserUnixTask nsUserUnixTask, IsNSFileHandle value) => nsUserUnixTask -> value -> IO ()
setStandardInput nsUserUnixTask  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsUserUnixTask (mkSelector "setStandardInput:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- standardOutput@
standardOutput :: IsNSUserUnixTask nsUserUnixTask => nsUserUnixTask -> IO (Id NSFileHandle)
standardOutput nsUserUnixTask  =
  sendMsg nsUserUnixTask (mkSelector "standardOutput") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStandardOutput:@
setStandardOutput :: (IsNSUserUnixTask nsUserUnixTask, IsNSFileHandle value) => nsUserUnixTask -> value -> IO ()
setStandardOutput nsUserUnixTask  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsUserUnixTask (mkSelector "setStandardOutput:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- standardError@
standardError :: IsNSUserUnixTask nsUserUnixTask => nsUserUnixTask -> IO (Id NSFileHandle)
standardError nsUserUnixTask  =
  sendMsg nsUserUnixTask (mkSelector "standardError") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStandardError:@
setStandardError :: (IsNSUserUnixTask nsUserUnixTask, IsNSFileHandle value) => nsUserUnixTask -> value -> IO ()
setStandardError nsUserUnixTask  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsUserUnixTask (mkSelector "setStandardError:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @executeWithArguments:completionHandler:@
executeWithArguments_completionHandlerSelector :: Selector
executeWithArguments_completionHandlerSelector = mkSelector "executeWithArguments:completionHandler:"

-- | @Selector@ for @standardInput@
standardInputSelector :: Selector
standardInputSelector = mkSelector "standardInput"

-- | @Selector@ for @setStandardInput:@
setStandardInputSelector :: Selector
setStandardInputSelector = mkSelector "setStandardInput:"

-- | @Selector@ for @standardOutput@
standardOutputSelector :: Selector
standardOutputSelector = mkSelector "standardOutput"

-- | @Selector@ for @setStandardOutput:@
setStandardOutputSelector :: Selector
setStandardOutputSelector = mkSelector "setStandardOutput:"

-- | @Selector@ for @standardError@
standardErrorSelector :: Selector
standardErrorSelector = mkSelector "standardError"

-- | @Selector@ for @setStandardError:@
setStandardErrorSelector :: Selector
setStandardErrorSelector = mkSelector "setStandardError:"

