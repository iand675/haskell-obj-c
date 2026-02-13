{-# LANGUAGE DataKinds #-}
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
  , setStandardErrorSelector
  , setStandardInputSelector
  , setStandardOutputSelector
  , standardErrorSelector
  , standardInputSelector
  , standardOutputSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- executeWithArguments:completionHandler:@
executeWithArguments_completionHandler :: (IsNSUserUnixTask nsUserUnixTask, IsNSArray arguments) => nsUserUnixTask -> arguments -> Ptr () -> IO ()
executeWithArguments_completionHandler nsUserUnixTask arguments handler =
  sendMessage nsUserUnixTask executeWithArguments_completionHandlerSelector (toNSArray arguments) handler

-- | @- standardInput@
standardInput :: IsNSUserUnixTask nsUserUnixTask => nsUserUnixTask -> IO (Id NSFileHandle)
standardInput nsUserUnixTask =
  sendMessage nsUserUnixTask standardInputSelector

-- | @- setStandardInput:@
setStandardInput :: (IsNSUserUnixTask nsUserUnixTask, IsNSFileHandle value) => nsUserUnixTask -> value -> IO ()
setStandardInput nsUserUnixTask value =
  sendMessage nsUserUnixTask setStandardInputSelector (toNSFileHandle value)

-- | @- standardOutput@
standardOutput :: IsNSUserUnixTask nsUserUnixTask => nsUserUnixTask -> IO (Id NSFileHandle)
standardOutput nsUserUnixTask =
  sendMessage nsUserUnixTask standardOutputSelector

-- | @- setStandardOutput:@
setStandardOutput :: (IsNSUserUnixTask nsUserUnixTask, IsNSFileHandle value) => nsUserUnixTask -> value -> IO ()
setStandardOutput nsUserUnixTask value =
  sendMessage nsUserUnixTask setStandardOutputSelector (toNSFileHandle value)

-- | @- standardError@
standardError :: IsNSUserUnixTask nsUserUnixTask => nsUserUnixTask -> IO (Id NSFileHandle)
standardError nsUserUnixTask =
  sendMessage nsUserUnixTask standardErrorSelector

-- | @- setStandardError:@
setStandardError :: (IsNSUserUnixTask nsUserUnixTask, IsNSFileHandle value) => nsUserUnixTask -> value -> IO ()
setStandardError nsUserUnixTask value =
  sendMessage nsUserUnixTask setStandardErrorSelector (toNSFileHandle value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @executeWithArguments:completionHandler:@
executeWithArguments_completionHandlerSelector :: Selector '[Id NSArray, Ptr ()] ()
executeWithArguments_completionHandlerSelector = mkSelector "executeWithArguments:completionHandler:"

-- | @Selector@ for @standardInput@
standardInputSelector :: Selector '[] (Id NSFileHandle)
standardInputSelector = mkSelector "standardInput"

-- | @Selector@ for @setStandardInput:@
setStandardInputSelector :: Selector '[Id NSFileHandle] ()
setStandardInputSelector = mkSelector "setStandardInput:"

-- | @Selector@ for @standardOutput@
standardOutputSelector :: Selector '[] (Id NSFileHandle)
standardOutputSelector = mkSelector "standardOutput"

-- | @Selector@ for @setStandardOutput:@
setStandardOutputSelector :: Selector '[Id NSFileHandle] ()
setStandardOutputSelector = mkSelector "setStandardOutput:"

-- | @Selector@ for @standardError@
standardErrorSelector :: Selector '[] (Id NSFileHandle)
standardErrorSelector = mkSelector "standardError"

-- | @Selector@ for @setStandardError:@
setStandardErrorSelector :: Selector '[Id NSFileHandle] ()
setStandardErrorSelector = mkSelector "setStandardError:"

