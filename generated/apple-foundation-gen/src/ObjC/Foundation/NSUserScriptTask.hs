{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUserScriptTask@.
module ObjC.Foundation.NSUserScriptTask
  ( NSUserScriptTask
  , IsNSUserScriptTask(..)
  , initWithURL_error
  , executeWithCompletionHandler
  , scriptURL
  , executeWithCompletionHandlerSelector
  , initWithURL_errorSelector
  , scriptURLSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- initWithURL:error:@
initWithURL_error :: (IsNSUserScriptTask nsUserScriptTask, IsNSURL url, IsNSError error_) => nsUserScriptTask -> url -> error_ -> IO (Id NSUserScriptTask)
initWithURL_error nsUserScriptTask url error_ =
  sendOwnedMessage nsUserScriptTask initWithURL_errorSelector (toNSURL url) (toNSError error_)

-- | @- executeWithCompletionHandler:@
executeWithCompletionHandler :: IsNSUserScriptTask nsUserScriptTask => nsUserScriptTask -> Ptr () -> IO ()
executeWithCompletionHandler nsUserScriptTask handler =
  sendMessage nsUserScriptTask executeWithCompletionHandlerSelector handler

-- | @- scriptURL@
scriptURL :: IsNSUserScriptTask nsUserScriptTask => nsUserScriptTask -> IO (Id NSURL)
scriptURL nsUserScriptTask =
  sendMessage nsUserScriptTask scriptURLSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithURL:error:@
initWithURL_errorSelector :: Selector '[Id NSURL, Id NSError] (Id NSUserScriptTask)
initWithURL_errorSelector = mkSelector "initWithURL:error:"

-- | @Selector@ for @executeWithCompletionHandler:@
executeWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
executeWithCompletionHandlerSelector = mkSelector "executeWithCompletionHandler:"

-- | @Selector@ for @scriptURL@
scriptURLSelector :: Selector '[] (Id NSURL)
scriptURLSelector = mkSelector "scriptURL"

