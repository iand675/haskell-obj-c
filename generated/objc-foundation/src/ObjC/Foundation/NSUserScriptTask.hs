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
  , initWithURL_errorSelector
  , executeWithCompletionHandlerSelector
  , scriptURLSelector


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

-- | @- initWithURL:error:@
initWithURL_error :: (IsNSUserScriptTask nsUserScriptTask, IsNSURL url, IsNSError error_) => nsUserScriptTask -> url -> error_ -> IO (Id NSUserScriptTask)
initWithURL_error nsUserScriptTask  url error_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg nsUserScriptTask (mkSelector "initWithURL:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- executeWithCompletionHandler:@
executeWithCompletionHandler :: IsNSUserScriptTask nsUserScriptTask => nsUserScriptTask -> Ptr () -> IO ()
executeWithCompletionHandler nsUserScriptTask  handler =
  sendMsg nsUserScriptTask (mkSelector "executeWithCompletionHandler:") retVoid [argPtr (castPtr handler :: Ptr ())]

-- | @- scriptURL@
scriptURL :: IsNSUserScriptTask nsUserScriptTask => nsUserScriptTask -> IO (Id NSURL)
scriptURL nsUserScriptTask  =
  sendMsg nsUserScriptTask (mkSelector "scriptURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithURL:error:@
initWithURL_errorSelector :: Selector
initWithURL_errorSelector = mkSelector "initWithURL:error:"

-- | @Selector@ for @executeWithCompletionHandler:@
executeWithCompletionHandlerSelector :: Selector
executeWithCompletionHandlerSelector = mkSelector "executeWithCompletionHandler:"

-- | @Selector@ for @scriptURL@
scriptURLSelector :: Selector
scriptURLSelector = mkSelector "scriptURL"

