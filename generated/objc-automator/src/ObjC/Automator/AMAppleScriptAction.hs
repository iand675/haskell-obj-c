{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AMAppleScriptAction@.
module ObjC.Automator.AMAppleScriptAction
  ( AMAppleScriptAction
  , IsAMAppleScriptAction(..)
  , script
  , setScript
  , scriptSelector
  , setScriptSelector


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

import ObjC.Automator.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- script@
script :: IsAMAppleScriptAction amAppleScriptAction => amAppleScriptAction -> IO (Id OSAScript)
script amAppleScriptAction  =
  sendMsg amAppleScriptAction (mkSelector "script") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setScript:@
setScript :: (IsAMAppleScriptAction amAppleScriptAction, IsOSAScript value) => amAppleScriptAction -> value -> IO ()
setScript amAppleScriptAction  value =
withObjCPtr value $ \raw_value ->
    sendMsg amAppleScriptAction (mkSelector "setScript:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @script@
scriptSelector :: Selector
scriptSelector = mkSelector "script"

-- | @Selector@ for @setScript:@
setScriptSelector :: Selector
setScriptSelector = mkSelector "setScript:"

