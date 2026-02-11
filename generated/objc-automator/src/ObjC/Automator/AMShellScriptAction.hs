{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AMShellScriptAction@.
module ObjC.Automator.AMShellScriptAction
  ( AMShellScriptAction
  , IsAMShellScriptAction(..)
  , remapLineEndings
  , inputFieldSeparator
  , outputFieldSeparator
  , remapLineEndingsSelector
  , inputFieldSeparatorSelector
  , outputFieldSeparatorSelector


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

-- | @- remapLineEndings@
remapLineEndings :: IsAMShellScriptAction amShellScriptAction => amShellScriptAction -> IO Bool
remapLineEndings amShellScriptAction  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg amShellScriptAction (mkSelector "remapLineEndings") retCULong []

-- | @- inputFieldSeparator@
inputFieldSeparator :: IsAMShellScriptAction amShellScriptAction => amShellScriptAction -> IO (Id NSString)
inputFieldSeparator amShellScriptAction  =
  sendMsg amShellScriptAction (mkSelector "inputFieldSeparator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- outputFieldSeparator@
outputFieldSeparator :: IsAMShellScriptAction amShellScriptAction => amShellScriptAction -> IO (Id NSString)
outputFieldSeparator amShellScriptAction  =
  sendMsg amShellScriptAction (mkSelector "outputFieldSeparator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @remapLineEndings@
remapLineEndingsSelector :: Selector
remapLineEndingsSelector = mkSelector "remapLineEndings"

-- | @Selector@ for @inputFieldSeparator@
inputFieldSeparatorSelector :: Selector
inputFieldSeparatorSelector = mkSelector "inputFieldSeparator"

-- | @Selector@ for @outputFieldSeparator@
outputFieldSeparatorSelector :: Selector
outputFieldSeparatorSelector = mkSelector "outputFieldSeparator"

