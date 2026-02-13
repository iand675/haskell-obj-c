{-# LANGUAGE DataKinds #-}
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
  , inputFieldSeparatorSelector
  , outputFieldSeparatorSelector
  , remapLineEndingsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Automator.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- remapLineEndings@
remapLineEndings :: IsAMShellScriptAction amShellScriptAction => amShellScriptAction -> IO Bool
remapLineEndings amShellScriptAction =
  sendMessage amShellScriptAction remapLineEndingsSelector

-- | @- inputFieldSeparator@
inputFieldSeparator :: IsAMShellScriptAction amShellScriptAction => amShellScriptAction -> IO (Id NSString)
inputFieldSeparator amShellScriptAction =
  sendMessage amShellScriptAction inputFieldSeparatorSelector

-- | @- outputFieldSeparator@
outputFieldSeparator :: IsAMShellScriptAction amShellScriptAction => amShellScriptAction -> IO (Id NSString)
outputFieldSeparator amShellScriptAction =
  sendMessage amShellScriptAction outputFieldSeparatorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @remapLineEndings@
remapLineEndingsSelector :: Selector '[] Bool
remapLineEndingsSelector = mkSelector "remapLineEndings"

-- | @Selector@ for @inputFieldSeparator@
inputFieldSeparatorSelector :: Selector '[] (Id NSString)
inputFieldSeparatorSelector = mkSelector "inputFieldSeparator"

-- | @Selector@ for @outputFieldSeparator@
outputFieldSeparatorSelector :: Selector '[] (Id NSString)
outputFieldSeparatorSelector = mkSelector "outputFieldSeparator"

