{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCloneCommand@.
module ObjC.Foundation.NSCloneCommand
  ( NSCloneCommand
  , IsNSCloneCommand(..)
  , setReceiversSpecifier
  , keySpecifier
  , keySpecifierSelector
  , setReceiversSpecifierSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- setReceiversSpecifier:@
setReceiversSpecifier :: (IsNSCloneCommand nsCloneCommand, IsNSScriptObjectSpecifier receiversRef) => nsCloneCommand -> receiversRef -> IO ()
setReceiversSpecifier nsCloneCommand receiversRef =
  sendMessage nsCloneCommand setReceiversSpecifierSelector (toNSScriptObjectSpecifier receiversRef)

-- | @- keySpecifier@
keySpecifier :: IsNSCloneCommand nsCloneCommand => nsCloneCommand -> IO (Id NSScriptObjectSpecifier)
keySpecifier nsCloneCommand =
  sendMessage nsCloneCommand keySpecifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setReceiversSpecifier:@
setReceiversSpecifierSelector :: Selector '[Id NSScriptObjectSpecifier] ()
setReceiversSpecifierSelector = mkSelector "setReceiversSpecifier:"

-- | @Selector@ for @keySpecifier@
keySpecifierSelector :: Selector '[] (Id NSScriptObjectSpecifier)
keySpecifierSelector = mkSelector "keySpecifier"

