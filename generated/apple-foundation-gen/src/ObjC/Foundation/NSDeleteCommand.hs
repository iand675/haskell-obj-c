{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSDeleteCommand@.
module ObjC.Foundation.NSDeleteCommand
  ( NSDeleteCommand
  , IsNSDeleteCommand(..)
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
setReceiversSpecifier :: (IsNSDeleteCommand nsDeleteCommand, IsNSScriptObjectSpecifier receiversRef) => nsDeleteCommand -> receiversRef -> IO ()
setReceiversSpecifier nsDeleteCommand receiversRef =
  sendMessage nsDeleteCommand setReceiversSpecifierSelector (toNSScriptObjectSpecifier receiversRef)

-- | @- keySpecifier@
keySpecifier :: IsNSDeleteCommand nsDeleteCommand => nsDeleteCommand -> IO (Id NSScriptObjectSpecifier)
keySpecifier nsDeleteCommand =
  sendMessage nsDeleteCommand keySpecifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setReceiversSpecifier:@
setReceiversSpecifierSelector :: Selector '[Id NSScriptObjectSpecifier] ()
setReceiversSpecifierSelector = mkSelector "setReceiversSpecifier:"

-- | @Selector@ for @keySpecifier@
keySpecifierSelector :: Selector '[] (Id NSScriptObjectSpecifier)
keySpecifierSelector = mkSelector "keySpecifier"

