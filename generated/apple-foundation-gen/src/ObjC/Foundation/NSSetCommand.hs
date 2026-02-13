{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSSetCommand@.
module ObjC.Foundation.NSSetCommand
  ( NSSetCommand
  , IsNSSetCommand(..)
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
setReceiversSpecifier :: (IsNSSetCommand nsSetCommand, IsNSScriptObjectSpecifier receiversRef) => nsSetCommand -> receiversRef -> IO ()
setReceiversSpecifier nsSetCommand receiversRef =
  sendMessage nsSetCommand setReceiversSpecifierSelector (toNSScriptObjectSpecifier receiversRef)

-- | @- keySpecifier@
keySpecifier :: IsNSSetCommand nsSetCommand => nsSetCommand -> IO (Id NSScriptObjectSpecifier)
keySpecifier nsSetCommand =
  sendMessage nsSetCommand keySpecifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setReceiversSpecifier:@
setReceiversSpecifierSelector :: Selector '[Id NSScriptObjectSpecifier] ()
setReceiversSpecifierSelector = mkSelector "setReceiversSpecifier:"

-- | @Selector@ for @keySpecifier@
keySpecifierSelector :: Selector '[] (Id NSScriptObjectSpecifier)
keySpecifierSelector = mkSelector "keySpecifier"

