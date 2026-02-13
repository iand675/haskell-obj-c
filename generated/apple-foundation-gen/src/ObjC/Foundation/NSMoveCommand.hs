{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMoveCommand@.
module ObjC.Foundation.NSMoveCommand
  ( NSMoveCommand
  , IsNSMoveCommand(..)
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
setReceiversSpecifier :: (IsNSMoveCommand nsMoveCommand, IsNSScriptObjectSpecifier receiversRef) => nsMoveCommand -> receiversRef -> IO ()
setReceiversSpecifier nsMoveCommand receiversRef =
  sendMessage nsMoveCommand setReceiversSpecifierSelector (toNSScriptObjectSpecifier receiversRef)

-- | @- keySpecifier@
keySpecifier :: IsNSMoveCommand nsMoveCommand => nsMoveCommand -> IO (Id NSScriptObjectSpecifier)
keySpecifier nsMoveCommand =
  sendMessage nsMoveCommand keySpecifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setReceiversSpecifier:@
setReceiversSpecifierSelector :: Selector '[Id NSScriptObjectSpecifier] ()
setReceiversSpecifierSelector = mkSelector "setReceiversSpecifier:"

-- | @Selector@ for @keySpecifier@
keySpecifierSelector :: Selector '[] (Id NSScriptObjectSpecifier)
keySpecifierSelector = mkSelector "keySpecifier"

