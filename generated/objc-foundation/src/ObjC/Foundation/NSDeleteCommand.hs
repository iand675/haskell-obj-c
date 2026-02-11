{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSDeleteCommand@.
module ObjC.Foundation.NSDeleteCommand
  ( NSDeleteCommand
  , IsNSDeleteCommand(..)
  , setReceiversSpecifier
  , keySpecifier
  , setReceiversSpecifierSelector
  , keySpecifierSelector


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

-- | @- setReceiversSpecifier:@
setReceiversSpecifier :: (IsNSDeleteCommand nsDeleteCommand, IsNSScriptObjectSpecifier receiversRef) => nsDeleteCommand -> receiversRef -> IO ()
setReceiversSpecifier nsDeleteCommand  receiversRef =
withObjCPtr receiversRef $ \raw_receiversRef ->
    sendMsg nsDeleteCommand (mkSelector "setReceiversSpecifier:") retVoid [argPtr (castPtr raw_receiversRef :: Ptr ())]

-- | @- keySpecifier@
keySpecifier :: IsNSDeleteCommand nsDeleteCommand => nsDeleteCommand -> IO (Id NSScriptObjectSpecifier)
keySpecifier nsDeleteCommand  =
  sendMsg nsDeleteCommand (mkSelector "keySpecifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setReceiversSpecifier:@
setReceiversSpecifierSelector :: Selector
setReceiversSpecifierSelector = mkSelector "setReceiversSpecifier:"

-- | @Selector@ for @keySpecifier@
keySpecifierSelector :: Selector
keySpecifierSelector = mkSelector "keySpecifier"

