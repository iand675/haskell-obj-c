{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSSetCommand@.
module ObjC.Foundation.NSSetCommand
  ( NSSetCommand
  , IsNSSetCommand(..)
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
setReceiversSpecifier :: (IsNSSetCommand nsSetCommand, IsNSScriptObjectSpecifier receiversRef) => nsSetCommand -> receiversRef -> IO ()
setReceiversSpecifier nsSetCommand  receiversRef =
withObjCPtr receiversRef $ \raw_receiversRef ->
    sendMsg nsSetCommand (mkSelector "setReceiversSpecifier:") retVoid [argPtr (castPtr raw_receiversRef :: Ptr ())]

-- | @- keySpecifier@
keySpecifier :: IsNSSetCommand nsSetCommand => nsSetCommand -> IO (Id NSScriptObjectSpecifier)
keySpecifier nsSetCommand  =
  sendMsg nsSetCommand (mkSelector "keySpecifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setReceiversSpecifier:@
setReceiversSpecifierSelector :: Selector
setReceiversSpecifierSelector = mkSelector "setReceiversSpecifier:"

-- | @Selector@ for @keySpecifier@
keySpecifierSelector :: Selector
keySpecifierSelector = mkSelector "keySpecifier"

