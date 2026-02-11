{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCloneCommand@.
module ObjC.Foundation.NSCloneCommand
  ( NSCloneCommand
  , IsNSCloneCommand(..)
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
setReceiversSpecifier :: (IsNSCloneCommand nsCloneCommand, IsNSScriptObjectSpecifier receiversRef) => nsCloneCommand -> receiversRef -> IO ()
setReceiversSpecifier nsCloneCommand  receiversRef =
withObjCPtr receiversRef $ \raw_receiversRef ->
    sendMsg nsCloneCommand (mkSelector "setReceiversSpecifier:") retVoid [argPtr (castPtr raw_receiversRef :: Ptr ())]

-- | @- keySpecifier@
keySpecifier :: IsNSCloneCommand nsCloneCommand => nsCloneCommand -> IO (Id NSScriptObjectSpecifier)
keySpecifier nsCloneCommand  =
  sendMsg nsCloneCommand (mkSelector "keySpecifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setReceiversSpecifier:@
setReceiversSpecifierSelector :: Selector
setReceiversSpecifierSelector = mkSelector "setReceiversSpecifier:"

-- | @Selector@ for @keySpecifier@
keySpecifierSelector :: Selector
keySpecifierSelector = mkSelector "keySpecifier"

