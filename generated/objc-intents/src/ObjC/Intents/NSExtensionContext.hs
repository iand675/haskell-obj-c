{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSExtensionContext@.
module ObjC.Intents.NSExtensionContext
  ( NSExtensionContext
  , IsNSExtensionContext(..)
  , intent
  , intentSelector


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

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- intent@
intent :: IsNSExtensionContext nsExtensionContext => nsExtensionContext -> IO (Id INIntent)
intent nsExtensionContext  =
  sendMsg nsExtensionContext (mkSelector "intent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @intent@
intentSelector :: Selector
intentSelector = mkSelector "intent"

