{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- intent@
intent :: IsNSExtensionContext nsExtensionContext => nsExtensionContext -> IO (Id INIntent)
intent nsExtensionContext =
  sendMessage nsExtensionContext intentSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @intent@
intentSelector :: Selector '[] (Id INIntent)
intentSelector = mkSelector "intent"

