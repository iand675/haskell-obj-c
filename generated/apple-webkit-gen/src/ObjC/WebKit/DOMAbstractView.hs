{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMAbstractView@.
module ObjC.WebKit.DOMAbstractView
  ( DOMAbstractView
  , IsDOMAbstractView(..)
  , document
  , documentSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- document@
document :: IsDOMAbstractView domAbstractView => domAbstractView -> IO (Id DOMDocument)
document domAbstractView =
  sendMessage domAbstractView documentSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @document@
documentSelector :: Selector '[] (Id DOMDocument)
documentSelector = mkSelector "document"

