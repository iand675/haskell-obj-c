{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMObject@.
module ObjC.WebKit.DOMObject
  ( DOMObject
  , IsDOMObject(..)
  , init_
  , sheet
  , initSelector
  , sheetSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsDOMObject domObject => domObject -> IO (Id DOMObject)
init_ domObject =
  sendOwnedMessage domObject initSelector

-- | @- sheet@
sheet :: IsDOMObject domObject => domObject -> IO (Id DOMStyleSheet)
sheet domObject =
  sendMessage domObject sheetSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id DOMObject)
initSelector = mkSelector "init"

-- | @Selector@ for @sheet@
sheetSelector :: Selector '[] (Id DOMStyleSheet)
sheetSelector = mkSelector "sheet"

