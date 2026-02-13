{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A collection of key-value observations which may be registered with multiple observable objects. Create using ``-[NSKeyValueSharedObservers snapshot]``
--
-- Generated bindings for @NSKeyValueSharedObserversSnapshot@.
module ObjC.Foundation.NSKeyValueSharedObserversSnapshot
  ( NSKeyValueSharedObserversSnapshot
  , IsNSKeyValueSharedObserversSnapshot(..)
  , init_
  , new
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSKeyValueSharedObserversSnapshot nsKeyValueSharedObserversSnapshot => nsKeyValueSharedObserversSnapshot -> IO RawId
init_ nsKeyValueSharedObserversSnapshot =
  sendOwnedMessage nsKeyValueSharedObserversSnapshot initSelector

-- | @+ new@
new :: IO RawId
new  =
  do
    cls' <- getRequiredClass "NSKeyValueSharedObserversSnapshot"
    sendOwnedClassMessage cls' newSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] RawId
newSelector = mkSelector "new"

