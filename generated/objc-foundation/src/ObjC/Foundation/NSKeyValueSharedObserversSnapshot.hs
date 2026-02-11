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

-- | @- init@
init_ :: IsNSKeyValueSharedObserversSnapshot nsKeyValueSharedObserversSnapshot => nsKeyValueSharedObserversSnapshot -> IO RawId
init_ nsKeyValueSharedObserversSnapshot  =
  fmap (RawId . castPtr) $ sendMsg nsKeyValueSharedObserversSnapshot (mkSelector "init") (retPtr retVoid) []

-- | @+ new@
new :: IO RawId
new  =
  do
    cls' <- getRequiredClass "NSKeyValueSharedObserversSnapshot"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "new") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

