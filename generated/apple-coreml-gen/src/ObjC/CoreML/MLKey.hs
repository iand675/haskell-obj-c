{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class representing key used to store any value against
--
-- Generated bindings for @MLKey@.
module ObjC.CoreML.MLKey
  ( MLKey
  , IsMLKey(..)
  , init_
  , new
  , name
  , scope
  , initSelector
  , nameSelector
  , newSelector
  , scopeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMLKey mlKey => mlKey -> IO (Id MLKey)
init_ mlKey =
  sendOwnedMessage mlKey initSelector

-- | @+ new@
new :: IO RawId
new  =
  do
    cls' <- getRequiredClass "MLKey"
    sendOwnedClassMessage cls' newSelector

-- | @- name@
name :: IsMLKey mlKey => mlKey -> IO (Id NSString)
name mlKey =
  sendMessage mlKey nameSelector

-- | @- scope@
scope :: IsMLKey mlKey => mlKey -> IO (Id NSString)
scope mlKey =
  sendMessage mlKey scopeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLKey)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] RawId
newSelector = mkSelector "new"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @scope@
scopeSelector :: Selector '[] (Id NSString)
scopeSelector = mkSelector "scope"

