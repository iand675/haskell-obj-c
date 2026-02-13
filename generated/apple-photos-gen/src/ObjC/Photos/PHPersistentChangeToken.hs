{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHPersistentChangeToken@.
module ObjC.Photos.PHPersistentChangeToken
  ( PHPersistentChangeToken
  , IsPHPersistentChangeToken(..)
  , new
  , init_
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Photos.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id PHPersistentChangeToken)
new  =
  do
    cls' <- getRequiredClass "PHPersistentChangeToken"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsPHPersistentChangeToken phPersistentChangeToken => phPersistentChangeToken -> IO (Id PHPersistentChangeToken)
init_ phPersistentChangeToken =
  sendOwnedMessage phPersistentChangeToken initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHPersistentChangeToken)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHPersistentChangeToken)
initSelector = mkSelector "init"

