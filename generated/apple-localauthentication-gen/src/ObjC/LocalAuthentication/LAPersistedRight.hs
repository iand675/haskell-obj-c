{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A type of right that, when authorized, grants access to a key and secret
--
-- Generated bindings for @LAPersistedRight@.
module ObjC.LocalAuthentication.LAPersistedRight
  ( LAPersistedRight
  , IsLAPersistedRight(..)
  , new
  , init_
  , key
  , secret
  , initSelector
  , keySelector
  , newSelector
  , secretSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.LocalAuthentication.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Clients cannot create @LAPersistedRight@ instances directly. They can only obtain them from the @LARightStore@ .
--
-- ObjC selector: @+ new@
new :: IO (Id LAPersistedRight)
new  =
  do
    cls' <- getRequiredClass "LAPersistedRight"
    sendOwnedClassMessage cls' newSelector

-- | Clients cannot create @LAPersistedRight@ instances directly. They can only obtain them from the @LARightStore@ .
--
-- ObjC selector: @- init@
init_ :: IsLAPersistedRight laPersistedRight => laPersistedRight -> IO (Id LAPersistedRight)
init_ laPersistedRight =
  sendOwnedMessage laPersistedRight initSelector

-- | Managed private key
--
-- ObjC selector: @- key@
key :: IsLAPersistedRight laPersistedRight => laPersistedRight -> IO (Id LAPrivateKey)
key laPersistedRight =
  sendMessage laPersistedRight keySelector

-- | Generic secret
--
-- This is the generic secret that would have been stored along with the right
--
-- ObjC selector: @- secret@
secret :: IsLAPersistedRight laPersistedRight => laPersistedRight -> IO (Id LASecret)
secret laPersistedRight =
  sendMessage laPersistedRight secretSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id LAPersistedRight)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id LAPersistedRight)
initSelector = mkSelector "init"

-- | @Selector@ for @key@
keySelector :: Selector '[] (Id LAPrivateKey)
keySelector = mkSelector "key"

-- | @Selector@ for @secret@
secretSelector :: Selector '[] (Id LASecret)
secretSelector = mkSelector "secret"

