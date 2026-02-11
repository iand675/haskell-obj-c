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
  , newSelector
  , initSelector
  , keySelector
  , secretSelector


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

import ObjC.LocalAuthentication.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Clients cannot create @LAPersistedRight@ instances directly. They can only obtain them from the @LARightStore@ .
--
-- ObjC selector: @+ new@
new :: IO (Id LAPersistedRight)
new  =
  do
    cls' <- getRequiredClass "LAPersistedRight"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Clients cannot create @LAPersistedRight@ instances directly. They can only obtain them from the @LARightStore@ .
--
-- ObjC selector: @- init@
init_ :: IsLAPersistedRight laPersistedRight => laPersistedRight -> IO (Id LAPersistedRight)
init_ laPersistedRight  =
  sendMsg laPersistedRight (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Managed private key
--
-- ObjC selector: @- key@
key :: IsLAPersistedRight laPersistedRight => laPersistedRight -> IO (Id LAPrivateKey)
key laPersistedRight  =
  sendMsg laPersistedRight (mkSelector "key") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Generic secret
--
-- This is the generic secret that would have been stored along with the right
--
-- ObjC selector: @- secret@
secret :: IsLAPersistedRight laPersistedRight => laPersistedRight -> IO (Id LASecret)
secret laPersistedRight  =
  sendMsg laPersistedRight (mkSelector "secret") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @key@
keySelector :: Selector
keySelector = mkSelector "key"

-- | @Selector@ for @secret@
secretSelector :: Selector
secretSelector = mkSelector "secret"

