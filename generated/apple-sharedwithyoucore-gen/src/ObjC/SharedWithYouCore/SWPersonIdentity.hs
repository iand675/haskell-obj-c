{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SWPersonIdentity
--
-- Represents an opaque Merkle tree where the root hash of the tree can uniquely identify the individual by all of their devices. The individual's devices can prove themselves to be part of this identity, and can then be used for cryptographic signatures for that individual.
--
-- Generated bindings for @SWPersonIdentity@.
module ObjC.SharedWithYouCore.SWPersonIdentity
  ( SWPersonIdentity
  , IsSWPersonIdentity(..)
  , init_
  , new
  , initWithRootHash
  , rootHash
  , initSelector
  , initWithRootHashSelector
  , newSelector
  , rootHashSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SharedWithYouCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSWPersonIdentity swPersonIdentity => swPersonIdentity -> IO (Id SWPersonIdentity)
init_ swPersonIdentity =
  sendOwnedMessage swPersonIdentity initSelector

-- | @+ new@
new :: IO (Id SWPersonIdentity)
new  =
  do
    cls' <- getRequiredClass "SWPersonIdentity"
    sendOwnedClassMessage cls' newSelector

-- | An initializer
--
-- @rootHash@ — The root hash of the tree that represents this individual's identity.
--
-- The data contains a SHA256 hash of the user's combined public identities.
--
-- ObjC selector: @- initWithRootHash:@
initWithRootHash :: (IsSWPersonIdentity swPersonIdentity, IsNSData rootHash) => swPersonIdentity -> rootHash -> IO (Id SWPersonIdentity)
initWithRootHash swPersonIdentity rootHash =
  sendOwnedMessage swPersonIdentity initWithRootHashSelector (toNSData rootHash)

-- | The root hash of the tree that represents this individual's identity.
--
-- The data contains a SHA256 hash of the user's combined public identities.
--
-- ObjC selector: @- rootHash@
rootHash :: IsSWPersonIdentity swPersonIdentity => swPersonIdentity -> IO (Id NSData)
rootHash swPersonIdentity =
  sendMessage swPersonIdentity rootHashSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SWPersonIdentity)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SWPersonIdentity)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithRootHash:@
initWithRootHashSelector :: Selector '[Id NSData] (Id SWPersonIdentity)
initWithRootHashSelector = mkSelector "initWithRootHash:"

-- | @Selector@ for @rootHash@
rootHashSelector :: Selector '[] (Id NSData)
rootHashSelector = mkSelector "rootHash"

