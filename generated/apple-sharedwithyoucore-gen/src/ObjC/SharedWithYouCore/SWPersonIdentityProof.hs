{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SWPersonIdentityProof
--
-- Represents an opaque Merkle tree proof of inclusion. Inclusion hashes are provided to verify that the individual device has access to the document.
--
-- Generated bindings for @SWPersonIdentityProof@.
module ObjC.SharedWithYouCore.SWPersonIdentityProof
  ( SWPersonIdentityProof
  , IsSWPersonIdentityProof(..)
  , init_
  , new
  , inclusionHashes
  , publicKey
  , publicKeyIndex
  , inclusionHashesSelector
  , initSelector
  , newSelector
  , publicKeyIndexSelector
  , publicKeySelector


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
init_ :: IsSWPersonIdentityProof swPersonIdentityProof => swPersonIdentityProof -> IO (Id SWPersonIdentityProof)
init_ swPersonIdentityProof =
  sendOwnedMessage swPersonIdentityProof initSelector

-- | @+ new@
new :: IO (Id SWPersonIdentityProof)
new  =
  do
    cls' <- getRequiredClass "SWPersonIdentityProof"
    sendOwnedClassMessage cls' newSelector

-- | Hashes of missing Merkle tree nodes that can provide proof of inclusion.
--
-- The data contains an array of SHA256 hash of the user's combined public identities.
--
-- ObjC selector: @- inclusionHashes@
inclusionHashes :: IsSWPersonIdentityProof swPersonIdentityProof => swPersonIdentityProof -> IO (Id NSArray)
inclusionHashes swPersonIdentityProof =
  sendMessage swPersonIdentityProof inclusionHashesSelector

-- | Public key of local device
--
-- ObjC selector: @- publicKey@
publicKey :: IsSWPersonIdentityProof swPersonIdentityProof => swPersonIdentityProof -> IO (Id NSData)
publicKey swPersonIdentityProof =
  sendMessage swPersonIdentityProof publicKeySelector

-- | Index of local public key in the Merkle tree
--
-- This data can be used to determine if the node is the left or the right child
--
-- ObjC selector: @- publicKeyIndex@
publicKeyIndex :: IsSWPersonIdentityProof swPersonIdentityProof => swPersonIdentityProof -> IO CULong
publicKeyIndex swPersonIdentityProof =
  sendMessage swPersonIdentityProof publicKeyIndexSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SWPersonIdentityProof)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SWPersonIdentityProof)
newSelector = mkSelector "new"

-- | @Selector@ for @inclusionHashes@
inclusionHashesSelector :: Selector '[] (Id NSArray)
inclusionHashesSelector = mkSelector "inclusionHashes"

-- | @Selector@ for @publicKey@
publicKeySelector :: Selector '[] (Id NSData)
publicKeySelector = mkSelector "publicKey"

-- | @Selector@ for @publicKeyIndex@
publicKeyIndexSelector :: Selector '[] CULong
publicKeyIndexSelector = mkSelector "publicKeyIndex"

