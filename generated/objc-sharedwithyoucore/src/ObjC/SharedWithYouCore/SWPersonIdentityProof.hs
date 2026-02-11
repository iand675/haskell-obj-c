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
  , initSelector
  , newSelector
  , inclusionHashesSelector
  , publicKeySelector
  , publicKeyIndexSelector


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

import ObjC.SharedWithYouCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsSWPersonIdentityProof swPersonIdentityProof => swPersonIdentityProof -> IO (Id SWPersonIdentityProof)
init_ swPersonIdentityProof  =
  sendMsg swPersonIdentityProof (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SWPersonIdentityProof)
new  =
  do
    cls' <- getRequiredClass "SWPersonIdentityProof"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Hashes of missing Merkle tree nodes that can provide proof of inclusion.
--
-- The data contains an array of SHA256 hash of the user's combined public identities.
--
-- ObjC selector: @- inclusionHashes@
inclusionHashes :: IsSWPersonIdentityProof swPersonIdentityProof => swPersonIdentityProof -> IO (Id NSArray)
inclusionHashes swPersonIdentityProof  =
  sendMsg swPersonIdentityProof (mkSelector "inclusionHashes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Public key of local device
--
-- ObjC selector: @- publicKey@
publicKey :: IsSWPersonIdentityProof swPersonIdentityProof => swPersonIdentityProof -> IO (Id NSData)
publicKey swPersonIdentityProof  =
  sendMsg swPersonIdentityProof (mkSelector "publicKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Index of local public key in the Merkle tree
--
-- This data can be used to determine if the node is the left or the right child
--
-- ObjC selector: @- publicKeyIndex@
publicKeyIndex :: IsSWPersonIdentityProof swPersonIdentityProof => swPersonIdentityProof -> IO CULong
publicKeyIndex swPersonIdentityProof  =
  sendMsg swPersonIdentityProof (mkSelector "publicKeyIndex") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @inclusionHashes@
inclusionHashesSelector :: Selector
inclusionHashesSelector = mkSelector "inclusionHashes"

-- | @Selector@ for @publicKey@
publicKeySelector :: Selector
publicKeySelector = mkSelector "publicKey"

-- | @Selector@ for @publicKeyIndex@
publicKeyIndexSelector :: Selector
publicKeyIndexSelector = mkSelector "publicKeyIndex"

