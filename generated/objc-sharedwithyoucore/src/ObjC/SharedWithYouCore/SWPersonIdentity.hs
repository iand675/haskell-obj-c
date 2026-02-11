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
  , newSelector
  , initWithRootHashSelector
  , rootHashSelector


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
init_ :: IsSWPersonIdentity swPersonIdentity => swPersonIdentity -> IO (Id SWPersonIdentity)
init_ swPersonIdentity  =
  sendMsg swPersonIdentity (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SWPersonIdentity)
new  =
  do
    cls' <- getRequiredClass "SWPersonIdentity"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | An initializer
--
-- @rootHash@ — The root hash of the tree that represents this individual's identity.
--
-- The data contains a SHA256 hash of the user's combined public identities.
--
-- ObjC selector: @- initWithRootHash:@
initWithRootHash :: (IsSWPersonIdentity swPersonIdentity, IsNSData rootHash) => swPersonIdentity -> rootHash -> IO (Id SWPersonIdentity)
initWithRootHash swPersonIdentity  rootHash =
withObjCPtr rootHash $ \raw_rootHash ->
    sendMsg swPersonIdentity (mkSelector "initWithRootHash:") (retPtr retVoid) [argPtr (castPtr raw_rootHash :: Ptr ())] >>= ownedObject . castPtr

-- | The root hash of the tree that represents this individual's identity.
--
-- The data contains a SHA256 hash of the user's combined public identities.
--
-- ObjC selector: @- rootHash@
rootHash :: IsSWPersonIdentity swPersonIdentity => swPersonIdentity -> IO (Id NSData)
rootHash swPersonIdentity  =
  sendMsg swPersonIdentity (mkSelector "rootHash") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithRootHash:@
initWithRootHashSelector :: Selector
initWithRootHashSelector = mkSelector "initWithRootHash:"

-- | @Selector@ for @rootHash@
rootHashSelector :: Selector
rootHashSelector = mkSelector "rootHash"

