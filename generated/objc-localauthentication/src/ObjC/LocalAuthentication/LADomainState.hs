{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @LADomainState@.
module ObjC.LocalAuthentication.LADomainState
  ( LADomainState
  , IsLADomainState(..)
  , new
  , init_
  , biometry
  , companion
  , stateHash
  , newSelector
  , initSelector
  , biometrySelector
  , companionSelector
  , stateHashSelector


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

-- | @+ new@
new :: IO (Id LADomainState)
new  =
  do
    cls' <- getRequiredClass "LADomainState"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsLADomainState laDomainState => laDomainState -> IO (Id LADomainState)
init_ laDomainState  =
  sendMsg laDomainState (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Contains biometric domain state.
--
-- ObjC selector: @- biometry@
biometry :: IsLADomainState laDomainState => laDomainState -> IO (Id LADomainStateBiometry)
biometry laDomainState  =
  sendMsg laDomainState (mkSelector "biometry") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Contains companion domain state.
--
-- ObjC selector: @- companion@
companion :: IsLADomainState laDomainState => laDomainState -> IO (Id LADomainStateCompanion)
companion laDomainState  =
  sendMsg laDomainState (mkSelector "companion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Contains combined state hash data for biometry and companion state hashes.
--
-- Warning: Please note that the value returned by this property can change exceptionally between major OS versions even if          the list of paired companions has not changed.
--
-- ObjC selector: @- stateHash@
stateHash :: IsLADomainState laDomainState => laDomainState -> IO (Id NSData)
stateHash laDomainState  =
  sendMsg laDomainState (mkSelector "stateHash") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @biometry@
biometrySelector :: Selector
biometrySelector = mkSelector "biometry"

-- | @Selector@ for @companion@
companionSelector :: Selector
companionSelector = mkSelector "companion"

-- | @Selector@ for @stateHash@
stateHashSelector :: Selector
stateHashSelector = mkSelector "stateHash"

