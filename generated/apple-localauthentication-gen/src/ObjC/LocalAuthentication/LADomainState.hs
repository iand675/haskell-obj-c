{-# LANGUAGE DataKinds #-}
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
  , biometrySelector
  , companionSelector
  , initSelector
  , newSelector
  , stateHashSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.LocalAuthentication.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id LADomainState)
new  =
  do
    cls' <- getRequiredClass "LADomainState"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsLADomainState laDomainState => laDomainState -> IO (Id LADomainState)
init_ laDomainState =
  sendOwnedMessage laDomainState initSelector

-- | Contains biometric domain state.
--
-- ObjC selector: @- biometry@
biometry :: IsLADomainState laDomainState => laDomainState -> IO (Id LADomainStateBiometry)
biometry laDomainState =
  sendMessage laDomainState biometrySelector

-- | Contains companion domain state.
--
-- ObjC selector: @- companion@
companion :: IsLADomainState laDomainState => laDomainState -> IO (Id LADomainStateCompanion)
companion laDomainState =
  sendMessage laDomainState companionSelector

-- | Contains combined state hash data for biometry and companion state hashes.
--
-- Warning: Please note that the value returned by this property can change exceptionally between major OS versions even if          the list of paired companions has not changed.
--
-- ObjC selector: @- stateHash@
stateHash :: IsLADomainState laDomainState => laDomainState -> IO (Id NSData)
stateHash laDomainState =
  sendMessage laDomainState stateHashSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id LADomainState)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id LADomainState)
initSelector = mkSelector "init"

-- | @Selector@ for @biometry@
biometrySelector :: Selector '[] (Id LADomainStateBiometry)
biometrySelector = mkSelector "biometry"

-- | @Selector@ for @companion@
companionSelector :: Selector '[] (Id LADomainStateCompanion)
companionSelector = mkSelector "companion"

-- | @Selector@ for @stateHash@
stateHashSelector :: Selector '[] (Id NSData)
stateHashSelector = mkSelector "stateHash"

