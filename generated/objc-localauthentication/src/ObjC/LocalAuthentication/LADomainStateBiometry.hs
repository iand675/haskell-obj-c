{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @LADomainStateBiometry@.
module ObjC.LocalAuthentication.LADomainStateBiometry
  ( LADomainStateBiometry
  , IsLADomainStateBiometry(..)
  , new
  , init_
  , biometryType
  , stateHash
  , newSelector
  , initSelector
  , biometryTypeSelector
  , stateHashSelector

  -- * Enum types
  , LABiometryType(LABiometryType)
  , pattern LABiometryTypeNone
  , pattern LABiometryNone
  , pattern LABiometryTypeTouchID
  , pattern LABiometryTypeFaceID
  , pattern LABiometryTypeOpticID

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
import ObjC.LocalAuthentication.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id LADomainStateBiometry)
new  =
  do
    cls' <- getRequiredClass "LADomainStateBiometry"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsLADomainStateBiometry laDomainStateBiometry => laDomainStateBiometry -> IO (Id LADomainStateBiometry)
init_ laDomainStateBiometry  =
  sendMsg laDomainStateBiometry (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Indicates biometry type available on the device.
--
-- ObjC selector: @- biometryType@
biometryType :: IsLADomainStateBiometry laDomainStateBiometry => laDomainStateBiometry -> IO LABiometryType
biometryType laDomainStateBiometry  =
  fmap (coerce :: CLong -> LABiometryType) $ sendMsg laDomainStateBiometry (mkSelector "biometryType") retCLong []

-- | Contains state hash data for the available biometry type. Returns @nil@ if no biometry entities are enrolled.
--
-- If biometric database was modified (fingers, faces were removed or added), @stateHash@              data will change. Nature of such database changes cannot be determined              but comparing data of @stateHash@ after different evaluatePolicy calls              will reveal the fact database was changed between the calls.
--
-- Warning: Please note that the value returned by this property can change exceptionally between major OS versions even if          the state of biometry has not changed.
--
-- ObjC selector: @- stateHash@
stateHash :: IsLADomainStateBiometry laDomainStateBiometry => laDomainStateBiometry -> IO (Id NSData)
stateHash laDomainStateBiometry  =
  sendMsg laDomainStateBiometry (mkSelector "stateHash") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @biometryType@
biometryTypeSelector :: Selector
biometryTypeSelector = mkSelector "biometryType"

-- | @Selector@ for @stateHash@
stateHashSelector :: Selector
stateHashSelector = mkSelector "stateHash"

