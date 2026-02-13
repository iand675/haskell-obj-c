{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @LADomainStateCompanion@.
module ObjC.LocalAuthentication.LADomainStateCompanion
  ( LADomainStateCompanion
  , IsLADomainStateCompanion(..)
  , stateHashForCompanionType
  , new
  , init_
  , availableCompanionTypes
  , stateHash
  , availableCompanionTypesSelector
  , initSelector
  , newSelector
  , stateHashForCompanionTypeSelector
  , stateHashSelector

  -- * Enum types
  , LACompanionType(LACompanionType)
  , pattern LACompanionTypeWatch
  , pattern LACompanionTypeMac
  , pattern LACompanionTypeVision

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.LocalAuthentication.Internal.Classes
import ObjC.LocalAuthentication.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Returns state hash data for the given companion type.
--
-- If database of paired devices of the given type was modified state hash              data will change. Nature of such database changes cannot be determined              but comparing data of state hash after different policy evaluation              will reveal the fact database was changed between calls.
--
-- @companionType@ — The companion type for which state hash data should be returned.
--
-- ObjC selector: @- stateHashForCompanionType:@
stateHashForCompanionType :: IsLADomainStateCompanion laDomainStateCompanion => laDomainStateCompanion -> LACompanionType -> IO (Id NSData)
stateHashForCompanionType laDomainStateCompanion companionType =
  sendMessage laDomainStateCompanion stateHashForCompanionTypeSelector companionType

-- | @+ new@
new :: IO (Id LADomainStateCompanion)
new  =
  do
    cls' <- getRequiredClass "LADomainStateCompanion"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsLADomainStateCompanion laDomainStateCompanion => laDomainStateCompanion -> IO (Id LADomainStateCompanion)
init_ laDomainStateCompanion =
  sendOwnedMessage laDomainStateCompanion initSelector

-- | Indicates types of companions paired with the device. The elements are NSNumber-wrapped instances of @`LACompanionType`.@
--
-- ObjC selector: @- availableCompanionTypes@
availableCompanionTypes :: IsLADomainStateCompanion laDomainStateCompanion => laDomainStateCompanion -> IO (Id NSSet)
availableCompanionTypes laDomainStateCompanion =
  sendMessage laDomainStateCompanion availableCompanionTypesSelector

-- | Contains combined state hash data for all available companion types. . Returns @nil@ if no companion devices are paired.
--
-- As long as database of paired companion devices doesn't change,              @stateHash@ stays the same for the same set of @availableCompanions@.
--
-- If database of paired companion devices was modified, @stateHash@              data will change. Nature of such database changes cannot be determined              but comparing data of @stateHash@ after different policy evaluation              will reveal the fact database was changed between calls.
--
-- If you are interested in a state hash for a specific companion type              you can use @stateHashForCompanionType@ method.
--
-- Warning: Please note that the value returned by this property can change exceptionally between major OS versions even if          the list of paired companions has not changed.
--
-- ObjC selector: @- stateHash@
stateHash :: IsLADomainStateCompanion laDomainStateCompanion => laDomainStateCompanion -> IO (Id NSData)
stateHash laDomainStateCompanion =
  sendMessage laDomainStateCompanion stateHashSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stateHashForCompanionType:@
stateHashForCompanionTypeSelector :: Selector '[LACompanionType] (Id NSData)
stateHashForCompanionTypeSelector = mkSelector "stateHashForCompanionType:"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id LADomainStateCompanion)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id LADomainStateCompanion)
initSelector = mkSelector "init"

-- | @Selector@ for @availableCompanionTypes@
availableCompanionTypesSelector :: Selector '[] (Id NSSet)
availableCompanionTypesSelector = mkSelector "availableCompanionTypes"

-- | @Selector@ for @stateHash@
stateHashSelector :: Selector '[] (Id NSData)
stateHashSelector = mkSelector "stateHash"

