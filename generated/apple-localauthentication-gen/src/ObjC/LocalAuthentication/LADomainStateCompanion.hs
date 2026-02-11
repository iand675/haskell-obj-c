{-# LANGUAGE PatternSynonyms #-}
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
  , stateHashForCompanionTypeSelector
  , newSelector
  , initSelector
  , availableCompanionTypesSelector
  , stateHashSelector

  -- * Enum types
  , LACompanionType(LACompanionType)
  , pattern LACompanionTypeWatch
  , pattern LACompanionTypeMac
  , pattern LACompanionTypeVision

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

-- | Returns state hash data for the given companion type.
--
-- If database of paired devices of the given type was modified state hash              data will change. Nature of such database changes cannot be determined              but comparing data of state hash after different policy evaluation              will reveal the fact database was changed between calls.
--
-- @companionType@ — The companion type for which state hash data should be returned.
--
-- ObjC selector: @- stateHashForCompanionType:@
stateHashForCompanionType :: IsLADomainStateCompanion laDomainStateCompanion => laDomainStateCompanion -> LACompanionType -> IO (Id NSData)
stateHashForCompanionType laDomainStateCompanion  companionType =
    sendMsg laDomainStateCompanion (mkSelector "stateHashForCompanionType:") (retPtr retVoid) [argCLong (coerce companionType)] >>= retainedObject . castPtr

-- | @+ new@
new :: IO (Id LADomainStateCompanion)
new  =
  do
    cls' <- getRequiredClass "LADomainStateCompanion"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsLADomainStateCompanion laDomainStateCompanion => laDomainStateCompanion -> IO (Id LADomainStateCompanion)
init_ laDomainStateCompanion  =
    sendMsg laDomainStateCompanion (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Indicates types of companions paired with the device. The elements are NSNumber-wrapped instances of @`LACompanionType`.@
--
-- ObjC selector: @- availableCompanionTypes@
availableCompanionTypes :: IsLADomainStateCompanion laDomainStateCompanion => laDomainStateCompanion -> IO (Id NSSet)
availableCompanionTypes laDomainStateCompanion  =
    sendMsg laDomainStateCompanion (mkSelector "availableCompanionTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

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
stateHash laDomainStateCompanion  =
    sendMsg laDomainStateCompanion (mkSelector "stateHash") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stateHashForCompanionType:@
stateHashForCompanionTypeSelector :: Selector
stateHashForCompanionTypeSelector = mkSelector "stateHashForCompanionType:"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @availableCompanionTypes@
availableCompanionTypesSelector :: Selector
availableCompanionTypesSelector = mkSelector "availableCompanionTypes"

-- | @Selector@ for @stateHash@
stateHashSelector :: Selector
stateHashSelector = mkSelector "stateHash"

