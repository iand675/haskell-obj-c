{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CLRegion@.
module ObjC.CoreLocation.CLRegion
  ( CLRegion
  , IsCLRegion(..)
  , radius
  , identifier
  , notifyOnEntry
  , setNotifyOnEntry
  , notifyOnExit
  , setNotifyOnExit
  , identifierSelector
  , notifyOnEntrySelector
  , notifyOnExitSelector
  , radiusSelector
  , setNotifyOnEntrySelector
  , setNotifyOnExitSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- radius@
radius :: IsCLRegion clRegion => clRegion -> IO CDouble
radius clRegion =
  sendMessage clRegion radiusSelector

-- | @- identifier@
identifier :: IsCLRegion clRegion => clRegion -> IO (Id NSString)
identifier clRegion =
  sendMessage clRegion identifierSelector

-- | @- notifyOnEntry@
notifyOnEntry :: IsCLRegion clRegion => clRegion -> IO Bool
notifyOnEntry clRegion =
  sendMessage clRegion notifyOnEntrySelector

-- | @- setNotifyOnEntry:@
setNotifyOnEntry :: IsCLRegion clRegion => clRegion -> Bool -> IO ()
setNotifyOnEntry clRegion value =
  sendMessage clRegion setNotifyOnEntrySelector value

-- | @- notifyOnExit@
notifyOnExit :: IsCLRegion clRegion => clRegion -> IO Bool
notifyOnExit clRegion =
  sendMessage clRegion notifyOnExitSelector

-- | @- setNotifyOnExit:@
setNotifyOnExit :: IsCLRegion clRegion => clRegion -> Bool -> IO ()
setNotifyOnExit clRegion value =
  sendMessage clRegion setNotifyOnExitSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @radius@
radiusSelector :: Selector '[] CDouble
radiusSelector = mkSelector "radius"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @notifyOnEntry@
notifyOnEntrySelector :: Selector '[] Bool
notifyOnEntrySelector = mkSelector "notifyOnEntry"

-- | @Selector@ for @setNotifyOnEntry:@
setNotifyOnEntrySelector :: Selector '[Bool] ()
setNotifyOnEntrySelector = mkSelector "setNotifyOnEntry:"

-- | @Selector@ for @notifyOnExit@
notifyOnExitSelector :: Selector '[] Bool
notifyOnExitSelector = mkSelector "notifyOnExit"

-- | @Selector@ for @setNotifyOnExit:@
setNotifyOnExitSelector :: Selector '[Bool] ()
setNotifyOnExitSelector = mkSelector "setNotifyOnExit:"

