{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRBasicInformationClusterStartUpEvent@.
module ObjC.Matter.MTRBasicInformationClusterStartUpEvent
  ( MTRBasicInformationClusterStartUpEvent
  , IsMTRBasicInformationClusterStartUpEvent(..)
  , softwareVersion
  , setSoftwareVersion
  , setSoftwareVersionSelector
  , softwareVersionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- softwareVersion@
softwareVersion :: IsMTRBasicInformationClusterStartUpEvent mtrBasicInformationClusterStartUpEvent => mtrBasicInformationClusterStartUpEvent -> IO (Id NSNumber)
softwareVersion mtrBasicInformationClusterStartUpEvent =
  sendMessage mtrBasicInformationClusterStartUpEvent softwareVersionSelector

-- | @- setSoftwareVersion:@
setSoftwareVersion :: (IsMTRBasicInformationClusterStartUpEvent mtrBasicInformationClusterStartUpEvent, IsNSNumber value) => mtrBasicInformationClusterStartUpEvent -> value -> IO ()
setSoftwareVersion mtrBasicInformationClusterStartUpEvent value =
  sendMessage mtrBasicInformationClusterStartUpEvent setSoftwareVersionSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @softwareVersion@
softwareVersionSelector :: Selector '[] (Id NSNumber)
softwareVersionSelector = mkSelector "softwareVersion"

-- | @Selector@ for @setSoftwareVersion:@
setSoftwareVersionSelector :: Selector '[Id NSNumber] ()
setSoftwareVersionSelector = mkSelector "setSoftwareVersion:"

