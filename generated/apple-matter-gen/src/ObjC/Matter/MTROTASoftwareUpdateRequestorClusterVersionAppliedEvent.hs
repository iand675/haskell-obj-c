{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROTASoftwareUpdateRequestorClusterVersionAppliedEvent@.
module ObjC.Matter.MTROTASoftwareUpdateRequestorClusterVersionAppliedEvent
  ( MTROTASoftwareUpdateRequestorClusterVersionAppliedEvent
  , IsMTROTASoftwareUpdateRequestorClusterVersionAppliedEvent(..)
  , softwareVersion
  , setSoftwareVersion
  , productID
  , setProductID
  , productIDSelector
  , setProductIDSelector
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
softwareVersion :: IsMTROTASoftwareUpdateRequestorClusterVersionAppliedEvent mtrotaSoftwareUpdateRequestorClusterVersionAppliedEvent => mtrotaSoftwareUpdateRequestorClusterVersionAppliedEvent -> IO (Id NSNumber)
softwareVersion mtrotaSoftwareUpdateRequestorClusterVersionAppliedEvent =
  sendMessage mtrotaSoftwareUpdateRequestorClusterVersionAppliedEvent softwareVersionSelector

-- | @- setSoftwareVersion:@
setSoftwareVersion :: (IsMTROTASoftwareUpdateRequestorClusterVersionAppliedEvent mtrotaSoftwareUpdateRequestorClusterVersionAppliedEvent, IsNSNumber value) => mtrotaSoftwareUpdateRequestorClusterVersionAppliedEvent -> value -> IO ()
setSoftwareVersion mtrotaSoftwareUpdateRequestorClusterVersionAppliedEvent value =
  sendMessage mtrotaSoftwareUpdateRequestorClusterVersionAppliedEvent setSoftwareVersionSelector (toNSNumber value)

-- | @- productID@
productID :: IsMTROTASoftwareUpdateRequestorClusterVersionAppliedEvent mtrotaSoftwareUpdateRequestorClusterVersionAppliedEvent => mtrotaSoftwareUpdateRequestorClusterVersionAppliedEvent -> IO (Id NSNumber)
productID mtrotaSoftwareUpdateRequestorClusterVersionAppliedEvent =
  sendMessage mtrotaSoftwareUpdateRequestorClusterVersionAppliedEvent productIDSelector

-- | @- setProductID:@
setProductID :: (IsMTROTASoftwareUpdateRequestorClusterVersionAppliedEvent mtrotaSoftwareUpdateRequestorClusterVersionAppliedEvent, IsNSNumber value) => mtrotaSoftwareUpdateRequestorClusterVersionAppliedEvent -> value -> IO ()
setProductID mtrotaSoftwareUpdateRequestorClusterVersionAppliedEvent value =
  sendMessage mtrotaSoftwareUpdateRequestorClusterVersionAppliedEvent setProductIDSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @softwareVersion@
softwareVersionSelector :: Selector '[] (Id NSNumber)
softwareVersionSelector = mkSelector "softwareVersion"

-- | @Selector@ for @setSoftwareVersion:@
setSoftwareVersionSelector :: Selector '[Id NSNumber] ()
setSoftwareVersionSelector = mkSelector "setSoftwareVersion:"

-- | @Selector@ for @productID@
productIDSelector :: Selector '[] (Id NSNumber)
productIDSelector = mkSelector "productID"

-- | @Selector@ for @setProductID:@
setProductIDSelector :: Selector '[Id NSNumber] ()
setProductIDSelector = mkSelector "setProductID:"

