{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROvenCavityOperationalStateClusterOperationalStateStruct@.
module ObjC.Matter.MTROvenCavityOperationalStateClusterOperationalStateStruct
  ( MTROvenCavityOperationalStateClusterOperationalStateStruct
  , IsMTROvenCavityOperationalStateClusterOperationalStateStruct(..)
  , operationalStateID
  , setOperationalStateID
  , operationalStateLabel
  , setOperationalStateLabel
  , operationalStateIDSelector
  , operationalStateLabelSelector
  , setOperationalStateIDSelector
  , setOperationalStateLabelSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- operationalStateID@
operationalStateID :: IsMTROvenCavityOperationalStateClusterOperationalStateStruct mtrOvenCavityOperationalStateClusterOperationalStateStruct => mtrOvenCavityOperationalStateClusterOperationalStateStruct -> IO (Id NSNumber)
operationalStateID mtrOvenCavityOperationalStateClusterOperationalStateStruct =
  sendMessage mtrOvenCavityOperationalStateClusterOperationalStateStruct operationalStateIDSelector

-- | @- setOperationalStateID:@
setOperationalStateID :: (IsMTROvenCavityOperationalStateClusterOperationalStateStruct mtrOvenCavityOperationalStateClusterOperationalStateStruct, IsNSNumber value) => mtrOvenCavityOperationalStateClusterOperationalStateStruct -> value -> IO ()
setOperationalStateID mtrOvenCavityOperationalStateClusterOperationalStateStruct value =
  sendMessage mtrOvenCavityOperationalStateClusterOperationalStateStruct setOperationalStateIDSelector (toNSNumber value)

-- | @- operationalStateLabel@
operationalStateLabel :: IsMTROvenCavityOperationalStateClusterOperationalStateStruct mtrOvenCavityOperationalStateClusterOperationalStateStruct => mtrOvenCavityOperationalStateClusterOperationalStateStruct -> IO (Id NSString)
operationalStateLabel mtrOvenCavityOperationalStateClusterOperationalStateStruct =
  sendMessage mtrOvenCavityOperationalStateClusterOperationalStateStruct operationalStateLabelSelector

-- | @- setOperationalStateLabel:@
setOperationalStateLabel :: (IsMTROvenCavityOperationalStateClusterOperationalStateStruct mtrOvenCavityOperationalStateClusterOperationalStateStruct, IsNSString value) => mtrOvenCavityOperationalStateClusterOperationalStateStruct -> value -> IO ()
setOperationalStateLabel mtrOvenCavityOperationalStateClusterOperationalStateStruct value =
  sendMessage mtrOvenCavityOperationalStateClusterOperationalStateStruct setOperationalStateLabelSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @operationalStateID@
operationalStateIDSelector :: Selector '[] (Id NSNumber)
operationalStateIDSelector = mkSelector "operationalStateID"

-- | @Selector@ for @setOperationalStateID:@
setOperationalStateIDSelector :: Selector '[Id NSNumber] ()
setOperationalStateIDSelector = mkSelector "setOperationalStateID:"

-- | @Selector@ for @operationalStateLabel@
operationalStateLabelSelector :: Selector '[] (Id NSString)
operationalStateLabelSelector = mkSelector "operationalStateLabel"

-- | @Selector@ for @setOperationalStateLabel:@
setOperationalStateLabelSelector :: Selector '[Id NSString] ()
setOperationalStateLabelSelector = mkSelector "setOperationalStateLabel:"

