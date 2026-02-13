{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROperationalStateClusterOperationalStateStruct@.
module ObjC.Matter.MTROperationalStateClusterOperationalStateStruct
  ( MTROperationalStateClusterOperationalStateStruct
  , IsMTROperationalStateClusterOperationalStateStruct(..)
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
operationalStateID :: IsMTROperationalStateClusterOperationalStateStruct mtrOperationalStateClusterOperationalStateStruct => mtrOperationalStateClusterOperationalStateStruct -> IO (Id NSNumber)
operationalStateID mtrOperationalStateClusterOperationalStateStruct =
  sendMessage mtrOperationalStateClusterOperationalStateStruct operationalStateIDSelector

-- | @- setOperationalStateID:@
setOperationalStateID :: (IsMTROperationalStateClusterOperationalStateStruct mtrOperationalStateClusterOperationalStateStruct, IsNSNumber value) => mtrOperationalStateClusterOperationalStateStruct -> value -> IO ()
setOperationalStateID mtrOperationalStateClusterOperationalStateStruct value =
  sendMessage mtrOperationalStateClusterOperationalStateStruct setOperationalStateIDSelector (toNSNumber value)

-- | @- operationalStateLabel@
operationalStateLabel :: IsMTROperationalStateClusterOperationalStateStruct mtrOperationalStateClusterOperationalStateStruct => mtrOperationalStateClusterOperationalStateStruct -> IO (Id NSString)
operationalStateLabel mtrOperationalStateClusterOperationalStateStruct =
  sendMessage mtrOperationalStateClusterOperationalStateStruct operationalStateLabelSelector

-- | @- setOperationalStateLabel:@
setOperationalStateLabel :: (IsMTROperationalStateClusterOperationalStateStruct mtrOperationalStateClusterOperationalStateStruct, IsNSString value) => mtrOperationalStateClusterOperationalStateStruct -> value -> IO ()
setOperationalStateLabel mtrOperationalStateClusterOperationalStateStruct value =
  sendMessage mtrOperationalStateClusterOperationalStateStruct setOperationalStateLabelSelector (toNSString value)

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

