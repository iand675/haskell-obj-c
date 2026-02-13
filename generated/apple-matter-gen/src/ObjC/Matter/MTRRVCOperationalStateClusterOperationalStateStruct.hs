{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRRVCOperationalStateClusterOperationalStateStruct@.
module ObjC.Matter.MTRRVCOperationalStateClusterOperationalStateStruct
  ( MTRRVCOperationalStateClusterOperationalStateStruct
  , IsMTRRVCOperationalStateClusterOperationalStateStruct(..)
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
operationalStateID :: IsMTRRVCOperationalStateClusterOperationalStateStruct mtrrvcOperationalStateClusterOperationalStateStruct => mtrrvcOperationalStateClusterOperationalStateStruct -> IO (Id NSNumber)
operationalStateID mtrrvcOperationalStateClusterOperationalStateStruct =
  sendMessage mtrrvcOperationalStateClusterOperationalStateStruct operationalStateIDSelector

-- | @- setOperationalStateID:@
setOperationalStateID :: (IsMTRRVCOperationalStateClusterOperationalStateStruct mtrrvcOperationalStateClusterOperationalStateStruct, IsNSNumber value) => mtrrvcOperationalStateClusterOperationalStateStruct -> value -> IO ()
setOperationalStateID mtrrvcOperationalStateClusterOperationalStateStruct value =
  sendMessage mtrrvcOperationalStateClusterOperationalStateStruct setOperationalStateIDSelector (toNSNumber value)

-- | @- operationalStateLabel@
operationalStateLabel :: IsMTRRVCOperationalStateClusterOperationalStateStruct mtrrvcOperationalStateClusterOperationalStateStruct => mtrrvcOperationalStateClusterOperationalStateStruct -> IO (Id NSString)
operationalStateLabel mtrrvcOperationalStateClusterOperationalStateStruct =
  sendMessage mtrrvcOperationalStateClusterOperationalStateStruct operationalStateLabelSelector

-- | @- setOperationalStateLabel:@
setOperationalStateLabel :: (IsMTRRVCOperationalStateClusterOperationalStateStruct mtrrvcOperationalStateClusterOperationalStateStruct, IsNSString value) => mtrrvcOperationalStateClusterOperationalStateStruct -> value -> IO ()
setOperationalStateLabel mtrrvcOperationalStateClusterOperationalStateStruct value =
  sendMessage mtrrvcOperationalStateClusterOperationalStateStruct setOperationalStateLabelSelector (toNSString value)

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

