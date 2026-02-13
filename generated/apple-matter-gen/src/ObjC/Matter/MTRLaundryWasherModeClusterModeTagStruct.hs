{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRLaundryWasherModeClusterModeTagStruct@.
module ObjC.Matter.MTRLaundryWasherModeClusterModeTagStruct
  ( MTRLaundryWasherModeClusterModeTagStruct
  , IsMTRLaundryWasherModeClusterModeTagStruct(..)
  , mfgCode
  , setMfgCode
  , value
  , setValue
  , mfgCodeSelector
  , setMfgCodeSelector
  , setValueSelector
  , valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- mfgCode@
mfgCode :: IsMTRLaundryWasherModeClusterModeTagStruct mtrLaundryWasherModeClusterModeTagStruct => mtrLaundryWasherModeClusterModeTagStruct -> IO (Id NSNumber)
mfgCode mtrLaundryWasherModeClusterModeTagStruct =
  sendMessage mtrLaundryWasherModeClusterModeTagStruct mfgCodeSelector

-- | @- setMfgCode:@
setMfgCode :: (IsMTRLaundryWasherModeClusterModeTagStruct mtrLaundryWasherModeClusterModeTagStruct, IsNSNumber value) => mtrLaundryWasherModeClusterModeTagStruct -> value -> IO ()
setMfgCode mtrLaundryWasherModeClusterModeTagStruct value =
  sendMessage mtrLaundryWasherModeClusterModeTagStruct setMfgCodeSelector (toNSNumber value)

-- | @- value@
value :: IsMTRLaundryWasherModeClusterModeTagStruct mtrLaundryWasherModeClusterModeTagStruct => mtrLaundryWasherModeClusterModeTagStruct -> IO (Id NSNumber)
value mtrLaundryWasherModeClusterModeTagStruct =
  sendMessage mtrLaundryWasherModeClusterModeTagStruct valueSelector

-- | @- setValue:@
setValue :: (IsMTRLaundryWasherModeClusterModeTagStruct mtrLaundryWasherModeClusterModeTagStruct, IsNSNumber value) => mtrLaundryWasherModeClusterModeTagStruct -> value -> IO ()
setValue mtrLaundryWasherModeClusterModeTagStruct value =
  sendMessage mtrLaundryWasherModeClusterModeTagStruct setValueSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mfgCode@
mfgCodeSelector :: Selector '[] (Id NSNumber)
mfgCodeSelector = mkSelector "mfgCode"

-- | @Selector@ for @setMfgCode:@
setMfgCodeSelector :: Selector '[Id NSNumber] ()
setMfgCodeSelector = mkSelector "setMfgCode:"

-- | @Selector@ for @value@
valueSelector :: Selector '[] (Id NSNumber)
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector '[Id NSNumber] ()
setValueSelector = mkSelector "setValue:"

