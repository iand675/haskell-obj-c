{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CLLocationSourceInformation@.
module ObjC.CoreLocation.CLLocationSourceInformation
  ( CLLocationSourceInformation
  , IsCLLocationSourceInformation(..)
  , initWithSoftwareSimulationState_andExternalAccessoryState
  , isSimulatedBySoftware
  , isProducedByAccessory
  , initWithSoftwareSimulationState_andExternalAccessoryStateSelector
  , isSimulatedBySoftwareSelector
  , isProducedByAccessorySelector


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

import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithSoftwareSimulationState:andExternalAccessoryState:@
initWithSoftwareSimulationState_andExternalAccessoryState :: IsCLLocationSourceInformation clLocationSourceInformation => clLocationSourceInformation -> Bool -> Bool -> IO (Id CLLocationSourceInformation)
initWithSoftwareSimulationState_andExternalAccessoryState clLocationSourceInformation  isSoftware isAccessory =
  sendMsg clLocationSourceInformation (mkSelector "initWithSoftwareSimulationState:andExternalAccessoryState:") (retPtr retVoid) [argCULong (if isSoftware then 1 else 0), argCULong (if isAccessory then 1 else 0)] >>= ownedObject . castPtr

-- | @- isSimulatedBySoftware@
isSimulatedBySoftware :: IsCLLocationSourceInformation clLocationSourceInformation => clLocationSourceInformation -> IO Bool
isSimulatedBySoftware clLocationSourceInformation  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clLocationSourceInformation (mkSelector "isSimulatedBySoftware") retCULong []

-- | @- isProducedByAccessory@
isProducedByAccessory :: IsCLLocationSourceInformation clLocationSourceInformation => clLocationSourceInformation -> IO Bool
isProducedByAccessory clLocationSourceInformation  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clLocationSourceInformation (mkSelector "isProducedByAccessory") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSoftwareSimulationState:andExternalAccessoryState:@
initWithSoftwareSimulationState_andExternalAccessoryStateSelector :: Selector
initWithSoftwareSimulationState_andExternalAccessoryStateSelector = mkSelector "initWithSoftwareSimulationState:andExternalAccessoryState:"

-- | @Selector@ for @isSimulatedBySoftware@
isSimulatedBySoftwareSelector :: Selector
isSimulatedBySoftwareSelector = mkSelector "isSimulatedBySoftware"

-- | @Selector@ for @isProducedByAccessory@
isProducedByAccessorySelector :: Selector
isProducedByAccessorySelector = mkSelector "isProducedByAccessory"

