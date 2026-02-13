{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTREnergyPreferenceClusterBalanceStruct@.
module ObjC.Matter.MTREnergyPreferenceClusterBalanceStruct
  ( MTREnergyPreferenceClusterBalanceStruct
  , IsMTREnergyPreferenceClusterBalanceStruct(..)
  , step
  , setStep
  , label
  , setLabel
  , labelSelector
  , setLabelSelector
  , setStepSelector
  , stepSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- step@
step :: IsMTREnergyPreferenceClusterBalanceStruct mtrEnergyPreferenceClusterBalanceStruct => mtrEnergyPreferenceClusterBalanceStruct -> IO (Id NSNumber)
step mtrEnergyPreferenceClusterBalanceStruct =
  sendMessage mtrEnergyPreferenceClusterBalanceStruct stepSelector

-- | @- setStep:@
setStep :: (IsMTREnergyPreferenceClusterBalanceStruct mtrEnergyPreferenceClusterBalanceStruct, IsNSNumber value) => mtrEnergyPreferenceClusterBalanceStruct -> value -> IO ()
setStep mtrEnergyPreferenceClusterBalanceStruct value =
  sendMessage mtrEnergyPreferenceClusterBalanceStruct setStepSelector (toNSNumber value)

-- | @- label@
label :: IsMTREnergyPreferenceClusterBalanceStruct mtrEnergyPreferenceClusterBalanceStruct => mtrEnergyPreferenceClusterBalanceStruct -> IO (Id NSString)
label mtrEnergyPreferenceClusterBalanceStruct =
  sendMessage mtrEnergyPreferenceClusterBalanceStruct labelSelector

-- | @- setLabel:@
setLabel :: (IsMTREnergyPreferenceClusterBalanceStruct mtrEnergyPreferenceClusterBalanceStruct, IsNSString value) => mtrEnergyPreferenceClusterBalanceStruct -> value -> IO ()
setLabel mtrEnergyPreferenceClusterBalanceStruct value =
  sendMessage mtrEnergyPreferenceClusterBalanceStruct setLabelSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @step@
stepSelector :: Selector '[] (Id NSNumber)
stepSelector = mkSelector "step"

-- | @Selector@ for @setStep:@
setStepSelector :: Selector '[Id NSNumber] ()
setStepSelector = mkSelector "setStep:"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

