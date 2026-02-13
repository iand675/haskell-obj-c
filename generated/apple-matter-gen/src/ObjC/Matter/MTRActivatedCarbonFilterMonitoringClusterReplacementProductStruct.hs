{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRActivatedCarbonFilterMonitoringClusterReplacementProductStruct@.
module ObjC.Matter.MTRActivatedCarbonFilterMonitoringClusterReplacementProductStruct
  ( MTRActivatedCarbonFilterMonitoringClusterReplacementProductStruct
  , IsMTRActivatedCarbonFilterMonitoringClusterReplacementProductStruct(..)
  , productIdentifierType
  , setProductIdentifierType
  , productIdentifierValue
  , setProductIdentifierValue
  , productIdentifierTypeSelector
  , productIdentifierValueSelector
  , setProductIdentifierTypeSelector
  , setProductIdentifierValueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- productIdentifierType@
productIdentifierType :: IsMTRActivatedCarbonFilterMonitoringClusterReplacementProductStruct mtrActivatedCarbonFilterMonitoringClusterReplacementProductStruct => mtrActivatedCarbonFilterMonitoringClusterReplacementProductStruct -> IO (Id NSNumber)
productIdentifierType mtrActivatedCarbonFilterMonitoringClusterReplacementProductStruct =
  sendMessage mtrActivatedCarbonFilterMonitoringClusterReplacementProductStruct productIdentifierTypeSelector

-- | @- setProductIdentifierType:@
setProductIdentifierType :: (IsMTRActivatedCarbonFilterMonitoringClusterReplacementProductStruct mtrActivatedCarbonFilterMonitoringClusterReplacementProductStruct, IsNSNumber value) => mtrActivatedCarbonFilterMonitoringClusterReplacementProductStruct -> value -> IO ()
setProductIdentifierType mtrActivatedCarbonFilterMonitoringClusterReplacementProductStruct value =
  sendMessage mtrActivatedCarbonFilterMonitoringClusterReplacementProductStruct setProductIdentifierTypeSelector (toNSNumber value)

-- | @- productIdentifierValue@
productIdentifierValue :: IsMTRActivatedCarbonFilterMonitoringClusterReplacementProductStruct mtrActivatedCarbonFilterMonitoringClusterReplacementProductStruct => mtrActivatedCarbonFilterMonitoringClusterReplacementProductStruct -> IO (Id NSString)
productIdentifierValue mtrActivatedCarbonFilterMonitoringClusterReplacementProductStruct =
  sendMessage mtrActivatedCarbonFilterMonitoringClusterReplacementProductStruct productIdentifierValueSelector

-- | @- setProductIdentifierValue:@
setProductIdentifierValue :: (IsMTRActivatedCarbonFilterMonitoringClusterReplacementProductStruct mtrActivatedCarbonFilterMonitoringClusterReplacementProductStruct, IsNSString value) => mtrActivatedCarbonFilterMonitoringClusterReplacementProductStruct -> value -> IO ()
setProductIdentifierValue mtrActivatedCarbonFilterMonitoringClusterReplacementProductStruct value =
  sendMessage mtrActivatedCarbonFilterMonitoringClusterReplacementProductStruct setProductIdentifierValueSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @productIdentifierType@
productIdentifierTypeSelector :: Selector '[] (Id NSNumber)
productIdentifierTypeSelector = mkSelector "productIdentifierType"

-- | @Selector@ for @setProductIdentifierType:@
setProductIdentifierTypeSelector :: Selector '[Id NSNumber] ()
setProductIdentifierTypeSelector = mkSelector "setProductIdentifierType:"

-- | @Selector@ for @productIdentifierValue@
productIdentifierValueSelector :: Selector '[] (Id NSString)
productIdentifierValueSelector = mkSelector "productIdentifierValue"

-- | @Selector@ for @setProductIdentifierValue:@
setProductIdentifierValueSelector :: Selector '[Id NSString] ()
setProductIdentifierValueSelector = mkSelector "setProductIdentifierValue:"

