{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRHEPAFilterMonitoringClusterReplacementProductStruct@.
module ObjC.Matter.MTRHEPAFilterMonitoringClusterReplacementProductStruct
  ( MTRHEPAFilterMonitoringClusterReplacementProductStruct
  , IsMTRHEPAFilterMonitoringClusterReplacementProductStruct(..)
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
productIdentifierType :: IsMTRHEPAFilterMonitoringClusterReplacementProductStruct mtrhepaFilterMonitoringClusterReplacementProductStruct => mtrhepaFilterMonitoringClusterReplacementProductStruct -> IO (Id NSNumber)
productIdentifierType mtrhepaFilterMonitoringClusterReplacementProductStruct =
  sendMessage mtrhepaFilterMonitoringClusterReplacementProductStruct productIdentifierTypeSelector

-- | @- setProductIdentifierType:@
setProductIdentifierType :: (IsMTRHEPAFilterMonitoringClusterReplacementProductStruct mtrhepaFilterMonitoringClusterReplacementProductStruct, IsNSNumber value) => mtrhepaFilterMonitoringClusterReplacementProductStruct -> value -> IO ()
setProductIdentifierType mtrhepaFilterMonitoringClusterReplacementProductStruct value =
  sendMessage mtrhepaFilterMonitoringClusterReplacementProductStruct setProductIdentifierTypeSelector (toNSNumber value)

-- | @- productIdentifierValue@
productIdentifierValue :: IsMTRHEPAFilterMonitoringClusterReplacementProductStruct mtrhepaFilterMonitoringClusterReplacementProductStruct => mtrhepaFilterMonitoringClusterReplacementProductStruct -> IO (Id NSString)
productIdentifierValue mtrhepaFilterMonitoringClusterReplacementProductStruct =
  sendMessage mtrhepaFilterMonitoringClusterReplacementProductStruct productIdentifierValueSelector

-- | @- setProductIdentifierValue:@
setProductIdentifierValue :: (IsMTRHEPAFilterMonitoringClusterReplacementProductStruct mtrhepaFilterMonitoringClusterReplacementProductStruct, IsNSString value) => mtrhepaFilterMonitoringClusterReplacementProductStruct -> value -> IO ()
setProductIdentifierValue mtrhepaFilterMonitoringClusterReplacementProductStruct value =
  sendMessage mtrhepaFilterMonitoringClusterReplacementProductStruct setProductIdentifierValueSelector (toNSString value)

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

