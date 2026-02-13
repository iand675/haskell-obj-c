{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRApplicationBasicClusterApplicationStruct@.
module ObjC.Matter.MTRApplicationBasicClusterApplicationStruct
  ( MTRApplicationBasicClusterApplicationStruct
  , IsMTRApplicationBasicClusterApplicationStruct(..)
  , catalogVendorID
  , setCatalogVendorID
  , catalogVendorId
  , setCatalogVendorId
  , applicationID
  , setApplicationID
  , applicationId
  , setApplicationId
  , applicationIDSelector
  , applicationIdSelector
  , catalogVendorIDSelector
  , catalogVendorIdSelector
  , setApplicationIDSelector
  , setApplicationIdSelector
  , setCatalogVendorIDSelector
  , setCatalogVendorIdSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- catalogVendorID@
catalogVendorID :: IsMTRApplicationBasicClusterApplicationStruct mtrApplicationBasicClusterApplicationStruct => mtrApplicationBasicClusterApplicationStruct -> IO (Id NSNumber)
catalogVendorID mtrApplicationBasicClusterApplicationStruct =
  sendMessage mtrApplicationBasicClusterApplicationStruct catalogVendorIDSelector

-- | @- setCatalogVendorID:@
setCatalogVendorID :: (IsMTRApplicationBasicClusterApplicationStruct mtrApplicationBasicClusterApplicationStruct, IsNSNumber value) => mtrApplicationBasicClusterApplicationStruct -> value -> IO ()
setCatalogVendorID mtrApplicationBasicClusterApplicationStruct value =
  sendMessage mtrApplicationBasicClusterApplicationStruct setCatalogVendorIDSelector (toNSNumber value)

-- | @- catalogVendorId@
catalogVendorId :: IsMTRApplicationBasicClusterApplicationStruct mtrApplicationBasicClusterApplicationStruct => mtrApplicationBasicClusterApplicationStruct -> IO (Id NSNumber)
catalogVendorId mtrApplicationBasicClusterApplicationStruct =
  sendMessage mtrApplicationBasicClusterApplicationStruct catalogVendorIdSelector

-- | @- setCatalogVendorId:@
setCatalogVendorId :: (IsMTRApplicationBasicClusterApplicationStruct mtrApplicationBasicClusterApplicationStruct, IsNSNumber value) => mtrApplicationBasicClusterApplicationStruct -> value -> IO ()
setCatalogVendorId mtrApplicationBasicClusterApplicationStruct value =
  sendMessage mtrApplicationBasicClusterApplicationStruct setCatalogVendorIdSelector (toNSNumber value)

-- | @- applicationID@
applicationID :: IsMTRApplicationBasicClusterApplicationStruct mtrApplicationBasicClusterApplicationStruct => mtrApplicationBasicClusterApplicationStruct -> IO (Id NSString)
applicationID mtrApplicationBasicClusterApplicationStruct =
  sendMessage mtrApplicationBasicClusterApplicationStruct applicationIDSelector

-- | @- setApplicationID:@
setApplicationID :: (IsMTRApplicationBasicClusterApplicationStruct mtrApplicationBasicClusterApplicationStruct, IsNSString value) => mtrApplicationBasicClusterApplicationStruct -> value -> IO ()
setApplicationID mtrApplicationBasicClusterApplicationStruct value =
  sendMessage mtrApplicationBasicClusterApplicationStruct setApplicationIDSelector (toNSString value)

-- | @- applicationId@
applicationId :: IsMTRApplicationBasicClusterApplicationStruct mtrApplicationBasicClusterApplicationStruct => mtrApplicationBasicClusterApplicationStruct -> IO (Id NSString)
applicationId mtrApplicationBasicClusterApplicationStruct =
  sendMessage mtrApplicationBasicClusterApplicationStruct applicationIdSelector

-- | @- setApplicationId:@
setApplicationId :: (IsMTRApplicationBasicClusterApplicationStruct mtrApplicationBasicClusterApplicationStruct, IsNSString value) => mtrApplicationBasicClusterApplicationStruct -> value -> IO ()
setApplicationId mtrApplicationBasicClusterApplicationStruct value =
  sendMessage mtrApplicationBasicClusterApplicationStruct setApplicationIdSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @catalogVendorID@
catalogVendorIDSelector :: Selector '[] (Id NSNumber)
catalogVendorIDSelector = mkSelector "catalogVendorID"

-- | @Selector@ for @setCatalogVendorID:@
setCatalogVendorIDSelector :: Selector '[Id NSNumber] ()
setCatalogVendorIDSelector = mkSelector "setCatalogVendorID:"

-- | @Selector@ for @catalogVendorId@
catalogVendorIdSelector :: Selector '[] (Id NSNumber)
catalogVendorIdSelector = mkSelector "catalogVendorId"

-- | @Selector@ for @setCatalogVendorId:@
setCatalogVendorIdSelector :: Selector '[Id NSNumber] ()
setCatalogVendorIdSelector = mkSelector "setCatalogVendorId:"

-- | @Selector@ for @applicationID@
applicationIDSelector :: Selector '[] (Id NSString)
applicationIDSelector = mkSelector "applicationID"

-- | @Selector@ for @setApplicationID:@
setApplicationIDSelector :: Selector '[Id NSString] ()
setApplicationIDSelector = mkSelector "setApplicationID:"

-- | @Selector@ for @applicationId@
applicationIdSelector :: Selector '[] (Id NSString)
applicationIdSelector = mkSelector "applicationId"

-- | @Selector@ for @setApplicationId:@
setApplicationIdSelector :: Selector '[Id NSString] ()
setApplicationIdSelector = mkSelector "setApplicationId:"

