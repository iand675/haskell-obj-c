{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRApplicationLauncherClusterApplicationStruct@.
module ObjC.Matter.MTRApplicationLauncherClusterApplicationStruct
  ( MTRApplicationLauncherClusterApplicationStruct
  , IsMTRApplicationLauncherClusterApplicationStruct(..)
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
catalogVendorID :: IsMTRApplicationLauncherClusterApplicationStruct mtrApplicationLauncherClusterApplicationStruct => mtrApplicationLauncherClusterApplicationStruct -> IO (Id NSNumber)
catalogVendorID mtrApplicationLauncherClusterApplicationStruct =
  sendMessage mtrApplicationLauncherClusterApplicationStruct catalogVendorIDSelector

-- | @- setCatalogVendorID:@
setCatalogVendorID :: (IsMTRApplicationLauncherClusterApplicationStruct mtrApplicationLauncherClusterApplicationStruct, IsNSNumber value) => mtrApplicationLauncherClusterApplicationStruct -> value -> IO ()
setCatalogVendorID mtrApplicationLauncherClusterApplicationStruct value =
  sendMessage mtrApplicationLauncherClusterApplicationStruct setCatalogVendorIDSelector (toNSNumber value)

-- | @- catalogVendorId@
catalogVendorId :: IsMTRApplicationLauncherClusterApplicationStruct mtrApplicationLauncherClusterApplicationStruct => mtrApplicationLauncherClusterApplicationStruct -> IO (Id NSNumber)
catalogVendorId mtrApplicationLauncherClusterApplicationStruct =
  sendMessage mtrApplicationLauncherClusterApplicationStruct catalogVendorIdSelector

-- | @- setCatalogVendorId:@
setCatalogVendorId :: (IsMTRApplicationLauncherClusterApplicationStruct mtrApplicationLauncherClusterApplicationStruct, IsNSNumber value) => mtrApplicationLauncherClusterApplicationStruct -> value -> IO ()
setCatalogVendorId mtrApplicationLauncherClusterApplicationStruct value =
  sendMessage mtrApplicationLauncherClusterApplicationStruct setCatalogVendorIdSelector (toNSNumber value)

-- | @- applicationID@
applicationID :: IsMTRApplicationLauncherClusterApplicationStruct mtrApplicationLauncherClusterApplicationStruct => mtrApplicationLauncherClusterApplicationStruct -> IO (Id NSString)
applicationID mtrApplicationLauncherClusterApplicationStruct =
  sendMessage mtrApplicationLauncherClusterApplicationStruct applicationIDSelector

-- | @- setApplicationID:@
setApplicationID :: (IsMTRApplicationLauncherClusterApplicationStruct mtrApplicationLauncherClusterApplicationStruct, IsNSString value) => mtrApplicationLauncherClusterApplicationStruct -> value -> IO ()
setApplicationID mtrApplicationLauncherClusterApplicationStruct value =
  sendMessage mtrApplicationLauncherClusterApplicationStruct setApplicationIDSelector (toNSString value)

-- | @- applicationId@
applicationId :: IsMTRApplicationLauncherClusterApplicationStruct mtrApplicationLauncherClusterApplicationStruct => mtrApplicationLauncherClusterApplicationStruct -> IO (Id NSString)
applicationId mtrApplicationLauncherClusterApplicationStruct =
  sendMessage mtrApplicationLauncherClusterApplicationStruct applicationIdSelector

-- | @- setApplicationId:@
setApplicationId :: (IsMTRApplicationLauncherClusterApplicationStruct mtrApplicationLauncherClusterApplicationStruct, IsNSString value) => mtrApplicationLauncherClusterApplicationStruct -> value -> IO ()
setApplicationId mtrApplicationLauncherClusterApplicationStruct value =
  sendMessage mtrApplicationLauncherClusterApplicationStruct setApplicationIdSelector (toNSString value)

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

