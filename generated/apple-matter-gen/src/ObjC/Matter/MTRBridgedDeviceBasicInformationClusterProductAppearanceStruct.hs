{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRBridgedDeviceBasicInformationClusterProductAppearanceStruct@.
module ObjC.Matter.MTRBridgedDeviceBasicInformationClusterProductAppearanceStruct
  ( MTRBridgedDeviceBasicInformationClusterProductAppearanceStruct
  , IsMTRBridgedDeviceBasicInformationClusterProductAppearanceStruct(..)
  , finish
  , setFinish
  , primaryColor
  , setPrimaryColor
  , finishSelector
  , primaryColorSelector
  , setFinishSelector
  , setPrimaryColorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- finish@
finish :: IsMTRBridgedDeviceBasicInformationClusterProductAppearanceStruct mtrBridgedDeviceBasicInformationClusterProductAppearanceStruct => mtrBridgedDeviceBasicInformationClusterProductAppearanceStruct -> IO (Id NSNumber)
finish mtrBridgedDeviceBasicInformationClusterProductAppearanceStruct =
  sendMessage mtrBridgedDeviceBasicInformationClusterProductAppearanceStruct finishSelector

-- | @- setFinish:@
setFinish :: (IsMTRBridgedDeviceBasicInformationClusterProductAppearanceStruct mtrBridgedDeviceBasicInformationClusterProductAppearanceStruct, IsNSNumber value) => mtrBridgedDeviceBasicInformationClusterProductAppearanceStruct -> value -> IO ()
setFinish mtrBridgedDeviceBasicInformationClusterProductAppearanceStruct value =
  sendMessage mtrBridgedDeviceBasicInformationClusterProductAppearanceStruct setFinishSelector (toNSNumber value)

-- | @- primaryColor@
primaryColor :: IsMTRBridgedDeviceBasicInformationClusterProductAppearanceStruct mtrBridgedDeviceBasicInformationClusterProductAppearanceStruct => mtrBridgedDeviceBasicInformationClusterProductAppearanceStruct -> IO (Id NSNumber)
primaryColor mtrBridgedDeviceBasicInformationClusterProductAppearanceStruct =
  sendMessage mtrBridgedDeviceBasicInformationClusterProductAppearanceStruct primaryColorSelector

-- | @- setPrimaryColor:@
setPrimaryColor :: (IsMTRBridgedDeviceBasicInformationClusterProductAppearanceStruct mtrBridgedDeviceBasicInformationClusterProductAppearanceStruct, IsNSNumber value) => mtrBridgedDeviceBasicInformationClusterProductAppearanceStruct -> value -> IO ()
setPrimaryColor mtrBridgedDeviceBasicInformationClusterProductAppearanceStruct value =
  sendMessage mtrBridgedDeviceBasicInformationClusterProductAppearanceStruct setPrimaryColorSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @finish@
finishSelector :: Selector '[] (Id NSNumber)
finishSelector = mkSelector "finish"

-- | @Selector@ for @setFinish:@
setFinishSelector :: Selector '[Id NSNumber] ()
setFinishSelector = mkSelector "setFinish:"

-- | @Selector@ for @primaryColor@
primaryColorSelector :: Selector '[] (Id NSNumber)
primaryColorSelector = mkSelector "primaryColor"

-- | @Selector@ for @setPrimaryColor:@
setPrimaryColorSelector :: Selector '[Id NSNumber] ()
setPrimaryColorSelector = mkSelector "setPrimaryColor:"

