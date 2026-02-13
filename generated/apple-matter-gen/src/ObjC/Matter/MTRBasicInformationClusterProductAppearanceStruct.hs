{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRBasicInformationClusterProductAppearanceStruct@.
module ObjC.Matter.MTRBasicInformationClusterProductAppearanceStruct
  ( MTRBasicInformationClusterProductAppearanceStruct
  , IsMTRBasicInformationClusterProductAppearanceStruct(..)
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
finish :: IsMTRBasicInformationClusterProductAppearanceStruct mtrBasicInformationClusterProductAppearanceStruct => mtrBasicInformationClusterProductAppearanceStruct -> IO (Id NSNumber)
finish mtrBasicInformationClusterProductAppearanceStruct =
  sendMessage mtrBasicInformationClusterProductAppearanceStruct finishSelector

-- | @- setFinish:@
setFinish :: (IsMTRBasicInformationClusterProductAppearanceStruct mtrBasicInformationClusterProductAppearanceStruct, IsNSNumber value) => mtrBasicInformationClusterProductAppearanceStruct -> value -> IO ()
setFinish mtrBasicInformationClusterProductAppearanceStruct value =
  sendMessage mtrBasicInformationClusterProductAppearanceStruct setFinishSelector (toNSNumber value)

-- | @- primaryColor@
primaryColor :: IsMTRBasicInformationClusterProductAppearanceStruct mtrBasicInformationClusterProductAppearanceStruct => mtrBasicInformationClusterProductAppearanceStruct -> IO (Id NSNumber)
primaryColor mtrBasicInformationClusterProductAppearanceStruct =
  sendMessage mtrBasicInformationClusterProductAppearanceStruct primaryColorSelector

-- | @- setPrimaryColor:@
setPrimaryColor :: (IsMTRBasicInformationClusterProductAppearanceStruct mtrBasicInformationClusterProductAppearanceStruct, IsNSNumber value) => mtrBasicInformationClusterProductAppearanceStruct -> value -> IO ()
setPrimaryColor mtrBasicInformationClusterProductAppearanceStruct value =
  sendMessage mtrBasicInformationClusterProductAppearanceStruct setPrimaryColorSelector (toNSNumber value)

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

