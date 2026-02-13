{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentLauncherClusterAdditionalInfoStruct@.
module ObjC.Matter.MTRContentLauncherClusterAdditionalInfoStruct
  ( MTRContentLauncherClusterAdditionalInfoStruct
  , IsMTRContentLauncherClusterAdditionalInfoStruct(..)
  , name
  , setName
  , value
  , setValue
  , nameSelector
  , setNameSelector
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

-- | @- name@
name :: IsMTRContentLauncherClusterAdditionalInfoStruct mtrContentLauncherClusterAdditionalInfoStruct => mtrContentLauncherClusterAdditionalInfoStruct -> IO (Id NSString)
name mtrContentLauncherClusterAdditionalInfoStruct =
  sendMessage mtrContentLauncherClusterAdditionalInfoStruct nameSelector

-- | @- setName:@
setName :: (IsMTRContentLauncherClusterAdditionalInfoStruct mtrContentLauncherClusterAdditionalInfoStruct, IsNSString value) => mtrContentLauncherClusterAdditionalInfoStruct -> value -> IO ()
setName mtrContentLauncherClusterAdditionalInfoStruct value =
  sendMessage mtrContentLauncherClusterAdditionalInfoStruct setNameSelector (toNSString value)

-- | @- value@
value :: IsMTRContentLauncherClusterAdditionalInfoStruct mtrContentLauncherClusterAdditionalInfoStruct => mtrContentLauncherClusterAdditionalInfoStruct -> IO (Id NSString)
value mtrContentLauncherClusterAdditionalInfoStruct =
  sendMessage mtrContentLauncherClusterAdditionalInfoStruct valueSelector

-- | @- setValue:@
setValue :: (IsMTRContentLauncherClusterAdditionalInfoStruct mtrContentLauncherClusterAdditionalInfoStruct, IsNSString value) => mtrContentLauncherClusterAdditionalInfoStruct -> value -> IO ()
setValue mtrContentLauncherClusterAdditionalInfoStruct value =
  sendMessage mtrContentLauncherClusterAdditionalInfoStruct setValueSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @value@
valueSelector :: Selector '[] (Id NSString)
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector '[Id NSString] ()
setValueSelector = mkSelector "setValue:"

