{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentLauncherClusterAdditionalInfo@.
module ObjC.Matter.MTRContentLauncherClusterAdditionalInfo
  ( MTRContentLauncherClusterAdditionalInfo
  , IsMTRContentLauncherClusterAdditionalInfo(..)
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
name :: IsMTRContentLauncherClusterAdditionalInfo mtrContentLauncherClusterAdditionalInfo => mtrContentLauncherClusterAdditionalInfo -> IO (Id NSString)
name mtrContentLauncherClusterAdditionalInfo =
  sendMessage mtrContentLauncherClusterAdditionalInfo nameSelector

-- | @- setName:@
setName :: (IsMTRContentLauncherClusterAdditionalInfo mtrContentLauncherClusterAdditionalInfo, IsNSString value) => mtrContentLauncherClusterAdditionalInfo -> value -> IO ()
setName mtrContentLauncherClusterAdditionalInfo value =
  sendMessage mtrContentLauncherClusterAdditionalInfo setNameSelector (toNSString value)

-- | @- value@
value :: IsMTRContentLauncherClusterAdditionalInfo mtrContentLauncherClusterAdditionalInfo => mtrContentLauncherClusterAdditionalInfo -> IO (Id NSString)
value mtrContentLauncherClusterAdditionalInfo =
  sendMessage mtrContentLauncherClusterAdditionalInfo valueSelector

-- | @- setValue:@
setValue :: (IsMTRContentLauncherClusterAdditionalInfo mtrContentLauncherClusterAdditionalInfo, IsNSString value) => mtrContentLauncherClusterAdditionalInfo -> value -> IO ()
setValue mtrContentLauncherClusterAdditionalInfo value =
  sendMessage mtrContentLauncherClusterAdditionalInfo setValueSelector (toNSString value)

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

