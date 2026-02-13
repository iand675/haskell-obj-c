{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentLauncherClusterContentSearchStruct@.
module ObjC.Matter.MTRContentLauncherClusterContentSearchStruct
  ( MTRContentLauncherClusterContentSearchStruct
  , IsMTRContentLauncherClusterContentSearchStruct(..)
  , parameterList
  , setParameterList
  , parameterListSelector
  , setParameterListSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- parameterList@
parameterList :: IsMTRContentLauncherClusterContentSearchStruct mtrContentLauncherClusterContentSearchStruct => mtrContentLauncherClusterContentSearchStruct -> IO (Id NSArray)
parameterList mtrContentLauncherClusterContentSearchStruct =
  sendMessage mtrContentLauncherClusterContentSearchStruct parameterListSelector

-- | @- setParameterList:@
setParameterList :: (IsMTRContentLauncherClusterContentSearchStruct mtrContentLauncherClusterContentSearchStruct, IsNSArray value) => mtrContentLauncherClusterContentSearchStruct -> value -> IO ()
setParameterList mtrContentLauncherClusterContentSearchStruct value =
  sendMessage mtrContentLauncherClusterContentSearchStruct setParameterListSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @parameterList@
parameterListSelector :: Selector '[] (Id NSArray)
parameterListSelector = mkSelector "parameterList"

-- | @Selector@ for @setParameterList:@
setParameterListSelector :: Selector '[Id NSArray] ()
setParameterListSelector = mkSelector "setParameterList:"

