{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentLauncherClusterContentSearch@.
module ObjC.Matter.MTRContentLauncherClusterContentSearch
  ( MTRContentLauncherClusterContentSearch
  , IsMTRContentLauncherClusterContentSearch(..)
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
parameterList :: IsMTRContentLauncherClusterContentSearch mtrContentLauncherClusterContentSearch => mtrContentLauncherClusterContentSearch -> IO (Id NSArray)
parameterList mtrContentLauncherClusterContentSearch =
  sendMessage mtrContentLauncherClusterContentSearch parameterListSelector

-- | @- setParameterList:@
setParameterList :: (IsMTRContentLauncherClusterContentSearch mtrContentLauncherClusterContentSearch, IsNSArray value) => mtrContentLauncherClusterContentSearch -> value -> IO ()
setParameterList mtrContentLauncherClusterContentSearch value =
  sendMessage mtrContentLauncherClusterContentSearch setParameterListSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @parameterList@
parameterListSelector :: Selector '[] (Id NSArray)
parameterListSelector = mkSelector "parameterList"

-- | @Selector@ for @setParameterList:@
setParameterListSelector :: Selector '[Id NSArray] ()
setParameterListSelector = mkSelector "setParameterList:"

