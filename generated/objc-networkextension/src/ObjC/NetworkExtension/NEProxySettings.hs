{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEProxySettings
--
-- The NEProxySettings class declares the programmatic interface for an object that contains proxy settings.
--
-- NEProxySettings is used in the context of a Network Extension configuration to specify the proxy that should be used for network traffic when the Network Extension is active.
--
-- Generated bindings for @NEProxySettings@.
module ObjC.NetworkExtension.NEProxySettings
  ( NEProxySettings
  , IsNEProxySettings(..)
  , autoProxyConfigurationEnabled
  , setAutoProxyConfigurationEnabled
  , httpEnabled
  , setHTTPEnabled
  , httpsEnabled
  , setHTTPSEnabled
  , excludeSimpleHostnames
  , setExcludeSimpleHostnames
  , autoProxyConfigurationEnabledSelector
  , setAutoProxyConfigurationEnabledSelector
  , httpEnabledSelector
  , setHTTPEnabledSelector
  , httpsEnabledSelector
  , setHTTPSEnabledSelector
  , excludeSimpleHostnamesSelector
  , setExcludeSimpleHostnamesSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | autoProxyConfigurationEnabled
--
-- A boolean indicating if proxy auto-configuration is enabled.
--
-- ObjC selector: @- autoProxyConfigurationEnabled@
autoProxyConfigurationEnabled :: IsNEProxySettings neProxySettings => neProxySettings -> IO Bool
autoProxyConfigurationEnabled neProxySettings  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg neProxySettings (mkSelector "autoProxyConfigurationEnabled") retCULong []

-- | autoProxyConfigurationEnabled
--
-- A boolean indicating if proxy auto-configuration is enabled.
--
-- ObjC selector: @- setAutoProxyConfigurationEnabled:@
setAutoProxyConfigurationEnabled :: IsNEProxySettings neProxySettings => neProxySettings -> Bool -> IO ()
setAutoProxyConfigurationEnabled neProxySettings  value =
  sendMsg neProxySettings (mkSelector "setAutoProxyConfigurationEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | HTTPEnabled
--
-- A boolean indicating if the static HTTP proxy is enabled.
--
-- ObjC selector: @- HTTPEnabled@
httpEnabled :: IsNEProxySettings neProxySettings => neProxySettings -> IO Bool
httpEnabled neProxySettings  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg neProxySettings (mkSelector "HTTPEnabled") retCULong []

-- | HTTPEnabled
--
-- A boolean indicating if the static HTTP proxy is enabled.
--
-- ObjC selector: @- setHTTPEnabled:@
setHTTPEnabled :: IsNEProxySettings neProxySettings => neProxySettings -> Bool -> IO ()
setHTTPEnabled neProxySettings  value =
  sendMsg neProxySettings (mkSelector "setHTTPEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | HTTPSEnabled
--
-- A boolean indicating if the static HTTPS proxy is enabled.
--
-- ObjC selector: @- HTTPSEnabled@
httpsEnabled :: IsNEProxySettings neProxySettings => neProxySettings -> IO Bool
httpsEnabled neProxySettings  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg neProxySettings (mkSelector "HTTPSEnabled") retCULong []

-- | HTTPSEnabled
--
-- A boolean indicating if the static HTTPS proxy is enabled.
--
-- ObjC selector: @- setHTTPSEnabled:@
setHTTPSEnabled :: IsNEProxySettings neProxySettings => neProxySettings -> Bool -> IO ()
setHTTPSEnabled neProxySettings  value =
  sendMsg neProxySettings (mkSelector "setHTTPSEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | excludeSimpleHostnames
--
-- A flag indicating if the proxy settings should not be used for network destinations specified using single-label host names.
--
-- ObjC selector: @- excludeSimpleHostnames@
excludeSimpleHostnames :: IsNEProxySettings neProxySettings => neProxySettings -> IO Bool
excludeSimpleHostnames neProxySettings  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg neProxySettings (mkSelector "excludeSimpleHostnames") retCULong []

-- | excludeSimpleHostnames
--
-- A flag indicating if the proxy settings should not be used for network destinations specified using single-label host names.
--
-- ObjC selector: @- setExcludeSimpleHostnames:@
setExcludeSimpleHostnames :: IsNEProxySettings neProxySettings => neProxySettings -> Bool -> IO ()
setExcludeSimpleHostnames neProxySettings  value =
  sendMsg neProxySettings (mkSelector "setExcludeSimpleHostnames:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @autoProxyConfigurationEnabled@
autoProxyConfigurationEnabledSelector :: Selector
autoProxyConfigurationEnabledSelector = mkSelector "autoProxyConfigurationEnabled"

-- | @Selector@ for @setAutoProxyConfigurationEnabled:@
setAutoProxyConfigurationEnabledSelector :: Selector
setAutoProxyConfigurationEnabledSelector = mkSelector "setAutoProxyConfigurationEnabled:"

-- | @Selector@ for @HTTPEnabled@
httpEnabledSelector :: Selector
httpEnabledSelector = mkSelector "HTTPEnabled"

-- | @Selector@ for @setHTTPEnabled:@
setHTTPEnabledSelector :: Selector
setHTTPEnabledSelector = mkSelector "setHTTPEnabled:"

-- | @Selector@ for @HTTPSEnabled@
httpsEnabledSelector :: Selector
httpsEnabledSelector = mkSelector "HTTPSEnabled"

-- | @Selector@ for @setHTTPSEnabled:@
setHTTPSEnabledSelector :: Selector
setHTTPSEnabledSelector = mkSelector "setHTTPSEnabled:"

-- | @Selector@ for @excludeSimpleHostnames@
excludeSimpleHostnamesSelector :: Selector
excludeSimpleHostnamesSelector = mkSelector "excludeSimpleHostnames"

-- | @Selector@ for @setExcludeSimpleHostnames:@
setExcludeSimpleHostnamesSelector :: Selector
setExcludeSimpleHostnamesSelector = mkSelector "setExcludeSimpleHostnames:"

