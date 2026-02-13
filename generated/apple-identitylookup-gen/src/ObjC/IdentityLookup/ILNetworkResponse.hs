{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A response to an HTTPS network request.
--
-- Generated bindings for @ILNetworkResponse@.
module ObjC.IdentityLookup.ILNetworkResponse
  ( ILNetworkResponse
  , IsILNetworkResponse(..)
  , init_
  , urlResponse
  , data_
  , dataSelector
  , initSelector
  , urlResponseSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.IdentityLookup.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsILNetworkResponse ilNetworkResponse => ilNetworkResponse -> IO (Id ILNetworkResponse)
init_ ilNetworkResponse =
  sendOwnedMessage ilNetworkResponse initSelector

-- | Represents the URL response itself. See documentation for NSHTTPURLResponse.
--
-- ObjC selector: @- urlResponse@
urlResponse :: IsILNetworkResponse ilNetworkResponse => ilNetworkResponse -> IO (Id NSHTTPURLResponse)
urlResponse ilNetworkResponse =
  sendMessage ilNetworkResponse urlResponseSelector

-- | Data returned in the HTTPS response.
--
-- ObjC selector: @- data@
data_ :: IsILNetworkResponse ilNetworkResponse => ilNetworkResponse -> IO (Id NSData)
data_ ilNetworkResponse =
  sendMessage ilNetworkResponse dataSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ILNetworkResponse)
initSelector = mkSelector "init"

-- | @Selector@ for @urlResponse@
urlResponseSelector :: Selector '[] (Id NSHTTPURLResponse)
urlResponseSelector = mkSelector "urlResponse"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSData)
dataSelector = mkSelector "data"

