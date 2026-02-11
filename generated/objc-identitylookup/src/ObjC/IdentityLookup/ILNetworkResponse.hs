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
  , initSelector
  , urlResponseSelector
  , dataSelector


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

import ObjC.IdentityLookup.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsILNetworkResponse ilNetworkResponse => ilNetworkResponse -> IO (Id ILNetworkResponse)
init_ ilNetworkResponse  =
  sendMsg ilNetworkResponse (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Represents the URL response itself. See documentation for NSHTTPURLResponse.
--
-- ObjC selector: @- urlResponse@
urlResponse :: IsILNetworkResponse ilNetworkResponse => ilNetworkResponse -> IO (Id NSHTTPURLResponse)
urlResponse ilNetworkResponse  =
  sendMsg ilNetworkResponse (mkSelector "urlResponse") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Data returned in the HTTPS response.
--
-- ObjC selector: @- data@
data_ :: IsILNetworkResponse ilNetworkResponse => ilNetworkResponse -> IO (Id NSData)
data_ ilNetworkResponse  =
  sendMsg ilNetworkResponse (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @urlResponse@
urlResponseSelector :: Selector
urlResponseSelector = mkSelector "urlResponse"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

