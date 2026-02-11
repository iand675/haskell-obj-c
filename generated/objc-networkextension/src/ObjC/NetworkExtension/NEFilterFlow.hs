{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEFilterFlow
--
-- The NEFilterFlow class declares the programmatic interface of an object that represents a flow of network data to be filtered.
--
-- NEFilterFlow is part of NetworkExtension.framework
--
-- Generated bindings for @NEFilterFlow@.
module ObjC.NetworkExtension.NEFilterFlow
  ( NEFilterFlow
  , IsNEFilterFlow(..)
  , url
  , direction
  , identifier
  , urlSelector
  , directionSelector
  , identifierSelector

  -- * Enum types
  , NETrafficDirection(NETrafficDirection)
  , pattern NETrafficDirectionAny
  , pattern NETrafficDirectionInbound
  , pattern NETrafficDirectionOutbound

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
import ObjC.NetworkExtension.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | URL
--
-- The flow's HTTP request URL. Will be nil if the flow did not originate from WebKit.
--
-- ObjC selector: @- URL@
url :: IsNEFilterFlow neFilterFlow => neFilterFlow -> IO (Id NSURL)
url neFilterFlow  =
  sendMsg neFilterFlow (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | direction
--
-- Initial direction of the flow (outgoing or incoming flow)
--
-- ObjC selector: @- direction@
direction :: IsNEFilterFlow neFilterFlow => neFilterFlow -> IO NETrafficDirection
direction neFilterFlow  =
  fmap (coerce :: CLong -> NETrafficDirection) $ sendMsg neFilterFlow (mkSelector "direction") retCLong []

-- | identifier
--
-- The unique identifier of the flow.
--
-- ObjC selector: @- identifier@
identifier :: IsNEFilterFlow neFilterFlow => neFilterFlow -> IO (Id NSUUID)
identifier neFilterFlow  =
  sendMsg neFilterFlow (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

-- | @Selector@ for @direction@
directionSelector :: Selector
directionSelector = mkSelector "direction"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

