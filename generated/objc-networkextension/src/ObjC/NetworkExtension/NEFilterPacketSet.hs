{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEFilterPacketProvider.h
--
-- This file declares the NEFilterPacketProvider API. The NEFilterPacketProvider API is used to implement custom network packet filters.
--
-- This API is part of NetworkExtension.framework.
--
-- Generated bindings for @NEFilterPacketSet@.
module ObjC.NetworkExtension.NEFilterPacketSet
  ( NEFilterPacketSet
  , IsNEFilterPacketSet(..)


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

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

