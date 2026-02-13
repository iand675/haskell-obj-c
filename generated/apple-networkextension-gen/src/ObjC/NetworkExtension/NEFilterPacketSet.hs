{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NetworkExtension.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

