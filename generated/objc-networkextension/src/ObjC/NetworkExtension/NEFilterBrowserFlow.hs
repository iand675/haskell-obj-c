{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEFilterBrowserFlow
--
-- The NEFilterBrowserFlow class declares the programmatic interface of an object that represents a flow of network data to be filtered, which is originated from NEFilterSource.
--
-- NEFilterBrowserFlow is part of NetworkExtension.framework
--
-- Generated bindings for @NEFilterBrowserFlow@.
module ObjC.NetworkExtension.NEFilterBrowserFlow
  ( NEFilterBrowserFlow
  , IsNEFilterBrowserFlow(..)


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

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

