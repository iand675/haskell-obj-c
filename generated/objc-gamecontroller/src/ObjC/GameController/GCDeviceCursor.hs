{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A cursor is a Direction Pad that has its axis extended from [-1; 1] to [width; height] range Up, down, left, right allows to use mouse to simulate DirectionaPad or Thumbstick since values are normalized for these elements
--
-- Generated bindings for @GCDeviceCursor@.
module ObjC.GameController.GCDeviceCursor
  ( GCDeviceCursor
  , IsGCDeviceCursor(..)


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

import ObjC.GameController.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

