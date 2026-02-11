{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | OSLogPosition
--
-- An opaque abstraction representing a point in a sequence of entries in the unified logging system.
--
-- Generate positions with OSLogStore instance methods and use them to start viewing entries from a particular starting point.
--
-- Generated bindings for @OSLogPosition@.
module ObjC.OSLog.OSLogPosition
  ( OSLogPosition
  , IsOSLogPosition(..)
  , init_
  , initSelector


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

import ObjC.OSLog.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsOSLogPosition osLogPosition => osLogPosition -> IO (Id OSLogPosition)
init_ osLogPosition  =
  sendMsg osLogPosition (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

