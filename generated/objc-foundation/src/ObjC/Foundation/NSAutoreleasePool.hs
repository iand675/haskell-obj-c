{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSAutoreleasePool@.
module ObjC.Foundation.NSAutoreleasePool
  ( NSAutoreleasePool
  , IsNSAutoreleasePool(..)
  , nsAutoreleasePoolAddObject
  , addObject
  , drain
  , addObjectSelector
  , drainSelector


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

import ObjC.Foundation.Internal.Classes

-- | @+ addObject:@
nsAutoreleasePoolAddObject :: RawId -> IO ()
nsAutoreleasePoolAddObject anObject =
  do
    cls' <- getRequiredClass "NSAutoreleasePool"
    sendClassMsg cls' (mkSelector "addObject:") retVoid [argPtr (castPtr (unRawId anObject) :: Ptr ())]

-- | @- addObject:@
addObject :: IsNSAutoreleasePool nsAutoreleasePool => nsAutoreleasePool -> RawId -> IO ()
addObject nsAutoreleasePool  anObject =
  sendMsg nsAutoreleasePool (mkSelector "addObject:") retVoid [argPtr (castPtr (unRawId anObject) :: Ptr ())]

-- | @- drain@
drain :: IsNSAutoreleasePool nsAutoreleasePool => nsAutoreleasePool -> IO ()
drain nsAutoreleasePool  =
  sendMsg nsAutoreleasePool (mkSelector "drain") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addObject:@
addObjectSelector :: Selector
addObjectSelector = mkSelector "addObject:"

-- | @Selector@ for @drain@
drainSelector :: Selector
drainSelector = mkSelector "drain"

