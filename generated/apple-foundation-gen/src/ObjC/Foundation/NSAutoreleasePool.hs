{-# LANGUAGE DataKinds #-}
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
  , nsAutoreleasePoolAddObjectSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ addObject:@
nsAutoreleasePoolAddObject :: RawId -> IO ()
nsAutoreleasePoolAddObject anObject =
  do
    cls' <- getRequiredClass "NSAutoreleasePool"
    sendClassMessage cls' nsAutoreleasePoolAddObjectSelector anObject

-- | @- addObject:@
addObject :: IsNSAutoreleasePool nsAutoreleasePool => nsAutoreleasePool -> RawId -> IO ()
addObject nsAutoreleasePool anObject =
  sendMessage nsAutoreleasePool addObjectSelector anObject

-- | @- drain@
drain :: IsNSAutoreleasePool nsAutoreleasePool => nsAutoreleasePool -> IO ()
drain nsAutoreleasePool =
  sendMessage nsAutoreleasePool drainSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addObject:@
nsAutoreleasePoolAddObjectSelector :: Selector '[RawId] ()
nsAutoreleasePoolAddObjectSelector = mkSelector "addObject:"

-- | @Selector@ for @addObject:@
addObjectSelector :: Selector '[RawId] ()
addObjectSelector = mkSelector "addObject:"

-- | @Selector@ for @drain@
drainSelector :: Selector '[] ()
drainSelector = mkSelector "drain"

