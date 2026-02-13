{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSInputServer@.
module ObjC.AppKit.NSInputServer
  ( NSInputServer
  , IsNSInputServer(..)
  , initWithDelegate_name
  , initWithDelegate_nameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithDelegate:name:@
initWithDelegate_name :: (IsNSInputServer nsInputServer, IsNSString name) => nsInputServer -> RawId -> name -> IO (Id NSInputServer)
initWithDelegate_name nsInputServer delegate name =
  sendOwnedMessage nsInputServer initWithDelegate_nameSelector delegate (toNSString name)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDelegate:name:@
initWithDelegate_nameSelector :: Selector '[RawId, Id NSString] (Id NSInputServer)
initWithDelegate_nameSelector = mkSelector "initWithDelegate:name:"

