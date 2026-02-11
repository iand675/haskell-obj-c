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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithDelegate:name:@
initWithDelegate_name :: (IsNSInputServer nsInputServer, IsNSString name) => nsInputServer -> RawId -> name -> IO (Id NSInputServer)
initWithDelegate_name nsInputServer  delegate name =
withObjCPtr name $ \raw_name ->
    sendMsg nsInputServer (mkSelector "initWithDelegate:name:") (retPtr retVoid) [argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (castPtr raw_name :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDelegate:name:@
initWithDelegate_nameSelector :: Selector
initWithDelegate_nameSelector = mkSelector "initWithDelegate:name:"

