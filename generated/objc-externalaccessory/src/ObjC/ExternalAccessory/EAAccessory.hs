{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @EAAccessory@.
module ObjC.ExternalAccessory.EAAccessory
  ( EAAccessory
  , IsEAAccessory(..)
  , connected
  , connectionID
  , dockType
  , connectedSelector
  , connectionIDSelector
  , dockTypeSelector


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

import ObjC.ExternalAccessory.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- connected@
connected :: IsEAAccessory eaAccessory => eaAccessory -> IO Bool
connected eaAccessory  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg eaAccessory (mkSelector "connected") retCULong []

-- | @- connectionID@
connectionID :: IsEAAccessory eaAccessory => eaAccessory -> IO CULong
connectionID eaAccessory  =
  sendMsg eaAccessory (mkSelector "connectionID") retCULong []

-- | @- dockType@
dockType :: IsEAAccessory eaAccessory => eaAccessory -> IO (Id NSString)
dockType eaAccessory  =
  sendMsg eaAccessory (mkSelector "dockType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @connected@
connectedSelector :: Selector
connectedSelector = mkSelector "connected"

-- | @Selector@ for @connectionID@
connectionIDSelector :: Selector
connectionIDSelector = mkSelector "connectionID"

-- | @Selector@ for @dockType@
dockTypeSelector :: Selector
dockTypeSelector = mkSelector "dockType"

