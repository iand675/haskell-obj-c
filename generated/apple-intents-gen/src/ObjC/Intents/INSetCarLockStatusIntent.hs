{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INSetCarLockStatusIntent@.
module ObjC.Intents.INSetCarLockStatusIntent
  ( INSetCarLockStatusIntent
  , IsINSetCarLockStatusIntent(..)
  , initWithLocked_carName
  , locked
  , carName
  , initWithLocked_carNameSelector
  , lockedSelector
  , carNameSelector


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

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithLocked:carName:@
initWithLocked_carName :: (IsINSetCarLockStatusIntent inSetCarLockStatusIntent, IsNSNumber locked, IsINSpeakableString carName) => inSetCarLockStatusIntent -> locked -> carName -> IO (Id INSetCarLockStatusIntent)
initWithLocked_carName inSetCarLockStatusIntent  locked carName =
  withObjCPtr locked $ \raw_locked ->
    withObjCPtr carName $ \raw_carName ->
        sendMsg inSetCarLockStatusIntent (mkSelector "initWithLocked:carName:") (retPtr retVoid) [argPtr (castPtr raw_locked :: Ptr ()), argPtr (castPtr raw_carName :: Ptr ())] >>= ownedObject . castPtr

-- | @- locked@
locked :: IsINSetCarLockStatusIntent inSetCarLockStatusIntent => inSetCarLockStatusIntent -> IO (Id NSNumber)
locked inSetCarLockStatusIntent  =
    sendMsg inSetCarLockStatusIntent (mkSelector "locked") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- carName@
carName :: IsINSetCarLockStatusIntent inSetCarLockStatusIntent => inSetCarLockStatusIntent -> IO (Id INSpeakableString)
carName inSetCarLockStatusIntent  =
    sendMsg inSetCarLockStatusIntent (mkSelector "carName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLocked:carName:@
initWithLocked_carNameSelector :: Selector
initWithLocked_carNameSelector = mkSelector "initWithLocked:carName:"

-- | @Selector@ for @locked@
lockedSelector :: Selector
lockedSelector = mkSelector "locked"

-- | @Selector@ for @carName@
carNameSelector :: Selector
carNameSelector = mkSelector "carName"

