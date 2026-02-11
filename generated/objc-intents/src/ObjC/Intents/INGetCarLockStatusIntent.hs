{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INGetCarLockStatusIntent@.
module ObjC.Intents.INGetCarLockStatusIntent
  ( INGetCarLockStatusIntent
  , IsINGetCarLockStatusIntent(..)
  , initWithCarName
  , carName
  , initWithCarNameSelector
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

-- | @- initWithCarName:@
initWithCarName :: (IsINGetCarLockStatusIntent inGetCarLockStatusIntent, IsINSpeakableString carName) => inGetCarLockStatusIntent -> carName -> IO (Id INGetCarLockStatusIntent)
initWithCarName inGetCarLockStatusIntent  carName =
withObjCPtr carName $ \raw_carName ->
    sendMsg inGetCarLockStatusIntent (mkSelector "initWithCarName:") (retPtr retVoid) [argPtr (castPtr raw_carName :: Ptr ())] >>= ownedObject . castPtr

-- | @- carName@
carName :: IsINGetCarLockStatusIntent inGetCarLockStatusIntent => inGetCarLockStatusIntent -> IO (Id INSpeakableString)
carName inGetCarLockStatusIntent  =
  sendMsg inGetCarLockStatusIntent (mkSelector "carName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCarName:@
initWithCarNameSelector :: Selector
initWithCarNameSelector = mkSelector "initWithCarName:"

-- | @Selector@ for @carName@
carNameSelector :: Selector
carNameSelector = mkSelector "carName"

