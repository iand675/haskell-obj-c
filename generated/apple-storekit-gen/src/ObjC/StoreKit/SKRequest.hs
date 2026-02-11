{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKRequest@.
module ObjC.StoreKit.SKRequest
  ( SKRequest
  , IsSKRequest(..)
  , cancel
  , start
  , delegate
  , setDelegate
  , cancelSelector
  , startSelector
  , delegateSelector
  , setDelegateSelector


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

import ObjC.StoreKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- cancel@
cancel :: IsSKRequest skRequest => skRequest -> IO ()
cancel skRequest  =
    sendMsg skRequest (mkSelector "cancel") retVoid []

-- | @- start@
start :: IsSKRequest skRequest => skRequest -> IO ()
start skRequest  =
    sendMsg skRequest (mkSelector "start") retVoid []

-- | @- delegate@
delegate :: IsSKRequest skRequest => skRequest -> IO RawId
delegate skRequest  =
    fmap (RawId . castPtr) $ sendMsg skRequest (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsSKRequest skRequest => skRequest -> RawId -> IO ()
setDelegate skRequest  value =
    sendMsg skRequest (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cancel@
cancelSelector :: Selector
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @start@
startSelector :: Selector
startSelector = mkSelector "start"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

