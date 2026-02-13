{-# LANGUAGE DataKinds #-}
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
  , delegateSelector
  , setDelegateSelector
  , startSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.StoreKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- cancel@
cancel :: IsSKRequest skRequest => skRequest -> IO ()
cancel skRequest =
  sendMessage skRequest cancelSelector

-- | @- start@
start :: IsSKRequest skRequest => skRequest -> IO ()
start skRequest =
  sendMessage skRequest startSelector

-- | @- delegate@
delegate :: IsSKRequest skRequest => skRequest -> IO RawId
delegate skRequest =
  sendMessage skRequest delegateSelector

-- | @- setDelegate:@
setDelegate :: IsSKRequest skRequest => skRequest -> RawId -> IO ()
setDelegate skRequest value =
  sendMessage skRequest setDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cancel@
cancelSelector :: Selector '[] ()
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @start@
startSelector :: Selector '[] ()
startSelector = mkSelector "start"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

