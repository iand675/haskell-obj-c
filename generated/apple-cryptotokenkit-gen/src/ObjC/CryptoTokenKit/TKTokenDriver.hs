{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class for token drivers.  SmartCard token drivers should use TKSmartCardTokenDriver subclass.
--
-- Generated bindings for @TKTokenDriver@.
module ObjC.CryptoTokenKit.TKTokenDriver
  ( TKTokenDriver
  , IsTKTokenDriver(..)
  , delegate
  , setDelegate
  , delegateSelector
  , setDelegateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- delegate@
delegate :: IsTKTokenDriver tkTokenDriver => tkTokenDriver -> IO RawId
delegate tkTokenDriver =
  sendMessage tkTokenDriver delegateSelector

-- | @- setDelegate:@
setDelegate :: IsTKTokenDriver tkTokenDriver => tkTokenDriver -> RawId -> IO ()
setDelegate tkTokenDriver value =
  sendMessage tkTokenDriver setDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

