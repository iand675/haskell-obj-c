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

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- delegate@
delegate :: IsTKTokenDriver tkTokenDriver => tkTokenDriver -> IO RawId
delegate tkTokenDriver  =
    fmap (RawId . castPtr) $ sendMsg tkTokenDriver (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsTKTokenDriver tkTokenDriver => tkTokenDriver -> RawId -> IO ()
setDelegate tkTokenDriver  value =
    sendMsg tkTokenDriver (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

