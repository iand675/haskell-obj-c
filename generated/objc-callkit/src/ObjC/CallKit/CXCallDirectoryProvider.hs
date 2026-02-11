{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CXCallDirectoryProvider@.
module ObjC.CallKit.CXCallDirectoryProvider
  ( CXCallDirectoryProvider
  , IsCXCallDirectoryProvider(..)
  , beginRequestWithExtensionContext
  , beginRequestWithExtensionContextSelector


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

import ObjC.CallKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- beginRequestWithExtensionContext:@
beginRequestWithExtensionContext :: (IsCXCallDirectoryProvider cxCallDirectoryProvider, IsCXCallDirectoryExtensionContext context) => cxCallDirectoryProvider -> context -> IO ()
beginRequestWithExtensionContext cxCallDirectoryProvider  context =
withObjCPtr context $ \raw_context ->
    sendMsg cxCallDirectoryProvider (mkSelector "beginRequestWithExtensionContext:") retVoid [argPtr (castPtr raw_context :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @beginRequestWithExtensionContext:@
beginRequestWithExtensionContextSelector :: Selector
beginRequestWithExtensionContextSelector = mkSelector "beginRequestWithExtensionContext:"

