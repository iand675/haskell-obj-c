{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CallKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- beginRequestWithExtensionContext:@
beginRequestWithExtensionContext :: (IsCXCallDirectoryProvider cxCallDirectoryProvider, IsCXCallDirectoryExtensionContext context) => cxCallDirectoryProvider -> context -> IO ()
beginRequestWithExtensionContext cxCallDirectoryProvider context =
  sendMessage cxCallDirectoryProvider beginRequestWithExtensionContextSelector (toCXCallDirectoryExtensionContext context)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @beginRequestWithExtensionContext:@
beginRequestWithExtensionContextSelector :: Selector '[Id CXCallDirectoryExtensionContext] ()
beginRequestWithExtensionContextSelector = mkSelector "beginRequestWithExtensionContext:"

