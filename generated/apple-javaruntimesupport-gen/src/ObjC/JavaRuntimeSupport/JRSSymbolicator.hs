{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @JRSSymbolicator@.
module ObjC.JavaRuntimeSupport.JRSSymbolicator
  ( JRSSymbolicator
  , IsJRSSymbolicator(..)
  , symbolicatorForPid
  , addressForSymbol
  , addressForSymbolSelector
  , symbolicatorForPidSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.JavaRuntimeSupport.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ symbolicatorForPid:@
symbolicatorForPid :: CInt -> IO (Id JRSSymbolicator)
symbolicatorForPid pid =
  do
    cls' <- getRequiredClass "JRSSymbolicator"
    sendClassMessage cls' symbolicatorForPidSelector pid

-- | @- addressForSymbol:@
addressForSymbol :: (IsJRSSymbolicator jrsSymbolicator, IsNSString symbolName) => jrsSymbolicator -> symbolName -> IO CULong
addressForSymbol jrsSymbolicator symbolName =
  sendMessage jrsSymbolicator addressForSymbolSelector (toNSString symbolName)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @symbolicatorForPid:@
symbolicatorForPidSelector :: Selector '[CInt] (Id JRSSymbolicator)
symbolicatorForPidSelector = mkSelector "symbolicatorForPid:"

-- | @Selector@ for @addressForSymbol:@
addressForSymbolSelector :: Selector '[Id NSString] CULong
addressForSymbolSelector = mkSelector "addressForSymbol:"

