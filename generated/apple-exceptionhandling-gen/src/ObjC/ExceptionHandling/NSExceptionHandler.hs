{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSExceptionHandler@.
module ObjC.ExceptionHandling.NSExceptionHandler
  ( NSExceptionHandler
  , IsNSExceptionHandler(..)
  , defaultExceptionHandler
  , setExceptionHandlingMask
  , exceptionHandlingMask
  , setExceptionHangingMask
  , exceptionHangingMask
  , setDelegate
  , delegate
  , defaultExceptionHandlerSelector
  , delegateSelector
  , exceptionHandlingMaskSelector
  , exceptionHangingMaskSelector
  , setDelegateSelector
  , setExceptionHandlingMaskSelector
  , setExceptionHangingMaskSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ExceptionHandling.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ defaultExceptionHandler@
defaultExceptionHandler :: IO (Id NSExceptionHandler)
defaultExceptionHandler  =
  do
    cls' <- getRequiredClass "NSExceptionHandler"
    sendClassMessage cls' defaultExceptionHandlerSelector

-- | @- setExceptionHandlingMask:@
setExceptionHandlingMask :: IsNSExceptionHandler nsExceptionHandler => nsExceptionHandler -> CULong -> IO ()
setExceptionHandlingMask nsExceptionHandler aMask =
  sendMessage nsExceptionHandler setExceptionHandlingMaskSelector aMask

-- | @- exceptionHandlingMask@
exceptionHandlingMask :: IsNSExceptionHandler nsExceptionHandler => nsExceptionHandler -> IO CULong
exceptionHandlingMask nsExceptionHandler =
  sendMessage nsExceptionHandler exceptionHandlingMaskSelector

-- | @- setExceptionHangingMask:@
setExceptionHangingMask :: IsNSExceptionHandler nsExceptionHandler => nsExceptionHandler -> CULong -> IO ()
setExceptionHangingMask nsExceptionHandler aMask =
  sendMessage nsExceptionHandler setExceptionHangingMaskSelector aMask

-- | @- exceptionHangingMask@
exceptionHangingMask :: IsNSExceptionHandler nsExceptionHandler => nsExceptionHandler -> IO CULong
exceptionHangingMask nsExceptionHandler =
  sendMessage nsExceptionHandler exceptionHangingMaskSelector

-- | @- setDelegate:@
setDelegate :: IsNSExceptionHandler nsExceptionHandler => nsExceptionHandler -> RawId -> IO ()
setDelegate nsExceptionHandler anObject =
  sendMessage nsExceptionHandler setDelegateSelector anObject

-- | @- delegate@
delegate :: IsNSExceptionHandler nsExceptionHandler => nsExceptionHandler -> IO RawId
delegate nsExceptionHandler =
  sendMessage nsExceptionHandler delegateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultExceptionHandler@
defaultExceptionHandlerSelector :: Selector '[] (Id NSExceptionHandler)
defaultExceptionHandlerSelector = mkSelector "defaultExceptionHandler"

-- | @Selector@ for @setExceptionHandlingMask:@
setExceptionHandlingMaskSelector :: Selector '[CULong] ()
setExceptionHandlingMaskSelector = mkSelector "setExceptionHandlingMask:"

-- | @Selector@ for @exceptionHandlingMask@
exceptionHandlingMaskSelector :: Selector '[] CULong
exceptionHandlingMaskSelector = mkSelector "exceptionHandlingMask"

-- | @Selector@ for @setExceptionHangingMask:@
setExceptionHangingMaskSelector :: Selector '[CULong] ()
setExceptionHangingMaskSelector = mkSelector "setExceptionHangingMask:"

-- | @Selector@ for @exceptionHangingMask@
exceptionHangingMaskSelector :: Selector '[] CULong
exceptionHangingMaskSelector = mkSelector "exceptionHangingMask"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

