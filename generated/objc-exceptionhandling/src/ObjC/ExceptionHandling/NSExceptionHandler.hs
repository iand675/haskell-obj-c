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
  , setExceptionHandlingMaskSelector
  , exceptionHandlingMaskSelector
  , setExceptionHangingMaskSelector
  , exceptionHangingMaskSelector
  , setDelegateSelector
  , delegateSelector


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

import ObjC.ExceptionHandling.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ defaultExceptionHandler@
defaultExceptionHandler :: IO (Id NSExceptionHandler)
defaultExceptionHandler  =
  do
    cls' <- getRequiredClass "NSExceptionHandler"
    sendClassMsg cls' (mkSelector "defaultExceptionHandler") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setExceptionHandlingMask:@
setExceptionHandlingMask :: IsNSExceptionHandler nsExceptionHandler => nsExceptionHandler -> CULong -> IO ()
setExceptionHandlingMask nsExceptionHandler  aMask =
  sendMsg nsExceptionHandler (mkSelector "setExceptionHandlingMask:") retVoid [argCULong (fromIntegral aMask)]

-- | @- exceptionHandlingMask@
exceptionHandlingMask :: IsNSExceptionHandler nsExceptionHandler => nsExceptionHandler -> IO CULong
exceptionHandlingMask nsExceptionHandler  =
  sendMsg nsExceptionHandler (mkSelector "exceptionHandlingMask") retCULong []

-- | @- setExceptionHangingMask:@
setExceptionHangingMask :: IsNSExceptionHandler nsExceptionHandler => nsExceptionHandler -> CULong -> IO ()
setExceptionHangingMask nsExceptionHandler  aMask =
  sendMsg nsExceptionHandler (mkSelector "setExceptionHangingMask:") retVoid [argCULong (fromIntegral aMask)]

-- | @- exceptionHangingMask@
exceptionHangingMask :: IsNSExceptionHandler nsExceptionHandler => nsExceptionHandler -> IO CULong
exceptionHangingMask nsExceptionHandler  =
  sendMsg nsExceptionHandler (mkSelector "exceptionHangingMask") retCULong []

-- | @- setDelegate:@
setDelegate :: IsNSExceptionHandler nsExceptionHandler => nsExceptionHandler -> RawId -> IO ()
setDelegate nsExceptionHandler  anObject =
  sendMsg nsExceptionHandler (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId anObject) :: Ptr ())]

-- | @- delegate@
delegate :: IsNSExceptionHandler nsExceptionHandler => nsExceptionHandler -> IO RawId
delegate nsExceptionHandler  =
  fmap (RawId . castPtr) $ sendMsg nsExceptionHandler (mkSelector "delegate") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultExceptionHandler@
defaultExceptionHandlerSelector :: Selector
defaultExceptionHandlerSelector = mkSelector "defaultExceptionHandler"

-- | @Selector@ for @setExceptionHandlingMask:@
setExceptionHandlingMaskSelector :: Selector
setExceptionHandlingMaskSelector = mkSelector "setExceptionHandlingMask:"

-- | @Selector@ for @exceptionHandlingMask@
exceptionHandlingMaskSelector :: Selector
exceptionHandlingMaskSelector = mkSelector "exceptionHandlingMask"

-- | @Selector@ for @setExceptionHangingMask:@
setExceptionHangingMaskSelector :: Selector
setExceptionHangingMaskSelector = mkSelector "setExceptionHangingMask:"

-- | @Selector@ for @exceptionHangingMask@
exceptionHangingMaskSelector :: Selector
exceptionHangingMaskSelector = mkSelector "exceptionHangingMask"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

