{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSObject@.
module ObjC.ExceptionHandling.NSObject
  ( NSObject
  , IsNSObject(..)
  , exceptionHandler_shouldLogException_mask
  , exceptionHandler_shouldHandleException_mask
  , exceptionHandler_shouldLogException_maskSelector
  , exceptionHandler_shouldHandleException_maskSelector


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

-- | @- exceptionHandler:shouldLogException:mask:@
exceptionHandler_shouldLogException_mask :: (IsNSObject nsObject, IsNSExceptionHandler sender, IsNSException exception) => nsObject -> sender -> exception -> CULong -> IO Bool
exceptionHandler_shouldLogException_mask nsObject  sender exception aMask =
withObjCPtr sender $ \raw_sender ->
  withObjCPtr exception $ \raw_exception ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "exceptionHandler:shouldLogException:mask:") retCULong [argPtr (castPtr raw_sender :: Ptr ()), argPtr (castPtr raw_exception :: Ptr ()), argCULong (fromIntegral aMask)]

-- | @- exceptionHandler:shouldHandleException:mask:@
exceptionHandler_shouldHandleException_mask :: (IsNSObject nsObject, IsNSExceptionHandler sender, IsNSException exception) => nsObject -> sender -> exception -> CULong -> IO Bool
exceptionHandler_shouldHandleException_mask nsObject  sender exception aMask =
withObjCPtr sender $ \raw_sender ->
  withObjCPtr exception $ \raw_exception ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "exceptionHandler:shouldHandleException:mask:") retCULong [argPtr (castPtr raw_sender :: Ptr ()), argPtr (castPtr raw_exception :: Ptr ()), argCULong (fromIntegral aMask)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @exceptionHandler:shouldLogException:mask:@
exceptionHandler_shouldLogException_maskSelector :: Selector
exceptionHandler_shouldLogException_maskSelector = mkSelector "exceptionHandler:shouldLogException:mask:"

-- | @Selector@ for @exceptionHandler:shouldHandleException:mask:@
exceptionHandler_shouldHandleException_maskSelector :: Selector
exceptionHandler_shouldHandleException_maskSelector = mkSelector "exceptionHandler:shouldHandleException:mask:"

