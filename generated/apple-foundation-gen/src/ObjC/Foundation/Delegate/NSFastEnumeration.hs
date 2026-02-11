{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSFastEnumeration@.
--
-- Usage:
--
-- @
-- delegate <- newNSFastEnumeration defaultNSFastEnumerationOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Foundation.Delegate.NSFastEnumeration
  ( NSFastEnumerationOverrides(..)
  , defaultNSFastEnumerationOverrides
  , newNSFastEnumeration
  ) where

import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr)
import Foreign.C.Types
import Foreign.StablePtr (newStablePtr, deRefStablePtr)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.String (withCString)
import Foreign.LibFFI (retCULong, argPtr)

import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass, class_createInstance)
import ObjC.Runtime.ClassBuilder (objc_allocateClassPair, objc_registerClassPair)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.MsgSend (sendSuperMsg)
import ObjC.Runtime.StableIvar

-- | Overrides record for @\@protocol NSFastEnumeration@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSFastEnumerationOverrides = NSFastEnumerationOverrides
  { _countByEnumeratingWithState_objects_count :: !(Maybe (RawId -> RawId -> Int -> IO Int))
  }

-- | Default overrides with all methods unimplemented.
defaultNSFastEnumerationOverrides :: NSFastEnumerationOverrides
defaultNSFastEnumerationOverrides = NSFastEnumerationOverrides
  { _countByEnumeratingWithState_objects_count = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_Q_Q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsFastEnumerationDelegateClass #-}
nsFastEnumerationDelegateClass :: Class
nsFastEnumerationDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSFastEnumeration" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_countByEnumeratingWithState_objects_count = unSelector (mkSelector "countByEnumeratingWithState:objects:count:")
  -- countByEnumeratingWithState:objects:count:
  stub_0 <- wrap_at_at_Q_Q $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFastEnumerationOverrides
    case _countByEnumeratingWithState_objects_count rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (fromIntegral arg2)
        pure (fromIntegral r)
  addObjCMethod cls "countByEnumeratingWithState:objects:count:" "Q@:@@Q" stub_0

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSFastEnumerationOverrides
    if queriedSel == sel_countByEnumeratingWithState_objects_count then pure (maybe 0 (const 1) (_countByEnumeratingWithState_objects_count rec_))
    else do
      let super_ = ObjCSuper (RawId self) superCls
      sendSuperMsg super_ (mkSelector "respondsToSelector:") retCULong
        [argPtr (castPtr queriedSel :: Ptr ())]
  addObjCMethod cls "respondsToSelector:" "B@::" rtsStub

  addStablePtrDeallocHandler cls
  objc_registerClassPair cls
  pure cls

-- | Create a new delegate implementing this protocol.
--
-- The returned 'RawId' can be used as a delegate or data source.
newNSFastEnumeration :: NSFastEnumerationOverrides -> IO RawId
newNSFastEnumeration overrides = do
  inst <- class_createInstance nsFastEnumerationDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
