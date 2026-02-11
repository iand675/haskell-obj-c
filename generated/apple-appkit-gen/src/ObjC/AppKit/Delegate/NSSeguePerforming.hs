{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSSeguePerforming@.
--
-- Usage:
--
-- @
-- delegate <- newNSSeguePerforming defaultNSSeguePerformingOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSSeguePerforming
  ( NSSeguePerformingOverrides(..)
  , defaultNSSeguePerformingOverrides
  , newNSSeguePerforming
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

-- | Overrides record for @\@protocol NSSeguePerforming@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSSeguePerformingOverrides = NSSeguePerformingOverrides
  { _prepareForSegue_sender :: !(Maybe (RawId -> RawId -> IO ()))
  , _performSegueWithIdentifier_sender :: !(Maybe (RawId -> RawId -> IO ()))
  , _shouldPerformSegueWithIdentifier_sender :: !(Maybe (RawId -> RawId -> IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultNSSeguePerformingOverrides :: NSSeguePerformingOverrides
defaultNSSeguePerformingOverrides = NSSeguePerformingOverrides
  { _prepareForSegue_sender = Nothing
  , _performSegueWithIdentifier_sender = Nothing
  , _shouldPerformSegueWithIdentifier_sender = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsSeguePerformingDelegateClass #-}
nsSeguePerformingDelegateClass :: Class
nsSeguePerformingDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSSeguePerforming" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_prepareForSegue_sender = unSelector (mkSelector "prepareForSegue:sender:")
      sel_performSegueWithIdentifier_sender = unSelector (mkSelector "performSegueWithIdentifier:sender:")
      sel_shouldPerformSegueWithIdentifier_sender = unSelector (mkSelector "shouldPerformSegueWithIdentifier:sender:")
  -- prepareForSegue:sender:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSSeguePerformingOverrides
    case _prepareForSegue_sender rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "prepareForSegue:sender:" "v@:@@" stub_0

  -- performSegueWithIdentifier:sender:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSSeguePerformingOverrides
    case _performSegueWithIdentifier_sender rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "performSegueWithIdentifier:sender:" "v@:@@" stub_1

  -- shouldPerformSegueWithIdentifier:sender:
  stub_2 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSSeguePerformingOverrides
    case _shouldPerformSegueWithIdentifier_sender rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "shouldPerformSegueWithIdentifier:sender:" "B@:@@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSSeguePerformingOverrides
    if queriedSel == sel_prepareForSegue_sender then pure (maybe 0 (const 1) (_prepareForSegue_sender rec_))
    else if queriedSel == sel_performSegueWithIdentifier_sender then pure (maybe 0 (const 1) (_performSegueWithIdentifier_sender rec_))
    else if queriedSel == sel_shouldPerformSegueWithIdentifier_sender then pure (maybe 0 (const 1) (_shouldPerformSegueWithIdentifier_sender rec_))
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
newNSSeguePerforming :: NSSeguePerformingOverrides -> IO RawId
newNSSeguePerforming overrides = do
  inst <- class_createInstance nsSeguePerformingDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
