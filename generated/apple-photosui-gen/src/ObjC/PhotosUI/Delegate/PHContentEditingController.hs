{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol PHContentEditingController@.
--
-- Usage:
--
-- @
-- delegate <- newPHContentEditingController defaultPHContentEditingControllerOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.PhotosUI.Delegate.PHContentEditingController
  ( PHContentEditingControllerOverrides(..)
  , defaultPHContentEditingControllerOverrides
  , newPHContentEditingController
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

-- | Overrides record for @\@protocol PHContentEditingController@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data PHContentEditingControllerOverrides = PHContentEditingControllerOverrides
  { _canHandleAdjustmentData :: !(Maybe (RawId -> IO Bool))
  , _startContentEditingWithInput_placeholderImage :: !(Maybe (RawId -> RawId -> IO ()))
  , _cancelContentEditing :: !(Maybe (IO ()))
  , _shouldShowCancelConfirmation :: !(Maybe (IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultPHContentEditingControllerOverrides :: PHContentEditingControllerOverrides
defaultPHContentEditingControllerOverrides = PHContentEditingControllerOverrides
  { _canHandleAdjustmentData = Nothing
  , _startContentEditingWithInput_placeholderImage = Nothing
  , _cancelContentEditing = Nothing
  , _shouldShowCancelConfirmation = Nothing
  }

foreign import ccall "wrapper"
  wrap_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE phContentEditingControllerDelegateClass #-}
phContentEditingControllerDelegateClass :: Class
phContentEditingControllerDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsPHContentEditingController" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_canHandleAdjustmentData = unSelector (mkSelector "canHandleAdjustmentData:")
      sel_startContentEditingWithInput_placeholderImage = unSelector (mkSelector "startContentEditingWithInput:placeholderImage:")
      sel_cancelContentEditing = unSelector (mkSelector "cancelContentEditing")
      sel_shouldShowCancelConfirmation = unSelector (mkSelector "shouldShowCancelConfirmation")
  -- canHandleAdjustmentData:
  stub_0 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PHContentEditingControllerOverrides
    case _canHandleAdjustmentData rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "canHandleAdjustmentData:" "B@:@" stub_0

  -- startContentEditingWithInput:placeholderImage:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PHContentEditingControllerOverrides
    case _startContentEditingWithInput_placeholderImage rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "startContentEditingWithInput:placeholderImage:" "v@:@@" stub_1

  -- cancelContentEditing
  stub_2 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PHContentEditingControllerOverrides
    case _cancelContentEditing rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "cancelContentEditing" "v@:" stub_2

  -- shouldShowCancelConfirmation
  stub_3 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PHContentEditingControllerOverrides
    case _shouldShowCancelConfirmation rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "shouldShowCancelConfirmation" "B@:" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PHContentEditingControllerOverrides
    if queriedSel == sel_canHandleAdjustmentData then pure (maybe 0 (const 1) (_canHandleAdjustmentData rec_))
    else if queriedSel == sel_startContentEditingWithInput_placeholderImage then pure (maybe 0 (const 1) (_startContentEditingWithInput_placeholderImage rec_))
    else if queriedSel == sel_cancelContentEditing then pure (maybe 0 (const 1) (_cancelContentEditing rec_))
    else if queriedSel == sel_shouldShowCancelConfirmation then pure (maybe 0 (const 1) (_shouldShowCancelConfirmation rec_))
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
newPHContentEditingController :: PHContentEditingControllerOverrides -> IO RawId
newPHContentEditingController overrides = do
  inst <- class_createInstance phContentEditingControllerDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
