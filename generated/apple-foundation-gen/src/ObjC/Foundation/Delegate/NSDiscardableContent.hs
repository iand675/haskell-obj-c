{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSDiscardableContent@.
--
-- Usage:
--
-- @
-- delegate <- newNSDiscardableContent defaultNSDiscardableContentOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Foundation.Delegate.NSDiscardableContent
  ( NSDiscardableContentOverrides(..)
  , defaultNSDiscardableContentOverrides
  , newNSDiscardableContent
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

-- | Overrides record for @\@protocol NSDiscardableContent@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSDiscardableContentOverrides = NSDiscardableContentOverrides
  { _beginContentAccess :: !(Maybe (IO Bool))
  , _endContentAccess :: !(Maybe (IO ()))
  , _discardContentIfPossible :: !(Maybe (IO ()))
  , _isContentDiscarded :: !(Maybe (IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultNSDiscardableContentOverrides :: NSDiscardableContentOverrides
defaultNSDiscardableContentOverrides = NSDiscardableContentOverrides
  { _beginContentAccess = Nothing
  , _endContentAccess = Nothing
  , _discardContentIfPossible = Nothing
  , _isContentDiscarded = Nothing
  }

foreign import ccall "wrapper"
  wrap_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO ()))

foreign import ccall "wrapper"
  wrap_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsDiscardableContentDelegateClass #-}
nsDiscardableContentDelegateClass :: Class
nsDiscardableContentDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSDiscardableContent" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_beginContentAccess = unSelector (mkSelector "beginContentAccess")
      sel_endContentAccess = unSelector (mkSelector "endContentAccess")
      sel_discardContentIfPossible = unSelector (mkSelector "discardContentIfPossible")
      sel_isContentDiscarded = unSelector (mkSelector "isContentDiscarded")
  -- beginContentAccess
  stub_0 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSDiscardableContentOverrides
    case _beginContentAccess rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "beginContentAccess" "B@:" stub_0

  -- endContentAccess
  stub_1 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSDiscardableContentOverrides
    case _endContentAccess rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "endContentAccess" "v@:" stub_1

  -- discardContentIfPossible
  stub_2 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSDiscardableContentOverrides
    case _discardContentIfPossible rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "discardContentIfPossible" "v@:" stub_2

  -- isContentDiscarded
  stub_3 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSDiscardableContentOverrides
    case _isContentDiscarded rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "isContentDiscarded" "B@:" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSDiscardableContentOverrides
    if queriedSel == sel_beginContentAccess then pure (maybe 0 (const 1) (_beginContentAccess rec_))
    else if queriedSel == sel_endContentAccess then pure (maybe 0 (const 1) (_endContentAccess rec_))
    else if queriedSel == sel_discardContentIfPossible then pure (maybe 0 (const 1) (_discardContentIfPossible rec_))
    else if queriedSel == sel_isContentDiscarded then pure (maybe 0 (const 1) (_isContentDiscarded rec_))
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
newNSDiscardableContent :: NSDiscardableContentOverrides -> IO RawId
newNSDiscardableContent overrides = do
  inst <- class_createInstance nsDiscardableContentDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
