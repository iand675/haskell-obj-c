{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSTextFinderBarContainer@.
--
-- Usage:
--
-- @
-- delegate <- newNSTextFinderBarContainer defaultNSTextFinderBarContainerOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSTextFinderBarContainer
  ( NSTextFinderBarContainerOverrides(..)
  , defaultNSTextFinderBarContainerOverrides
  , newNSTextFinderBarContainer
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

-- | Overrides record for @\@protocol NSTextFinderBarContainer@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSTextFinderBarContainerOverrides = NSTextFinderBarContainerOverrides
  { _findBarViewDidChangeHeight :: !(Maybe (IO ()))
  , _contentView :: !(Maybe (IO RawId))
  , _findBarView :: !(Maybe (IO RawId))
  , _setFindBarView :: !(Maybe (RawId -> IO ()))
  , _findBarVisible :: !(Maybe (IO Bool))
  , _setFindBarVisible :: !(Maybe (Bool -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSTextFinderBarContainerOverrides :: NSTextFinderBarContainerOverrides
defaultNSTextFinderBarContainerOverrides = NSTextFinderBarContainerOverrides
  { _findBarViewDidChangeHeight = Nothing
  , _contentView = Nothing
  , _findBarView = Nothing
  , _setFindBarView = Nothing
  , _findBarVisible = Nothing
  , _setFindBarVisible = Nothing
  }

foreign import ccall "wrapper"
  wrap_B_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsTextFinderBarContainerDelegateClass #-}
nsTextFinderBarContainerDelegateClass :: Class
nsTextFinderBarContainerDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSTextFinderBarContainer" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_findBarViewDidChangeHeight = unSelector (mkSelector "findBarViewDidChangeHeight")
      sel_contentView = unSelector (mkSelector "contentView")
      sel_findBarView = unSelector (mkSelector "findBarView")
      sel_setFindBarView = unSelector (mkSelector "setFindBarView:")
      sel_findBarVisible = unSelector (mkSelector "findBarVisible")
      sel_setFindBarVisible = unSelector (mkSelector "setFindBarVisible:")
  -- findBarViewDidChangeHeight
  stub_0 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextFinderBarContainerOverrides
    case _findBarViewDidChangeHeight rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "findBarViewDidChangeHeight" "v@:" stub_0

  -- contentView
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextFinderBarContainerOverrides
    case _contentView rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "contentView" "@@:" stub_1

  -- findBarView
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextFinderBarContainerOverrides
    case _findBarView rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "findBarView" "@@:" stub_2

  -- setFindBarView:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextFinderBarContainerOverrides
    case _setFindBarView rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setFindBarView:" "v@:@" stub_3

  -- findBarVisible
  stub_4 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextFinderBarContainerOverrides
    case _findBarVisible rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "findBarVisible" "B@:" stub_4

  -- setFindBarVisible:
  stub_5 <- wrap_B_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextFinderBarContainerOverrides
    case _setFindBarVisible rec_ of
      Nothing -> pure ()
      Just f -> f (arg0 /= 0)
  addObjCMethod cls "setFindBarVisible:" "v@:B" stub_5

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextFinderBarContainerOverrides
    if queriedSel == sel_findBarViewDidChangeHeight then pure (maybe 0 (const 1) (_findBarViewDidChangeHeight rec_))
    else if queriedSel == sel_contentView then pure (maybe 0 (const 1) (_contentView rec_))
    else if queriedSel == sel_findBarView then pure (maybe 0 (const 1) (_findBarView rec_))
    else if queriedSel == sel_setFindBarView then pure (maybe 0 (const 1) (_setFindBarView rec_))
    else if queriedSel == sel_findBarVisible then pure (maybe 0 (const 1) (_findBarVisible rec_))
    else if queriedSel == sel_setFindBarVisible then pure (maybe 0 (const 1) (_setFindBarVisible rec_))
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
newNSTextFinderBarContainer :: NSTextFinderBarContainerOverrides -> IO RawId
newNSTextFinderBarContainer overrides = do
  inst <- class_createInstance nsTextFinderBarContainerDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
