{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSTextElementProvider@.
--
-- Usage:
--
-- @
-- delegate <- newNSTextElementProvider defaultNSTextElementProviderOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSTextElementProvider
  ( NSTextElementProviderOverrides(..)
  , defaultNSTextElementProviderOverrides
  , newNSTextElementProvider
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

-- | Overrides record for @\@protocol NSTextElementProvider@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSTextElementProviderOverrides = NSTextElementProviderOverrides
  { _replaceContentsInRange_withTextElements :: !(Maybe (RawId -> RawId -> IO ()))
  , _locationFromLocation_withOffset :: !(Maybe (RawId -> Int -> IO RawId))
  , _offsetFromLocation_toLocation :: !(Maybe (RawId -> RawId -> IO Int))
  , _adjustedRangeFromRange_forEditingTextSelection :: !(Maybe (RawId -> Bool -> IO RawId))
  , _documentRange :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultNSTextElementProviderOverrides :: NSTextElementProviderOverrides
defaultNSTextElementProviderOverrides = NSTextElementProviderOverrides
  { _replaceContentsInRange_withTextElements = Nothing
  , _locationFromLocation_withOffset = Nothing
  , _offsetFromLocation_toLocation = Nothing
  , _adjustedRangeFromRange_forEditingTextSelection = Nothing
  , _documentRange = Nothing
  }

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_B_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CLong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CLong))

foreign import ccall "wrapper"
  wrap_at_q_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsTextElementProviderDelegateClass #-}
nsTextElementProviderDelegateClass :: Class
nsTextElementProviderDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSTextElementProvider" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_replaceContentsInRange_withTextElements = unSelector (mkSelector "replaceContentsInRange:withTextElements:")
      sel_locationFromLocation_withOffset = unSelector (mkSelector "locationFromLocation:withOffset:")
      sel_offsetFromLocation_toLocation = unSelector (mkSelector "offsetFromLocation:toLocation:")
      sel_adjustedRangeFromRange_forEditingTextSelection = unSelector (mkSelector "adjustedRangeFromRange:forEditingTextSelection:")
      sel_documentRange = unSelector (mkSelector "documentRange")
  -- replaceContentsInRange:withTextElements:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextElementProviderOverrides
    case _replaceContentsInRange_withTextElements rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "replaceContentsInRange:withTextElements:" "v@:@@" stub_0

  -- locationFromLocation:withOffset:
  stub_1 <- wrap_at_q_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextElementProviderOverrides
    case _locationFromLocation_withOffset rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (fromIntegral arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "locationFromLocation:withOffset:" "@@:@q" stub_1

  -- offsetFromLocation:toLocation:
  stub_2 <- wrap_at_at_q $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextElementProviderOverrides
    case _offsetFromLocation_toLocation rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (fromIntegral r)
  addObjCMethod cls "offsetFromLocation:toLocation:" "q@:@@" stub_2

  -- adjustedRangeFromRange:forEditingTextSelection:
  stub_3 <- wrap_at_B_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextElementProviderOverrides
    case _adjustedRangeFromRange_forEditingTextSelection rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (arg1 /= 0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "adjustedRangeFromRange:forEditingTextSelection:" "@@:@B" stub_3

  -- documentRange
  stub_4 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextElementProviderOverrides
    case _documentRange rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "documentRange" "@@:" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTextElementProviderOverrides
    if queriedSel == sel_replaceContentsInRange_withTextElements then pure (maybe 0 (const 1) (_replaceContentsInRange_withTextElements rec_))
    else if queriedSel == sel_locationFromLocation_withOffset then pure (maybe 0 (const 1) (_locationFromLocation_withOffset rec_))
    else if queriedSel == sel_offsetFromLocation_toLocation then pure (maybe 0 (const 1) (_offsetFromLocation_toLocation rec_))
    else if queriedSel == sel_adjustedRangeFromRange_forEditingTextSelection then pure (maybe 0 (const 1) (_adjustedRangeFromRange_forEditingTextSelection rec_))
    else if queriedSel == sel_documentRange then pure (maybe 0 (const 1) (_documentRange rec_))
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
newNSTextElementProvider :: NSTextElementProviderOverrides -> IO RawId
newNSTextElementProvider overrides = do
  inst <- class_createInstance nsTextElementProviderDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
