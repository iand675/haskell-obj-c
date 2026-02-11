{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSCollectionViewElement@.
--
-- Usage:
--
-- @
-- delegate <- newNSCollectionViewElement defaultNSCollectionViewElementOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSCollectionViewElement
  ( NSCollectionViewElementOverrides(..)
  , defaultNSCollectionViewElementOverrides
  , newNSCollectionViewElement
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

-- | Overrides record for @\@protocol NSCollectionViewElement@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSCollectionViewElementOverrides = NSCollectionViewElementOverrides
  { _prepareForReuse :: !(Maybe (IO ()))
  , _applyLayoutAttributes :: !(Maybe (RawId -> IO ()))
  , _willTransitionFromLayout_toLayout :: !(Maybe (RawId -> RawId -> IO ()))
  , _didTransitionFromLayout_toLayout :: !(Maybe (RawId -> RawId -> IO ()))
  , _preferredLayoutAttributesFittingAttributes :: !(Maybe (RawId -> IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultNSCollectionViewElementOverrides :: NSCollectionViewElementOverrides
defaultNSCollectionViewElementOverrides = NSCollectionViewElementOverrides
  { _prepareForReuse = Nothing
  , _applyLayoutAttributes = Nothing
  , _willTransitionFromLayout_toLayout = Nothing
  , _didTransitionFromLayout_toLayout = Nothing
  , _preferredLayoutAttributesFittingAttributes = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsCollectionViewElementDelegateClass #-}
nsCollectionViewElementDelegateClass :: Class
nsCollectionViewElementDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSCollectionViewElement" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_prepareForReuse = unSelector (mkSelector "prepareForReuse")
      sel_applyLayoutAttributes = unSelector (mkSelector "applyLayoutAttributes:")
      sel_willTransitionFromLayout_toLayout = unSelector (mkSelector "willTransitionFromLayout:toLayout:")
      sel_didTransitionFromLayout_toLayout = unSelector (mkSelector "didTransitionFromLayout:toLayout:")
      sel_preferredLayoutAttributesFittingAttributes = unSelector (mkSelector "preferredLayoutAttributesFittingAttributes:")
  -- prepareForReuse
  stub_0 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewElementOverrides
    case _prepareForReuse rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "prepareForReuse" "v@:" stub_0

  -- applyLayoutAttributes:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewElementOverrides
    case _applyLayoutAttributes rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "applyLayoutAttributes:" "v@:@" stub_1

  -- willTransitionFromLayout:toLayout:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewElementOverrides
    case _willTransitionFromLayout_toLayout rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "willTransitionFromLayout:toLayout:" "v@:@@" stub_2

  -- didTransitionFromLayout:toLayout:
  stub_3 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewElementOverrides
    case _didTransitionFromLayout_toLayout rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "didTransitionFromLayout:toLayout:" "v@:@@" stub_3

  -- preferredLayoutAttributesFittingAttributes:
  stub_4 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewElementOverrides
    case _preferredLayoutAttributesFittingAttributes rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "preferredLayoutAttributesFittingAttributes:" "@@:@" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewElementOverrides
    if queriedSel == sel_prepareForReuse then pure (maybe 0 (const 1) (_prepareForReuse rec_))
    else if queriedSel == sel_applyLayoutAttributes then pure (maybe 0 (const 1) (_applyLayoutAttributes rec_))
    else if queriedSel == sel_willTransitionFromLayout_toLayout then pure (maybe 0 (const 1) (_willTransitionFromLayout_toLayout rec_))
    else if queriedSel == sel_didTransitionFromLayout_toLayout then pure (maybe 0 (const 1) (_didTransitionFromLayout_toLayout rec_))
    else if queriedSel == sel_preferredLayoutAttributesFittingAttributes then pure (maybe 0 (const 1) (_preferredLayoutAttributesFittingAttributes rec_))
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
newNSCollectionViewElement :: NSCollectionViewElementOverrides -> IO RawId
newNSCollectionViewElement overrides = do
  inst <- class_createInstance nsCollectionViewElementDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
