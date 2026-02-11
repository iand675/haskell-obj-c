{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSCollectionViewSectionHeaderView@.
--
-- Usage:
--
-- @
-- delegate <- newNSCollectionViewSectionHeaderView defaultNSCollectionViewSectionHeaderViewOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSCollectionViewSectionHeaderView
  ( NSCollectionViewSectionHeaderViewOverrides(..)
  , defaultNSCollectionViewSectionHeaderViewOverrides
  , newNSCollectionViewSectionHeaderView
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

-- | Overrides record for @\@protocol NSCollectionViewSectionHeaderView@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSCollectionViewSectionHeaderViewOverrides = NSCollectionViewSectionHeaderViewOverrides
  { _sectionCollapseButton :: !(Maybe (IO RawId))
  , _setSectionCollapseButton :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSCollectionViewSectionHeaderViewOverrides :: NSCollectionViewSectionHeaderViewOverrides
defaultNSCollectionViewSectionHeaderViewOverrides = NSCollectionViewSectionHeaderViewOverrides
  { _sectionCollapseButton = Nothing
  , _setSectionCollapseButton = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsCollectionViewSectionHeaderViewDelegateClass #-}
nsCollectionViewSectionHeaderViewDelegateClass :: Class
nsCollectionViewSectionHeaderViewDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSCollectionViewSectionHeaderView" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_sectionCollapseButton = unSelector (mkSelector "sectionCollapseButton")
      sel_setSectionCollapseButton = unSelector (mkSelector "setSectionCollapseButton:")
  -- sectionCollapseButton
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewSectionHeaderViewOverrides
    case _sectionCollapseButton rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "sectionCollapseButton" "@@:" stub_0

  -- setSectionCollapseButton:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewSectionHeaderViewOverrides
    case _setSectionCollapseButton rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setSectionCollapseButton:" "v@:@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewSectionHeaderViewOverrides
    if queriedSel == sel_sectionCollapseButton then pure (maybe 0 (const 1) (_sectionCollapseButton rec_))
    else if queriedSel == sel_setSectionCollapseButton then pure (maybe 0 (const 1) (_setSectionCollapseButton rec_))
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
newNSCollectionViewSectionHeaderView :: NSCollectionViewSectionHeaderViewOverrides -> IO RawId
newNSCollectionViewSectionHeaderView overrides = do
  inst <- class_createInstance nsCollectionViewSectionHeaderViewDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
