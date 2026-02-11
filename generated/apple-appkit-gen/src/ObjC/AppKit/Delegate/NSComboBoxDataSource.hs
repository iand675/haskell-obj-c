{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSComboBoxDataSource@.
--
-- Usage:
--
-- @
-- delegate <- newNSComboBoxDataSource defaultNSComboBoxDataSourceOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSComboBoxDataSource
  ( NSComboBoxDataSourceOverrides(..)
  , defaultNSComboBoxDataSourceOverrides
  , newNSComboBoxDataSource
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

-- | Overrides record for @\@protocol NSComboBoxDataSource@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSComboBoxDataSourceOverrides = NSComboBoxDataSourceOverrides
  { _numberOfItemsInComboBox :: !(Maybe (RawId -> IO Int))
  , _comboBox_objectValueForItemAtIndex :: !(Maybe (RawId -> Int -> IO RawId))
  , _comboBox_indexOfItemWithStringValue :: !(Maybe (RawId -> RawId -> IO Int))
  , _comboBox_completedString :: !(Maybe (RawId -> RawId -> IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultNSComboBoxDataSourceOverrides :: NSComboBoxDataSourceOverrides
defaultNSComboBoxDataSourceOverrides = NSComboBoxDataSourceOverrides
  { _numberOfItemsInComboBox = Nothing
  , _comboBox_objectValueForItemAtIndex = Nothing
  , _comboBox_indexOfItemWithStringValue = Nothing
  , _comboBox_completedString = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_Q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_q_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CLong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CLong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsComboBoxDataSourceDelegateClass #-}
nsComboBoxDataSourceDelegateClass :: Class
nsComboBoxDataSourceDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSComboBoxDataSource" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_numberOfItemsInComboBox = unSelector (mkSelector "numberOfItemsInComboBox:")
      sel_comboBox_objectValueForItemAtIndex = unSelector (mkSelector "comboBox:objectValueForItemAtIndex:")
      sel_comboBox_indexOfItemWithStringValue = unSelector (mkSelector "comboBox:indexOfItemWithStringValue:")
      sel_comboBox_completedString = unSelector (mkSelector "comboBox:completedString:")
  -- numberOfItemsInComboBox:
  stub_0 <- wrap_at_q $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSComboBoxDataSourceOverrides
    case _numberOfItemsInComboBox rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (fromIntegral r)
  addObjCMethod cls "numberOfItemsInComboBox:" "q@:@" stub_0

  -- comboBox:objectValueForItemAtIndex:
  stub_1 <- wrap_at_q_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSComboBoxDataSourceOverrides
    case _comboBox_objectValueForItemAtIndex rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (fromIntegral arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "comboBox:objectValueForItemAtIndex:" "@@:@q" stub_1

  -- comboBox:indexOfItemWithStringValue:
  stub_2 <- wrap_at_at_Q $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSComboBoxDataSourceOverrides
    case _comboBox_indexOfItemWithStringValue rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (fromIntegral r)
  addObjCMethod cls "comboBox:indexOfItemWithStringValue:" "Q@:@@" stub_2

  -- comboBox:completedString:
  stub_3 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSComboBoxDataSourceOverrides
    case _comboBox_completedString rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "comboBox:completedString:" "@@:@@" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSComboBoxDataSourceOverrides
    if queriedSel == sel_numberOfItemsInComboBox then pure (maybe 0 (const 1) (_numberOfItemsInComboBox rec_))
    else if queriedSel == sel_comboBox_objectValueForItemAtIndex then pure (maybe 0 (const 1) (_comboBox_objectValueForItemAtIndex rec_))
    else if queriedSel == sel_comboBox_indexOfItemWithStringValue then pure (maybe 0 (const 1) (_comboBox_indexOfItemWithStringValue rec_))
    else if queriedSel == sel_comboBox_completedString then pure (maybe 0 (const 1) (_comboBox_completedString rec_))
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
newNSComboBoxDataSource :: NSComboBoxDataSourceOverrides -> IO RawId
newNSComboBoxDataSource overrides = do
  inst <- class_createInstance nsComboBoxDataSourceDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
