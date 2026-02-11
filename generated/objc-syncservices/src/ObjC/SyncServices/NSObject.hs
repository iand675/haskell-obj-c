{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSObject@.
module ObjC.SyncServices.NSObject
  ( NSObject
  , IsNSObject(..)
  , sessionDriver_didRegisterClientAndReturnError
  , sessionDriver_willNegotiateAndReturnError
  , sessionDriver_didNegotiateAndReturnError
  , sessionDriver_willPushAndReturnError
  , sessionDriver_didPushAndReturnError
  , sessionDriver_willPullAndReturnError
  , sessionDriver_didPullAndReturnError
  , sessionDriver_willFinishSessionAndReturnError
  , sessionDriverDidFinishSession
  , sessionDriverWillCancelSession
  , sessionDriverDidCancelSession
  , sessionDriver_didReceiveSyncAlertAndReturnError
  , sessionDriver_didRegisterClientAndReturnErrorSelector
  , sessionDriver_willNegotiateAndReturnErrorSelector
  , sessionDriver_didNegotiateAndReturnErrorSelector
  , sessionDriver_willPushAndReturnErrorSelector
  , sessionDriver_didPushAndReturnErrorSelector
  , sessionDriver_willPullAndReturnErrorSelector
  , sessionDriver_didPullAndReturnErrorSelector
  , sessionDriver_willFinishSessionAndReturnErrorSelector
  , sessionDriverDidFinishSessionSelector
  , sessionDriverWillCancelSessionSelector
  , sessionDriverDidCancelSessionSelector
  , sessionDriver_didReceiveSyncAlertAndReturnErrorSelector


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

import ObjC.SyncServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- sessionDriver:didRegisterClientAndReturnError:@
sessionDriver_didRegisterClientAndReturnError :: (IsNSObject nsObject, IsISyncSessionDriver sender, IsNSError outError) => nsObject -> sender -> outError -> IO Bool
sessionDriver_didRegisterClientAndReturnError nsObject  sender outError =
withObjCPtr sender $ \raw_sender ->
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "sessionDriver:didRegisterClientAndReturnError:") retCULong [argPtr (castPtr raw_sender :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | @- sessionDriver:willNegotiateAndReturnError:@
sessionDriver_willNegotiateAndReturnError :: (IsNSObject nsObject, IsISyncSessionDriver sender, IsNSError outError) => nsObject -> sender -> outError -> IO Bool
sessionDriver_willNegotiateAndReturnError nsObject  sender outError =
withObjCPtr sender $ \raw_sender ->
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "sessionDriver:willNegotiateAndReturnError:") retCULong [argPtr (castPtr raw_sender :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | @- sessionDriver:didNegotiateAndReturnError:@
sessionDriver_didNegotiateAndReturnError :: (IsNSObject nsObject, IsISyncSessionDriver sender, IsNSError outError) => nsObject -> sender -> outError -> IO Bool
sessionDriver_didNegotiateAndReturnError nsObject  sender outError =
withObjCPtr sender $ \raw_sender ->
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "sessionDriver:didNegotiateAndReturnError:") retCULong [argPtr (castPtr raw_sender :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | @- sessionDriver:willPushAndReturnError:@
sessionDriver_willPushAndReturnError :: (IsNSObject nsObject, IsISyncSessionDriver sender, IsNSError outError) => nsObject -> sender -> outError -> IO Bool
sessionDriver_willPushAndReturnError nsObject  sender outError =
withObjCPtr sender $ \raw_sender ->
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "sessionDriver:willPushAndReturnError:") retCULong [argPtr (castPtr raw_sender :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | @- sessionDriver:didPushAndReturnError:@
sessionDriver_didPushAndReturnError :: (IsNSObject nsObject, IsISyncSessionDriver sender, IsNSError outError) => nsObject -> sender -> outError -> IO Bool
sessionDriver_didPushAndReturnError nsObject  sender outError =
withObjCPtr sender $ \raw_sender ->
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "sessionDriver:didPushAndReturnError:") retCULong [argPtr (castPtr raw_sender :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | @- sessionDriver:willPullAndReturnError:@
sessionDriver_willPullAndReturnError :: (IsNSObject nsObject, IsISyncSessionDriver sender, IsNSError outError) => nsObject -> sender -> outError -> IO Bool
sessionDriver_willPullAndReturnError nsObject  sender outError =
withObjCPtr sender $ \raw_sender ->
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "sessionDriver:willPullAndReturnError:") retCULong [argPtr (castPtr raw_sender :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | @- sessionDriver:didPullAndReturnError:@
sessionDriver_didPullAndReturnError :: (IsNSObject nsObject, IsISyncSessionDriver sender, IsNSError outError) => nsObject -> sender -> outError -> IO Bool
sessionDriver_didPullAndReturnError nsObject  sender outError =
withObjCPtr sender $ \raw_sender ->
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "sessionDriver:didPullAndReturnError:") retCULong [argPtr (castPtr raw_sender :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | @- sessionDriver:willFinishSessionAndReturnError:@
sessionDriver_willFinishSessionAndReturnError :: (IsNSObject nsObject, IsISyncSessionDriver sender, IsNSError outError) => nsObject -> sender -> outError -> IO Bool
sessionDriver_willFinishSessionAndReturnError nsObject  sender outError =
withObjCPtr sender $ \raw_sender ->
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "sessionDriver:willFinishSessionAndReturnError:") retCULong [argPtr (castPtr raw_sender :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | @- sessionDriverDidFinishSession:@
sessionDriverDidFinishSession :: (IsNSObject nsObject, IsISyncSessionDriver sender) => nsObject -> sender -> IO ()
sessionDriverDidFinishSession nsObject  sender =
withObjCPtr sender $ \raw_sender ->
    sendMsg nsObject (mkSelector "sessionDriverDidFinishSession:") retVoid [argPtr (castPtr raw_sender :: Ptr ())]

-- | @- sessionDriverWillCancelSession:@
sessionDriverWillCancelSession :: (IsNSObject nsObject, IsISyncSessionDriver sender) => nsObject -> sender -> IO ()
sessionDriverWillCancelSession nsObject  sender =
withObjCPtr sender $ \raw_sender ->
    sendMsg nsObject (mkSelector "sessionDriverWillCancelSession:") retVoid [argPtr (castPtr raw_sender :: Ptr ())]

-- | @- sessionDriverDidCancelSession:@
sessionDriverDidCancelSession :: (IsNSObject nsObject, IsISyncSessionDriver sender) => nsObject -> sender -> IO ()
sessionDriverDidCancelSession nsObject  sender =
withObjCPtr sender $ \raw_sender ->
    sendMsg nsObject (mkSelector "sessionDriverDidCancelSession:") retVoid [argPtr (castPtr raw_sender :: Ptr ())]

-- | @- sessionDriver:didReceiveSyncAlertAndReturnError:@
sessionDriver_didReceiveSyncAlertAndReturnError :: (IsNSObject nsObject, IsISyncSessionDriver sender, IsNSError outError) => nsObject -> sender -> outError -> IO Bool
sessionDriver_didReceiveSyncAlertAndReturnError nsObject  sender outError =
withObjCPtr sender $ \raw_sender ->
  withObjCPtr outError $ \raw_outError ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "sessionDriver:didReceiveSyncAlertAndReturnError:") retCULong [argPtr (castPtr raw_sender :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sessionDriver:didRegisterClientAndReturnError:@
sessionDriver_didRegisterClientAndReturnErrorSelector :: Selector
sessionDriver_didRegisterClientAndReturnErrorSelector = mkSelector "sessionDriver:didRegisterClientAndReturnError:"

-- | @Selector@ for @sessionDriver:willNegotiateAndReturnError:@
sessionDriver_willNegotiateAndReturnErrorSelector :: Selector
sessionDriver_willNegotiateAndReturnErrorSelector = mkSelector "sessionDriver:willNegotiateAndReturnError:"

-- | @Selector@ for @sessionDriver:didNegotiateAndReturnError:@
sessionDriver_didNegotiateAndReturnErrorSelector :: Selector
sessionDriver_didNegotiateAndReturnErrorSelector = mkSelector "sessionDriver:didNegotiateAndReturnError:"

-- | @Selector@ for @sessionDriver:willPushAndReturnError:@
sessionDriver_willPushAndReturnErrorSelector :: Selector
sessionDriver_willPushAndReturnErrorSelector = mkSelector "sessionDriver:willPushAndReturnError:"

-- | @Selector@ for @sessionDriver:didPushAndReturnError:@
sessionDriver_didPushAndReturnErrorSelector :: Selector
sessionDriver_didPushAndReturnErrorSelector = mkSelector "sessionDriver:didPushAndReturnError:"

-- | @Selector@ for @sessionDriver:willPullAndReturnError:@
sessionDriver_willPullAndReturnErrorSelector :: Selector
sessionDriver_willPullAndReturnErrorSelector = mkSelector "sessionDriver:willPullAndReturnError:"

-- | @Selector@ for @sessionDriver:didPullAndReturnError:@
sessionDriver_didPullAndReturnErrorSelector :: Selector
sessionDriver_didPullAndReturnErrorSelector = mkSelector "sessionDriver:didPullAndReturnError:"

-- | @Selector@ for @sessionDriver:willFinishSessionAndReturnError:@
sessionDriver_willFinishSessionAndReturnErrorSelector :: Selector
sessionDriver_willFinishSessionAndReturnErrorSelector = mkSelector "sessionDriver:willFinishSessionAndReturnError:"

-- | @Selector@ for @sessionDriverDidFinishSession:@
sessionDriverDidFinishSessionSelector :: Selector
sessionDriverDidFinishSessionSelector = mkSelector "sessionDriverDidFinishSession:"

-- | @Selector@ for @sessionDriverWillCancelSession:@
sessionDriverWillCancelSessionSelector :: Selector
sessionDriverWillCancelSessionSelector = mkSelector "sessionDriverWillCancelSession:"

-- | @Selector@ for @sessionDriverDidCancelSession:@
sessionDriverDidCancelSessionSelector :: Selector
sessionDriverDidCancelSessionSelector = mkSelector "sessionDriverDidCancelSession:"

-- | @Selector@ for @sessionDriver:didReceiveSyncAlertAndReturnError:@
sessionDriver_didReceiveSyncAlertAndReturnErrorSelector :: Selector
sessionDriver_didReceiveSyncAlertAndReturnErrorSelector = mkSelector "sessionDriver:didReceiveSyncAlertAndReturnError:"

