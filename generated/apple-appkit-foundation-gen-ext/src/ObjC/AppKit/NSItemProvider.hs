{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSItemProvider@.
module ObjC.AppKit.NSItemProvider
  ( NSItemProvider
  , IsNSItemProvider(..)
  , registerCloudKitShareWithPreparationHandler
  , registerCloudKitShare_container
  , sourceFrame
  , containerFrame
  , preferredPresentationSize
  , containerFrameSelector
  , preferredPresentationSizeSelector
  , registerCloudKitShareWithPreparationHandlerSelector
  , registerCloudKitShare_containerSelector
  , sourceFrameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Use this method when you want to share a collection of CKRecords but don't currently have a CKShare. When the preparationHandler is called, you should create a new CKShare with the appropriate root CKRecord. After ensuring the share and all records have been saved to the server, invoke the preparationCompletionHandler with either the resulting CKShare and its CKContainer, or an NSError if saving failed. Invoking the service with a CKShare registered with this method will prompt the user to start sharing.
--
-- ObjC selector: @- registerCloudKitShareWithPreparationHandler:@
registerCloudKitShareWithPreparationHandler :: IsNSItemProvider nsItemProvider => nsItemProvider -> Ptr () -> IO ()
registerCloudKitShareWithPreparationHandler nsItemProvider preparationHandler =
  sendMessage nsItemProvider registerCloudKitShareWithPreparationHandlerSelector preparationHandler

-- | Use this method when you have a CKShare that is already saved to the server. Invoking the service with a CKShare registerd with this method will allow the owner to make modifications to the share settings, or will allow a participant to view the share settings.
--
-- ObjC selector: @- registerCloudKitShare:container:@
registerCloudKitShare_container :: (IsNSItemProvider nsItemProvider, IsCKShare share, IsCKContainer container) => nsItemProvider -> share -> container -> IO ()
registerCloudKitShare_container nsItemProvider share container =
  sendMessage nsItemProvider registerCloudKitShare_containerSelector (toCKShare share) (toCKContainer container)

-- | @- sourceFrame@
sourceFrame :: IsNSItemProvider nsItemProvider => nsItemProvider -> IO NSRect
sourceFrame nsItemProvider =
  sendMessage nsItemProvider sourceFrameSelector

-- | @- containerFrame@
containerFrame :: IsNSItemProvider nsItemProvider => nsItemProvider -> IO NSRect
containerFrame nsItemProvider =
  sendMessage nsItemProvider containerFrameSelector

-- | @- preferredPresentationSize@
preferredPresentationSize :: IsNSItemProvider nsItemProvider => nsItemProvider -> IO NSSize
preferredPresentationSize nsItemProvider =
  sendMessage nsItemProvider preferredPresentationSizeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @registerCloudKitShareWithPreparationHandler:@
registerCloudKitShareWithPreparationHandlerSelector :: Selector '[Ptr ()] ()
registerCloudKitShareWithPreparationHandlerSelector = mkSelector "registerCloudKitShareWithPreparationHandler:"

-- | @Selector@ for @registerCloudKitShare:container:@
registerCloudKitShare_containerSelector :: Selector '[Id CKShare, Id CKContainer] ()
registerCloudKitShare_containerSelector = mkSelector "registerCloudKitShare:container:"

-- | @Selector@ for @sourceFrame@
sourceFrameSelector :: Selector '[] NSRect
sourceFrameSelector = mkSelector "sourceFrame"

-- | @Selector@ for @containerFrame@
containerFrameSelector :: Selector '[] NSRect
containerFrameSelector = mkSelector "containerFrame"

-- | @Selector@ for @preferredPresentationSize@
preferredPresentationSizeSelector :: Selector '[] NSSize
preferredPresentationSizeSelector = mkSelector "preferredPresentationSize"

