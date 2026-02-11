{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSItemProvider@.
module ObjC.CloudKit.NSItemProvider
  ( NSItemProvider
  , IsNSItemProvider(..)
  , registerCKShareWithContainer_allowedSharingOptions_preparationHandler
  , registerCKShare_container_allowedSharingOptions
  , registerCKShareWithContainer_allowedSharingOptions_preparationHandlerSelector
  , registerCKShare_container_allowedSharingOptionsSelector


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

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Use this method when you want to share a collection of @CKRecords@ but don't currently have a @CKShare.@ When the @preparationHandler@ is called, you should create a new @CKShare@ with the appropriate root @CKRecord@ or @CKRecordZoneID.@ After ensuring the share and all records have been saved to the server, invoke the @preparationCompletionHandler@ with either the resulting @CKShare,@ or an @NSError@ if saving failed. Invoking the share sheet with a @CKShare@ registered with this method will prompt the user to start sharing.
--
-- ObjC selector: @- registerCKShareWithContainer:allowedSharingOptions:preparationHandler:@
registerCKShareWithContainer_allowedSharingOptions_preparationHandler :: (IsNSItemProvider nsItemProvider, IsCKContainer container, IsCKAllowedSharingOptions allowedOptions) => nsItemProvider -> container -> allowedOptions -> Ptr () -> IO ()
registerCKShareWithContainer_allowedSharingOptions_preparationHandler nsItemProvider  container allowedOptions preparationHandler =
withObjCPtr container $ \raw_container ->
  withObjCPtr allowedOptions $ \raw_allowedOptions ->
      sendMsg nsItemProvider (mkSelector "registerCKShareWithContainer:allowedSharingOptions:preparationHandler:") retVoid [argPtr (castPtr raw_container :: Ptr ()), argPtr (castPtr raw_allowedOptions :: Ptr ()), argPtr (castPtr preparationHandler :: Ptr ())]

-- | Use this method when you have a @CKShare@ that is already saved to the server. Invoking the share sheet with a @CKShare@ registered with this method will allow the owner to make modifications to the share settings, or will allow a participant to view the share settings.
--
-- ObjC selector: @- registerCKShare:container:allowedSharingOptions:@
registerCKShare_container_allowedSharingOptions :: (IsNSItemProvider nsItemProvider, IsCKShare share, IsCKContainer container, IsCKAllowedSharingOptions allowedOptions) => nsItemProvider -> share -> container -> allowedOptions -> IO ()
registerCKShare_container_allowedSharingOptions nsItemProvider  share container allowedOptions =
withObjCPtr share $ \raw_share ->
  withObjCPtr container $ \raw_container ->
    withObjCPtr allowedOptions $ \raw_allowedOptions ->
        sendMsg nsItemProvider (mkSelector "registerCKShare:container:allowedSharingOptions:") retVoid [argPtr (castPtr raw_share :: Ptr ()), argPtr (castPtr raw_container :: Ptr ()), argPtr (castPtr raw_allowedOptions :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @registerCKShareWithContainer:allowedSharingOptions:preparationHandler:@
registerCKShareWithContainer_allowedSharingOptions_preparationHandlerSelector :: Selector
registerCKShareWithContainer_allowedSharingOptions_preparationHandlerSelector = mkSelector "registerCKShareWithContainer:allowedSharingOptions:preparationHandler:"

-- | @Selector@ for @registerCKShare:container:allowedSharingOptions:@
registerCKShare_container_allowedSharingOptionsSelector :: Selector
registerCKShare_container_allowedSharingOptionsSelector = mkSelector "registerCKShare:container:allowedSharingOptions:"

