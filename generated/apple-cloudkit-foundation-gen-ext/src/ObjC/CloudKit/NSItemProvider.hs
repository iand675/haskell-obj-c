{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Use this method when you want to share a collection of @CKRecords@ but don't currently have a @CKShare.@ When the @preparationHandler@ is called, you should create a new @CKShare@ with the appropriate root @CKRecord@ or @CKRecordZoneID.@ After ensuring the share and all records have been saved to the server, invoke the @preparationCompletionHandler@ with either the resulting @CKShare,@ or an @NSError@ if saving failed. Invoking the share sheet with a @CKShare@ registered with this method will prompt the user to start sharing.
--
-- ObjC selector: @- registerCKShareWithContainer:allowedSharingOptions:preparationHandler:@
registerCKShareWithContainer_allowedSharingOptions_preparationHandler :: (IsNSItemProvider nsItemProvider, IsCKContainer container, IsCKAllowedSharingOptions allowedOptions) => nsItemProvider -> container -> allowedOptions -> Ptr () -> IO ()
registerCKShareWithContainer_allowedSharingOptions_preparationHandler nsItemProvider container allowedOptions preparationHandler =
  sendMessage nsItemProvider registerCKShareWithContainer_allowedSharingOptions_preparationHandlerSelector (toCKContainer container) (toCKAllowedSharingOptions allowedOptions) preparationHandler

-- | Use this method when you have a @CKShare@ that is already saved to the server. Invoking the share sheet with a @CKShare@ registered with this method will allow the owner to make modifications to the share settings, or will allow a participant to view the share settings.
--
-- ObjC selector: @- registerCKShare:container:allowedSharingOptions:@
registerCKShare_container_allowedSharingOptions :: (IsNSItemProvider nsItemProvider, IsCKShare share, IsCKContainer container, IsCKAllowedSharingOptions allowedOptions) => nsItemProvider -> share -> container -> allowedOptions -> IO ()
registerCKShare_container_allowedSharingOptions nsItemProvider share container allowedOptions =
  sendMessage nsItemProvider registerCKShare_container_allowedSharingOptionsSelector (toCKShare share) (toCKContainer container) (toCKAllowedSharingOptions allowedOptions)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @registerCKShareWithContainer:allowedSharingOptions:preparationHandler:@
registerCKShareWithContainer_allowedSharingOptions_preparationHandlerSelector :: Selector '[Id CKContainer, Id CKAllowedSharingOptions, Ptr ()] ()
registerCKShareWithContainer_allowedSharingOptions_preparationHandlerSelector = mkSelector "registerCKShareWithContainer:allowedSharingOptions:preparationHandler:"

-- | @Selector@ for @registerCKShare:container:allowedSharingOptions:@
registerCKShare_container_allowedSharingOptionsSelector :: Selector '[Id CKShare, Id CKContainer, Id CKAllowedSharingOptions] ()
registerCKShare_container_allowedSharingOptionsSelector = mkSelector "registerCKShare:container:allowedSharingOptions:"

