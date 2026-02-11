{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSItemProvider@.
module ObjC.AppKit.NSItemProvider
  ( NSItemProvider
  , IsNSItemProvider(..)
  , registerCloudKitShareWithPreparationHandler
  , registerCloudKitShare_container
  , registerCloudKitShareWithPreparationHandlerSelector
  , registerCloudKitShare_containerSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes

-- | Use this method when you want to share a collection of CKRecords but don't currently have a CKShare. When the preparationHandler is called, you should create a new CKShare with the appropriate root CKRecord. After ensuring the share and all records have been saved to the server, invoke the preparationCompletionHandler with either the resulting CKShare and its CKContainer, or an NSError if saving failed. Invoking the service with a CKShare registered with this method will prompt the user to start sharing.
--
-- ObjC selector: @- registerCloudKitShareWithPreparationHandler:@
registerCloudKitShareWithPreparationHandler :: IsNSItemProvider nsItemProvider => nsItemProvider -> Ptr () -> IO ()
registerCloudKitShareWithPreparationHandler nsItemProvider  preparationHandler =
    sendMsg nsItemProvider (mkSelector "registerCloudKitShareWithPreparationHandler:") retVoid [argPtr (castPtr preparationHandler :: Ptr ())]

-- | Use this method when you have a CKShare that is already saved to the server. Invoking the service with a CKShare registerd with this method will allow the owner to make modifications to the share settings, or will allow a participant to view the share settings.
--
-- ObjC selector: @- registerCloudKitShare:container:@
registerCloudKitShare_container :: IsNSItemProvider nsItemProvider => nsItemProvider -> RawId -> RawId -> IO ()
registerCloudKitShare_container nsItemProvider  share container =
    sendMsg nsItemProvider (mkSelector "registerCloudKitShare:container:") retVoid [argPtr (castPtr (unRawId share) :: Ptr ()), argPtr (castPtr (unRawId container) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @registerCloudKitShareWithPreparationHandler:@
registerCloudKitShareWithPreparationHandlerSelector :: Selector
registerCloudKitShareWithPreparationHandlerSelector = mkSelector "registerCloudKitShareWithPreparationHandler:"

-- | @Selector@ for @registerCloudKitShare:container:@
registerCloudKitShare_containerSelector :: Selector
registerCloudKitShare_containerSelector = mkSelector "registerCloudKitShare:container:"

