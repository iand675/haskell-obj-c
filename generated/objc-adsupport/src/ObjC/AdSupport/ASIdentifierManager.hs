{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The object that contains the advertising identifier.
--
-- Generated bindings for @ASIdentifierManager@.
module ObjC.AdSupport.ASIdentifierManager
  ( ASIdentifierManager
  , IsASIdentifierManager(..)
  , sharedManager
  , advertisingIdentifier
  , advertisingTrackingEnabled
  , sharedManagerSelector
  , advertisingIdentifierSelector
  , advertisingTrackingEnabledSelector


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

import ObjC.AdSupport.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The shared instance of the identifier manager class.
--
-- - Returns: Returns the shared instance of the AdSupport identifier manager class.
--
-- ObjC selector: @+ sharedManager@
sharedManager :: IO (Id ASIdentifierManager)
sharedManager  =
  do
    cls' <- getRequiredClass "ASIdentifierManager"
    sendClassMsg cls' (mkSelector "sharedManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The UUID that is specific to a device.
--
-- The ``ASIdentifierManager/advertisingIdentifier`` is an alphanumeric string that’s unique to each device, and which you only use for advertising. Use this string for frequency capping, attribution, conversion events, estimating the number of unique users, advertising fraud detection, and debugging. On devices running iOS 14.5 and later and iPadOS 14.5 and later, your app must request tracking authorization before it can get the advertising identifier. For more information on getting the advertising identifier, see ``AdSupport``.
--
-- The advertising identifier returns either a unique UUID, or all zeros. It returns a unique UUID in the following cases:
--
-- - If Settings > Privacy > Tracking > Allow Apps to Request to Track is On, you’ve requested tracking authorization from the user by calling the <doc://com.apple.documentation/documentation/apptrackingtransparency> APIs, and received authorization, indicated by <doc://com.apple.documentation/documentation/apptrackingtransparency/attrackingmanager/authorizationstatus/authorized>. - If the user changes Settings > Privacy > Tracking > Allow Apps to Request to Track to Off after authorizing your app, and leaves the permissions On for your app.
--
-- The advertising identifier returns all zeros (@00000000-0000-0000-0000-000000000000@) in the following cases:
--
-- - In Simulator, regardless of any settings. - When you call this API on a device running macOS.  - When you call this API in a compatible iPad or iPhone app running in visionOS. - On devices running iOS 14.5 and later and iPadOS 14.5 and later, if you haven’t requested authorization using the <doc://com.apple.documentation/documentation/apptrackingtransparency> framework. - If you’ve requested authorization using the <doc://com.apple.documentation/documentation/apptrackingtransparency> framework and the user declines, which results in an authorization status of <doc://com.apple.documentation/documentation/apptrackingtransparency/attrackingmanager/authorizationstatus/denied>. - When a profile or configuration restricts access to the advertising identifier. For more information about restrictions, see <doc://com.apple.documentation/documentation/apptrackingtransparency/attrackingmanager/authorizationstatus/restricted>.
--
-- As a best practice, don’t store the advertising identifier value; access ``ASIdentifierManager/advertisingIdentifier`` instead. Users can change their authorization for tracking at any time in Settings > Privacy > Tracking. Check your app’s authorization using the App Tracking Transparency API <doc://com.apple.documentation/documentation/apptrackingtransparency/attrackingmanager/3547038-trackingauthorizationstatus> to determine the user’s intent.
--
-- For more information about asking users for permission to track, see [User Privacy and Data Use](https://developer.apple.com/app-store/user-privacy-and-data-use/).
--
-- ObjC selector: @- advertisingIdentifier@
advertisingIdentifier :: IsASIdentifierManager asIdentifierManager => asIdentifierManager -> IO (Id NSUUID)
advertisingIdentifier asIdentifierManager  =
  sendMsg asIdentifierManager (mkSelector "advertisingIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A Boolean value that indicates whether the user has limited ad tracking enabled.
--
-- - Warning: This property is deprecated. Functionality has been replaced by the <doc://com.apple.documentation/documentation/apptrackingtransparency> framework.
--
-- ObjC selector: @- advertisingTrackingEnabled@
advertisingTrackingEnabled :: IsASIdentifierManager asIdentifierManager => asIdentifierManager -> IO Bool
advertisingTrackingEnabled asIdentifierManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg asIdentifierManager (mkSelector "advertisingTrackingEnabled") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedManager@
sharedManagerSelector :: Selector
sharedManagerSelector = mkSelector "sharedManager"

-- | @Selector@ for @advertisingIdentifier@
advertisingIdentifierSelector :: Selector
advertisingIdentifierSelector = mkSelector "advertisingIdentifier"

-- | @Selector@ for @advertisingTrackingEnabled@
advertisingTrackingEnabledSelector :: Selector
advertisingTrackingEnabledSelector = mkSelector "advertisingTrackingEnabled"

