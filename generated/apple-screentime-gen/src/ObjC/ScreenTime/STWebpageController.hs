{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @STWebpageController@.
module ObjC.ScreenTime.STWebpageController
  ( STWebpageController
  , IsSTWebpageController(..)
  , setBundleIdentifier_error
  , initWithNibName_bundle
  , initWithCoder
  , suppressUsageRecording
  , setSuppressUsageRecording
  , url
  , setURL
  , urlIsPlayingVideo
  , setURLIsPlayingVideo
  , urlIsPictureInPicture
  , setURLIsPictureInPicture
  , urlIsBlocked
  , profileIdentifier
  , setProfileIdentifier
  , initWithCoderSelector
  , initWithNibName_bundleSelector
  , profileIdentifierSelector
  , setBundleIdentifier_errorSelector
  , setProfileIdentifierSelector
  , setSuppressUsageRecordingSelector
  , setURLIsPictureInPictureSelector
  , setURLIsPlayingVideoSelector
  , setURLSelector
  , suppressUsageRecordingSelector
  , urlIsBlockedSelector
  , urlIsPictureInPictureSelector
  , urlIsPlayingVideoSelector
  , urlSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ScreenTime.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Changes the bundle identifier used to report web usage.
--
-- This is only supported for web browsers that have been properly registered with Screen Time.
--
-- - Parameters:   - bundleIdentifier: The bundle identifier that can be changed to facilitate web usage     reporting for a parent web browser from one of its helper processes or extensions.   - error: Any error that occurred while changing the bundle identifier.
--
-- ObjC selector: @- setBundleIdentifier:error:@
setBundleIdentifier_error :: (IsSTWebpageController stWebpageController, IsNSString bundleIdentifier, IsNSError error_) => stWebpageController -> bundleIdentifier -> error_ -> IO Bool
setBundleIdentifier_error stWebpageController bundleIdentifier error_ =
  sendMessage stWebpageController setBundleIdentifier_errorSelector (toNSString bundleIdentifier) (toNSError error_)

-- | @- initWithNibName:bundle:@
initWithNibName_bundle :: (IsSTWebpageController stWebpageController, IsNSString nibNameOrNil, IsNSBundle nibBundleOrNil) => stWebpageController -> nibNameOrNil -> nibBundleOrNil -> IO (Id STWebpageController)
initWithNibName_bundle stWebpageController nibNameOrNil nibBundleOrNil =
  sendOwnedMessage stWebpageController initWithNibName_bundleSelector (toNSString nibNameOrNil) (toNSBundle nibBundleOrNil)

-- | @- initWithCoder:@
initWithCoder :: (IsSTWebpageController stWebpageController, IsNSCoder aDecoder) => stWebpageController -> aDecoder -> IO (Id STWebpageController)
initWithCoder stWebpageController aDecoder =
  sendOwnedMessage stWebpageController initWithCoderSelector (toNSCoder aDecoder)

-- | A Boolean that indicates whether the webpage controller is not recording web usage.
--
-- Set to <doc://com.apple.documentation/documentation/objectivec/yes> to stop recording and reporting web-usage data.
--
-- ObjC selector: @- suppressUsageRecording@
suppressUsageRecording :: IsSTWebpageController stWebpageController => stWebpageController -> IO Bool
suppressUsageRecording stWebpageController =
  sendMessage stWebpageController suppressUsageRecordingSelector

-- | A Boolean that indicates whether the webpage controller is not recording web usage.
--
-- Set to <doc://com.apple.documentation/documentation/objectivec/yes> to stop recording and reporting web-usage data.
--
-- ObjC selector: @- setSuppressUsageRecording:@
setSuppressUsageRecording :: IsSTWebpageController stWebpageController => stWebpageController -> Bool -> IO ()
setSuppressUsageRecording stWebpageController value =
  sendMessage stWebpageController setSuppressUsageRecordingSelector value

-- | The URL for the webpage.
--
-- Set this value to the webpage’s URL when the user navigates to a new URL.
--
-- ObjC selector: @- URL@
url :: IsSTWebpageController stWebpageController => stWebpageController -> IO (Id NSURL)
url stWebpageController =
  sendMessage stWebpageController urlSelector

-- | The URL for the webpage.
--
-- Set this value to the webpage’s URL when the user navigates to a new URL.
--
-- ObjC selector: @- setURL:@
setURL :: (IsSTWebpageController stWebpageController, IsNSURL value) => stWebpageController -> value -> IO ()
setURL stWebpageController value =
  sendMessage stWebpageController setURLSelector (toNSURL value)

-- | A Boolean that indicates whether there are one or more videos currently playing in the webpage.
--
-- The default value is <doc://com.apple.documentation/documentation/objectivec/no>. Set this value when the webpage starts or stops playing video.
--
-- - Important: Set this value to <doc://com.apple.documentation/documentation/objectivec/no> prior to changing ``ScreenTime/STWebpageController/URL`` if the new webpage at that URL stops currently playing media and won’t immediately start playing new media.
--
-- ObjC selector: @- URLIsPlayingVideo@
urlIsPlayingVideo :: IsSTWebpageController stWebpageController => stWebpageController -> IO Bool
urlIsPlayingVideo stWebpageController =
  sendMessage stWebpageController urlIsPlayingVideoSelector

-- | A Boolean that indicates whether there are one or more videos currently playing in the webpage.
--
-- The default value is <doc://com.apple.documentation/documentation/objectivec/no>. Set this value when the webpage starts or stops playing video.
--
-- - Important: Set this value to <doc://com.apple.documentation/documentation/objectivec/no> prior to changing ``ScreenTime/STWebpageController/URL`` if the new webpage at that URL stops currently playing media and won’t immediately start playing new media.
--
-- ObjC selector: @- setURLIsPlayingVideo:@
setURLIsPlayingVideo :: IsSTWebpageController stWebpageController => stWebpageController -> Bool -> IO ()
setURLIsPlayingVideo stWebpageController value =
  sendMessage stWebpageController setURLIsPlayingVideoSelector value

-- | A Boolean that indicates whether the webpage is currently displaying a floating picture in picture window.
--
-- The default value is <doc://com.apple.documentation/documentation/objectivec/no>. Set this value when the webpage starts or stops displaying a Picture in Picture window.
--
-- - Important: Set this value to <doc://com.apple.documentation/documentation/objectivec/no> prior to changing ``ScreenTime/STWebpageController/URL`` if the new webpage at that URL ends all currently displayed Picture in Picture windows, and won’t immediately display a new one.
--
-- ObjC selector: @- URLIsPictureInPicture@
urlIsPictureInPicture :: IsSTWebpageController stWebpageController => stWebpageController -> IO Bool
urlIsPictureInPicture stWebpageController =
  sendMessage stWebpageController urlIsPictureInPictureSelector

-- | A Boolean that indicates whether the webpage is currently displaying a floating picture in picture window.
--
-- The default value is <doc://com.apple.documentation/documentation/objectivec/no>. Set this value when the webpage starts or stops displaying a Picture in Picture window.
--
-- - Important: Set this value to <doc://com.apple.documentation/documentation/objectivec/no> prior to changing ``ScreenTime/STWebpageController/URL`` if the new webpage at that URL ends all currently displayed Picture in Picture windows, and won’t immediately display a new one.
--
-- ObjC selector: @- setURLIsPictureInPicture:@
setURLIsPictureInPicture :: IsSTWebpageController stWebpageController => stWebpageController -> Bool -> IO ()
setURLIsPictureInPicture stWebpageController value =
  sendMessage stWebpageController setURLIsPictureInPictureSelector value

-- | A Boolean that indicates whether a parent or guardian has blocked the URL.
--
-- When a parent or guardian blocks the webpage’s URL, the webpage controller displays a blocking UI and then sets this property to <doc://com.apple.documentation/documentation/objectivec/yes>.
--
-- ObjC selector: @- URLIsBlocked@
urlIsBlocked :: IsSTWebpageController stWebpageController => stWebpageController -> IO Bool
urlIsBlocked stWebpageController =
  sendMessage stWebpageController urlIsBlockedSelector

-- | An optional identifier for the current browsing profile.
--
-- The default value is @nil@. This identifier represents a profile and allows you to keep your browsing separate for topics like work, personal, or school. Using @nil@ will report web history without a profile identifier. Web browsers with a "default" profile may want to use @nil@ in order to match any web history reported prior to this API.
--
-- ObjC selector: @- profileIdentifier@
profileIdentifier :: IsSTWebpageController stWebpageController => stWebpageController -> IO (Id NSString)
profileIdentifier stWebpageController =
  sendMessage stWebpageController profileIdentifierSelector

-- | An optional identifier for the current browsing profile.
--
-- The default value is @nil@. This identifier represents a profile and allows you to keep your browsing separate for topics like work, personal, or school. Using @nil@ will report web history without a profile identifier. Web browsers with a "default" profile may want to use @nil@ in order to match any web history reported prior to this API.
--
-- ObjC selector: @- setProfileIdentifier:@
setProfileIdentifier :: (IsSTWebpageController stWebpageController, IsNSString value) => stWebpageController -> value -> IO ()
setProfileIdentifier stWebpageController value =
  sendMessage stWebpageController setProfileIdentifierSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setBundleIdentifier:error:@
setBundleIdentifier_errorSelector :: Selector '[Id NSString, Id NSError] Bool
setBundleIdentifier_errorSelector = mkSelector "setBundleIdentifier:error:"

-- | @Selector@ for @initWithNibName:bundle:@
initWithNibName_bundleSelector :: Selector '[Id NSString, Id NSBundle] (Id STWebpageController)
initWithNibName_bundleSelector = mkSelector "initWithNibName:bundle:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id STWebpageController)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @suppressUsageRecording@
suppressUsageRecordingSelector :: Selector '[] Bool
suppressUsageRecordingSelector = mkSelector "suppressUsageRecording"

-- | @Selector@ for @setSuppressUsageRecording:@
setSuppressUsageRecordingSelector :: Selector '[Bool] ()
setSuppressUsageRecordingSelector = mkSelector "setSuppressUsageRecording:"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

-- | @Selector@ for @setURL:@
setURLSelector :: Selector '[Id NSURL] ()
setURLSelector = mkSelector "setURL:"

-- | @Selector@ for @URLIsPlayingVideo@
urlIsPlayingVideoSelector :: Selector '[] Bool
urlIsPlayingVideoSelector = mkSelector "URLIsPlayingVideo"

-- | @Selector@ for @setURLIsPlayingVideo:@
setURLIsPlayingVideoSelector :: Selector '[Bool] ()
setURLIsPlayingVideoSelector = mkSelector "setURLIsPlayingVideo:"

-- | @Selector@ for @URLIsPictureInPicture@
urlIsPictureInPictureSelector :: Selector '[] Bool
urlIsPictureInPictureSelector = mkSelector "URLIsPictureInPicture"

-- | @Selector@ for @setURLIsPictureInPicture:@
setURLIsPictureInPictureSelector :: Selector '[Bool] ()
setURLIsPictureInPictureSelector = mkSelector "setURLIsPictureInPicture:"

-- | @Selector@ for @URLIsBlocked@
urlIsBlockedSelector :: Selector '[] Bool
urlIsBlockedSelector = mkSelector "URLIsBlocked"

-- | @Selector@ for @profileIdentifier@
profileIdentifierSelector :: Selector '[] (Id NSString)
profileIdentifierSelector = mkSelector "profileIdentifier"

-- | @Selector@ for @setProfileIdentifier:@
setProfileIdentifierSelector :: Selector '[Id NSString] ()
setProfileIdentifierSelector = mkSelector "setProfileIdentifier:"

