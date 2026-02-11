{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | InstallerSection
--
-- An object representing a specific Section of the Installer's UI.
--
-- The InstallerSection class declares an interface for a section within the Installer's UI.
--
-- The InstallerSection is the main controller of this section and contains all the panes which				are actually displayed in the Installer's UI.  The InstallerSection itself does not display				anything, but rather provides the InstallerPanes which do.
--
-- Each InstallerSection (or subclass) must be within its own bundle.  The NSPrincipalClass for				this bundle must be specified as an InstallerSection or a subclass.
--
-- Typically an InstallerSection is not subclassed because most of the functionality can be				provided through the Info.plist and the default nib.
--
-- The default nib for a section is specified by the NSMainNibFile key in the Info.plist				for the section's bundle.
--
-- The title for the section is specified by the InstallerSectionTitle key.
--
-- Generated bindings for @InstallerState@.
module ObjC.InstallerPlugins.InstallerState
  ( InstallerState
  , IsInstallerState(..)
  , choiceDictionaryForIdentifier
  , licenseAgreed
  , licenseAgreedLanguage
  , targetVolumePath
  , targetPath
  , choiceDictionaries
  , installStarted
  , installSucceeded
  , choiceDictionaryForIdentifierSelector
  , licenseAgreedSelector
  , licenseAgreedLanguageSelector
  , targetVolumePathSelector
  , targetPathSelector
  , choiceDictionariesSelector
  , installStartedSelector
  , installSucceededSelector


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

import ObjC.InstallerPlugins.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | choiceDictionaryForIdentifier:
--
-- Retrieves choice dictionaries by identifier.
--
-- See choiceDictionaries for the values returned.
--
-- ObjC selector: @- choiceDictionaryForIdentifier:@
choiceDictionaryForIdentifier :: (IsInstallerState installerState, IsNSString choiceIdentifier) => installerState -> choiceIdentifier -> IO (Id NSDictionary)
choiceDictionaryForIdentifier installerState  choiceIdentifier =
withObjCPtr choiceIdentifier $ \raw_choiceIdentifier ->
    sendMsg installerState (mkSelector "choiceDictionaryForIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_choiceIdentifier :: Ptr ())] >>= retainedObject . castPtr

-- | licenseAgreed
--
-- Specifies the user agreed to the license, if there is no license, this will return NO.
--
-- ObjC selector: @- licenseAgreed@
licenseAgreed :: IsInstallerState installerState => installerState -> IO Bool
licenseAgreed installerState  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg installerState (mkSelector "licenseAgreed") retCULong []

-- | licenseAgreedLanguage
--
-- Specifies the language the language was last viewed or agreed with.
--
-- ObjC selector: @- licenseAgreedLanguage@
licenseAgreedLanguage :: IsInstallerState installerState => installerState -> IO (Id NSString)
licenseAgreedLanguage installerState  =
  sendMsg installerState (mkSelector "licenseAgreedLanguage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | targetVolumePath
--
-- Specifies the mount point of the selected target
--
-- Only Available after target has been selected.
--
-- ObjC selector: @- targetVolumePath@
targetVolumePath :: IsInstallerState installerState => installerState -> IO (Id NSString)
targetVolumePath installerState  =
  sendMsg installerState (mkSelector "targetVolumePath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | targetPath
--
-- Full target path selected.
--
-- Specifies the full path selected by the user.  This path contains the targetVolumePath.
--
-- ObjC selector: @- targetPath@
targetPath :: IsInstallerState installerState => installerState -> IO (Id NSString)
targetPath installerState  =
  sendMsg installerState (mkSelector "targetPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | choiceDictionaries
--
-- Returns an array of choice dictionaries.
--
-- Each choice dictionary contains the keys InstallerState_Choice_Identifier,InstallerState_Choice_Installed, and optionally 				InstallerState_Choice_CustomLocation.  These keys specify a choice and whether they were installed or not.  This is only 				available after choice selections have been made.
--
-- ObjC selector: @- choiceDictionaries@
choiceDictionaries :: IsInstallerState installerState => installerState -> IO (Id NSArray)
choiceDictionaries installerState  =
  sendMsg installerState (mkSelector "choiceDictionaries") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | installStarted
--
-- Specifies if the install process has started or not.
--
-- Will return YES after an install has been initiated.  If YES is returned, you can assume the install has taken 				place.
--
-- ObjC selector: @- installStarted@
installStarted :: IsInstallerState installerState => installerState -> IO Bool
installStarted installerState  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg installerState (mkSelector "installStarted") retCULong []

-- | installSucceeded
--
-- Specifies if the install was successfull or not.
--
-- This value is only valid if installStarted returns True.
--
-- ObjC selector: @- installSucceeded@
installSucceeded :: IsInstallerState installerState => installerState -> IO Bool
installSucceeded installerState  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg installerState (mkSelector "installSucceeded") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @choiceDictionaryForIdentifier:@
choiceDictionaryForIdentifierSelector :: Selector
choiceDictionaryForIdentifierSelector = mkSelector "choiceDictionaryForIdentifier:"

-- | @Selector@ for @licenseAgreed@
licenseAgreedSelector :: Selector
licenseAgreedSelector = mkSelector "licenseAgreed"

-- | @Selector@ for @licenseAgreedLanguage@
licenseAgreedLanguageSelector :: Selector
licenseAgreedLanguageSelector = mkSelector "licenseAgreedLanguage"

-- | @Selector@ for @targetVolumePath@
targetVolumePathSelector :: Selector
targetVolumePathSelector = mkSelector "targetVolumePath"

-- | @Selector@ for @targetPath@
targetPathSelector :: Selector
targetPathSelector = mkSelector "targetPath"

-- | @Selector@ for @choiceDictionaries@
choiceDictionariesSelector :: Selector
choiceDictionariesSelector = mkSelector "choiceDictionaries"

-- | @Selector@ for @installStarted@
installStartedSelector :: Selector
installStartedSelector = mkSelector "installStarted"

-- | @Selector@ for @installSucceeded@
installSucceededSelector :: Selector
installSucceededSelector = mkSelector "installSucceeded"

