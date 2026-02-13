{-# LANGUAGE DataKinds #-}
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
  , choiceDictionariesSelector
  , choiceDictionaryForIdentifierSelector
  , installStartedSelector
  , installSucceededSelector
  , licenseAgreedLanguageSelector
  , licenseAgreedSelector
  , targetPathSelector
  , targetVolumePathSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
choiceDictionaryForIdentifier installerState choiceIdentifier =
  sendMessage installerState choiceDictionaryForIdentifierSelector (toNSString choiceIdentifier)

-- | licenseAgreed
--
-- Specifies the user agreed to the license, if there is no license, this will return NO.
--
-- ObjC selector: @- licenseAgreed@
licenseAgreed :: IsInstallerState installerState => installerState -> IO Bool
licenseAgreed installerState =
  sendMessage installerState licenseAgreedSelector

-- | licenseAgreedLanguage
--
-- Specifies the language the language was last viewed or agreed with.
--
-- ObjC selector: @- licenseAgreedLanguage@
licenseAgreedLanguage :: IsInstallerState installerState => installerState -> IO (Id NSString)
licenseAgreedLanguage installerState =
  sendMessage installerState licenseAgreedLanguageSelector

-- | targetVolumePath
--
-- Specifies the mount point of the selected target
--
-- Only Available after target has been selected.
--
-- ObjC selector: @- targetVolumePath@
targetVolumePath :: IsInstallerState installerState => installerState -> IO (Id NSString)
targetVolumePath installerState =
  sendMessage installerState targetVolumePathSelector

-- | targetPath
--
-- Full target path selected.
--
-- Specifies the full path selected by the user.  This path contains the targetVolumePath.
--
-- ObjC selector: @- targetPath@
targetPath :: IsInstallerState installerState => installerState -> IO (Id NSString)
targetPath installerState =
  sendMessage installerState targetPathSelector

-- | choiceDictionaries
--
-- Returns an array of choice dictionaries.
--
-- Each choice dictionary contains the keys InstallerState_Choice_Identifier,InstallerState_Choice_Installed, and optionally 				InstallerState_Choice_CustomLocation.  These keys specify a choice and whether they were installed or not.  This is only 				available after choice selections have been made.
--
-- ObjC selector: @- choiceDictionaries@
choiceDictionaries :: IsInstallerState installerState => installerState -> IO (Id NSArray)
choiceDictionaries installerState =
  sendMessage installerState choiceDictionariesSelector

-- | installStarted
--
-- Specifies if the install process has started or not.
--
-- Will return YES after an install has been initiated.  If YES is returned, you can assume the install has taken 				place.
--
-- ObjC selector: @- installStarted@
installStarted :: IsInstallerState installerState => installerState -> IO Bool
installStarted installerState =
  sendMessage installerState installStartedSelector

-- | installSucceeded
--
-- Specifies if the install was successfull or not.
--
-- This value is only valid if installStarted returns True.
--
-- ObjC selector: @- installSucceeded@
installSucceeded :: IsInstallerState installerState => installerState -> IO Bool
installSucceeded installerState =
  sendMessage installerState installSucceededSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @choiceDictionaryForIdentifier:@
choiceDictionaryForIdentifierSelector :: Selector '[Id NSString] (Id NSDictionary)
choiceDictionaryForIdentifierSelector = mkSelector "choiceDictionaryForIdentifier:"

-- | @Selector@ for @licenseAgreed@
licenseAgreedSelector :: Selector '[] Bool
licenseAgreedSelector = mkSelector "licenseAgreed"

-- | @Selector@ for @licenseAgreedLanguage@
licenseAgreedLanguageSelector :: Selector '[] (Id NSString)
licenseAgreedLanguageSelector = mkSelector "licenseAgreedLanguage"

-- | @Selector@ for @targetVolumePath@
targetVolumePathSelector :: Selector '[] (Id NSString)
targetVolumePathSelector = mkSelector "targetVolumePath"

-- | @Selector@ for @targetPath@
targetPathSelector :: Selector '[] (Id NSString)
targetPathSelector = mkSelector "targetPath"

-- | @Selector@ for @choiceDictionaries@
choiceDictionariesSelector :: Selector '[] (Id NSArray)
choiceDictionariesSelector = mkSelector "choiceDictionaries"

-- | @Selector@ for @installStarted@
installStartedSelector :: Selector '[] Bool
installStartedSelector = mkSelector "installStarted"

-- | @Selector@ for @installSucceeded@
installSucceededSelector :: Selector '[] Bool
installSucceededSelector = mkSelector "installSucceeded"

