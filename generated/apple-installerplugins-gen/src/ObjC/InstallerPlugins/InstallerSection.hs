{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @InstallerSection@.
module ObjC.InstallerPlugins.InstallerSection
  ( InstallerSection
  , IsInstallerSection(..)
  , willLoadMainNib
  , didLoadMainNib
  , sharedDictionary
  , gotoPane
  , bundle
  , title
  , firstPane
  , shouldLoad
  , installerState
  , activePane
  , activePaneSelector
  , bundleSelector
  , didLoadMainNibSelector
  , firstPaneSelector
  , gotoPaneSelector
  , installerStateSelector
  , sharedDictionarySelector
  , shouldLoadSelector
  , titleSelector
  , willLoadMainNibSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.InstallerPlugins.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | willLoadMainNib
--
-- Each InstallerSection object can define a default nib to be loaded by the Installer at the optimal				time.  Before this default nib is loaded willLoadMainNib will be called.  didLoadMainNib is				called when the nib is successfully loaded.  The nib may be loaded way before the content				is display on the screen, so awakeFromNib methods should not assume the content is displayed				to the user.  WillEnter/DidEnter method should be used to determine when views are actually "in view."
--
-- A default nib is specified for a section in the Info.plist for the section using the NSMainNibFile key.
--
-- A subclass can override this method to do any necessary work before the main nib is loaded or				to actually load a custom nib if no default nib is specified.
--
-- ObjC selector: @- willLoadMainNib@
willLoadMainNib :: IsInstallerSection installerSection => installerSection -> IO ()
willLoadMainNib installerSection =
  sendMessage installerSection willLoadMainNibSelector

-- | didLoadMainNib
--
-- Called immediatly after the default nib for the section is loaded.				If no default nib is specified, didLoadMainNib is called immediately				after willLoadMainNib is called.
--
-- didLoadMainNib is called before the section's panes are in view.
--
-- ObjC selector: @- didLoadMainNib@
didLoadMainNib :: IsInstallerSection installerSection => installerSection -> IO ()
didLoadMainNib installerSection =
  sendMessage installerSection didLoadMainNibSelector

-- | sharedDictionary
--
-- A global Mutable Dictionary which is global to the Install				session. Use this dictionary to pass information between sections.
--
-- This dictionary should not be used to store state for your section or its panes.
--
-- ObjC selector: @- sharedDictionary@
sharedDictionary :: IsInstallerSection installerSection => installerSection -> IO (Id NSMutableDictionary)
sharedDictionary installerSection =
  sendMessage installerSection sharedDictionarySelector

-- | gotoPane:
--
-- This method causes the current pane to exit and "pane" to be made active.  This effectively replaces the current				pane and does not place the current pane onto the pane stack.
--
-- gotoPane does not invoke shouldExit method for the current pane.
--
-- gotoPane is typically not overriden by a subclass.
--
-- ObjC selector: @- gotoPane:@
gotoPane :: (IsInstallerSection installerSection, IsInstallerPane pane) => installerSection -> pane -> IO Bool
gotoPane installerSection pane =
  sendMessage installerSection gotoPaneSelector (toInstallerPane pane)

-- | bundle
--
-- This method returns the NSBundle in which the InstallerSection is located.  Since InstallerSection is not				typically overriden, the bundle returned may not necessarily be the same bundle as the InstallerSection				class.
--
-- Use this method to gain access to bundle resources.
--
-- ObjC selector: @- bundle@
bundle :: IsInstallerSection installerSection => installerSection -> IO (Id NSBundle)
bundle installerSection =
  sendMessage installerSection bundleSelector

-- | title
--
-- Returns the title for the section defined in the Info.plist file for the section's bundle.  The title				retrieved using the "InstallerSectionTitle" key in the Info.plist for the section's bundle and that key must				be present in the InfoPlist.strings file for title to be localized.
--
-- Although subclasses can override this method and return a dynamic title at runtime, the title is only				retrieved for display once (immediatly following the shouldLoad method, if shouldLoad returns YES).
--
-- ObjC selector: @- title@
title :: IsInstallerSection installerSection => installerSection -> IO (Id NSString)
title installerSection =
  sendMessage installerSection titleSelector

-- | firstPane
--
-- Returns the first pane specified by the firstPane outlet.  This pane is the first				pane entered when the section first becomes active.
--
-- ObjC selector: @- firstPane@
firstPane :: IsInstallerSection installerSection => installerSection -> IO (Id InstallerPane)
firstPane installerSection =
  sendMessage installerSection firstPaneSelector

-- | shouldLoad
--
-- Called when a section is first about to be fully loaded.  By default this method				returns YES.  A Subclass can override this method and determine at runtime if the				section makes sense.  Return NO and the section will not be further loaded.  sections are				never fully unloaded.
--
-- ObjC selector: @- shouldLoad@
shouldLoad :: IsInstallerSection installerSection => installerSection -> IO Bool
shouldLoad installerSection =
  sendMessage installerSection shouldLoadSelector

-- | installerState
--
-- Returns the Installer State object for the current install session.
--
-- Returns an object which describes the Installer choices and status  				at the given time.  Plugins cannot influence this state, it should only 				be used for informational purposes.  See InstallerState.h for more details.
--
-- ObjC selector: @- installerState@
installerState :: IsInstallerSection installerSection => installerSection -> IO (Id InstallerState)
installerState installerSection =
  sendMessage installerSection installerStateSelector

-- | activePane
--
-- Returns the current active page for this section.
--
-- If the section is active, it will return the current active page.  If the section				is not active, nil will be returned.
--
-- ObjC selector: @- activePane@
activePane :: IsInstallerSection installerSection => installerSection -> IO (Id InstallerPane)
activePane installerSection =
  sendMessage installerSection activePaneSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @willLoadMainNib@
willLoadMainNibSelector :: Selector '[] ()
willLoadMainNibSelector = mkSelector "willLoadMainNib"

-- | @Selector@ for @didLoadMainNib@
didLoadMainNibSelector :: Selector '[] ()
didLoadMainNibSelector = mkSelector "didLoadMainNib"

-- | @Selector@ for @sharedDictionary@
sharedDictionarySelector :: Selector '[] (Id NSMutableDictionary)
sharedDictionarySelector = mkSelector "sharedDictionary"

-- | @Selector@ for @gotoPane:@
gotoPaneSelector :: Selector '[Id InstallerPane] Bool
gotoPaneSelector = mkSelector "gotoPane:"

-- | @Selector@ for @bundle@
bundleSelector :: Selector '[] (Id NSBundle)
bundleSelector = mkSelector "bundle"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @firstPane@
firstPaneSelector :: Selector '[] (Id InstallerPane)
firstPaneSelector = mkSelector "firstPane"

-- | @Selector@ for @shouldLoad@
shouldLoadSelector :: Selector '[] Bool
shouldLoadSelector = mkSelector "shouldLoad"

-- | @Selector@ for @installerState@
installerStateSelector :: Selector '[] (Id InstallerState)
installerStateSelector = mkSelector "installerState"

-- | @Selector@ for @activePane@
activePaneSelector :: Selector '[] (Id InstallerPane)
activePaneSelector = mkSelector "activePane"

