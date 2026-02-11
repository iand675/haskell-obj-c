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
  , willLoadMainNibSelector
  , didLoadMainNibSelector
  , sharedDictionarySelector
  , gotoPaneSelector
  , bundleSelector
  , titleSelector
  , firstPaneSelector
  , shouldLoadSelector
  , installerStateSelector
  , activePaneSelector


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
willLoadMainNib installerSection  =
  sendMsg installerSection (mkSelector "willLoadMainNib") retVoid []

-- | didLoadMainNib
--
-- Called immediatly after the default nib for the section is loaded.				If no default nib is specified, didLoadMainNib is called immediately				after willLoadMainNib is called.
--
-- didLoadMainNib is called before the section's panes are in view.
--
-- ObjC selector: @- didLoadMainNib@
didLoadMainNib :: IsInstallerSection installerSection => installerSection -> IO ()
didLoadMainNib installerSection  =
  sendMsg installerSection (mkSelector "didLoadMainNib") retVoid []

-- | sharedDictionary
--
-- A global Mutable Dictionary which is global to the Install				session. Use this dictionary to pass information between sections.
--
-- This dictionary should not be used to store state for your section or its panes.
--
-- ObjC selector: @- sharedDictionary@
sharedDictionary :: IsInstallerSection installerSection => installerSection -> IO (Id NSMutableDictionary)
sharedDictionary installerSection  =
  sendMsg installerSection (mkSelector "sharedDictionary") (retPtr retVoid) [] >>= retainedObject . castPtr

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
gotoPane installerSection  pane =
withObjCPtr pane $ \raw_pane ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg installerSection (mkSelector "gotoPane:") retCULong [argPtr (castPtr raw_pane :: Ptr ())]

-- | bundle
--
-- This method returns the NSBundle in which the InstallerSection is located.  Since InstallerSection is not				typically overriden, the bundle returned may not necessarily be the same bundle as the InstallerSection				class.
--
-- Use this method to gain access to bundle resources.
--
-- ObjC selector: @- bundle@
bundle :: IsInstallerSection installerSection => installerSection -> IO (Id NSBundle)
bundle installerSection  =
  sendMsg installerSection (mkSelector "bundle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | title
--
-- Returns the title for the section defined in the Info.plist file for the section's bundle.  The title				retrieved using the "InstallerSectionTitle" key in the Info.plist for the section's bundle and that key must				be present in the InfoPlist.strings file for title to be localized.
--
-- Although subclasses can override this method and return a dynamic title at runtime, the title is only				retrieved for display once (immediatly following the shouldLoad method, if shouldLoad returns YES).
--
-- ObjC selector: @- title@
title :: IsInstallerSection installerSection => installerSection -> IO (Id NSString)
title installerSection  =
  sendMsg installerSection (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | firstPane
--
-- Returns the first pane specified by the firstPane outlet.  This pane is the first				pane entered when the section first becomes active.
--
-- ObjC selector: @- firstPane@
firstPane :: IsInstallerSection installerSection => installerSection -> IO (Id InstallerPane)
firstPane installerSection  =
  sendMsg installerSection (mkSelector "firstPane") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | shouldLoad
--
-- Called when a section is first about to be fully loaded.  By default this method				returns YES.  A Subclass can override this method and determine at runtime if the				section makes sense.  Return NO and the section will not be further loaded.  sections are				never fully unloaded.
--
-- ObjC selector: @- shouldLoad@
shouldLoad :: IsInstallerSection installerSection => installerSection -> IO Bool
shouldLoad installerSection  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg installerSection (mkSelector "shouldLoad") retCULong []

-- | installerState
--
-- Returns the Installer State object for the current install session.
--
-- Returns an object which describes the Installer choices and status  				at the given time.  Plugins cannot influence this state, it should only 				be used for informational purposes.  See InstallerState.h for more details.
--
-- ObjC selector: @- installerState@
installerState :: IsInstallerSection installerSection => installerSection -> IO (Id InstallerState)
installerState installerSection  =
  sendMsg installerSection (mkSelector "installerState") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | activePane
--
-- Returns the current active page for this section.
--
-- If the section is active, it will return the current active page.  If the section				is not active, nil will be returned.
--
-- ObjC selector: @- activePane@
activePane :: IsInstallerSection installerSection => installerSection -> IO (Id InstallerPane)
activePane installerSection  =
  sendMsg installerSection (mkSelector "activePane") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @willLoadMainNib@
willLoadMainNibSelector :: Selector
willLoadMainNibSelector = mkSelector "willLoadMainNib"

-- | @Selector@ for @didLoadMainNib@
didLoadMainNibSelector :: Selector
didLoadMainNibSelector = mkSelector "didLoadMainNib"

-- | @Selector@ for @sharedDictionary@
sharedDictionarySelector :: Selector
sharedDictionarySelector = mkSelector "sharedDictionary"

-- | @Selector@ for @gotoPane:@
gotoPaneSelector :: Selector
gotoPaneSelector = mkSelector "gotoPane:"

-- | @Selector@ for @bundle@
bundleSelector :: Selector
bundleSelector = mkSelector "bundle"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @firstPane@
firstPaneSelector :: Selector
firstPaneSelector = mkSelector "firstPane"

-- | @Selector@ for @shouldLoad@
shouldLoadSelector :: Selector
shouldLoadSelector = mkSelector "shouldLoad"

-- | @Selector@ for @installerState@
installerStateSelector :: Selector
installerStateSelector = mkSelector "installerState"

-- | @Selector@ for @activePane@
activePaneSelector :: Selector
activePaneSelector = mkSelector "activePane"

