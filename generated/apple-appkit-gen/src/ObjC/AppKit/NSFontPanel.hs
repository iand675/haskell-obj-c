{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSFontPanel@.
module ObjC.AppKit.NSFontPanel
  ( NSFontPanel
  , IsNSFontPanel(..)
  , setPanelFont_isMultiple
  , panelConvertFont
  , reloadDefaultFontFamilies
  , sharedFontPanel
  , sharedFontPanelExists
  , accessoryView
  , setAccessoryView
  , worksWhenModal
  , setWorksWhenModal
  , enabled
  , setEnabled
  , accessoryViewSelector
  , enabledSelector
  , panelConvertFontSelector
  , reloadDefaultFontFamiliesSelector
  , setAccessoryViewSelector
  , setEnabledSelector
  , setPanelFont_isMultipleSelector
  , setWorksWhenModalSelector
  , sharedFontPanelExistsSelector
  , sharedFontPanelSelector
  , worksWhenModalSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- setPanelFont:isMultiple:@
setPanelFont_isMultiple :: (IsNSFontPanel nsFontPanel, IsNSFont fontObj) => nsFontPanel -> fontObj -> Bool -> IO ()
setPanelFont_isMultiple nsFontPanel fontObj flag =
  sendMessage nsFontPanel setPanelFont_isMultipleSelector (toNSFont fontObj) flag

-- | @- panelConvertFont:@
panelConvertFont :: (IsNSFontPanel nsFontPanel, IsNSFont fontObj) => nsFontPanel -> fontObj -> IO (Id NSFont)
panelConvertFont nsFontPanel fontObj =
  sendMessage nsFontPanel panelConvertFontSelector (toNSFont fontObj)

-- | @- reloadDefaultFontFamilies@
reloadDefaultFontFamilies :: IsNSFontPanel nsFontPanel => nsFontPanel -> IO ()
reloadDefaultFontFamilies nsFontPanel =
  sendMessage nsFontPanel reloadDefaultFontFamiliesSelector

-- | @+ sharedFontPanel@
sharedFontPanel :: IO (Id NSFontPanel)
sharedFontPanel  =
  do
    cls' <- getRequiredClass "NSFontPanel"
    sendClassMessage cls' sharedFontPanelSelector

-- | @+ sharedFontPanelExists@
sharedFontPanelExists :: IO Bool
sharedFontPanelExists  =
  do
    cls' <- getRequiredClass "NSFontPanel"
    sendClassMessage cls' sharedFontPanelExistsSelector

-- | @- accessoryView@
accessoryView :: IsNSFontPanel nsFontPanel => nsFontPanel -> IO (Id NSView)
accessoryView nsFontPanel =
  sendMessage nsFontPanel accessoryViewSelector

-- | @- setAccessoryView:@
setAccessoryView :: (IsNSFontPanel nsFontPanel, IsNSView value) => nsFontPanel -> value -> IO ()
setAccessoryView nsFontPanel value =
  sendMessage nsFontPanel setAccessoryViewSelector (toNSView value)

-- | @- worksWhenModal@
worksWhenModal :: IsNSFontPanel nsFontPanel => nsFontPanel -> IO Bool
worksWhenModal nsFontPanel =
  sendMessage nsFontPanel worksWhenModalSelector

-- | @- setWorksWhenModal:@
setWorksWhenModal :: IsNSFontPanel nsFontPanel => nsFontPanel -> Bool -> IO ()
setWorksWhenModal nsFontPanel value =
  sendMessage nsFontPanel setWorksWhenModalSelector value

-- | @- enabled@
enabled :: IsNSFontPanel nsFontPanel => nsFontPanel -> IO Bool
enabled nsFontPanel =
  sendMessage nsFontPanel enabledSelector

-- | @- setEnabled:@
setEnabled :: IsNSFontPanel nsFontPanel => nsFontPanel -> Bool -> IO ()
setEnabled nsFontPanel value =
  sendMessage nsFontPanel setEnabledSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setPanelFont:isMultiple:@
setPanelFont_isMultipleSelector :: Selector '[Id NSFont, Bool] ()
setPanelFont_isMultipleSelector = mkSelector "setPanelFont:isMultiple:"

-- | @Selector@ for @panelConvertFont:@
panelConvertFontSelector :: Selector '[Id NSFont] (Id NSFont)
panelConvertFontSelector = mkSelector "panelConvertFont:"

-- | @Selector@ for @reloadDefaultFontFamilies@
reloadDefaultFontFamiliesSelector :: Selector '[] ()
reloadDefaultFontFamiliesSelector = mkSelector "reloadDefaultFontFamilies"

-- | @Selector@ for @sharedFontPanel@
sharedFontPanelSelector :: Selector '[] (Id NSFontPanel)
sharedFontPanelSelector = mkSelector "sharedFontPanel"

-- | @Selector@ for @sharedFontPanelExists@
sharedFontPanelExistsSelector :: Selector '[] Bool
sharedFontPanelExistsSelector = mkSelector "sharedFontPanelExists"

-- | @Selector@ for @accessoryView@
accessoryViewSelector :: Selector '[] (Id NSView)
accessoryViewSelector = mkSelector "accessoryView"

-- | @Selector@ for @setAccessoryView:@
setAccessoryViewSelector :: Selector '[Id NSView] ()
setAccessoryViewSelector = mkSelector "setAccessoryView:"

-- | @Selector@ for @worksWhenModal@
worksWhenModalSelector :: Selector '[] Bool
worksWhenModalSelector = mkSelector "worksWhenModal"

-- | @Selector@ for @setWorksWhenModal:@
setWorksWhenModalSelector :: Selector '[Bool] ()
setWorksWhenModalSelector = mkSelector "setWorksWhenModal:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[Bool] ()
setEnabledSelector = mkSelector "setEnabled:"

