{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKMapItemDetailSelectionAccessoryPresentationStyle@.
module ObjC.MapKit.MKMapItemDetailSelectionAccessoryPresentationStyle
  ( MKMapItemDetailSelectionAccessoryPresentationStyle
  , IsMKMapItemDetailSelectionAccessoryPresentationStyle(..)
  , new
  , init_
  , automaticWithPresentationViewController
  , calloutWithCalloutStyle
  , sheetPresentedFromViewController
  , callout
  , openInMaps
  , automaticWithPresentationViewControllerSelector
  , calloutSelector
  , calloutWithCalloutStyleSelector
  , initSelector
  , newSelector
  , openInMapsSelector
  , sheetPresentedFromViewControllerSelector

  -- * Enum types
  , MKMapItemDetailSelectionAccessoryCalloutStyle(MKMapItemDetailSelectionAccessoryCalloutStyle)
  , pattern MKMapItemDetailSelectionAccessoryCalloutStyleAutomatic
  , pattern MKMapItemDetailSelectionAccessoryCalloutStyleFull
  , pattern MKMapItemDetailSelectionAccessoryCalloutStyleCompact

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.MapKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MKMapItemDetailSelectionAccessoryPresentationStyle)
new  =
  do
    cls' <- getRequiredClass "MKMapItemDetailSelectionAccessoryPresentationStyle"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMKMapItemDetailSelectionAccessoryPresentationStyle mkMapItemDetailSelectionAccessoryPresentationStyle => mkMapItemDetailSelectionAccessoryPresentationStyle -> IO (Id MKMapItemDetailSelectionAccessoryPresentationStyle)
init_ mkMapItemDetailSelectionAccessoryPresentationStyle =
  sendOwnedMessage mkMapItemDetailSelectionAccessoryPresentationStyle initSelector

-- | @+ automaticWithPresentationViewController:@
automaticWithPresentationViewController :: IsNSViewController presentationViewController => presentationViewController -> IO (Id MKMapItemDetailSelectionAccessoryPresentationStyle)
automaticWithPresentationViewController presentationViewController =
  do
    cls' <- getRequiredClass "MKMapItemDetailSelectionAccessoryPresentationStyle"
    sendClassMessage cls' automaticWithPresentationViewControllerSelector (toNSViewController presentationViewController)

-- | @+ calloutWithCalloutStyle:@
calloutWithCalloutStyle :: MKMapItemDetailSelectionAccessoryCalloutStyle -> IO (Id MKMapItemDetailSelectionAccessoryPresentationStyle)
calloutWithCalloutStyle style =
  do
    cls' <- getRequiredClass "MKMapItemDetailSelectionAccessoryPresentationStyle"
    sendClassMessage cls' calloutWithCalloutStyleSelector style

-- | @+ sheetPresentedFromViewController:@
sheetPresentedFromViewController :: IsNSViewController viewController => viewController -> IO (Id MKMapItemDetailSelectionAccessoryPresentationStyle)
sheetPresentedFromViewController viewController =
  do
    cls' <- getRequiredClass "MKMapItemDetailSelectionAccessoryPresentationStyle"
    sendClassMessage cls' sheetPresentedFromViewControllerSelector (toNSViewController viewController)

-- | @+ callout@
callout :: IO (Id MKMapItemDetailSelectionAccessoryPresentationStyle)
callout  =
  do
    cls' <- getRequiredClass "MKMapItemDetailSelectionAccessoryPresentationStyle"
    sendClassMessage cls' calloutSelector

-- | @+ openInMaps@
openInMaps :: IO (Id MKMapItemDetailSelectionAccessoryPresentationStyle)
openInMaps  =
  do
    cls' <- getRequiredClass "MKMapItemDetailSelectionAccessoryPresentationStyle"
    sendClassMessage cls' openInMapsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MKMapItemDetailSelectionAccessoryPresentationStyle)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MKMapItemDetailSelectionAccessoryPresentationStyle)
initSelector = mkSelector "init"

-- | @Selector@ for @automaticWithPresentationViewController:@
automaticWithPresentationViewControllerSelector :: Selector '[Id NSViewController] (Id MKMapItemDetailSelectionAccessoryPresentationStyle)
automaticWithPresentationViewControllerSelector = mkSelector "automaticWithPresentationViewController:"

-- | @Selector@ for @calloutWithCalloutStyle:@
calloutWithCalloutStyleSelector :: Selector '[MKMapItemDetailSelectionAccessoryCalloutStyle] (Id MKMapItemDetailSelectionAccessoryPresentationStyle)
calloutWithCalloutStyleSelector = mkSelector "calloutWithCalloutStyle:"

-- | @Selector@ for @sheetPresentedFromViewController:@
sheetPresentedFromViewControllerSelector :: Selector '[Id NSViewController] (Id MKMapItemDetailSelectionAccessoryPresentationStyle)
sheetPresentedFromViewControllerSelector = mkSelector "sheetPresentedFromViewController:"

-- | @Selector@ for @callout@
calloutSelector :: Selector '[] (Id MKMapItemDetailSelectionAccessoryPresentationStyle)
calloutSelector = mkSelector "callout"

-- | @Selector@ for @openInMaps@
openInMapsSelector :: Selector '[] (Id MKMapItemDetailSelectionAccessoryPresentationStyle)
openInMapsSelector = mkSelector "openInMaps"

