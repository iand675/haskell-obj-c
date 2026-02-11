{-# LANGUAGE PatternSynonyms #-}
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
  , newSelector
  , initSelector
  , automaticWithPresentationViewControllerSelector
  , calloutWithCalloutStyleSelector
  , sheetPresentedFromViewControllerSelector
  , calloutSelector
  , openInMapsSelector

  -- * Enum types
  , MKMapItemDetailSelectionAccessoryCalloutStyle(MKMapItemDetailSelectionAccessoryCalloutStyle)
  , pattern MKMapItemDetailSelectionAccessoryCalloutStyleAutomatic
  , pattern MKMapItemDetailSelectionAccessoryCalloutStyleFull
  , pattern MKMapItemDetailSelectionAccessoryCalloutStyleCompact

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

import ObjC.MapKit.Internal.Classes
import ObjC.MapKit.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MKMapItemDetailSelectionAccessoryPresentationStyle)
new  =
  do
    cls' <- getRequiredClass "MKMapItemDetailSelectionAccessoryPresentationStyle"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMKMapItemDetailSelectionAccessoryPresentationStyle mkMapItemDetailSelectionAccessoryPresentationStyle => mkMapItemDetailSelectionAccessoryPresentationStyle -> IO (Id MKMapItemDetailSelectionAccessoryPresentationStyle)
init_ mkMapItemDetailSelectionAccessoryPresentationStyle  =
  sendMsg mkMapItemDetailSelectionAccessoryPresentationStyle (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ automaticWithPresentationViewController:@
automaticWithPresentationViewController :: IsNSViewController presentationViewController => presentationViewController -> IO (Id MKMapItemDetailSelectionAccessoryPresentationStyle)
automaticWithPresentationViewController presentationViewController =
  do
    cls' <- getRequiredClass "MKMapItemDetailSelectionAccessoryPresentationStyle"
    withObjCPtr presentationViewController $ \raw_presentationViewController ->
      sendClassMsg cls' (mkSelector "automaticWithPresentationViewController:") (retPtr retVoid) [argPtr (castPtr raw_presentationViewController :: Ptr ())] >>= retainedObject . castPtr

-- | @+ calloutWithCalloutStyle:@
calloutWithCalloutStyle :: MKMapItemDetailSelectionAccessoryCalloutStyle -> IO (Id MKMapItemDetailSelectionAccessoryPresentationStyle)
calloutWithCalloutStyle style =
  do
    cls' <- getRequiredClass "MKMapItemDetailSelectionAccessoryPresentationStyle"
    sendClassMsg cls' (mkSelector "calloutWithCalloutStyle:") (retPtr retVoid) [argCLong (coerce style)] >>= retainedObject . castPtr

-- | @+ sheetPresentedFromViewController:@
sheetPresentedFromViewController :: IsNSViewController viewController => viewController -> IO (Id MKMapItemDetailSelectionAccessoryPresentationStyle)
sheetPresentedFromViewController viewController =
  do
    cls' <- getRequiredClass "MKMapItemDetailSelectionAccessoryPresentationStyle"
    withObjCPtr viewController $ \raw_viewController ->
      sendClassMsg cls' (mkSelector "sheetPresentedFromViewController:") (retPtr retVoid) [argPtr (castPtr raw_viewController :: Ptr ())] >>= retainedObject . castPtr

-- | @+ callout@
callout :: IO (Id MKMapItemDetailSelectionAccessoryPresentationStyle)
callout  =
  do
    cls' <- getRequiredClass "MKMapItemDetailSelectionAccessoryPresentationStyle"
    sendClassMsg cls' (mkSelector "callout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ openInMaps@
openInMaps :: IO (Id MKMapItemDetailSelectionAccessoryPresentationStyle)
openInMaps  =
  do
    cls' <- getRequiredClass "MKMapItemDetailSelectionAccessoryPresentationStyle"
    sendClassMsg cls' (mkSelector "openInMaps") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @automaticWithPresentationViewController:@
automaticWithPresentationViewControllerSelector :: Selector
automaticWithPresentationViewControllerSelector = mkSelector "automaticWithPresentationViewController:"

-- | @Selector@ for @calloutWithCalloutStyle:@
calloutWithCalloutStyleSelector :: Selector
calloutWithCalloutStyleSelector = mkSelector "calloutWithCalloutStyle:"

-- | @Selector@ for @sheetPresentedFromViewController:@
sheetPresentedFromViewControllerSelector :: Selector
sheetPresentedFromViewControllerSelector = mkSelector "sheetPresentedFromViewController:"

-- | @Selector@ for @callout@
calloutSelector :: Selector
calloutSelector = mkSelector "callout"

-- | @Selector@ for @openInMaps@
openInMapsSelector :: Selector
openInMapsSelector = mkSelector "openInMaps"

