{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKAddressRepresentations@.
module ObjC.MapKit.MKAddressRepresentations
  ( MKAddressRepresentations
  , IsMKAddressRepresentations(..)
  , init_
  , new
  , fullAddressIncludingRegion_singleLine
  , cityWithContextUsingStyle
  , cityName
  , cityWithContext
  , regionName
  , regionCode
  , initSelector
  , newSelector
  , fullAddressIncludingRegion_singleLineSelector
  , cityWithContextUsingStyleSelector
  , cityNameSelector
  , cityWithContextSelector
  , regionNameSelector
  , regionCodeSelector

  -- * Enum types
  , MKAddressRepresentationsContextStyle(MKAddressRepresentationsContextStyle)
  , pattern MKAddressRepresentationsContextStyleAutomatic
  , pattern MKAddressRepresentationsContextStyleShort
  , pattern MKAddressRepresentationsContextStyleFull

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
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMKAddressRepresentations mkAddressRepresentations => mkAddressRepresentations -> IO (Id MKAddressRepresentations)
init_ mkAddressRepresentations  =
    sendMsg mkAddressRepresentations (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MKAddressRepresentations)
new  =
  do
    cls' <- getRequiredClass "MKAddressRepresentations"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- fullAddressIncludingRegion:singleLine:@
fullAddressIncludingRegion_singleLine :: IsMKAddressRepresentations mkAddressRepresentations => mkAddressRepresentations -> Bool -> Bool -> IO (Id NSString)
fullAddressIncludingRegion_singleLine mkAddressRepresentations  includingRegion singleLine =
    sendMsg mkAddressRepresentations (mkSelector "fullAddressIncludingRegion:singleLine:") (retPtr retVoid) [argCULong (if includingRegion then 1 else 0), argCULong (if singleLine then 1 else 0)] >>= retainedObject . castPtr

-- | @- cityWithContextUsingStyle:@
cityWithContextUsingStyle :: IsMKAddressRepresentations mkAddressRepresentations => mkAddressRepresentations -> MKAddressRepresentationsContextStyle -> IO (Id NSString)
cityWithContextUsingStyle mkAddressRepresentations  style =
    sendMsg mkAddressRepresentations (mkSelector "cityWithContextUsingStyle:") (retPtr retVoid) [argCLong (coerce style)] >>= retainedObject . castPtr

-- | @- cityName@
cityName :: IsMKAddressRepresentations mkAddressRepresentations => mkAddressRepresentations -> IO (Id NSString)
cityName mkAddressRepresentations  =
    sendMsg mkAddressRepresentations (mkSelector "cityName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- cityWithContext@
cityWithContext :: IsMKAddressRepresentations mkAddressRepresentations => mkAddressRepresentations -> IO (Id NSString)
cityWithContext mkAddressRepresentations  =
    sendMsg mkAddressRepresentations (mkSelector "cityWithContext") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- regionName@
regionName :: IsMKAddressRepresentations mkAddressRepresentations => mkAddressRepresentations -> IO (Id NSString)
regionName mkAddressRepresentations  =
    sendMsg mkAddressRepresentations (mkSelector "regionName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- regionCode@
regionCode :: IsMKAddressRepresentations mkAddressRepresentations => mkAddressRepresentations -> IO (Id NSString)
regionCode mkAddressRepresentations  =
    sendMsg mkAddressRepresentations (mkSelector "regionCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @fullAddressIncludingRegion:singleLine:@
fullAddressIncludingRegion_singleLineSelector :: Selector
fullAddressIncludingRegion_singleLineSelector = mkSelector "fullAddressIncludingRegion:singleLine:"

-- | @Selector@ for @cityWithContextUsingStyle:@
cityWithContextUsingStyleSelector :: Selector
cityWithContextUsingStyleSelector = mkSelector "cityWithContextUsingStyle:"

-- | @Selector@ for @cityName@
cityNameSelector :: Selector
cityNameSelector = mkSelector "cityName"

-- | @Selector@ for @cityWithContext@
cityWithContextSelector :: Selector
cityWithContextSelector = mkSelector "cityWithContext"

-- | @Selector@ for @regionName@
regionNameSelector :: Selector
regionNameSelector = mkSelector "regionName"

-- | @Selector@ for @regionCode@
regionCodeSelector :: Selector
regionCodeSelector = mkSelector "regionCode"

